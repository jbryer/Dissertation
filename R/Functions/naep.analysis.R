#' Perform propensity score analysis on NAEP using charter schools as treatement.
#' This is the workhourse function for performing the nine approaches to
#' propensity score analysis. Specifically, 
#' -Stratification
#'     -Logistic regression
#'     -Logistic regression with step AIC
#'     -Classification tree
#' -Matcing
#'     -One-to-one
#'     -One-to-five
#'     -One-to-ten
#' -Multilevel PSA
#'     -Logistic regression
#'     -Logistic regression with step AIC
#'     -Classification tree
#' 
#' Tables and figures will be saved with a filename starting with grade and subject.
#' For example, grade 4 math tables and figures will start with g4math.
#' 
#' @param naep the full NAEP data frame.
#' @param complete data frame of covariates and treatement variable with missing
#'        data imputed. See the impute.XXXX.R files for details on imputation.
#' @param score vector with the outcome variable.
#' @param grade the grade (either 4 or 8).
#' @param subject the subject (either math or read).
#' @param dir.fig directory to save figures.
#' @param dir.tab directory to save tables.
#' @param return a data frame with one row per method including the mean difference
#'        and confidence interval.
naep.analysis <- function(naep, complete, catalog, score, grade, subject,
						  dir.fig='../Figures2009',
						  dir.tab='../Tables2009',
						  dir.data='../Data2009') {
	naep$state <- as.character(factor(naep$FIPS, levels=as.character(states$name), 
						 labels=as.character(states$abbr)))
	
	cachefile <- paste0(dir.data, '/g', grade, subject, '.analysis.Rdata')
	if(file.exists(cachefile)) {
		load(cachefile)
	} else {
		message('PSA with stratification...')
		lr <- glm(charter ~ ., data=complete, family='binomial')
		lr.aic <- stepAIC(lr)
		tree <- ctree(charter ~ ., data=naep[,names(complete)])
		
		message('Matching...')
		exactMatch <- list(naep$state, complete$SRACE, complete$DSEX)
		ps <- fitted(lr)
		treat <- complete$charter
		
		match1 <- Matchby(Y=score, Tr=treat, X=ps, by=exactMatch, M=1, replace=FALSE)
		match5 <- Matchby(Y=score, Tr=treat, X=ps, by=exactMatch, M=5, replace=FALSE)
		match10 <- Matchby(Y=score, Tr=treat, X=ps, by=exactMatch, M=10, replace=FALSE)
	
		message('Multilevel PSA...')
		complete$state <- naep$state
		ml.data1 <- naep[,names(complete)]
		ml.data1$state <- naep$state
		
		ml.ctree <- mlpsa.ctree(ml.data1, charter ~ ., 'state')
		ml.lr <- mlpsa.logistic(complete, charter ~ ., 'state', stepAIC=FALSE)
		ml.lrAIC <- mlpsa.logistic(complete, charter ~ ., 'state', stepAIC=TRUE)
		
		ml.ctree.strata <- getStrata(ml.ctree, naep, level2='state')
		ml.lr.ps <- getPropensityScores(ml.lr)
		ml.lrAIC.ps <- getPropensityScores(ml.lrAIC)
			
		covariates = ml.data1[,!names(ml.data1) %in% c('charter','state')]
		covars <- catalog[catalog$FieldName %in% all.covars,c('FieldName','Description')]
		covars <- covars[covars$FieldName %in% names(covariates),]
		tmp <- covariates[,covars$FieldName]
		names(tmp) <- covars$Description
		ml.ctree.balance <- covariate.balance(covariates = tmp,
											  treatment = ml.ctree.strata$charter,
											  level2 = ml.ctree.strata$state,
										      strata = ml.ctree.strata$strata )
		
		
		covariates = complete[,!names(complete) %in% c('charter','state')]
		covars <- catalog[catalog$FieldName %in% all.covars,c('FieldName','Description')]
		covars <- covars[covars$FieldName %in% names(covariates),]
		tmp <- covariates[,covars$FieldName]
		names(tmp) <- covars$Description
		ml.lr.balance <- covariate.balance(covariates = tmp,
										      treatment = complete$charter,
										      level2 = complete$state,
										      strata = ml.lr.ps$strata)
		ml.lrAIC.balance <- covariate.balance(covariates = tmp,
											  treatment = complete$charter,
											  level2 = complete$state,
										      strata = ml.lrAIC.ps$strata)
		
		ml.ctree.result <- mlpsa(score, naep$charter, ml.ctree.strata$strata, naep$state)
		ml.lr.result <- mlpsa(score, naep$charter, ml.lr.ps$strata, naep$state)
		ml.lrAIC.result <- mlpsa(score, naep$charter, ml.lrAIC.ps$strata, naep$state)
		
		save(lr, lr.aic, tree,
			 match1, match5, match10,
			 ml.ctree, ml.lr, ml.lrAIC, ml.ctree.strata, ml.lr.ps, ml.lrAIC.ps,
			 ml.ctree.result, ml.lr.result, ml.lrAIC.result,
			 ml.ctree.balance, ml.lr.balance, ml.lrAIC.balance,
			 file=cachefile)
	}
	
	df <- data.frame(ps=fitted(lr),
					 psAIC=fitted(lr.aic),
					 score=score, 
					 charter=naep$charter,
					 leaf=where(tree) )
	df$strata5 <- cut(df$ps, breaks=quantile(df$ps, seq(0,1,1/5), na.rm=TRUE), 
					  labels=1:5, include.lowest=TRUE)
	df$strata10 <- cut(df$ps, breaks=quantile(df$ps, seq(0,1,1/10), na.rm=TRUE), 
					   labels=1:10, include.lowest=TRUE)
	df$strata5AIC <- cut(df$psAIC, breaks=quantile(df$psAIC, seq(0,1,1/5), na.rm=TRUE), 
						 labels=1:5, include.lowest=TRUE)
	df$strata10AIC <- cut(df$psAIC, breaks=quantile(df$psAIC, seq(0,1,1/10), na.rm=TRUE), 
						  labels=1:10, include.lowest=TRUE)
	
	# Balance plots
# 	message('Saving balance plot...')
# 	pdf(paste0(dir.fig, '/g', grade, subject, '-balance.pdf'))
# 	cv.bal.psa(cv.trans.psa(complete[,!names(complete) %in% c('charter','state')]), 
# 			   complete$charter, df$ps, strata=df$strata5)
# 	dev.off()
		
	# Loess plots
	message('Saving loess plots...')
	pdf(paste0(dir.fig, '/g', grade, subject, '-loess.pdf'), width=12.0, height=8.0)
	loess.plot(df$ps, response=score, treatment=df$charter, 
			   responseTitle=paste0('Grade ', grade, ' ', subject, ' Score'), 
			   treatmentTitle='Charter School',
			   percentPoints.control=0.05, 
			   percentPoints.treat=0.4)
	dev.off()
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-loessAIC.pdf'), width=12.0, height=8.0)
	loess.plot(df$psAIC, response=score, treatment=df$charter, 
			   responseTitle=paste0('Grade ', grade, ' ', subject, ' Score'), 
			   treatmentTitle='Charter School',
			   percentPoints.control=0.05, 
			   percentPoints.treat=0.4)
	dev.off()
	
	##### Stratification ###########################################################
	ggplot(df, aes(y=score, x=charter, colour=charter)) + geom_boxplot() + 
		facet_wrap(~ strata5, ncol=1) + coord_flip() + xlab(NULL) + 
		ylab(paste0('Grade ', grade, ' ', subject, ' Score')) + scale_colour_hue('Charter School')
	ggsave(paste0(dir.fig, '/g', grade, subject, '-strata5-boxplot.pdf'))
	
	ggplot(df, aes(y=score, x=charter, colour=charter)) + geom_boxplot() + 
		facet_wrap(~ strata10, ncol=1) + coord_flip() + xlab(NULL) + 
		ylab(paste0('Grade ', grade, ' ', subject, ' Score')) + scale_colour_hue('Charter School')
	ggsave(paste0(dir.fig, '/g', grade, subject, '-strata10-boxplot.pdf'))
	
	ggplot(df, aes(y=score, x=charter, colour=charter)) + geom_boxplot() + 
		facet_wrap(~ strata5AIC, ncol=1) + coord_flip() + xlab(NULL) + 
		ylab(paste0('Grade ', grade, ' ', subject, ' Score')) + scale_colour_hue('Charter School')
	ggsave(paste0(dir.fig, '/g', grade, subject, '-strata5AIC-boxplot.pdf'))
	
	ggplot(df, aes(y=score, x=charter, colour=charter)) + geom_boxplot() + 
		facet_wrap(~ strata10AIC, ncol=1) + coord_flip() + xlab(NULL) + 
		ylab(paste0('Grade ', grade, ' ', subject, ' Score')) + scale_colour_hue('Charter School')
	ggsave(paste0(dir.fig, '/g', grade, subject, '-strata10AIC-boxplot.pdf'))
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-circpsa5.pdf'))
	strata5.results = circ.psa(df$score, df$charter, df$strata5, revc=TRUE)
	dev.off()
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-circpsa10.pdf'))
	strata10.results = circ.psa(df$score, df$charter, df$strata10, revc=TRUE)
	dev.off()
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-circpsa5-AIC.pdf'))
	strata5AIC.results = circ.psa(df$score, df$charter, df$strata5AIC, revc=TRUE)
	dev.off()
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-circpsa10-AIC.pdf'))
	strata10AIC.results = circ.psa(df$score, df$charter, df$strata10AIC, revc=TRUE)
	dev.off()
	
	test <- as.data.frame(table(df$leaf, df$charter))
	badleaves <- test[test$Freq < 1,]$Var1
	if(length(badleaves) > 0) {
		df2 <- df[!df$leaf %in% badleaves,]
	} else {
		df2 <- df
	}
	pdf(paste0(dir.fig, '/g', grade, subject, '-circpsa-tree.pdf'))
	tree.results <- circ.psa(df2$score, df2$charter, df2$leaf, revc=TRUE)
	dev.off()
	
	##### Matching #################################################################
	df1 <- data.frame(state=naep[match1$index.control,]$FIPS02,
					  public=score[match1$index.control], 
					  charter=score[match1$index.treat])
	t1 = t.test(df1$charter, df1$public, paired=TRUE)
	
	df5 <- data.frame(state=naep[match5$index.control,]$FIPS02,
					  public=score[match5$index.control], 
					  charter=score[match5$index.treat])
	t5 = t.test(df5$charter, df5$public, paired=TRUE)
	
	df10 <- data.frame(state=naep[match10$index.control,]$FIPS02,
					   public=score[match10$index.control], 
					   charter=score[match10$index.treat])
	t10 = t.test(df10$charter, df10$public, paired=TRUE)
	
	##### Multilevel PSA ###########################################################
	
	#summary(ml.ctree.result)
	#summary(ml.lr.result)
	#summary(ml.lrAIC.result)
	dev.off()
		
	# Balance plots
 	plot(ml.ctree.balance)
 	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-ctree-balance.pdf'), width=11, height=8.5)

	plot(ml.lr.balance)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lr-balance.pdf'), width=11, height=8.5)
	
	plot(ml.lrAIC.balance)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lrAIC-balance.pdf'), width=11, height=8.5)
	
	# Tree heat map
	tree.plot(ml.ctree, level2Col=naep$state)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-ctree-heat.pdf'), width=8, height=8)
	
	# mlpsa plots
	pdf(paste0(dir.fig, '/g', grade, subject, '-mlpsa-ctree.pdf'), width=8, height=8)
	plot(ml.ctree.result, ratio=c(2,3), axis.text.size=6)
	dev.off()
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lr.pdf'), width=8, height=8)
	plot(ml.lr.result, ratio=c(2,3), axis.text.size=6)
	dev.off()
	
	pdf(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lrAIC.pdf'), width=8, height=8)
	plot(ml.lrAIC.result, ratio=c(2,3), axis.text.size=6)
	dev.off()
	
	# Circ and diff plots
	mlpsa.circ.plot(ml.ctree.result) + coord_flip() + theme(aspect.ratio=1)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-ctree-circ.pdf'))
	
	mlpsa.difference.plot(ml.ctree.result)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-ctree-diff.pdf'))
	
	mlpsa.circ.plot(ml.lr.result) + coord_flip() + theme(aspect.ratio=1)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lr-circ.pdf'))

	mlpsa.difference.plot(ml.lr.result)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lr-diff.pdf'))
	
	mlpsa.circ.plot(ml.lrAIC.result) + coord_flip() + theme(aspect.ratio=1)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lrAIC-circ.pdf'))
	
	mlpsa.difference.plot(ml.lrAIC.result)
	ggsave(paste0(dir.fig, '/g', grade, subject, '-mlpsa-lrAIC-diff.pdf'))
	
	x <- xtable(ml.ctree.result, label=paste0('g', grade, subject, '-mlpsa-ctree'), display=NULL,
				caption=paste0('Multilevel PSA Results using Conditional Inference Trees: Grade ', grade, ' ', subject))
	print(x, caption.placement='top', file=paste0(dir.tab, '/g', grade, subject, '-mlpsa-ctree.tex'))
	
	x <- xtable(ml.lr.result, label=paste0('g', grade, subject, '-mlpsa-lr'), display=NULL,
				caption=paste0('Multilevel PSA Results using Logistic Regression: Grade ', grade, ' ', subject))
	print(x, caption.placement='top', file=paste0(dir.tab, '/g', grade, subject, '-mlpsa-lr.tex'))
	
	x <- xtable(ml.lrAIC.result, label=paste0('g', grade, subject, '-mlpsa-lrAIC'), display=NULL,
				caption=paste0('Multilevel PSA Results using Logistic Regression AIC: Grade ', grade, ' ', subject))
	print(x, caption.placement='top', file=paste0(dir.tab, '/g', grade, subject, '-mlpsa-lrAIC.tex'))
	
	overall <- rbind(
		data.frame(class='Stratification',
				   method='Logistic Regression',
				   charter=strata10.results$wtd.Mn.TRUE,
				   public=strata10.results$wtd.Mn.FALSE,
				   diff=strata10.results$wtd.Mn.TRUE - strata10.results$wtd.Mn.FALSE,
				   ci.min=strata10.results$CI.95[2] * -1,
				   ci.max=strata10.results$CI.95[1]* -1),
		data.frame(class='Stratification',
				   method='Logistic Regression AIC',
				   charter=strata10AIC.results$wtd.Mn.TRUE,
				   public=strata10AIC.results$wtd.Mn.FALSE,
				   diff=strata10AIC.results$wtd.Mn.TRUE - strata10AIC.results$wtd.Mn.FALSE,
				   ci.min=strata10AIC.results$CI.95[2]* -1,
				   ci.max=strata10AIC.results$CI.95[1]* -1),
		data.frame(class='Stratification',
				   method='Classification Tree',
				   charter=tree.results$wtd.Mn.TRUE,
				   public=tree.results$wtd.Mn.FALSE,
				   diff=tree.results$wtd.Mn.TRUE - tree.results$wtd.Mn.FALSE,
				   ci.min=tree.results$CI.95[2]* -1,
				   ci.max=tree.results$CI.95[1]* -1),
		data.frame(class='Matching',
				   method='One-to-One',
				   charter=mean(df1$charter),
				   public=mean(df1$public),
				   diff=t1$estimate,
				   ci.min=t1$conf.int[1],
				   ci.max=t1$conf.int[2]),
		data.frame(class='Matching',
				   method='One-to-Five',
				   charter=mean(df5$charter),
				   public=mean(df5$public),
				   diff=t5$estimate,
				   ci.min=t5$conf.int[1],
				   ci.max=t5$conf.int[2]),
		data.frame(class='Matching',
				   method='One-to-Ten',
				   charter=mean(df10$charter),
				   public=mean(df10$public),
				   diff=t10$estimate,
				   ci.min=t10$conf.int[1],
				   ci.max=t10$conf.int[2]),
		data.frame(class='Multilevel PSA',
				   method='Logistic Regression',
				   charter=ml.lr.result$overall.mnx,
				   public=ml.lr.result$overall.mny,
				   diff=ml.lr.result$overall.wtd,
				   ci.min=ml.lr.result$overall.ci[2],
				   ci.max=ml.lr.result$overall.ci[1]),
		data.frame(class='Multilevel PSA',
				   method='Logistic Regression AIC',
				   charter=ml.lrAIC.result$overall.mnx,
				   public=ml.lrAIC.result$overall.mny,
				   diff=ml.lrAIC.result$overall.wtd,
				   ci.min=ml.lrAIC.result$overall.ci[2],
				   ci.max=ml.lrAIC.result$overall.ci[1]),
		data.frame(class='Multilevel PSA',
				   method='Classification Trees',
				   charter=ml.ctree.result$overall.mnx,
				   public=ml.ctree.result$overall.mny,
				   diff=ml.ctree.result$overall.wtd,
				   ci.min=ml.ctree.result$overall.ci[2],
				   ci.max=ml.ctree.result$overall.ci[1])
	)
	row.names(overall) <- 1:nrow(overall)
	return(overall)
}
