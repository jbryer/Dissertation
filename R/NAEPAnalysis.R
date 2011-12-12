naepAnalysis <- function(grade, subject, ask=FALSE) {
	par(ask=ask)
	
	results = data.frame()
	
	printSummary <- function(results) {
		print(paste('Public:   ', results$overall.mnx, sep=''))
		print(paste('Charter:  ', results$overall.mny, sep=''))
		print(paste('SE:       ', results$overall.se.wtd, sep=''))
		print(paste('ATE:      ', results$overall.wtd, sep=''))
		print(paste('approx t: ', results$approx.t, sep=''))
		print(paste('DF:       ', sum(results$level2.summary$df), sep=''))
		print(paste('CI:       ', results$overall.ci, sep=''))
		
		return(data.frame(Public=results$overall.mnx,
			Charter=results$overall.mny,
			SE=results$overall.se.wtd,
			ATE=results$overall.wtd,
			approxt=results$approx.t,
			DF=sum(results$level2.summary$df),
			CImin=results$overall.ci[1],
			CImax=results$overall.ci[2]))
	}
	
	df = get(paste('g', grade, subject, sep=''))
	complete = get(paste('g', grade, subject, '.complete', sep=''))
	formula = get(paste('g', grade, 'formula', sep=''))
	
	lr.filename = dataLocation(paste('g', grade, subject, '.core.lr.Rdata', sep=''))
	if(file.exists(lr.filename)) {
		load(lr.filename)
		lr.results = get(paste('lr.g', grade, subject, '.results', sep=''))
		lr.results.aic = get(paste('lr.g', grade, subject, '.results.aic', sep=''))
	} else {
		stop(paste(lr.filename, 'does not exist'))
	}
	
	print('Logistic regression full model:')
	ps = getPropensityScores(lr.results)
	complete$ps = NA
	complete[rownames(ps),]$ps = ps$ps
	complete$strata5 = cut(complete$ps, quantile(complete$ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
	#complete$strata5 = cut(complete$ps, fivenum(complete$ps), include.lowest=TRUE, labels=FALSE)
	#complete$strata5 = cut(complete$ps, breaks=seq(min(complete$ps), max(complete$ps), ((max(complete$ps)-min(complete$ps))/5)), include.lowest=TRUE, labels=FALSE)
	complete$state = as.factor(as.character(complete$state))
	pdf(paste('Figures/g', grade, subject, 'multilevellrloess.pdf', sep=''))
	print(xyplot(complete[,paste(subject,'score',sep='')] ~ complete$ps | abbreviateState(complete$state), group=complete$charter, panel=panel.plsmo, type='l', ylab=paste(subject, grade, 'Score'), xlab='Propensity Score', label.curves=TRUE, cex=.5))
	dev.off()
	#print('Cell sizes...')
	#print(table(complete$state, complete$strata5, complete$charter, useNA='ifany'))
	lr.psa.results = multilevelPSA(response=complete[,paste(subject,'score',sep='')], treatment=complete$charter, strata=complete$strata5, level2=complete$state, minN=4)
	plotpsa.multilevel.psa(multilevelPSA=lr.psa.results, level1.points=TRUE, ylab='State', jitter=FALSE) + opts(axis.text.y=theme_text(size=8, hjust=1)) + ylab(paste('Difference Score (', lr.psa.results$y.label, ' - ', lr.psa.results$x.label, ')', sep='')) + opts(legend.position=c(-1,-1))
	ggsave(paste('Figures/g', grade, subject, 'lrdiffplot.pdf', sep=''), width=8, height=6)
	plotcirc.multilevel.psa(lr.psa.results, ylab=lr.psa.results$y.label, xlab=lr.psa.results$x.label, legendlab=FALSE, level1.plot=FALSE, level1.rug.plot=NULL, level1.projection.lines=FALSE, level2.plot=TRUE, level2.rug.plot=geom_rug_alt, level2.projection.lines=TRUE, level2.label=FALSE, unweighted.means=FALSE, weighted.means=FALSE) + opts(legend.position='none') #opts(legend.position=c(0.20,0.85)) + scale_size_continuous('Num of Students')
	ggsave(paste('Figures/g', grade, subject, 'lrcircplot.pdf', sep=''), width=6, height=6)
	results = rbind(results, printSummary(lr.psa.results))
	level1Tex(lr.psa.results, file=paste('Tables/g', grade, subject, '.lr.level1.tex', sep=''), caption=paste("Logistic Regression Level 1 Summary: Grade", grade, subject, sep=" "), label=paste('g', grade, subject, 'lrlevel1', sep=''))
	level2Tex(lr.psa.results, file=paste('Tables/g', grade, subject, '.lr.level2.tex', sep=''), caption=paste("Logistic RegressionLevel 2 Summary: Grade", grade, subject, sep=" "), label=paste('g', grade, subject, 'lrlevel2', sep=''))
	#Distribution of mathced vs unmatched public school students
	df2 = complete[,c(paste(subject,'score',sep=''), 'charter', 'state')]
	df2$stateabvr = abbreviateState(df2$state)
	names(df2)[1] = 'Score'
	df2$Matched = TRUE
	df2[lr.psa.results$removed,]$Matched = FALSE
	df2 = df2[which(df2$charter == 'Public'),]
	df2$stateabvr = as.factor(as.character(df2$stateabvr))
	ggplot(df2, aes(x=Score, y=..density.., colour=Matched)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + xlab(simpleCap(paste(subject, ' Score', sep=''))) + ylab("Density") + opts(axis.text.x=theme_text(angle=-45, size=8))
	ggsave(paste('Figures/g', grade, subject, 'lrPublicDensity.pdf', sep=''), width=8, height=6)

	print('Logistic regression AIC:')
	ps = getPropensityScores(lr.results.aic)
	complete$ps = NA
	complete[rownames(ps),]$ps = ps$ps
	complete$strata5 = cut(complete$ps, quantile(complete$ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
	#complete$strata5 = cut(complete$ps, breaks=seq(min(complete$ps), max(complete$ps), ((max(complete$ps)-min(complete$ps))/5)), include.lowest=TRUE, labels=FALSE)
	complete$state = as.factor(as.character(complete$state))
	pdf(paste('Figures/g', grade, subject, 'multilevellraicloess.pdf', sep=''))
	print(xyplot(complete[,paste(subject,'score',sep='')] ~ complete$ps | complete$state, group=complete$charter, panel=panel.plsmo, type='l', ylab=paste(subject, grade, 'Score'), xlab='Propensity Score', label.curves=TRUE, cex=.5))
	dev.off()
	#print('Cell sizes...')
	#print(table(complete$state, complete$strata5, complete$charter, useNA='ifany'))
	lr.psa.results = multilevelPSA(response=complete[,paste(subject,'score',sep='')], treatment=complete$charter, strata=complete$strata5, level2=complete$state, minN=4)
	plotpsa.multilevel.psa(multilevelPSA=lr.psa.results, level1.points=TRUE, ylab='State', jitter=FALSE) + opts(axis.text.y=theme_text(size=8, hjust=1)) + ylab(paste('Difference Score (', lr.psa.results$y.label, ' - ', lr.psa.results$x.label, ')', sep='')) + opts(legend.position=c(-1,-1))
	ggsave(paste('Figures/g', grade, subject, 'lraicdiffplot.pdf', sep=''), width=8, height=6)
	plotcirc.multilevel.psa(lr.psa.results, ylab=lr.psa.results$y.label, xlab=lr.psa.results$x.label, legendlab=FALSE, level1.plot=FALSE, level1.rug.plot=NULL, level1.projection.lines=FALSE, level2.plot=TRUE, level2.rug.plot=geom_rug_alt, level2.projection.lines=TRUE, level2.label=FALSE, unweighted.means=FALSE, weighted.means=FALSE) + opts(legend.position='none') #opts(legend.position=c(0.20,0.85)) + scale_size_continuous('Num of Students')
	ggsave(paste('Figures/g', grade, subject, 'lraiccircplot.pdf', sep=''), width=6, height=6)
	results = rbind(results, printSummary(lr.psa.results))
	level1Tex(lr.psa.results, file=paste('Tables/g', grade, subject, '.lraic.level1.tex', sep=''), caption=paste("Logistic Regression Step AIC Level 1 Summary: Grade", grade, subject, sep=" "), label=paste('g', grade, subject, 'lraiclevel1', sep=''))
	level2Tex(lr.psa.results, file=paste('Tables/g', grade, subject, '.lraic.level2.tex', sep=''), caption=paste("Logistic Regression Step AIC Level 2 Summary: Grade", grade, subject, sep=" "), label=paste('g', grade, subject, 'lraiclevel2', sep=''))
	#Distribution of mathced vs unmatched public school students
	df2 = complete[,c(paste(subject,'score',sep=''), 'charter', 'state')]
	df2$stateabvr = abbreviateState(df2$state)
	names(df2)[1] = 'Score'
	df2$Matched = TRUE
	df2[lr.psa.results$removed,]$Matched = FALSE
	df2 = df2[which(df2$charter == 'Public'),]
	df2$stateabvr = as.factor(as.character(df2$stateabvr))
	ggplot(df2, aes(x=Score, y=..density.., colour=Matched)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + xlab(simpleCap(paste(subject, ' Score', sep=''))) + ylab("Density") + opts(axis.text.x=theme_text(angle=-45, size=8))
	ggsave(paste('Figures/g', grade, subject, 'lraicPublicDensity.pdf', sep=''), width=8, height=6)

	print('Conditional inference trees:')
	party.results = multilevelCtree(df, formula=formula, level2='state')
	party = getStrata(party.results, df, level2='state')
	df$state = as.factor(as.character(df$state))
	treeHeat(party.results, names(df)[c(10:26)], df$state)
	ggsave(paste('Figures/g', grade, subject, 'treeHeat.pdf', sep=''))
	tree.psa.results = multilevelPSA(response=party[,paste(subject,'score',sep='')], treatment=party$charter, strata=party$strata, level2=party$state, minN=4)
	plotpsa.multilevel.psa(multilevelPSA=tree.psa.results, level1.points=TRUE, ylab='State', jitter=FALSE) + opts(axis.text.y=theme_text(size=8, hjust=1)) + ylab(paste('Difference Score (', tree.psa.results$y.label, ' - ', tree.psa.results$x.label, ')', sep='')) + opts(legend.position=c(-1,-1))
	ggsave(paste('Figures/g', grade, subject, 'treediffplot.pdf', sep=''), width=8, height=6)
	plotcirc.multilevel.psa(tree.psa.results, ylab=tree.psa.results$y.label, xlab=tree.psa.results$x.label, legendlab=FALSE, level1.plot=FALSE, level1.rug.plot=NULL, level1.projection.lines=FALSE, level2.plot=TRUE, level2.rug.plot=geom_rug_alt, level2.projection.lines=TRUE, level2.label=FALSE, unweighted.means=FALSE, weighted.means=FALSE) + opts(legend.position='none') #opts(legend.position=c(0.20,0.85)) + scale_size_continuous('Num of Students')
	ggsave(paste('Figures/g', grade, subject, 'treecircplot.pdf', sep=''), width=6, height=6)
	results = rbind(results, printSummary(tree.psa.results))
	level1Tex(tree.psa.results, file=paste('Tables/g', grade, subject, '.tree.level1.tex', sep=''), caption=paste("Conditional Inference Trees Level 1 Summary: Grade", grade, subject, sep=" "), label=paste('g', grade, subject, 'treelevel1', sep=''))
	level2Tex(tree.psa.results, file=paste('Tables/g', grade, subject, '.tree.level2.tex', sep=''), caption=paste("Conditional Inference Trees Level 2 Summary: Grade", grade, subject, sep=" "), label=paste('g', grade, subject, 'treelevel2', sep=''))
	#Distribution of mathced vs unmatched public school students
	df2 = df[,c(paste(subject,'score',sep=''), 'charter', 'state')]
	df2$stateabvr = abbreviateState(df2$state)
	names(df2)[1] = 'Score'
	df2$Matched = TRUE
	df2[tree.psa.results$removed,]$Matched = FALSE
	df2 = df2[which(df2$charter == 'Public'),]
	df2$stateabvr = as.factor(as.character(df2$stateabvr))
	ggplot(df2, aes(x=Score, y=..density.., colour=Matched)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + xlab(simpleCap(paste(subject, ' Score', sep=''))) + ylab("Density") + opts(axis.text.x=theme_text(angle=-45, size=8))
	ggsave(paste('Figures/g', grade, subject, 'TreePublicDensity.pdf', sep=''), width=8, height=6)
	
	return(results)
}

naepLoess <- function(lr, thedata, grade, subject) {
	thedata$ps = fitted(lr)
	if(subject == 'math') {
		p = ggplot(thedata, aes(x=ps, y=mathscore, colour=charter)) + ylab('Math Score')
	} else if(subject == 'reading') {
		p = ggplot(thedata, aes(x=ps, y=readingscore, colour=charter)) + ylab('Reading Score')
	} else {
		stop("Subject should be math or reading")
	}
	p = p + geom_smooth() + xlab('Propensity Score') + scale_colour_hue('School Type')
	#p = p + ylim(175, 275)
	print(p)
	ggsave(paste('Figures/g', grade, subject, 'loess.pdf', sep=''), width=6, height=3)
}

naepAnalysisOverall <- function(grade, subject) {
	df = get(paste('g', grade, subject, sep=''))
	complete = get(paste('g', grade, subject, '.complete', sep=''))
	#formula = get(paste('g', grade, 'formula', sep=''))
	formula = charter ~ .
	
	#complete = dataCleanup(complete)
	score = complete[,paste(subject, 'score', sep='')]
	complete = complete[,!(names(complete) %in% c(paste(subject, 'score', sep=''), 'state', 'region'))]
	
	filename = dataLocation(paste('g', grade, subject, '.overall.Rdata', sep=''))
	overall.results = data.frame()
	
	if(file.exists(filename)) {
		load(filename)
	} else {
		print('Logistic regression...')
		lr = glm(formula, data=complete, family=binomial)
		print('Logistic regression with stepAIC...')
		lr.aic = stepAIC(lr, trace=FALSE)
		print('Conditional inference tree...')
		psa.party.results = ctree(formula, data=df[,names(complete)])
		save(lr, lr.aic, psa.party.results, file=filename)
	}
	
	print('Running analysis...')
	
 	print('Logistic regression...')
	summary(lr)
	ps = fitted.values(lr)
	strata5 = cut(ps, breaks=quantile(ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
	pdf(paste('Figures/overallg', grade, subject, 'lrcircpsa.pdf', sep=''))
	psa.lr.results = circ.psa(score, treatment=complete$charter, strata=strata5)
	dev.off()
	overall.results = rbind(overall.results, data.frame(Method='Logistic Regression', Public=psa.lr.results$wtd.Mn.Public, Charter=psa.lr.results$wtd.Mn.Charter, Diff=(psa.lr.results$wtd.Mn.Charter - psa.lr.results$wtd.Mn.Public), ATE=psa.lr.results$ATE, n=psa.lr.results$df, CImin=psa.lr.results$CI.95[1], CImax=psa.lr.results$CI.95[2]))
	
 	print('Loess...')
 	naepLoess(lr, get(paste('g', grade, subject, '.complete', sep='')), grade, subject)

 	print('Logistic Regression step AIC...')
	summary(lr.aic)
	ps = fitted.values(lr.aic)
	strata5 = cut(ps, breaks=quantile(ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
	pdf(paste('Figures/overallg', grade, subject, 'lraiccircpsa.pdf', sep=''))
	psa.lraic.results = circ.psa(score, treatment=complete$charter, strata=strata5)
	dev.off()
	overall.results = rbind(overall.results, data.frame(Method='Logistic Regression Step AIC', Public=psa.lraic.results$wtd.Mn.Public, Charter=psa.lraic.results$wtd.Mn.Charter, Diff=(psa.lraic.results$wtd.Mn.Charter - psa.lraic.results$wtd.Mn.Public), ATE=psa.lraic.results$ATE, n=psa.lraic.results$df, CImin=psa.lraic.results$CI.95[1], CImax=psa.lraic.results$CI.95[2]))
	
 	print('Conditional inference trees...')
	strata = where(psa.party.results)
	pdf(paste('Figures/overallg', grade, subject, 'treescircpsa.pdf', sep=''))
	psa.tree.results = circ.psa(df[,paste(subject,'score',sep='')], treatment=df$charter, strata=strata)
	dev.off()
	overall.results = rbind(overall.results, data.frame(Method='Conditional Inference Trees', Public=psa.tree.results$wtd.Mn.Public, Charter=psa.tree.results$wtd.Mn.Charter, Diff=(psa.tree.results$wtd.Mn.Charter - psa.tree.results$wtd.Mn.Public), ATE=psa.tree.results$ATE, n=psa.tree.results$df, CImin=psa.tree.results$CI.95[1], CImax=psa.tree.results$CI.95[2]))
	
	return(overall.results)
}

naepMatch <- function(thedata, naepScore, naepColName, subject, grade) {
	filename = dataLocation(paste('g', grade, subject, '.overall.Rdata', sep=''))

	if(file.exists(filename)) { 
		load(filename)
	} else {
	 	stop('Run naepAnalysisOverall first')
	}
 
	ps = fitted(lr)
	Tr = rep(FALSE, nrow(thedata))
	Tr[which(thedata[,'charter'] == 'Charter')] = TRUE
	thedata$Tr = Tr
	results = data.frame()
	for(M in c(1,5,10)) {
		#thedata = thedata[,all.vars(formula)]
		#gen = GenMatch(Tr=Tr, X=ps, BalanceMatrix=thedata, exact=(names(thedata) %in% c('state','race','gender')), replace=FALSE, M=M)
		#gen = GenMatch(Tr=Tr, X=ps, replace=FALSE, M=M)
		#match = Match(Y=naepScore, Tr=Tr, Weight.matrix=gen, replace=FALSE)
		
		match = Matchby(Y = naepScore, Tr = Tr, X = ps, by=list(thedata[,'state'], thedata[,'race'], thedata[,'gender']), M=M, replace=FALSE)
		
		#summary(match)
		#MatchBalance(g4formula, data=g4math.complete, match.out=match, nboots=10)
		#df = g4math.complete[c(match$index.treated, match$index.control),]
		df = cbind(thedata[match$index.control, c('state', naepColName)], thedata[match$index.treat, c(naepColName)])
		names(df) = c('state', 'public', 'charter')
		t = t.test(df$charter,df$public, paired=TRUE)
		results = rbind(results, data.frame(M=M, Charter=mean(df$charter), Public=mean(df$public), diff=t$estimate, p=cut(t$p.value, breaks=c(-Inf, .001, .01, .05, Inf), labels=c('***', '**', '*', '')), ES=(t$estimate / sd(c(df$charter, df$public))), ci.min=t$conf.int[1], ci.max=t$conf.int[2]))
		#ggplot(df, aes(x=public, y=charter)) + geom_abline(intercept=0, slop=1) + geom_smooth() + geom_point(alpha=.2) + facet_wrap(~ state) + xlab('Public') + ylab('Charter')
		#ggplot(df, aes(x=public, y=charter)) + geom_abline(intercept=0, slop=1) + geom_point(alpha=.2) + xlab('Public') + ylab('Charter')
	}
	
	return(results)
}
