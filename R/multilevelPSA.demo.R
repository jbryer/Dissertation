options(digits=3)
options(width=80)

install.packages('multilevelPSA', repos='http://cran.r-project.org')
require('multilevelPSA')

data(pisana)
data(pisa.psa.cols)


pisana$MathScore <- apply(pisana[,paste0('PV', 1:5, 'MATH')], 1, sum) / 5
pisana <- pisana[,!names(pisana) %in% 
				 	c(paste0('PV', 1:5, 'MATH'),paste0('PV', 1:5, 'SCIE'),paste0('PV', 1:5, 'READ'))]

mlpsa <- mlpsa.ctree(pisana[,c('CNT','PUBPRIV',pisa.psa.cols)], 
					 formula=PUBPRIV ~ ., level2='CNT')
mlpsa.df <- getStrata(mlpsa, pisana, level2='CNT')
names(mlpsa.df)

mlpsa.lr <- mlpsa.logistic(pisana[,c('CNT','PUBPRIV',pisa.psa.cols)],
			               formula=PUBPRIV ~ ., level2='CNT')
mlpsa.lr.df <- getPropensityScores(mlpsa.lr, nStrata=5)


cv.bal <- covariate.balance(covariates=student[,pisa.psa.cols],
							treatment=student$PUBPRIV,
							level2=student$CNT,
							strata=mlpsa.df$strata)
plot(cv.bal) + theme(axis.text.y=element_text(size=5))
ggsave('~/Dropbox/School/Dissertation/Figures/pisabalance.pdf', width=6, height=8.5)

mlpsa.df$PUBPRIV <- factor(as.character(mlpsa.df$PUBPRIV), levels=c('Public','Private'))
table(mlpsa.df$PUBPRIV)

results.psa.math <- mlpsa(response=mlpsa.df$MathScore, 
						  treatment=mlpsa.df$PUBPRIV, 
						  strata=mlpsa.df$strata, 
						  level2=mlpsa.df$CNT)

summary(results.psa.math)

pdf('Figures/pisamlpsa.pdf', width=6, height=6)
plot(results.psa.math)
dev.off()
mlpsa.circ.plot(results.psa.math, level2.label=FALSE)
ggsave('Figures/PISACircPlot.pdf', width=7, height=7)
mlpsa.difference.plot(results.psa.math, sd=mean(mlpsa.df$MathScore, na.rm=TRUE), xlim=c(-.2, .05))
ggsave('Figures/pisadiffplot.pdf', width=8, height=3)

results.psa.lr.math <- mlpsa(response=pisana$MathScore,
							 treatment=pisana$PUBPRIV,
							 strata=mlpsa.lr.df$strata,
							 level2=mlpsa.lr.df$level2)
summary(results.psa.lr.math)

plot(results.psa.lr.math)
