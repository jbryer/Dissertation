setwd('/Users/jbryer/Dropbox/School/Dissertation')
setwd('c:/Dropbox/My Dropbox/School/Dissertation')
setwd('C:/Bryer Dissertation')
getwd()

pkgs = c('granova', 'foreign', 'mice', 'rpart', 'PSAgraphics', 'MatchIt', 'party', 'ggplot2', 'psych', 'Matching', 'Hmisc')
repos = 'http://cran.r-project.org' #Main CRAN
repos = 'http://cran.skazkaforyou.com' #Ontario, Canada CRAN

##### Install & download the packages #####
install.packages(pkgs, repos=repos)
install.packages(pkgs, repos=paste('file://', getwd(), '/Rpackages', sep=''))
download.packages(pkgs, destdir=paste(getwd(), '/Rpackages', sep=''), repos=repos, type='source')
download.packages(pkgs, destdir=paste(getwd(), '/Rpackages', sep=''), repos=repos, type='win.binary')
ap = available.packages(contrib.url(repos, 'source'))
ap = ap[which(ap[,1] %in% pkgs),]
dp = unlist(package.dependencies(ap, depLevel="Suggest"))
dp = dp[!is.na(dp)]
download.packages(dp, destdir=paste(getwd(), '/Rpackages', sep=''), repos=repos, type='source')
download.packages(dp, destdir=paste(getwd(), '/Rpackages', sep=''), repos=repos, type='win.binary')

################################################################################
##### Load Libraries #####
library(vcd)
library(foreign)
library(granova)
library(mice)
library(lattice)
library(PSAgraphics)
library(party)
library(ggplot2)
library(psych)
library(Matching)
library(Hmisc)
source('Functions.R')
source('cbal.stes.R')
source('C:/Dropbox/My Dropbox/Modern Graphics/multilevel.psa.R')
source('/Users/jbryer/Dropbox/Modern Graphics/multilevel.psa.R')

################################################################################
##### This may or may not be necessary. On Windows it is possible to change how much memory R will use
memory.limit(size=NA) #Report the current limit
memory.limit(size=3072) #Set to 3GB


##### Load Data ################################################################
##### Grade 4 Math
source('g4math.R')
g4math.orig$stateabbr = abbreviateState(g4math.orig$state)
str(g4math.school.orig)
str(g4math.orig)
#str(g4math.merged.org)
nrow(g4math.orig)
g4math = dataCleanup(g4math.orig)
g4math.desc = descriptives(g4math, 'mathscore')
xtableDescriptives(g4math.desc, caption='Descriptive Statistics of Grade 4 Math', label='g4mathdesc', size='smaller', table.placement='htb', floating.environment='sidewaystable')
ggplot(g4math, aes(mathscore)) + geom_histogram(binwidth=10) + facet_wrap(~ stateabbr) + opts(title='Distribution of Grade 4 Math', axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Number of Students') + xlab('Math Score')
ggplot(g4math, aes(mathscore, y=..density.., colour=charter)) + geom_density(binwidth=10, alpha=.7) + opts(title='Density Distribution of Grade 4 Math', axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Density') + xlab('Math Score')
ggplot(g4math, aes(mathscore, y=..density.., colour=charter)) + geom_density(binwidth=10, alpha=.7) + facet_wrap( ~ stateabbr) + opts(title='Density Distribution of Grade 4 Math by State', axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Density') + xlab('Math Score')

##### Grade 8 Math
source('g8math.R')
str(g8math.school.orig)
str(g8math.orig)
str(g8math.merged.orig)
nrow(g8math.orig)
g8math = dataCleanup(g8math.orig)
g8math.desc = descriptives(g8math, 'mathscore')

##### Grade 4 Reading
source('g4reading.R')
str(g4reading.school.orig)
str(g4reading.orig)
str(g4reading.merged.orig)
nrow(g4reading.orig)	
g4reading = dataCleanup(g4reading.orig)
g4reading.desc = descriptives(g4reading, 'readingscore')

##### Grade 8 Reading
source('g8reading.R')
str(g8reading.school.orig)
str(g8reading.orig)
str(g8reading.merged.orig)
nrow(g8reading.orig)
g8reading = dataCleanup(g8reading.orig)
g8reading.desc = descriptives(g8reading, 'readingscore')

##### Grade 8 Writing
source('g8writing.R')
str(g8writing.school.orig)
str(g8writing.orig)
str(g8writing.merged.orig)
nrow(g8writing.orig)
g8writing = dataCleanup(g8writing.orig)
g8writing.desc = descriptives(g8writing, 'writingscore')


################################################################################
##### Missing Data Imputation #####
g8math2 = g8math[,c(8,10:26)]
g8reading2 = g8reading[,c(8,10:26)]
g4math2 = g4math[,c(8,10:25)]
g4reading2 = g4reading[,c(8,10:25)]
g8writing2 = g8writing[,c(8,10:26)]
g12writing2 = g12writing[,c(8,10:26)]
md.pairs(g8math2)
md.pairs(g8reading2)
md.pairs(g4math2)
md.pairs(g4reading2)
md.pairs(g8writing2)
md.pairs(g12writing2)

missingPlot(g8math2[,-1], g8math2$charter, "Percentage Missing Values on Variables for Grade 8 Math", "School Type")
missingPlot(g8math2[,-1], g8math$state, "Percentage Missing Values on Variables for Grade 8 Math", "State", flip=FALSE)
missingPlot(g8reading2[,-1], g8reading2$charter, "Percentage Missing Values on Variables for Grade 8 Reading", "School Type")
missingPlot(g8reading2[,-1], g8reading$state, "Percentage Missing Values on Variables for Grade 8 Reading", "State", flip=FALSE)
missingPlot(g4math2[,-1], g4math2$charter, "Percentage Missing Values on Variables for Grade 4 Math", "School Type")
missingPlot(g4math2[,-1], g4math$state, "Percentage Missing Values on Variables for Grade 4 Math", "State", flip=FALSE)
missingPlot(g4reading2[,-1], g4reading2$charter, "Percentage Missing Values on Variables for Grade 4 Reading", "School Type")
missingPlot(g4reading2[,-1], g4reading$state, "Percentage Missing Values on Variables for Grade 4 Reading", "State", flip=FALSE)
missingPlot(g8writing2[,-1], g8writing2$charter, "Percentage Missing Values on Variables for Grade 8 Writing", "School Type")
missingPlot(g8writing2[,-1], g8writing$state, "Percentage Missing Values on Variables for Grade 8 Writing", "State", flip=FALSE)
missingPlot(g12writing2[,-1], g12writing2$charter, "Percentage Missing Values on Variables for Grade 12 Writing", "School Type")
missingPlot(g12writing2[,-1], g12writing$state, "Percentage Missing Values on Variables for Grade 12 Writing", "State", flip=FALSE)


g8math.toimpute = g8math[,c('gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')]
ptm = proc.time(); g8math.mice = mice(g8math.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg')); proc.time() - ptm

g8reading.toimpute = g8reading[,c('gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')]
ptm = proc.time(); g8reading.mice = mice(g8reading.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg')); proc.time() - ptm; 

g4math.toimpute = g4math[,c('gender', 'race', 'iep', 'ell', 'lunch', 'books', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')]
ptm = proc.time(); g4math.mice = mice(g4math.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg')); proc.time() - ptm

g4reading.toimpute = g4reading[,c('gender', 'race', 'iep', 'ell', 'lunch', 'books', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')]
ptm = proc.time(); g4reading.mice = mice(g4reading.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg')); proc.time() - ptm

g8writing.toimpute = g8writing[,c('gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')]
ptm = proc.time(); g8writing.mice = mice(g8writing.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg')); proc.time() - ptm

g12writing.toimpute = g12writing[,c('gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')]
ptm = proc.time(); g12writing.mice = mice(g12writing.toimpute, method=c('logreg', '', 'polyreg','','polyreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg')); proc.time() - ptm; save(g12writing, g12writing.mice, file='data/g12writing.mice.Rdata')


#Note: Given the amount of time it takes to impute the missing values, the results
#are saved to an Rdata file and read back in later to avoid this lengthy process.
#    user   system  elapsed 
#21400.15   264.76 21744.98 
save(g8math, g8math.mice, file='data/g8math.mice.Rdata')
#    user   system  elapsed 
#21864.03   287.56 22255.01  
save(g8reading, g8reading.mice, file='data/g8reading.mice.Rdata')
save(g4math, g4math.mice, file='data/g4math.mice.Rdata')
save(g4reading, g4reading.mice, file='data/g4reading.mice.Rdata')
save(g8writing, g8writing.mice, file='data/g8writing.mice.Rdata')
save(g12writing, g12writing.mice, file='data/g12writing.mice.Rdata')

load('data/g8math.mice.Rdata')
load('data/g8reading.mice.Rdata')
load('data/g4math.mice.Rdata')
load('data/g4reading.mice.Rdata')
load('data/g8writing.mice.Rdata')
load('data/g12writing.mice.Rdata')

summary(g8math.mice)
summary(g8reading.mice)
summary(g4math.mice)
summary(g4reading.mice)
summary(g8writing.mice)
summary(g12writing.mice)

tmp = ifelse(complete(g4math.mice, 1) == complete(g4math.mice, 2), 0, 1)
apply(tmp, 2, sum) / nrow(tmp) * 100 #How much difference is there between the 1st and 2nd complete imputations

g8math.complete = cbind(g8math[,c(1:9,27)], complete(g8math.mice, 1))
g8reading.complete = cbind(g8reading[,c(1:9,27)], complete(g8reading.mice))
g4math.complete = cbind(g4math[,c(1:9,26)], complete(g4math.mice))
g4reading.complete = cbind(g4reading[,c(1:9,26)], complete(g4reading.mice))
g8writing.complete = cbind(g8writing[,c(1:9,27)], complete(g8writing.mice))
g12writing.complete = cbind(g12writing[,c(1:9,27)], complete(g12writing.mice))

################################################################################
##### Logistic regression #####
g4formula = charter ~ gender + race + iep + ell + lunch + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome
g8formula = charter ~ gender + race + iep + ell + lunch + parented + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome

##### Grade 8 Math
g8math.lr = glm(g8formula, data=g8math.complete, family=binomial)
summary(g8math.lr)
write.csv(summary(g8math.lr)$coefficients, 'Tables/g8math.logisticregression.csv')
g8math.ps = g8math.lr$fitted

#5 stratum
g8math.s5 = cut(g8math.ps, quantile(g8math.ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g8math.circpsa.5.pdf')
circ.psa(g8math.complete$mathscore, g8math.complete$charter, g8math.s5, revc=TRUE, main="Grade 8 Math (Logistic Regression)")
dev.off()
balancePlots(g8math.complete[,11:25], g8math.complete$charter, g8math.s5, save='Figures/Balance/math/logistic/g8math.logisitic.5strata.')

#10 stratum
g8math.s10 = cut(g8math.ps, quantile(g8math.ps, seq(0,1,1/10)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g8math.circpsa.10.pdf')
circ.psa(g8math.complete$mathscore, g8math.complete$charter, g8math.s10, revc=TRUE, main="Grade 8 Math (Logistic Regression)")
dev.off()
balancePlots(g8math.complete[,11:25], g8math.complete$charter, g8math.s10, save='Figures/Balance/math/logistic/g8math.logistic.10strata.')

loess.psa(g8math.complete$mathscore, g8math.complete$charter, g8math.ps, ylab="Grade 8 Math Score", legend=levels(g8math.complete$charter), legend.xy="bottomright")

##### Grade 8 Reading
g8reading.lr = glm(g8formula, data=g8reading.complete, family=binomial)
summary(g8reading.lr)
write.csv(summary(g8reading.lr)$coefficients, 'Tables/g8reading.logisticregression.csv')
g8reading.ps = g8reading.lr$fitted

#5 strata
g8reading.s5 = cut(g8reading.ps, quantile(g8reading.ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g8reading.circpsa.5.pdf')
circ.psa(g8reading.complete$readingscore, g8reading.complete$charter, s5, revc=TRUE, main="Grade 8 Reading (Logistic Regression)")
dev.off()
balancePlots(g8reading.complete[,11:25], g8reading.complete$charter, s5, 'Figures/Balance/reading/logistic/g8reading.logistic.5strata.')

#10 stratum
g8reading.s10 = cut(g8reading.ps, quantile(g8reading.ps, seq(0,1,1/10)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g8reading.circpsa.10.pdf')
circ.psa(g8reading.complete$readingscore, g8reading.complete$charter, g8reading.s10, revc=TRUE, main="Grade 8 Reading (Logistic Regression)")
dev.off()
balancePlots(g8reading.complete[,11:25], g8reading.complete$charter, g8reading.s10, 'Figures/Balance/reading/logistic/g8reading.logistic.10strata.')

##### Grade 4 Math
g4math.lr = glm(g4formula, data=g4math.complete, family=binomial)
summary(g4math.lr)
write.csv(summary(g4math.lr)$coefficients, 'Tables/g4math.logisticregression.csv')
g4math.ps = g4math.lr$fitted

g4math.s5 = cut(g4math.ps, quantile(g4math.ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g4math.circpsa.5.pdf')
circ.psa(g4math.complete$mathscore, g4math.complete$charter, g4math.s5, revc=TRUE, main="Grade 4 Math (Logisitic Regression)")
dev.off()
balancePlots(g4math.complete[,11:24], g4math.complete$charter, g4math.s5, 'Figures/Balance/math/logistic/g4math.logistic.5strata.')

g4math.s10 = cut(g4math.ps, quantile(g4math.ps, seq(0,1,1/10)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g4math.circpsa.10.pdf')
circ.psa(g4math.complete$mathscore, g4math.complete$charter, g4math.s10, revc=TRUE, main="Grade 4 Math (Logisitic Regression)")
dev.off()
balancePlots(g4math.complete[,11:24], g4math.complete$charter, g4math.s10, 'Figures/Balance/math/logistic/g4math.logistic.10strata.')

##### Grade 4 reading
g4reading.lr = glm(g4formula, data=g4reading.complete, family=binomial)
summary(g4reading.lr)
write.csv(summary(g4reading.lr)$coefficients, 'Tables/g4reading.logisticregression.csv')
g4reading.ps = g4reading.lr$fitted

g4reading.s5 = cut(g4reading.ps, quantile(g4reading.ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g4reading.circpsa.5.pdf')
circ.psa(g4reading.complete$readingscore, g4reading.complete$charter, g4reading.s5, revc=TRUE, main="Grade 4 Reading (Logisitic Regression)")
dev.off()
balancePlots(g4reading.complete[,11:24], g4reading.complete$charter, g4reading.s5, 'Figures/Balance/reading/logistic/g4reading.logistic.5strata.')

g4reading.s10 = cut(g4reading.ps, quantile(g4reading.ps, seq(0,1,1/10)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g4reading.circpsa.10.pdf')
circ.psa(g4reading.complete$readingscore, g4reading.complete$charter, g4reading.s10, revc=TRUE, main="Grade 4 Reading (Logisitic Regression)")
dev.off()
balancePlots(g4reading.complete[,11:24], g4reading.complete$charter, g4reading.s10, 'Figures/Balance/reading/logistic/g4reading.logistic.10strata.')

##### Grade 8 Writing
g8writing.lr = glm(g8formula, data=g8writing.complete, family=binomial)
summary(g8writing.lr)
write.csv(summary(g8writing.lr)$coefficients, 'Tables/g8writing.logisticregression.csv')
g8writing.ps = g8writing.lr$fitted

#5 strata
g8writing.s5 = cut(g8writing.ps, quantile(g8writing.ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g8writing.circpsa.5.pdf')
circ.psa(g8writing.complete$writingscore, g8writing.complete$charter, g8writing.s5, revc=TRUE, main="Grade 8 Writing (Logistic Regression)")
dev.off()
balancePlots(g8writing.complete[,11:25], g8writing.complete$charter, g8writing.s5, 'Figures/Balance/writing/logistic/g8writing.logistic.5strata.')

#10 stratum
g8writing.s10 = cut(ps, quantile(ps, seq(0,1,1/10)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g8writing.circpsa.10.pdf')
circ.psa(g8writing.complete$writingscore, g8writing.complete$charter, g8writing.s10, revc=TRUE, main="Grade 8 Writing (Logistic Regression)")
dev.off()
balancePlots(g8writing.complete[,11:25], g8writing.complete$charter, g8writing.s10, 'Figures/Balance/writing/logistic/g8writing.logistic.10strata.')

##### Grade 12 Writing
g12writing.lr = glm(g8formula, data=g12writing.complete, family=binomial)
summary(g12writing.lr)
write.csv(summary(g12writing.lr)$coefficients, 'Tables/g12writing.logisticregression.csv')
g12writing.ps = g12writing.lr$fitted

#5 strata
g12writing.s5 = cut(g12writing.ps, quantile(g12writing.ps, seq(0,1,1/5)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g12writing.circpsa.5.pdf')
circ.psa(g12writing.complete$writingscore, g8writing.complete$charter, g12writing.s5, revc=TRUE, main="Grade 12 Writing (Logistic Regression)")
dev.off()
balancePlots(g12writing.complete[,11:25], g12writing.complete$charter, g12writing.s5, 'Figures/Balance/writing/logistic/g12writing.logistic.5strata.')

#10 stratum
g12writing.s10 = cut(g12writing.ps, quantile(g12writing.ps, seq(0,1,1/10)), include.lowest=TRUE, labels=FALSE)
pdf('Figures/g12writing.circpsa.10.pdf')
circ.psa(g12writing.complete$writingscore, g12writing.complete$charter, g12writing.s10, revc=TRUE, main="Grade 12 Writing (Logistic Regression)")
dev.off()
balancePlots(g12writing.complete[,11:25], g12writing.complete$charter, g12writing.s10, 'Figures/Balance/writing/logistic/g12writing.logistic.10strata.')


################################################################################
##### Partition Trees (party package) #####
#### Grade 8 Math
g8math.party = ctree(g8formula, data=g8math.complete)
g8math.party
plot(g8math.party, drop_terminal=TRUE, terminal_panel=NULL)
table(g8math.complete$charter, where(g8math.party), useNA='ifany')
pdf('Figures/g8math.party.pdf')
circ.psa(g8math.complete$mathscore, g8math.complete$charter, where(g8math.party), revc=TRUE, main="Grade 8 Math (Partition Trees)")
dev.off()
balancePlots(g8math.complete[,11:25], g8math.complete$charter, where(g8math.party), 'Figures/Balance/math/party/g8math.party.')

##### Grade 8 Reading
g8reading.party = ctree(g8formula, data=g8reading.complete)
g8reading.party
plot(g8reading.party, drop_terminal=TRUE, terminal_panel=NULL)
table(g8reading.complete$charter, where(g8reading.party), useNA='ifany')
pdf('Figures/g8reading.party.pdf')
circ.psa(g8reading.complete$readingscore, g8reading.complete$charter, where(g8reading.party), revc=TRUE, main="Grade 8 reading (Partition Trees)")
dev.off()
balancePlots(g8reading.complete[,11:25], g8reading.complete$charter, where(g8reading.party), 'Figures/Balance/reading/party/g8reading.party.')

#### Grade 4 Math
g4math.party = ctree(g4formula, data=g4math)
g4math.party
plot(g4math.party, drop_terminal=TRUE, terminal_panel=NULL, type='simple')
table(g4math$charter, where(g4math.party), useNA='ifany')
pdf('Figures/g4math.party.pdf')
circ.psa(g4math$mathscore, g4math$charter, where(g4math.party), revc=TRUE, main="Grade 4 Math (Partition Trees)")
dev.off()
balancePlots(g4math[,11:24], g4math$charter, where(g4math.party), 'Figures/Balance/math/party/g4math.party.')
########## Multilevel PSA by state ##########
t = as.data.frame(table(g4math$state, g4math$charter))
smallStates = t[which(t$Freq < 20),'Var1']
g4math.mlpsa = g4math[-which(g4math$state %in% smallStates),]
g4math.mlpsa$state = as.factor(as.character(g4math.mlpsa$state))
g4math.mlpsa$stateabbr = as.factor(as.character(g4math.mlpsa$stateabbr))
g4math.mlpsa$strata = NA
g4math.party.results = list()
for(s in unique(g4math.mlpsa$stateabbr)) {
	rows = which(g4math.mlpsa$stateabbr == s)
	p = ctree(g4formula, data=g4math.mlpsa[rows,])
	g4math.party.results[s] = p
	g4math.mlpsa[rows,]$strata = where(p)
}
names(g4math.mlpsa)
results.psa = multilevelPSA(response=g4math.mlpsa$mathscore, treatment=g4math.mlpsa$charter, strata=g4math.mlpsa$strata, level2=g4math.mlpsa$stateabbr)
slotNames(results.psa)
results.psa@level2.summary
plotcirc(results.psa, xlab='Public Schools', ylab='Charter Schools', legendlab='State', title='Multilevel PSA Assessment Plot: Grade 4 Math', level1.plot=FALSE, level1.rug.plot=NULL, level1.projection.lines=FALSE, level2.plot=TRUE, level2.rug.plot=geom_rug_alt, level2.projection.lines=FALSE, level2.label=FALSE, unweighted.means=FALSE, weighted.means=FALSE, level1.point.size=1)
plotpsa(multilevelPSA=results.psa, level1.points=TRUE, ylab='State', jitter=FALSE) + opts(axis.text.y=theme_text(size=8))



#### Grade 4 Reading
g4reading.party = ctree(g4formula, data=g4reading.complete)
g4reading.party
plot(g4reading.party, drop_terminal=TRUE, terminal_panel=NULL)
table(g4reading.complete$charter, where(g4reading.party), useNA='ifany')
pdf('Figures/g4reading.party.pdf')
circ.psa(g4reading.complete$readingscore, g4reading.complete$charter, where(g4reading.party), revc=TRUE, main="Grade 4 Reading (Partition Trees)")
dev.off()
balancePlots(g4reading.complete[,11:24], g4reading.complete$charter, where(g4reading.party), 'Figures/Balance/reading/party/g4reading.party.')

##### Grade 8 Writing
g8writing.party = ctree(g8formula, data=g8writing.complete)
g8writing.party
plot(g8writing.party, drop_terminal=TRUE, terminal_panel=NULL)
table(g8writing.complete$charter, where(g8writing.party), useNA='ifany')
pdf('Figures/g8writing.party.pdf')
circ.psa(g8writing.complete$writingscore, g8writing.complete$charter, where(g8writing.party), revc=TRUE, main="Grade 8 Writing (Partition Trees)")
dev.off()
balancePlots(g8writing.complete[,11:25], g8writing.complete$charter, where(g8writing.party), 'Figures/Balance/writing/party/g8writing.party.')

##### Grade 12 Writing
g12writing.party = ctree(g8formula, data=g12writing.complete)
g12writing.party
plot(g12writing.party, drop_terminal=TRUE, terminal_panel=NULL)
table(g12writing.complete$charter, where(g12writing.party), useNA='ifany')
pdf('Figures/g12writing.party.pdf')
circ.psa(g12writing.complete$writingscore, g12writing.complete$charter, where(g12writing.party), revc=TRUE, main="Grade 12 Writing (Partition Trees)")
dev.off()
balancePlots(g12writing.complete[,11:25], g12writing.complete$charter, where(g12writing.party), 'Figures/Balance/writing/party/g12writing.party.')


##### Balance plots
pdf('Figures/g4math.cbal.pdf')
cbal.stes(g4math.complete[,c(11:24)], g4math.complete$charter, g4math.ps)
dev.off()

pdf('Figures/g4reading.cbal.pdf')
cbal.stes(g4reading.complete[,c(11:24)], g4reading.complete$charter, g4reading.ps)
dev.off()

pdf('Figures/g8math.cbal.pdf')
cbal.stes(g8math.complete[,c(11:25)], g8math.complete$charter, g8math.ps)
dev.off()

pdf('Figures/g8reading.cbal.pdf')
cbal.stes(g8reading.complete[,c(11:25)], g8reading.complete$charter, g8reading.ps)
dev.off()

pdf('Figures/g8writing.cbal.pdf')
cbal.stes(g8writing.complete[,c(11:25)], g8writing.complete$charter, g8writing.ps)
dev.off()

################################################################################
##### Matching #####
#Using Matching package
#We will require exact matching by state, race, and gender, then match on propensity score.
#Other matching routines take a very long time to complete otherwise.
matching <- function(thedata, ps, depCol, main=NULL, file=NULL) {
	Y = thedata[,depCol]
	Tr = rep(FALSE, nrow(thedata))
	Tr[which(thedata[,'charter'] == 'Charter')] = TRUE
	match = Matchby(Y = Y, Tr = Tr, X = ps, by=list(thedata[,'state'], thedata[,'race'], thedata[,'gender']))
	if(!is.null(file)) {
		pdf(file)
	}
	granova = granova.ds(cbind(thedata[match$index.control,depCol], thedata[match$index.treated,depCol]), xlab='Public', ylab='Charter', main=main)
	if(!is.null(file)) {
		dev.off()
	}
	list(match=match, granova=granova, Tr=Tr)
}

g4formula2 = Tr ~ gender + race + iep + ell + lunch + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome
g8formula2 = Tr ~ gender + race + iep + ell + lunch + parented + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome

g4math.match = matching(g4math.complete, g4math.ps, 'mathscore', file='Figures/g4math.matching.ds.pdf', main='Grade 4 Math (Matching)')
summary(g4math.match$match); g4math.match$granova
g4math.balance = MatchBalance(g4formula2, match.out=g4math.match$match, data=cbind(Tr=g4math.match$Tr, g4math.complete), nboots=1000)

mosaic(~ iep + ell + lunch  | charter, data=g4math.complete[c(g4math.match$index.control, g4math.match$index.treated),], shade=FALSE)




################################################################################
# Grade 12 Writing
source('g12writing.R')
#NOTE: The school data file for grade 12 writing is considerably different
#      than the other files. The recoding of columns needs to be fixed before
#      using these data.
#str(g12writing.school.orig)
str(g12writing.orig)
#str(g12writing.merged.orig)
nrow(g12writing.orig)

#This is the only data file that has missing charter school data. However, these NA values appear in states where there are no
#charter school counterpart so will be removed anyways.
table(g12writing$state, g12writing$charter, useNA='ifany')
charterStates = unique(g12writing[which(g12writing$charter=='Charter'),'state'])
g12writing = g12writing[which(g12writing$state %in% charterStates),]
hist(g12writing$writingscore, breaks=30)
ggplot(g12writing, aes(writingscore, y=..density..)) + geom_histogram(binwidth=10, alpha=.7) + facet_wrap( ~ charter, ncol=1)
describe.by(g12writing$writingscore, g12writing$charter, mat=TRUE)
################################################################################

