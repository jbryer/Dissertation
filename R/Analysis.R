setwd('/Users/jbryer/Dropbox/School/Dissertation')
setwd('c:/Dropbox/My Dropbox/School/Dissertation')
getwd()

pkgs = c('granova', 'foreign', 'mice', 'rpart', 'PSAgraphics', 'MatchIt', 'party', 'ggplot2', 'psych', 'Matching', 'Hmisc', 'plyr', 'lattice', 'vcd', 'rbounds', 'xtable')
repos = 'http://cran.r-project.org' #Main CRAN
repos = 'http://cran.skazkaforyou.com' #Ontario, Canada CRAN

##### Install & download the packages #####
install.packages('C:/Dropbox/My Dropbox/Projects/MultilevelPSA/multilevelPSA_1.0.zip', repos=repos)
install.packages('/Users/jbryer/Dropbox/Projects/MultilevelPSA/multilevelPSA_1.0.tar.gz', repos=repos)
install.packages(pkgs, repos=repos)
install.packages(pkgs, repos=paste('file://', getwd(), '/R/Packages', sep=''))
download.packages(pkgs, destdir=paste(getwd(), '/R/Packages', sep=''), repos=repos, type='source')
download.packages(pkgs, destdir=paste(getwd(), '/R/Packages', sep=''), repos=repos, type='win.binary')
download.packages(pkgs, destdir=paste(getwd(), '/R/Packages', sep=''), repos=repos, type='mac.binary')
ap = available.packages(contrib.url(repos, 'source'))
ap = ap[which(ap[,1] %in% pkgs),]
library(tools)
dp = unlist(package.dependencies(ap, depLevel="Suggest"))
dp = dp[!is.na(dp)]
download.packages(dp, destdir=paste(getwd(), '/R/Packages', sep=''), repos=repos, type='source')
download.packages(dp, destdir=paste(getwd(), '/R/Packages', sep=''), repos=repos, type='win.binary')
download.packages(dp, destdir=paste(getwd(), '/R/Packages', sep=''), repos=repos, type='mac.binary')

################################################################################
##### Load Libraries #####
#library(vcd)
library(foreign)
library(granova)
library(mice)
library(lattice)
library(PSAgraphics)
library(party)
library(ggplot2)
library(psych)
library(Matching)
library(rbounds)
library(Hmisc)
library(xtable)
source('R/Functions.R')
source('R/lsos.R')
source('R/NAEPAnalysis.R')
library(multilevelPSA)

ggplot.theme.orig = theme_update(theme_bryer())

################################################################################
##### Windows only: Tell R to use as much memory as possible
memory.limit(size=NA) #Report the current limit
memory.limit(size=4095) #Set to 4GB
memory.limit(size=16380) #Set to 16GB for 64-bit Windows


##### Load Data ################################################################

##### Grade 4 Math
source('R/g4math.R')
g4math.orig$stateabbr = abbreviateState(g4math.orig$state)
str(g4math.school.orig)
str(g4math.orig)
str(g4math.merged.orig)
nrow(g4math.orig)
g4math = dataCleanup(g4math.orig)

##### Grade 4 Reading
source('R/g4reading.R')
g4reading.orig$stateabbr = abbreviateState(g4reading.orig$state)
str(g4reading.school.orig)
str(g4reading.orig)
str(g4reading.merged.orig)
nrow(g4reading.orig)	
g4reading = dataCleanup(g4reading.orig)

##### Grade 8 Math
source('R/g8math.R')
nameg8math.orig$stateabbr = abbreviateState(g8math.orig$state)
str(g8math.school.orig)
str(g8math.orig)
str(g8math.merged.orig)
nrow(g8math.orig)
g8math = dataCleanup(g8math.orig)

##### Grade 8 Reading
source('R/g8reading.R')
g8reading.orig$stateabbr = abbreviateState(g8reading.orig$state)
str(g8reading.school.orig)
str(g8reading.orig)
str(g8reading.merged.orig)
nrow(g8reading.orig)
g8reading = dataCleanup(g8reading.orig)

#This will remove unused data frames for the analysis below
cleanup()

################################################################################
##### Descriptive Stats
g4math.desc = descriptives(g4math, 'mathscore')
xtableDescriptives(g4math.desc, caption='Descriptive Statistics: Grade 4 Math Scores by State', label='g4mathdesc', size='smaller', table.placement='htb', floating.environment='sidewaystable', file='Tables/g4math.unadjusted.tex')
ggplot(g4math, aes(mathscore, y=..density.., colour=charter)) + geom_density(binwidth=10, alpha=.7) + facet_wrap( ~ stateabbr, ncol=8) + opts(axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Density') + xlab('Math Score') + scale_colour_hue('School Type') #+ theme_minimal()
ggsave('Figures/g4mathDensityByState.pdf', width=8, height=6)

g4reading.desc = descriptives(g4reading, 'readingscore')
xtableDescriptives(g4reading.desc, caption='Descriptive Statistics: Grade 4 Reading Scores by State', label='g4readingdesc', size='smaller', table.placement='htb', floating.environment='sidewaystable', file='Tables/g4reading.unadjusted.tex')
ggplot(g4reading, aes(readingscore, y=..density.., colour=charter)) + geom_density(binwidth=10, alpha=.7) + facet_wrap( ~ stateabbr, ncol=8) + opts(axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Density') + xlab('Reading Score') + scale_colour_hue('School Type')
ggsave('Figures/g4readingDensityByState.pdf', width=8, height=6)

g8math.desc = descriptives(g8math, 'mathscore')
xtableDescriptives(g8math.desc, caption='Descriptive Statistics: Grade 8 Math Scores by State', label='g8mathdesc', size='smaller', table.placement='htb', floating.environment='sidewaystable', file='Tables/g8math.unadjusted.tex')
ggplot(g8math, aes(mathscore, y=..density.., colour=charter)) + geom_density(binwidth=10, alpha=.7) + facet_wrap( ~ stateabbr, ncol=8) + opts(axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Density') + xlab('Math Score') + scale_colour_hue('School Type')
ggsave('Figures/g8mathDensityByState.pdf', width=8, height=6)

g8reading.desc = descriptives(g8reading, 'readingscore')
xtableDescriptives(g8reading.desc, caption='Descriptive Statistics: Grade 8 Reading Scores by State', label='g8readingdesc', size='smaller', table.placement='htb', floating.environment='sidewaystable', file='Tables/g8reading.unadjusted.tex')
ggplot(g8reading, aes(readingscore, y=..density.., colour=charter)) + geom_density(binwidth=10, alpha=.7) + facet_wrap( ~ stateabbr, ncol=8) + opts(axis.text.x=theme_text(angle=-90, hjust=0)) + ylab('Density') + xlab('Reading Score') + scale_colour_hue('School Type')
ggsave('Figures/g8readingDensityByState.pdf', width=8, height=6)


latexDescriptives(g4math, names(g4math)[c(10:15, 17:25)], caption='Descriptive Statistics: Grade 4 Math Student Variables', label='g4mathstudent', filename='Tables/g4math.descriptives.tex')
latexDescriptives(g4reading, names(g4reading)[c(10:15, 17:25)], caption='Descriptive Statistics: Grade 4 Reading Student Variables', label='g4readingstudent', filename='Tables/g4reading.descriptives.tex')
latexDescriptives(g8math, names(g8math)[c(10:16, 18:26)], caption='Descriptive Statistics: Grade 8 Math Student Variables', label='g8mathstudent', filename='Tables/g8math.descriptives.tex')
latexDescriptives(g8reading, names(g8reading)[c(10:16, 18:26)], caption='Descriptive Statistics: Grade 8 Reading Student Variables', label='g8readingstudent', filename='Tables/g8reading.descriptives.tex')


################################################################################
##### Missing Data Imputation #####
g4math2 = g4math[,c(8,10:38)]
g4reading2 = g4reading[,c(8,10:51)]
g8math2 = g8math[,c(8,10:54)]
g8reading2 = g8reading[,c(8,10:26)]
md.pairs(g4math2)
md.pairs(g4reading2)
md.pairs(g8math2)
md.pairs(g8reading2)

missingPlot(g4math2[,-1], g4math2$charter)
missingPlot(g4math2[,-1], g4math$state)
missingPlot(g4reading2[,-1], g4reading2$charter)
missingPlot(g4reading2[,-1], g4reading$state)
missingPlot(g8math2[,-1], g8math2$charter)
missingPlot(g8math2[,-1], g8math$state)
missingPlot(g8reading2[,-1], g8reading2$charter)
missingPlot(g8reading2[,-1], g8reading$state)

g4math.toimpute = g4math[,c(10:15,18:38)]
for(i in 15:27) { g4math.toimpute[,i] = as.factor(g4math.toimpute[,i]) }
ptm = proc.time(); g4math.mice = mice(g4math.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg', rep('polyreg', length(17:29)) )); proc.time() - ptm
save(g4math.mice, file=dataLocation('g4math.mice.Rdata'))

g4reading.toimpute = g4reading[,c(10:15,18:49)]
for(i in 15:38) { g4reading.toimpute[,i] = as.factor(g4reading.toimpute[,i]) }
ptm = proc.time(); g4reading.mice = mice(g4reading.toimpute, method=c('logreg', '', 'polyreg','logreg','polyreg','polyreg','logreg','logreg','logreg','logreg','polyreg','polyreg','polyreg','polyreg', rep('polyreg', length(17:40)) )); proc.time() - ptm
save(g4reading.mice, file=dataLocation('g4reading.mice.Rdata'))

g8math.toimpute = g8math[,c(10:16,19:31,34:54)]
for(i in 16:41) { g8math.toimpute[,i] = as.factor(g8math.toimpute[,i]) }
ptm = proc.time(); g8math.mice = mice(g8math.toimpute); proc.time() - ptm
save(g8math.mice, file=dataLocation('g8math.mice.Rdata'))

g8reading.toimpute = g8reading[,c(10:16, 19:62)]
for(i in 16:51) { g8reading.toimpute[,i] = as.factor(g8reading.toimpute[,i]) }
ptm = proc.time(); g8reading.mice = mice(g8reading.toimpute); proc.time() - ptm; 
save(g8reading.mice, file=dataLocation('g8reading.mice.Rdata'))

################################################################################
# IMPORTANT NOTE: Due to the small number of charter school multilevel analysis
# will use only the core variables.

##### For stratification and matching ##########################################
load(dataLocation('g4math.mice.Rdata'))
load(dataLocation('g4reading.mice.Rdata'))
load(dataLocation('g8math.mice.Rdata'))
load(dataLocation('g8reading.mice.Rdata'))

g4math.complete = cbind(g4math[,c(6,8,40)], complete(g4math.mice, 5))
names(g4math.complete)[18:30] = c('Use computer at school for math', 'Use computer to practice or drill on math', 'Use computer to play math games', 'Kind of calculator you normally use', 'Use calculator for math tests-student', 'Difficulty of this math test', 'Effort on this math test', 'Importance of success on this math test', 'The math work is too hard', 'I have done a good job on my homework', 'I have done a good job in class', 'The math work is too easy', 'I like what we do in math class')
g4reading.complete = cbind(g4reading[,c(6,8,51)], complete(g4reading.mice, 5))
g8math.complete = cbind(g8math[,c(6,8,56)], complete(g8math.mice, 5))
g8reading.complete = cbind(g8reading[,c(6,8,64)], complete(g8reading.mice, 5))

##### For multilevelPSA ########################################################
load(dataLocation('g4math.core.mice.Rdata'))
load(dataLocation('g4reading.core.mice.Rdata'))
load(dataLocation('g8math.core.mice.Rdata'))
load(dataLocation('g8reading.core.mice.Rdata'))

g4math.core.complete = cbind(g4math[,c(6,8,40)], complete(g4math.mice, 5))
g4reading.core.complete = cbind(g4reading[,c(6,8,51)], complete(g4reading.mice, 5))
g8math.core.complete = cbind(g8math[,c(6,8,56)], complete(g8math.mice, 5))
g8reading.core.complete = cbind(g8reading[,c(6,8,64)], complete(g8reading.mice, 5))

summary(g4math.mice)
summary(g4reading.mice)
summary(g8math.mice)
summary(g8reading.mice)

save(g4math, g4reading, g8math, g8reading, 
	 g4math.core.complete, g4reading.core.complete, g8math.core.complete, g8reading.core.complete, 
	 g4math.complete, g4reading.complete, g8math.complete, g8reading.complete, 
	 file='Data/naep.Rdata')
load('Data/naep.Rdata')

##### Overall Results #####
describe.by(g4math$mathscore, group=g4math$charter, mat=TRUE)
t.test(mathscore ~ charter, data=g4math)
describe.by(g4reading$readingscore, group=g4reading$charter, mat=TRUE)
t.test(readingscore ~ charter, data=g4reading)
describe.by(g8math$mathscore, group=g8math$charter, mat=TRUE)
t.test(mathscore ~ charter, data=g8math)
describe.by(g8reading$readingscore, group=g8reading$charter, mat=TRUE)
t.test(readingscore ~ charter, data=g8reading)

overall.results = data.frame()
overall.results = rbind(overall.results, naepAnalysisOverall(4, 'math'))
overall.results = rbind(overall.results, naepAnalysisOverall(4, 'reading'))
overall.results = rbind(overall.results, naepAnalysisOverall(8, 'math'))
overall.results = rbind(overall.results, naepAnalysisOverall(8, 'reading'))
write.csv(overall.results, file='overall.csv', row.names=FALSE)

addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = c(0)
addtorow$pos[[3]] = c(3)
addtorow$pos[[4]] = c(6)
addtorow$pos[[5]] = c(9)
addtorow$command = c("& \\multicolumn{ 2}{c}{Adjusted Mean} &  & \\multicolumn{1x}{c}{} &  &  & \\multicolumn{1}{c}{} \\\\ \\cline{2-3} & Public & Charter & Diff & ATE & \\textit{n} & \\multicolumn{2}{c}{95\\% CI} \\\\ \\hline",
	' & \\multicolumn{7}{c}{Grade 4 Math} \\\\ \\cline{2-8}',
	' & \\multicolumn{7}{c}{Grade 4 Reading} \\\\ \\cline{2-8}',
	' & \\multicolumn{7}{c}{Grade 8 Math} \\\\ \\cline{2-8}',
	' & \\multicolumn{7}{c}{Grade 8 Reading} \\\\ \\cline{2-8}'
	)
x = xtable(overall.results, caption='Summary Propensity Score Analysis using Stratification', label='overallresults')
print(x, add.to.row=addtorow, include.rownames=FALSE, include.colnames=FALSE, caption.placement='top', file='Tables/overall.results.tex')


################################################################################
##### Loess plots #####
# Note that we will use the overall (kitchen sink) LR model since there is very
# little differences, other than the covariates used, between the two models.
naepLoess(glm(charter ~ ., data=g4math.complete[,!(names(g4math.complete) %in% c('state', 'mathscore'))], family=binomial), g4math.complete, 4, 'math')
naepLoess(glm(charter ~ ., data=g4reading.complete[,!(names(g4reading.complete) %in% c('state', 'readingscore'))], family=binomial), g4reading.complete, 4, 'reading')
naepLoess(glm(charter ~ ., data=g8math.complete[,!(names(g8math.complete) %in% c('state', 'mathscore'))], family=binomial), g8math.complete, 8, 'math')
naepLoess(glm(charter ~ ., data=g8reading.complete[,!(names(g8reading.complete) %in% c('state', 'readingscore'))], family=binomial), g8reading.complete, 8, 'reading')

g4math.c2 = g4math.complete[,c(8,14:38)]
g4lr = glm(charter ~ ., data=g4math.c2, family=binomial)
summary(g4lr)
g4lraic = stepAIC(g4lr)
thedata = g4math.complete
thedata$ps = fitted(g4lr)
ggplot(thedata, aes(x=ps, y=mathscore, colour=charter)) + ylab('Math Score') + geom_smooth() + xlab('Propensity Score') + scale_colour_hue('School Type') + ylim(175, 275)
Tr = rep(FALSE, nrow(g4math.complete))
Tr[which(g4math.complete[,'charter'] == 'Charter')] = TRUE
g4match = Matchby(Y = g4math.complete$mathscore, Tr = Tr, X = fitted(g4lr), by=list(g4math.complete[,'state'], g4math.complete[,'race'], g4math.complete[,'gender']), M=1, replace=FALSE)
MatchBalance(g4formula, data=g4math.complete, match.out=g4match, nboots=10)
psens(x=g4math.complete[g4match$index.treat,'mathscore'], y=g4math.complete[g4match$index.control,'mathscore'], Gamma=1.5, GammaInc=.1)


################################################################################
##### Density distributions of propensity scores

g4math.c2 = g4math.complete[,!(names(g4math.complete) %in% c('state','mathscore','readingscore'))]
g4math.c2$ps = fitted(glm(charter ~ ., data=g4math.c2, family=binomial))
g4math.c2$stateabvr = abbreviateState(g4math.complete$state)
ggplot(g4math.c2, aes(x=ps, y=..density.., colour=charter)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + scale_colour_hue('School Type') + xlab("Propensity Score") + ylab("Density") + opts(axis.text.x=theme_text(angle=-45))
ggsave('Figures/g4mathPropensityScoreDensityByState.pdf', width=8, height=6)

g4reading.c2 = g4reading.complete[,!(names(g4reading.complete) %in% c('state','mathscore','readingscore'))]
g4reading.c2$ps = fitted(glm(charter ~ ., data=g4reading.c2, family=binomial))
g4reading.c2$stateabvr = abbreviateState(g4reading.complete$state)
ggplot(g4reading.c2, aes(x=ps, y=..density.., colour=charter)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + scale_colour_hue('School Type') + xlab("Propensity Score") + ylab("Density") + opts(axis.text.x=theme_text(angle=-45))
ggsave('Figures/g4readingPropensityScoreDensityByState.pdf', width=8, height=6)

g8math.c2 = g8math.complete[,!(names(g8math.complete) %in% c('state','mathscore','readingscore'))]
g8math.c2$ps = fitted(glm(charter ~ ., data=g8math.c2, family=binomial))
g8math.c2$stateabvr = abbreviateState(g8math.complete$state)
ggplot(g8math.c2, aes(x=ps, y=..density.., colour=charter)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + scale_colour_hue('School Type') + xlab("Propensity Score") + ylab("Density") + opts(axis.text.x=theme_text(angle=-45))
ggsave('Figures/g8mathPropensityScoreDensityByState.pdf', width=8, height=6)

g8reading.c2 = g8reading.complete[,!(names(g8reading.complete) %in% c('state','mathscore','readingscore'))]
g8reading.c2$ps = fitted(glm(charter ~ ., data=g8reading.c2, family=binomial))
g8reading.c2$stateabvr = abbreviateState(g8reading.complete$state)
ggplot(g8reading.c2, aes(x=ps, y=..density.., colour=charter)) + geom_density() + facet_wrap(~ stateabvr, ncol=8) + scale_colour_hue('School Type') + xlab("Propensity Score") + ylab("Density") + opts(axis.text.x=theme_text(angle=-45))
ggsave('Figures/g8readingPropensityScoreDensityByState.pdf', width=8, height=6)


################################################################################
##### Matching #####
#Using Matching package
#We will require exact matching by state, race, and gender, then match on propensity score.
#Other matching routines take a very long time to complete otherwise.
matching.results = naepMatch(g4math.complete, g4math.complete$mathscore, 'mathscore', 'math', 4)
matching.results = rbind(matching.results, naepMatch(g4reading.complete, g4reading.complete$readingscore, 'readingscore', 'reading', 4))
matching.results = rbind(matching.results, naepMatch(g8math.complete, g8math.complete$mathscore, 'mathscore', 'math', 8))
matching.results = rbind(matching.results, naepMatch(g8reading.complete, g8reading.complete$readingscore, 'readingscore', 'reading', 8))
rownames(matching.results) = 1:nrow(matching.results)
matching.results$M = as.integer(matching.results$M)
write.csv(matching.results, 'matching.csv', row.names=FALSE)

addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = c(0)
addtorow$pos[[3]] = c(3)
addtorow$pos[[4]] = c(6)
addtorow$pos[[5]] = c(9)
addtorow$pos[[6]] = c(nrow(matching.results))
addtorow$command = c("M & Charter & Public & \\multicolumn{2}{c}{Diff} & ES & \\multicolumn{2}{c}{95\\% CI} \\\\ \\hline",
	' & \\multicolumn{7}{c}{Grade 4 Math} \\\\ \\cline{2-8}',
	' & \\multicolumn{7}{c}{Grade 4 Reading} \\\\ \\cline{2-8}',
	' & \\multicolumn{7}{c}{Grade 8 Math} \\\\ \\cline{2-8}',
	' & \\multicolumn{7}{c}{Grade 8 Reading} \\\\ \\cline{2-8}',
	'\\hline \\multicolumn{8}{l}{*\\textit{p} $<$ .05; **\\textit{p} $<$ .01; ***\\textit{p} $<$ .001} \\\\ '
	)
x = xtable(matching.results, caption='Summary of Propensity Score Matching', lable='overallresultsmatching', align=c('r','r','r','r','r@{}','l','r','r','r'))
print(x, add.to.row=addtorow, include.rownames=FALSE, include.colnames=FALSE, hline.after=c(-1), caption.placement='top', file='Tables/overall.matching.results.tex')


################################################################################
##### Mulitlevel PSA ###########################################################
g4formula = charter ~ gender + race + iep + ell + lunch + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome
g8formula = charter ~ gender + race + iep + ell + lunch + parented + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome

##### Logistic regression #####
##### Conditional inference trees (party package) #####

##### Grade 4 Math
lr.g4math.results = multilevelLR(g4math.core.complete, g4formula, level2='state', stepAIC=FALSE)
lr.g4math.results.aic = multilevelLR(g4math.core.complete, g4formula, level2='state', stepAIC=TRUE)
save(lr.g4math.results, lr.g4math.results.aic, file=dataLocation('g4math.lr.Rdata'))

##### Grade 4 reading
lr.g4reading.results = multilevelLR(g4reading.core.complete, g4formula, level2='state', stepAIC=FALSE)
lr.g4reading.results.aic = multilevelLR(g4reading.core.complete, g4formula, level2='state', stepAIC=TRUE)
save(lr.g4reading.results, lr.g4reading.results.aic, file=dataLocation('g4reading.lr.Rdata'))

##### Grade 8 Math
lr.g8math.results = multilevelLR(g8math.core.complete, g8ormula, level2='state', stepAIC=FALSE)
lr.g8math.results.aic = multilevelLR(g8math.core.complete, g8ormula, level2='state', stepAIC=TRUE)
save(lr.g8math.results, lr.g8math.results.aic, file=dataLocation('g8math.lr.Rdata'))

##### Grade 8 Reading
lr.g8reading.results = multilevelLR(g8reading.core.complete, g8ormula, level2='state', stepAIC=FALSE)
lr.g8reading.results.aic = multilevelLR(g8reading.core.complete, g8ormula, level2='state', stepAIC=TRUE)
save(lr.g8reading.results, lr.g8reading.results.aic, file=dataLocation('g8reading.lr.Rdata'))

load(dataLocation('g4math.lr.Rdata'))
load(dataLocation('g4reading.lr.Rdata'))
load(dataLocation('g8math.lr.Rdata'))
load(dataLocation('g8reading.lr.Rdata'))


################################################################################
#### Mulitlevel PSA
multilevel.results = naepAnalysis(grade=4, subject='math')
multilevel.results = rbind(multilevel.results, naepAnalysis(grade=4, subject='reading'))
multilevel.results = rbind(multilevel.results, naepAnalysis(grade=8, subject='math'))
multilevel.results = rbind(multilevel.results, naepAnalysis(grade=8, subject='reading'))
write.csv(multilevel.results, 'multilevel.csv', row.names=FALSE)


################################################################################
##### Combine all Results
overall.results = read.csv('overall.csv')
matching.results = read.csv('matching.csv')
multilevel.results = read.csv('multilevel.csv')

multilevel.results$Method = paste('Multilevel', overall.results$Method)
multilevel.results$Subject = c(rep('Grade 4 Math', 3), rep('Grade 4 Reading', 3), rep('Grade 8 Math', 3), rep('Grade 8 Reading', 3))
multilevel.results$Diff = multilevel.results$Charter - multilevel.results$Public
overall.results$Subject = c(rep('Grade 4 Math', 3), rep('Grade 4 Reading', 3), rep('Grade 8 Math', 3), rep('Grade 8 Reading', 3))
overall.results$Method = paste('Stratified', overall.results$Method)
matching.results$Subject = c(rep('Grade 4 Math', 3), rep('Grade 4 Reading', 3), rep('Grade 8 Math', 3), rep('Grade 8 Reading', 3))
names(matching.results)[1] = 'Method'
names(matching.results)[4] = 'Diff'
names(matching.results)[7:8] = c('CImin', 'CImax')
matching.results$Method = paste('Matching 1 to', matching.results$Method)

overall = rbind(overall.results[,c('Subject', 'Method', 'Diff', 'CImin', 'CImax')], 
				matching.results[,c('Subject', 'Method', 'Diff', 'CImin', 'CImax')],
				multilevel.results[,c('Subject', 'Method', 'Diff', 'CImin', 'CImax')])
ggplot(overall, aes(x=Diff, xmin=CImin, xmax=CImax, y=Method)) + geom_vline(x=0) + geom_errorbarh(colour='green') + geom_point(colour='blue') + facet_wrap(~ Subject) + xlim(-10,10) + xlab('Difference (charter - public)') + ylab(NULL) + ylim(unique(overall$Method)[9:1])
ggsave('Figures/overall.summary.pdf')
