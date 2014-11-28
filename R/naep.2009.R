setwd('./R')
getwd()

if(!require(naep) | !require(foreach) | !require(plyr) | !require(compiler) |
   	!require(mice) | !require(multilevelPSA) | !require(PSAgraphics) |
   	!require(xtable) | !require(party) | !require(MASS) | !require(Matching) |
   	!require(reshape)) {
	# Install packages if not already.
	install.packages(c('foreach','ggplot2','plyr','mice','PSAgraphics','xtable',
					   'party','MASS','Matching','devtools','reshape'), 
					 repos='http://cran.r-project.org')
	require(devtools)
	install_github('naep','jbryer')
	install_github('multilevelPSA','jbryer')
}

theme_update(panel.background=element_rect(size=1, color='grey70', fill=NA) )

source('Functions/naep.analysis.R') # This does most of the PSA analysis.
source('Functions/naep.data.R') # Read in NAEP data.
source('Functions/close.publics.R') # Remove publics further than 5 miles from a charters.
source('Functions/latexDescriptives.R') # Create descriptive tables.
source('Functions/descriptives.R')
source('Functions/recode.r') # Utility function to recode factors.
source('Functions/naep.recode.R') # Recode factors specifically for NAEP.
#source('Functions/impute.R') # Impute missing values.
source('../Data2009/states.r') # Data frame with state names and abbreviations.
 
# These are all the covariates that will be used at least once
all.covars <- c('DSEX','SRACE','SLUNCH1','SD4','ELL3','SDELL','IEP',#Base demographics
				'B017001','B000905','B013801','B017101','B017201','B001151', #Core items
				'B017451','B018101','B018201','B003501','B003601', #Core items
				'R846001','R846101','R846401','R846501','R831001','R831101','R847001', #Reading 4 only
				'R833101','R833401','R846301','R847901','R848001', #Reading 8 only
				'M821401','M824201','M824301','M824701', #Math 4 only
				'M824902','M824903','M824904','M820901','M820904','M820905') #Math 8 only
# List of all variables read from the NAEP data DVD
all.vars <- c('NCESSCHOOLID', 'YEAR', 'AGE', 'SCHID', 'FIPS02', 'SCH', 'PUBPRIV',
			  'BMONTH', 'BYEAR', 'SCRPSU', 'PCHARTR', 'FIPSLOC', 'TYPCLAS', 'CHARTER', 
			  'NCESSCH', 'MOB', 'YOB', 'REGION',  'REGIONS', 'CENSREG', 'CENSDIV', 
			  'REPGRP1', 'REPGRP2', 'JKUNIT', 'SCHWT', 'ORIGWT', 
			  #Replicate weights
			  'SRWT01', 'SRWT02', 'SRWT03', 'SRWT04', 'SRWT05', 'SRWT06',
			  'SRWT07', 'SRWT08', 'SRWT09', 'SRWT10', 'SRWT11', 'SRWT12', 
			  'SRWT13', 'SRWT14', 'SRWT15', 'SRWT16', 'SRWT17', 'SRWT18', 
			  'SRWT19', 'SRWT20', 'SRWT21', 'SRWT22', 'SRWT23', 'SRWT24', 
			  'SRWT25', 'SRWT26', 'SRWT27', 'SRWT28', 'SRWT29', 'SRWT30', 
			  'SRWT31', 'SRWT32', 'SRWT33', 'SRWT34', 'SRWT35', 'SRWT36', 
			  'SRWT37', 'SRWT38', 'SRWT39', 'SRWT40', 'SRWT41', 'SRWT42', 
			  'SRWT43', 'SRWT44', 'SRWT45', 'SRWT46', 'SRWT47', 'SRWT48', 
			  'SRWT49', 'SRWT50', 'SRWT51', 'SRWT52', 'SRWT53', 'SRWT54', 
			  'SRWT55', 'SRWT56', 'SRWT57', 'SRWT58', 'SRWT59', 'SRWT60', 
			  'SRWT61', 'SRWT62', 'SCHTYPE', 'RPTSAMP', 'UTOL12', 'UTOL4', 
			  'WEIGHT', 'DISTCOD', 'PARED', 'MODAGE', 'SDRACEM', 'SCHTYP2', 'LRGCITY', 
			  'CHRTRPT', 'FIPS', 'LEP', 'ACCOM2', 
			  'MRPCM1', 'MRPCM2', 'MRPCM3', 'MRPCM4', 'MRPCM5', #Math scores
			  'RRPCM1', 'RRPCM2', 'RRPCM3', 'RRPCM4', 'RRPCM5', #Reading scores
			  all.covars ) #Include the covariates listed above.

# Read in the data. The naep.data function will save an R version of the NAEP
# data. If that cache file is not available, it will read directly from the 
# NAEP data DVD. See also the naep package: http://jason.bryer.org/naep
g4math <- naep.data(grade=4, subject='math', vars=all.vars, 
					dir='/Volumes/NAEP2009/NAEP 2009 Math G4G8/')
g4read <- naep.data(grade=4, subject='read', vars=all.vars, 
					dir='/Volumes/NAEP2009/NAEP 2009 Reading G4G8/')
g8math <- naep.data(grade=8, subject='math', vars=all.vars, 
					dir='/Volumes/NAEP2009/NAEP 2009 Math G4G8/')
g8read <- naep.data(grade=8, subject='read', vars=all.vars, 
					dir='/Volumes/NAEP2009/NAEP 2009 Reading G4G8/')

g4math.cv.map <- g4math$catalog[g4math$catalog$FieldName %in% all.covars,
								c('FieldName','Description')]
g4read.cv.map <- g4read$catalog[g4read$catalog$FieldName %in% all.covars,
								c('FieldName','Description')]
g4read.cv.map <- g4read.cv.map[g4read.cv.map$FieldName %in% names(g4read$data),]
g8math.cv.map <- g8math$catalog[g8math$catalog$FieldName %in% all.covars,
								c('FieldName','Description')]
g8read.cv.map <- g8read$catalog[g8read$catalog$FieldName %in% all.covars,
								c('FieldName','Description')]

# Recode the "treatment" (i.e. charter) as a logical variable
g4math$data$charter <- ifelse(g4math$data$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g4math$data$charter, useNA='ifany')

g4read$data$charter <- ifelse(g4read$data$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g4read$data$charter, useNA='ifany')

g8math$data$charter <- ifelse(g8math$data$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g8math$data$charter, useNA='ifany')

g8read$data$charter <- ifelse(g8read$data$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g8read$data$charter, useNA='ifany')

##### Outcome of interest
# This is not entirely correct. Can use the mitools package to combine the 
# multiple plausible values, but there is very little difference in the results
# and this provides a cleaner way of creating the figures (otherwise we would need
# five figures per outcome).
g4math$data$mathscore = apply(g4math$data[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
g4math$data = g4math$data[!is.na(g4math$data$mathscore),]

g4read$data$readscore = apply(g4read$data[,c('RRPCM1','RRPCM2','RRPCM3','RRPCM4','RRPCM5')], 1, mean)
g4read$data = g4read$data[!is.na(g4read$data$readscore),]

g8math$data$mathscore = apply(g8math$data[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
g8math$data = g8math$data[!is.na(g8math$data$mathscore),]

g8read$data$readscore = apply(g8read$data[,c('RRPCM1','RRPCM2','RRPCM3','RRPCM4','RRPCM5')], 1, mean)
g8read$data = g8read$data[!is.na(g8read$data$readscore),]

# Save mean scores to a file
require(psych)
mean.col <- c('group1','n','mean','sd','median')
g4math.mean <- describeBy(g4math$data$mathscore, g4math$data$FIPS02, mat=TRUE, skew=FALSE)[,mean.col]
g4read.mean <- describeBy(g4read$data$readscore, g4read$data$FIPS02, mat=TRUE, skew=FALSE)[,mean.col]
g8math.mean <- describeBy(g8math$data$mathscore, g8math$data$FIPS02, mat=TRUE, skew=FALSE)[,mean.col]
g8read.mean <- describeBy(g8read$data$readscore, g8read$data$FIPS02, mat=TRUE, skew=FALSE)[,mean.col]
g4math.mean$Subject <- 'g4math'
g4read.mean$Subject <- 'g4read'
g8math.mean$Subject <- 'g8math'
g8read.mean$Subject <- 'g8read'
g4math.mean$zscore <- (g4math.mean$mean - mean(g4math$data$mathscore)) / sd(g4math$data$mathscore)
g4read.mean$zscore <- (g4read.mean$mean - mean(g4read$data$readscore)) / sd(g4read$data$readscore)
g8math.mean$zscore <- (g8math.mean$mean - mean(g8math$data$mathscore)) / sd(g8math$data$mathscore)
g8read.mean$zscore <- (g8read.mean$mean - mean(g8read$data$readscore)) / sd(g8read$data$readscore)
meanByState <- rbind(g4math.mean, g4read.mean, g8math.mean, g8read.mean)
meanByState <- meanByState[!is.na(meanByState$n),]
meanByState
write.csv(meanByState, file='../Data2009/NAEPDescriptivesByState.csv')

##### Descriptives #############################################################
covariate.descriptive(g4math, all.covars, 'Math', 4)
covariate.descriptive(g4read, all.covars, 'Reading', 4)
covariate.descriptive(g8math, all.covars, 'Math', 8)
covariate.descriptive(g8read, all.covars, 'Reading', 8)

g4math.desc <- descriptives(g4math$data, 'mathscore')
xtableDescriptives(g4math.desc[g4math.desc$Charter.n > 50,], 
				   caption='Grade 4 math unadjusted NAEP score', 
				   label='tab:g4math-unadjscore',
				   file='../Tables2009/g4math-unadjscore.tex',
				   floating.environment='sidewaystable')
g4read.desc <- descriptives(g4read$data, 'readscore')
xtableDescriptives(g4read.desc[g4read.desc$Charter.n > 50,], 
				   caption='Grade 4 reading unadjusted NAEP score', 
				   label='tab:g4read-unadjscore',
				   file='../Tables2009/g4read-unadjscore.tex',
				   floating.environment='sidewaystable')
g8math.desc <- descriptives(g8math$data, 'mathscore')
xtableDescriptives(g8math.desc[g8math.desc$Charter.n > 50,], 
				   caption='Grade 8 math unadjusted NAEP score', 
				   label='tab:g8math-unadjscore',
				   file='../Tables2009/g8math-unadjscore.tex',
				   floating.environment='sidewaystable')
g8read.desc <- descriptives(g8read$data, 'readscore')
xtableDescriptives(g8read.desc[g8read.desc$Charter.n > 50,], 
				   caption='Grade 8 reading unadjusted NAEP score', 
				   label='tab:g8read-unadjscore',
				   file='../Tables2009/g8read-unadjscore.tex',
				   floating.environment='sidewaystable')

# Recode factors
g4math$data <- naep.recode(g4math$data)
g4read$data <- naep.recode(g4read$data)
g8math$data <- naep.recode(g8math$data)
g8read$data <- naep.recode(g8read$data)

# Verify the covriates are coded correctly.
str(g4math$data[,names(g4math$data) %in% all.covars])
str(g4read$data[,names(g4read$data) %in% all.covars])
str(g8math$data[,names(g8math$data) %in% all.covars])
str(g8read$data[,names(g8read$data) %in% all.covars])

# Remove public schools more than 5 miles away from a charter school
g4math3 <- close.publics(g4math)
g4read3 <- close.publics(g4read)
g8math3 <- close.publics(g8math)
g8read3 <- close.publics(g8read)

# Missingness plots
pdf('../Figures2009/g4math-missing.pdf', width=11, height=8.5)
tmp <- g4math3[,g4math.cv.map$FieldName]
names(tmp) <- g4math.cv.map$Description
missing.plot(tmp, as.character(g4math3$FIPS))
#missing.plot(g4math3[,names(g4math3) %in% all.covars], as.character(g4math3$FIPS))
dev.off()
pdf('../Figures2009/g4read-missing.pdf', width=11, height=8.5)
g4read.cv.map <- g4read.cv.map[g4read.cv.map$FieldName %in% names(g4read3),]
tmp <- g4read3[,g4read.cv.map$FieldName]
names(tmp) <- g4read.cv.map$Description
missing.plot(tmp, as.character(g4read3$FIPS))
#missing.plot(g4read3[,names(g4read3) %in% all.covars], as.character(g4read3$FIPS))
dev.off()
pdf('../Figures2009/g8math-missing.pdf', width=11, height=8.5)
tmp <- g8math3[,g8math.cv.map$FieldName]
names(tmp) <- g8math.cv.map$Description
missing.plot(tmp, as.character(g8math3$FIPS))
#missing.plot(g8math3[,names(g8math3) %in% all.covars], as.character(g8math3$FIPS))
dev.off()
pdf('../Figures2009/g8read-missing.pdf', width=11, height=8.5)
tmp <- g8read3[,g8read.cv.map$FieldName]
names(tmp) <- g8read.cv.map$Description
missing.plot(tmp, as.character(g8read3$FIPS))
#missing.plot(g8read3[,names(g8read3) %in% all.covars], as.character(g8read3$FIPS))
dev.off()

# Check if missingness predicts treatment
missing.test <- function(df, cv.map, Tr) {
	tmp <- df[,cv.map$FieldName]
	names(tmp) <- cv.map$Description
	for(i in 1:ncol(tmp)) {
		tmp[,i] <- as.integer(is.na(tmp[,i]))
	}
	tmp$Tr <- Tr
	test <- glm(Tr ~ ., data=tmp, family=binomial)
	return(test)
}
summary(missing.test(g4math3, g4math.cv.map, g4math3$charter))
summary(missing.test(g4read3, g4read.cv.map, g4read3$charter))
summary(missing.test(g8math3, g8math.cv.map, g8math3$charter))
summary(missing.test(g8read3, g8read.cv.map, g8read3$charter))

# Remove Alaska due to large amounts of missing data
g4math3 <- g4math3[g4math3$FIPS != 'Alaska',]
g4read3 <- g4read3[g4read3$FIPS != 'Alaska',]
g8math3 <- g8math3[g8math3$FIPS != 'Alaska',]
g8read3 <- g8read3[g8read3$FIPS != 'Alaska',]

##### Impute missing values ####################################################
source('impute.g4math.R')
source('impute.g4read.R')
source('impute.g8math.R')
source('impute.g8read.R')

#Extract the complete data set
g4math.complete <- complete(g4math.mice, 1)
g4read.complete <- complete(g4read.mice, 1)
g8math.complete <- complete(g8math.mice, 1)
g8read.complete <- complete(g8read.mice, 1)

#Add in the charter school flag to the complete dataset
g4math.complete$charter <- g4math3$charter
g4read.complete$charter <- g4read3$charter
g8math.complete$charter <- g8math3$charter
g8read.complete$charter <- g8read3$charter

# View(g4math$catalog[g4math$catalog$FieldName %in% names(g4math.complete),
# 					c('FieldName','Description','CodeValues')], 'Catalog')

##### Unadjusted Results #######################################################
descriptives.out <- function(score, charter) {
	ttest <- t.test(score[charter], score[!charter])
	mn <- aggregate(score, by=list(charter), mean)
	std <- aggregate(score, by=list(charter), sd)
	result <- c(mn[2,2], std[2,2], mn[1,2], std[1,2], 
				(mn[2,2] - mn[1,2]), 
				as.numeric(ttest$conf.int) )
	names(result) <- c(paste0(mn[2,1], '.mean'), paste0(std[2,1], '.sd'),
					   paste0(mn[1,1], '.mean'), paste0(std[1,1], '.sd'),
					   'Diff', 'ci.min', 'ci.max')
	return(result)
}

# Descriptive stats of NAEP score for all TPS students and close TPS students
close.descriptives <- function(all, close, outvar) {
	close.desc <- c(
		descriptives.out(all[all$FIPS != 'Alaska',outvar], 
						 all[all$FIPS != 'Alaska',]$charter)[c('TRUE.mean', 'TRUE.sd', 
						 								'FALSE.mean', 'FALSE.sd', 'Diff')],
		table(all$charter)['FALSE'],
		descriptives.out(close[,outvar], close$charter)[c('FALSE.mean', 'FALSE.sd', 'Diff')],
		table(close$charter)['FALSE']
	)
	names(close.desc) <- c('Charter.mean','Charter.sd','All.mean','All.sd','All.diff','All.n',
								  'Close.mean','Close.sd','Close.diff','Close.n')
	return(close.desc)
}

close.desc <- as.data.frame(rbind(
	close.descriptives(g4math$data, g4math3, 'mathscore'),
	close.descriptives(g4read$data, g4read3, 'readscore'),
	close.descriptives(g8math$data, g8math3, 'mathscore'),
	close.descriptives(g8read$data, g8read3, 'readscore')
))
close.desc$Subject <- c('Grade 4 Math', 'Grade 4 Reading', 'Grade 8 Math', 'Grade 8 Reading')
close.desc <- close.desc[,c(11,1,3,6,5,7,10,9)]
close.desc$All.n <- as.integer(close.desc$All.n)
close.desc$Close.n <- as.integer(close.desc$Close.n)

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste0(' & \\multicolumn{1}{c}{Charter} & ',
							 '\\multicolumn{3}{c}{All Public Schools} & ',
							 '\\multicolumn{3}{c}{Close Public Schools} \\\\',
							 ' \\cline{2-2} \\cline{3-5} \\cline{6-8} ',
							 ' Subject & Mean & Mean & n & Diff & ',
							 ' Mean & n & Diff \\\\ '))
x <- xtable(close.desc, digits=1, label='dependentDescriptivesAllAndClose',
			caption='Descriptive statistics of dependent variables (unadjusted) for all and close (within 5 miles) traditional public schools',
			align=c('l','l','r@{\\extracolsep{.2cm}}','r','r','r@{\\extracolsep{.2cm}}','r','r','r') )
print(x, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow,
	  caption.placement='top', file='../Tables2009/descriptivesClose.tex')



unadj.out <- rbind(descriptives.out(g4math3$mathscore, g4math3$charter),
				   descriptives.out(g4read3$readscore, g4read3$charter),
				   descriptives.out(g8math3$mathscore, g8math3$charter),
				   descriptives.out(g8read3$readscore, g8read3$charter))
unadj.out <- as.data.frame(unadj.out)
unadj.out$Subject <- c('Grade 4 Math','Grade 4 Reading','Grade 8 Math','Grade 8 Reading')
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste0(' & \\multicolumn{2}{c}{Charter} & ',
							 '\\multicolumn{2}{c}{Public} & Mean & ',
							 '\\multicolumn{2}{c}{Confidence} \\\\',
							 ' \\cline{2-3} \\cline{4-5} ',
							 ' Subject & Mean & SD & Mean & SD & Difference & ',
							 '\\multicolumn{2}{c}{Interval} \\\\ '))
x <- xtable(unadj.out[,c(8,1:7)], digits=1, label='dependentDescriptives',
			caption='Descriptive statistics of dependent variables (unadjusted)',
			align=c('l','l','r','r@{\\extracolsep{.2cm}}','r','r','r','r','r') )
print(x, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow,
	  caption.placement='top', file='../Tables2009/descriptives.tex')

##### Propensity Score Analysis ################################################
# The naep.analysis function is the workhorse for all of the propensity score
# analysis to be conducted. It will save many figures and LaTeX tables as it
# runs and will return the a data frame with the overall results for each method.
g4math.overall <- naep.analysis(naep=g4math3,
								complete=g4math.complete,
								score=g4math3$mathscore,
								catalog=g4math$catalog,
								grade=4,
								subject='math',
								cv.map=g4math.cv.map)

g4read.overall <- naep.analysis(naep=g4read3, 
								complete=g4read.complete, 
								score=g4read3$readscore, 
								catalog=g4read$catalog,
								grade=4, 
								subject='read',
								cv.map=g4read.cv.map)

g8math.overall <- naep.analysis(naep=g8math3, 
								complete=g8math.complete, 
								score=g8math3$mathscore, 
								catalog=g8math$catalog,
								grade=8, 
								subject='math',
								cv.map=g8math.cv.map)

g8read.overall <- naep.analysis(naep=g8read3, 
								complete=g8read.complete, 
								score=g8read3$readscore, 
								catalog=g8read$catalog,
								grade=8, 
								subject='read',
								cv.map=g8read.cv.map)

save(g4math3, g4read3, g8math3, g8read3, file='../Data2009/SensitivityAnalysis.Rda')

# Divide by standard deviation to get effect size
g4math.overall$std.ci.min <- g4math.overall$ci.min / sd(g4math3$mathscore)
g4math.overall$std.ci.max <- g4math.overall$ci.max / sd(g4math3$mathscore)
g4math.overall$es <- g4math.overall$diff / sd(g4math3$mathscore)

g4read.overall$std.ci.min <- g4read.overall$ci.min / sd(g4read3$readscore)
g4read.overall$std.ci.max <- g4read.overall$ci.max / sd(g4read3$readscore)
g4read.overall$es <- g4read.overall$diff / sd(g4read3$readscore)

g8math.overall$std.ci.min <- g8math.overall$ci.min / sd(g8math3$mathscore)
g8math.overall$std.ci.max <- g8math.overall$ci.max / sd(g8math3$mathscore)
g8math.overall$es <- g8math.overall$diff / sd(g8math3$mathscore)

g8read.overall$std.ci.min <- g8read.overall$ci.min / sd(g8read3$readscore)
g8read.overall$std.ci.max <- g8read.overall$ci.max / sd(g8read3$readscore)
g8read.overall$es <- g8read.overall$diff / sd(g8read3$readscore)

g4math.overall$GradeSubject <- 'Grade 4 Math'
g4read.overall$GradeSubject <- 'Grade 4 Reading'
g8math.overall$GradeSubject <- 'Grade 8 Math'
g8read.overall$GradeSubject <- 'Grade 8 Reading'

overall <- rbind(g4math.overall, g4read.overall, g8math.overall, g8read.overall)
write.csv(overall, file='../Data2009/OverallResults.csv', row.names=FALSE)

# Create table with n's for number of students and states used in charter law analysis
mlpsa.overall <- overall[overall$class == 'Multilevel PSA' & 
						 overall$method == 'Classification Trees',
						 c('GradeSubject','n.states','charter.n','public.n')]
mlpsa.overall$n.states <- as.integer(mlpsa.overall$n.states)
mlpsa.overall$charter.n <- as.integer(mlpsa.overall$charter.n)
mlpsa.overall$public.n <- as.integer(mlpsa.overall$public.n)
names(mlpsa.overall) <- c('Grade and Subject', 'State n', 'Charter n', 'Public n')
x <- xtable(mlpsa.overall, align=c('l','l','r','r','r'),
			label='tab:stateNs',
			caption='Number of students and states used for the analysis of charter laws')
print(x, include.rownames=FALSE, include.colnames=TRUE,
	  caption.placement='top', file='../Tables2009/OverallStateNs.tex')


#overall <- read.csv('../Data2009/OverallResults.csv')

overall$x <- paste0(overall$class, ' ', overall$method)
overall$x <- factor(overall$x, levels=overall[9:1,]$x, ordered=TRUE)

# Create a figure to summarize the overall results
ggplot(overall, aes(x=x, y=es, ymin=std.ci.min, ymax=std.ci.max)) +
	geom_hline(xintercept=0, alpha=1) +
	geom_errorbar(color='green') + geom_point(color='blue') +
	facet_wrap(~ GradeSubject, ncol=2) + coord_flip() + 
	xlab('') + ylab('Effect Size (charter - public)') +
	ylim(c(-.25, 0.25))

ggsave('../Figures2009/Overall.pdf', width=8, height=6)


# Overall circle plot
circle <- function(center = c(0,0), diameter = 1, npoints = 100, label, GradeSubject){
	r = diameter / 2
	tt <- seq(0,2*pi,length.out = npoints)
	xx <- center[1] + r * cos(tt)
	yy <- center[2] + r * sin(tt)
	if(missing(label)) {
		return(data.frame(x = xx, y = yy))
	} else {
		return(data.frame(label = label, x = xx, y = yy, GradeSubject=GradeSubject))
	}
}
overall$ci <- overall$ci.max - overall$ci.min

# Unadjusted results
overall.unadj <- as.data.frame(matrix(c(
	mean(g4math3[g4math3$charter,]$mathscore), mean(g4math3[!g4math3$charter,]$mathscore), mean(g4math3$mathscore), sd(g4math3$mathscore),
	mean(g4read3[g4read3$charter,]$readscore), mean(g4read3[!g4read3$charter,]$readscore), mean(g4read3$readscore), sd(g4read3$readscore),
	mean(g8math3[g8math3$charter,]$mathscore), mean(g8math3[!g8math3$charter,]$mathscore), mean(g8math3$mathscore), sd(g8math3$mathscore),
	mean(g8read3[g8read3$charter,]$readscore), mean(g8read3[!g8read3$charter,]$readscore), mean(g8read3$readscore), sd(g8read3$readscore)	
	), ncol=4, byrow=TRUE))
names(overall.unadj) <- c('Charter', 'Public', 'Overall', 'SD')
overall.unadj$GradeSubject <- c('Grade 4 Math', 'Grade 4 Reading', 'Grade 8 Math', 'Grade 8 Reading')

circs <- data.frame()
for(i in 1:nrow(overall)) {
	circs <- rbind(circs, 
				   circle(center=c(overall[i,]$charter, overall[i,]$public), 
				   	   diameter=overall[i,]$ci, 
				   	   label=paste0(overall[i,]$GradeSubject, ' ', overall[i,]$method),
				   	   GradeSubject=paste0(overall[i,]$GradeSubject)))
}

limits <- range(overall[,c('charter','public')])
range <- diff(limits)
limits[1] <- limits[1] - .15 * range
limits[2] <- limits[2] + .15 * range
#ggplot(circs, aes(x=x, y=y)) +
ggplot(overall) +
	geom_segment(data=overall.unadj, aes(x=Charter, xend=Charter, y=limits[1], 
										 yend=Public, color=GradeSubject)) +
	geom_segment(data=overall.unadj, aes(x=limits[1], xend=Charter, y=Public,
										 yend=Public, color=GradeSubject)) +
	geom_text(data=overall.unadj, aes(x=limits[1], y=Public, 
									  label=paste0(round(Public), ' '), 
									  color=GradeSubject), 
			  hjust=-0.1, vjust=-.2, size=4) +
	geom_text(data=overall.unadj, aes(y=limits[1], x=Charter, 
									  label=paste0(round(Charter), ' '), 
									  color=GradeSubject), 
			  hjust=1.1, vjust=-.2, size=4, angle=-90) +
	geom_abline(slope=1, intercept=0, alpha=.3) +
	geom_polygon(data=circs, aes(x=x, y=y, group=label), fill='green', alpha=.3) + 
	geom_point(data=overall, aes(x=charter, y=public, color=GradeSubject, shape=class)) +
	coord_fixed(ratio=1) + xlab('Charter') + ylab('Public') +
	scale_x_continuous(limits=limits, expand=c(0,0)) + 
	scale_y_continuous(limits=limits, expand=c(0,0)) + 
	scale_color_hue('Grade & Subject') +
	scale_shape('PSA Method') +
	theme(panel.margin=rep(unit(0,'cm'), 4))

ggplot(overall) +
	geom_segment(data=overall.unadj, aes(x=Charter, xend=Charter, y=limits[1], 
										 yend=Public), alpha=.7) +
	geom_segment(data=overall.unadj, aes(x=limits[1], xend=Charter, y=Public,
										 yend=Public), alpha=.7) +
	geom_text(data=overall.unadj, aes(x=limits[1], y=Public, 
									  label=paste0(round(Public), ' ')), 
			  hjust=-0.1, vjust=-.2, size=4) +
 	geom_text(data=overall.unadj, aes(y=limits[1], x=Charter, 
 									  label=paste0(round(Charter), ' ')), 
 			  hjust=1.1, vjust=-.2, size=4, angle=-90) +
	geom_abline(slope=1, intercept=0, alpha=.3) +
	geom_polygon(data=circs, aes(x=x, y=y, group=label), fill='green', alpha=.3) + 
	geom_point(data=overall, aes(x=charter, y=public, shape=class, alpha=.5)) +
	coord_fixed(ratio=1) + xlab('Charter') + ylab('Public') +
	scale_x_continuous(limits=limits, expand=c(0,0)) + 
	scale_y_continuous(limits=limits, expand=c(0,0)) + 
	scale_shape('PSA Method') +
	theme(panel.margin=rep(unit(0,'cm'), 4)) +
	facet_wrap( ~ GradeSubject)

ggsave('../Figures2009/OverallScatter.pdf', width=8, height=6)

# Overall LaTeX Table
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$pos[[2]] <- c(9)
addtorow$pos[[3]] <- c(18)
addtorow$pos[[4]] <- c(27)
addtorow$pos[[5]] <- c(-1)
addtorow$command <- c(
	paste0(' \\hline & \\multicolumn{5}{c}{', 
		   c(overall[1,'GradeSubject'], overall[10,'GradeSubject'],
		     overall[19,'GradeSubject'], overall[28,'GradeSubject']),
		   '} \\\\ \\cline{2-6} '),
	'\\hline Method & Charter & Public & ATE & \\multicolumn{2}{c}{95\\% CI} \\\\')
x <- xtable(overall[,c('x','charter','public','es','ci.min','ci.max')],
			caption='Summary of overall propensity score results', label='tab:overall')
print(x, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, 
	  hline.after=c(nrow(overall)), file='../Tables2009/Overall.tex')
