setwd('~/Dropbox/School/Dissertation/R')

library(naep)
library(foreach)
library(plyr)
library(compiler)
library(mice)
library(multilevelPSA)
library(PSAgraphics)
library(xtable)

##### Grade 4 Math #############################################################
# Data loading. There are the data elements we will begin with.
# g4math - a data frame containing the NAEP data
# g4math.catalog - the NAEP catalog of the variables we will use
# g4math.vars - a vector of variables read from the NAEP database
if(file.exists('data2009/g4math.Rdata')) {
	load('data2009/g4math.Rdata')
} else {
	g4math.dir = getBaseDirectory()
	g4math.dir = '/Volumes/NAEP2009/NAEP 2009 Math G4G8'
	g4math.catalog = getNAEPCatalog(year=2009, grade=4, subject='Math', 
							directory=g4math.dir, type='Student', sample='AT')
	if(file.exists('naep.2009.math.vars.R')) {
		source('naep.2009.math4.vars.R')
	} else {
		g4math.vars = selectVariables(year=2009, grade=4, subject='Math', 
							directory=g4math.dir, type='Student', sample='AT')
		cat(paste("g4math.vars = c(", 
				  paste("'", g4math.vars, "'", collapse=", ", sep=''), 
				  ")\n"), file='naep.2009.math.vars.R')
	}
	g4math.catalog[which(g4math.catalog$FieldName %in% g4math.vars), 
				   c('FieldName', 'Description')]
	g4math = getNAEPData(year=2009, grade=4, subject='Math', directory=g4math.dir,
						 vars=g4math.vars, type='Student', sample='AT')
	g4math = g4math[which(g4math$SCHTYPE == 'Public'),] #Remove non-public schools
	save(g4math, g4math.catalog, g4math.vars, file='data2009/g4math.Rdata')
}

names(g4math); nrow(g4math)
table(g4math$SCHTYPE, useNA='ifany')
table(g4math$CHRTRPT, useNA='ifany')

##### Create the treatment variable ############################################
g4math$charter = ifelse(g4math$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g4math$charter, useNA='ifany')

##### Merge with the distances data ############################################
#See ccd.R for how the distances were calculated
distances = load('data2009/SchoolDistances.rda')
g4math2 = merge(g4math, distances, by.x='NCESSCH', by.y='NCESSCH', all.x=TRUE)
table(g4math2$FIPS, g4math2$charter, useNA='ifany')
table(g4math2$charter, is.na(g4math2$dist))
table(g4math2$FIPS, is.na(g4math2$dist))
#Distances of NA are from states with no charter schools
g4math2 = g4math2[(g4math2$charter | !is.na(g4math2$dist)),] 

##### Remove non-charter schools that are not close (i.e. > 5 miles) ###########
g4math3 = g4math2[which(g4math2$charter | g4math2$dist < 5),]
nrow(g4math3) / nrow(g4math) * 100 #Percent remaining
table(g4math3$charter, useNA='ifany')
table(g4math3$FIPS, g4math3$charter, useNA='ifany')
stateXcharter = as.data.frame(table(g4math3$FIPS, g4math3$charter, useNA='ifany'))
stateXcharter$Var2 = as.logical(stateXcharter$Var2)
stateXcharter$Var1 = as.character(stateXcharter$Var1)
stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),c('Var1','Freq')]
states = stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),'Var1']
g4math3 = g4math3[g4math3$FIPS %in% states,]

##### Descriptive Stats ########################################################
source('Functions.R')
latexDescriptives(g4math, cols=c('SEX','SRACE','SLUNCH1','SD4','ELL3','SDELL','IEP'), 
			#names=c('Gender','Race','Free or Reduced Lunch','Has IEP', 'English Language Learner', 'Student with Disability', 'IEP'),
			caption='Grade 4 Math Descriptive Statistics',
			label='g4mathdesc',
			filename='../Tables2009/g4math-desc.tex')

df = data.frame(mathscore=apply(g4math[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean),
				charter=g4math$charter)
t.test(mathscore ~ charter, data=df)
sd(df[df$charter,'mathscore'], na.rm=TRUE)
sd(df[!df$charter,'mathscore'], na.rm=TRUE)
mean(df[df$charter,'mathscore'], na.rm=TRUE) - mean(df[!df$charter,'mathscore'], na.rm=TRUE)

##### Impute missing values ####################################################
if(file.exists('data2009/g4math.mice.Rdata')) {
	load('data2009/g4math.mice.Rdata')	
} else {
	g4math.toimpute = g4math3[, c('PCHARTR','DSEX','SRACE','SLUNCH1','SD4','ELL3',
								'SDELL','IEP','B017001','B000905','B013801','B017101',
								'B017201','B001151','B017451','B018101',
								'B018201','M821401')]
	plot.missing(g4math.toimpute, g4math3$FIPS02)
	g4math.mice = mice(g4math.toimpute, m=1)
	save(g4math.mice, file='data2009/g4math.mice.Rdata')
}

##### Phase I: Logistic Regression #############################################
g4math.complete = complete(g4math.mice, 1)
names(g4math.complete)
table(g4math.complete$PCHARTR, useNA='ifany')
g4math.complete$PCHARTR = as.numeric(g4math.complete$PCHARTR)
g4math.complete$PCHARTR = g4math.complete$PCHARTR %% 2
g4math.lr = glm(PCHARTR ~ ., data=g4math.complete, family='binomial')

mathscore = apply(g4math3[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
df = data.frame(math=mathscore, ps=fitted(g4math.lr), charter=g4math3$charter, state=g4math3$FIPS)
df = df[!is.na(df$math),]

##### Loess plots ##############################################################
ggplot(df, aes(x=ps, y=math, colour=charter)) + geom_smooth() + 
	ylab('Grade 4 Math Score') + xlab('Propensity Score') + 
	scale_colour_hue('Is Charter')
ggsave('../Figures2009/g4math-loess.pdf')
ggplot(df, aes(x=ps, y=math, colour=charter)) + geom_smooth() + facet_wrap(~ state) + 
	ylab('Grade 4 Math Score') + xlab('Propensity Score') + scale_colour_hue('Is Charter')
ggsave('../Figures2009/g4math-loess-states.pdf')

source('plot.loess.R')
plot.loess(ps=df$ps, response=df$math, treatment=df$charter, 
		   responseTitle='Grade 4 Math Score', treatmentTitle='Charter School')

##### Circ plot with 5 strata ##################################################
nStrata = 5
strata = cut(df$ps, breaks=quantile(df$ps, seq(0,1,1/nStrata), na.rm=TRUE), 
			 labels=paste('Strata', 1:nStrata, sep=''))
table(strata)
df = cbind(df, strata)
df = df[!is.na(df$strata),]
ggplot(df, aes(x=math, colour=charter)) + geom_density() + facet_wrap(~ strata, ncol=1)
ggplot(df, aes(y=math, x=charter, colour=charter)) + geom_boxplot() + 
	facet_wrap(~ strata, ncol=1) + coord_flip() + xlab(NULL) + 
	ylab('Grade 4 Math Score') + scale_colour_hue('Charter School')
ggsave('../Figures2009/g4math-strata5-boxplot.pdf')

#TODO: need to replace with the ggplot2 version
pdf('../Figures2009/g4math-circpsa5.pdf')
g4math.strata5.results = circ.psa(df$math, df$charter, df$strata)
dev.off()

##### Multilevel PSA ###########################################################
g4math.ml = g4math[,c('DSEX','SRACE','SLUNCH1','SD4','ELL3',
					'SDELL','IEP','B017001','B000905','B013801','B017101',
					'B017201','B001151','B017451','B018101',
					'B018201','M821401','charter','FIPS')]
g4math.ctree = mlpsa.ctree(g4math.ml, charter ~., level2='FIPS')
g4math.ctree.df = getStrata(g4math.mlpsa.results, g4math, level2='FIPS')
g4math.ctree.df$mathscore = apply(
		g4math.strata[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
g4math.ctree.df$charter2 = ifelse(g4math.ctree.df$charter, 'Charter', 'Public')
g4math.ctree.df$FIPS = as.character(g4math.ctree.df$FIPS)
g4math.ctree.df = merge(g4math.ctree.df, states, by.x='FIPS02', by.y='name', all.x=TRUE)
table(is.na(g4math.ctree.df$abbr))
g4math.mlpsa = mlpsa(response=g4math.ctree.df$math, treatment=g4math.ctree.df$charter2, 
					 strata=g4math.ctree.df$strata, level2=g4math.ctree.df$abbr, minN=2)

#Save the figures
pdf('../Figures2009/g4math-mlpsa-ctree.pdf')
plot(g4math.mlpsa)
dev.off()
plot.mlpsa.difference(g4math.mlpsa)
ggsave('../Figures2009/g4math-mlpsa-ctree-diff.pdf')

#Save the level 1 and 2 summary tables as LaTeX files
align = c('l','l','r','r@{\\extracolsep{10pt}}','r','r@{\\extracolsep{10pt}}','r')
x = xtable(g4math.mlpsa$level2.summary[,
					c('level2','Public','Public.n','Charter','Charter.n','diffwtd')], 
		   caption='Level 2 Summary Grade 4 Math (CITs)', label='g4mathCIT2', 
		   digits=1, align=align)
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste('\\thickline \\multirow{2}{*}{State} & \\multicolumn{2}{c}{Public} & ',
		'\\multicolumn{2}{c}{Charter} & \\multirow{2}{*}{Diff} \\\\ ',
		'\\cline{2-3} \\cline{4-5} & Mean & n & Mean & n & \\\\', sep=''))
print(x, file='../Tables2009/g4math-ctree-level2.tex', include.rownames=FALSE, 
		include.colnames=FALSE, add.to.row=addtorow)

tmp = unlist(strsplit(as.character(g4math.mlpsa$level1.summary$strata2), 
			split='.', fixed=TRUE))
l1 = cbind(tmp[seq(1,length(tmp), by=2)], tmp[seq(2,length(tmp), by=2)], 
		   g4math.mlpsa$level1.summary[,c('Public','Public.n','Charter','Charter.n','Diff')])
l1$Public.n = as.integer(l1$Public.n)
l1$Charter.n = as.integer(l1$Charter.n)
names(l1)[1:2] = c('State', 'Strata')
align = c('l','l','c','r','r@{\\extracolsep{10pt}}','r','r@{\\extracolsep{10pt}}','r')
x = xtable(l1, caption='Level 1 Summary Grade 4 Math (CITs)', label='g4mathCIT1', 
		   digits=1, align=align)
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste('\\thickline \\multirow{2}{*}{State} & \\multirow{2}{*}{Strata} & ',
			'\\multicolumn{2}{c}{Public} & ',
			'\\multicolumn{2}{c}{Charter} & \\multirow{2}{*}{Diff} \\\\ ',
			'\\cline{3-4} \\cline{5-6} & & Mean & n & Mean & n & \\\\ \\hline',
			'\\endfirsthead \\multicolumn{7}{l}{...continued from previous page} \\\\ ',
			'\\thickline \\multirow{2}{*}{State} & \\multirow{2}{*}{Strata} & ',
			'\\multicolumn{2}{c}{Public} & ',
			'\\multicolumn{2}{c}{Charter} & \\multirow{2}{*}{Diff} \\\\ ',
			'\\cline{3-4} \\cline{5-6} & & Mean & n & Mean & n & \\\\ \\hline \\endhead ',
			'\\thickline \\multicolumn{7}{r}{continued on next page...} \\\\ ',
			'\\endfoot \\multicolumn{7}{c}{} \\\\ \\endlastfoot ',
			sep=''))
print(x, file='../Tables2009/g4math-ctree-level1.tex', include.rownames=FALSE,
	  include.colnames=FALSE, add.to.row=addtorow, tabular.environment='longtable', 
	  floating=FALSE, hline.after=NULL, caption.placement='top')

