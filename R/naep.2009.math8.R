setwd('~/Dropbox/School/Dissertation/R')

library(naep)
library(foreach)
library(plyr)
library(compiler)
library(mice)
library(multilevelPSA)

##### Grade 8 Math
g8math.dir = getBaseDirectory()
g8math.dir = '/Volumes/NAEP2009/NAEP 2009 Math G4G8'
g8math.catalog = getNAEPCatalog(year=2009, grade=8, subject='Math', 
								directory=g8math.dir, type='Student', sample='AT')
if(file.exists('naep.2009.math.vars.R')) {
	source('naep.2009.math.vars.R')
} else {
	g8math.vars = selectVariables(year=2009, grade=8, subject='Math', 
								  directory=g8math.dir, type='Student', sample='AT')
	cat(paste("g8math.vars = c(", 
			  paste("'", g8math.vars, "'", collapse=", ", sep=''), 
			  ")\n"), file='naep.2009.math.vars.R')
}
g8math.catalog[which(g8math.catalog$FieldName %in% g8math.vars), c('FieldName', 'Description')]
g8math = getNAEPData(year=2009, grade=8, subject='Math', directory=g8math.dir,
					 vars=g8math.vars, type='Student', sample='AT')
g8math = g8math[which(g8math$SCHTYPE == 'Public'),] #Remove non-public schools
save(g8math, g8math.catalog, g8math.vars, file='data2009/g8math.Rdata')

load('data2009/g8math.Rdata')

names(g8math); nrow(g8math)

table(g8math$SCHTYPE, useNA='ifany')
table(g8math$CHRTRPT, useNA='ifany')

g8math$charter = ifelse(g8math$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g8math$charter, useNA='ifany')

## Merge with the distances data (derived from he common core data)
distances = read.csv('data2009/SchoolDistances.csv')
head(g8math$NCESSCH)
head(distances$NCESSCH)
table(g8math$NCESSCH %in% distances$NCESSCH)
#distances$NCESSCH = as.character(distances$NCESSCH)
g8math$NCESSCH = as.character(g8math$NCESSCH)
g8math2 = merge(g8math, distances, by.x='NCESSCH', by.y='NCESSCH', all.x=TRUE)
table(is.na(g8math2$dist))
g8math2 = g8math2[g8math2$charter | !is.na(g8math2$dist),] #Distances of NA are from states with no charter schools
nrow(g8math); nrow(g8math2) #If not the same, something went wrong

## Remove non-charter schools that are not close (i.e. > 5 miles)
g8math3 = g8math2[which(g8math2$charter | g8math2$dist < 5),]
nrow(g8math3) / nrow(g8math) * 100 #Percent remaining
table(g8math3$charter, useNA='ifany')
table(g8math3$FIPS, g8math3$charter, useNA='ifany')
stateXcharter = as.data.frame(table(g8math3$FIPS, g8math3$charter, useNA='ifany'))
stateXcharter$Var2 = as.logical(stateXcharter$Var2)
stateXcharter$Var1 = as.character(stateXcharter$Var1)
stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),c('Var1','Freq')]
states = stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),'Var1']
g8math3 = g8math3[g8math3$FIPS %in% states,]

## Impute missing values
g8math.toimpute = g8math3[, c('PCHARTR','DSEX','SRACE','SLUNCH1','SD4','ELL3',
								  'SDELL','IEP','B017001','B000905','B013801','B017101',
							  'B017201','B001151','B017451','B018101','B003501',
							  'B003601','B018201','M821401')]
plot.missing(g8math.toimpute, g8math3$FIPS02)
g8math.mice = mice(g8math.toimpute, m=1)
save(g8math.mice, file='data2009/g8math.mice.Rdata')
load('data2009/g8math.mice.Rdata')


##
g8math.complete = complete(g8math.mice, 1)
names(g8math.complete)
table(g8math.complete$PCHARTR, useNA='ifany')
g8math.complete$PCHARTR = as.numeric(g8math.complete$PCHARTR)
g8math.complete$PCHARTR = g8math.complete$PCHARTR %% 2
g8math.lr = glm(PCHARTR ~ ., data=g8math.complete, family='binomial')

mathscore = apply(g8math3[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
df = data.frame(math=mathscore, ps=fitted(g8math.lr), charter=g8math3$charter, state=g8math3$FIPS)
ggplot(df, aes(x=ps, y=math, colour=charter)) + geom_smooth()
ggplot(df, aes(x=ps, y=math, colour=charter)) + geom_smooth() + facet_wrap(~ state)

source('align.R')
pmain = ggplot(df, aes(x=ps, y=math, colour=charter))
pmain = pmain + geom_point(data=df[sample(which(df$charter), (.1 * length(which(df$charter))), replace=FALSE),], aes(x=ps, y=math, colour=charter), alpha=.2)
pmain = pmain + geom_point(data=df[sample(which(!df$charter), round(.01 * length(which(!df$charter))), replace=FALSE),], aes(x=ps, y=math, colour=charter), alpha=.2)
pmain = pmain + geom_smooth() + ylab('Grade 8 Math Score') + xlab("Propensity Score") + opts(legend.position=c(.8,.8), legend.justification='left') + scale_colour_hue('Charter School')
ptop = ggplot(df, aes(x=ps, colour=charter)) + geom_density() + opts(legend.position=c(-1,-1)) + xlab(NULL) + ylab('Density')
pright = ggplot(df, aes(x=math, colour=charter)) + geom_density() + coord_flip() + opts(legend.position='none') + xlab(NULL) + ylab('Density')
grid_layout <- grid.layout(nrow=2, ncol=2, widths=c(3,1), heights=c(1,3))
grid.newpage()
pushViewport( viewport( layout=grid_layout ) )
align.plots(grid_layout, list(ptop, 1, 1), list(pmain, 2, 1), list(pright, 2, 2))

nStrata = 5
strata = cut(df$ps, breaks=quantile(df$ps, seq(0,1,1/nStrata), na.rm=TRUE), labels=paste('Strata', 1:nStrata, sep=''))
table(strata)
df = cbind(df, strata)
df = df[!is.na(df$strata),]
ggplot(df, aes(x=math, colour=charter)) + geom_density() + facet_wrap(~ strata, ncol=1)
ggplot(df, aes(y=math, x=charter, colour=charter)) + geom_boxplot() + facet_wrap(~ strata, ncol=1) + coord_flip() + xlab(NULL)

#Random forest
library(randomForest)
rf = dlply(g8math.complete)
fit <- randomForest(PCHARTR ~ ., data=g8math.complete) #TODO: Try other methods for na.action
print(fit) # view results 
importance(fit) # importance of each predictor


