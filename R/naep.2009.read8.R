setwd('~/Dropbox/School/Dissertation/R')

library(naep)
library(foreach)
library(plyr)
library(compiler)
library(mice)
library(multilevelPSA)

##### Grade 8 Reading
g8read.dir = getBaseDirectory()
g8read.dir = '/Volumes/NAEP2009/NAEP 2009 Reading G4G8'
g8read.catalog = getNAEPCatalog(year=2009, grade=8, subject='Read', 
								directory=g8read.dir, type='Student', sample='AT')
if(file.exists('naep.2009.read.vars.R')) {
	source('naep.2009.read.vars.R')
} else {
	g8read.vars = selectVariables(year=2009, grade=8, subject='Read', 
								  directory=g8read.dir, type='Student', sample='AT')
	cat(paste("g8read.vars = c(", 
			  paste("'", g8read.vars, "'", collapse=", ", sep=''), 
			  ")\n"), file='naep.2009.read.vars.R')
}
g8read.catalog[which(g8read.catalog$FieldName %in% g8read.vars), c('FieldName', 'Description')]
g8read = getNAEPData(year=2009, grade=8, subject='Read', directory=g8read.dir,
					 vars=g8read.vars, type='Student', sample='AT')
g8read = g8read[which(g8read$SCHTYPE == 'Public'),] #Remove non-public schools
save(g8read, g8read.catalog, g8read.vars, file='data2009/g8read.Rdata')

load('data2009/g8read.Rdata')

names(g8read); nrow(g8read)

table(g8read$SCHTYPE, useNA='ifany')
table(g8read$CHRTRPT, useNA='ifany')

g8read$charter = ifelse(g8read$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g8read$charter, useNA='ifany')

## Merge with the distances data (derived from he common core data)
distances = read.csv('data2009/SchoolDistances.csv')
head(g8read$NCESSCH)
head(distances$NCESSCH)
table(g8read$NCESSCH %in% distances$NCESSCH)
#distances$NCESSCH = as.character(distances$NCESSCH)
g8read$NCESSCH = as.character(g8read$NCESSCH)
g8read2 = merge(g8read, distances, by.x='NCESSCH', by.y='NCESSCH', all.x=TRUE)
table(is.na(g8read2$dist))
g8read2 = g8read2[g8read2$charter | !is.na(g8read2$dist),] #Distances of NA are from states with no charter schools
nrow(g8read); nrow(g8read2) #If not the same, something went wrong

## Remove non-charter schools that are not close (i.e. > 5 miles)
g8read3 = g8read2[which(g8read2$charter | g8read2$dist < 5),]
nrow(g8read3) / nrow(g8read) * 100 #Percent remaining
table(g8read3$charter, useNA='ifany')
table(g8read3$FIPS, g8read3$charter, useNA='ifany')
stateXcharter = as.data.frame(table(g8read3$FIPS, g8read3$charter, useNA='ifany'))
stateXcharter$Var2 = as.logical(stateXcharter$Var2)
stateXcharter$Var1 = as.character(stateXcharter$Var1)
stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),c('Var1','Freq')]
states = stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),'Var1']
g8read3 = g8read3[g8read3$FIPS %in% states,]

## Impute missing values
g8read.toimpute = g8read3[, c('DSEX','SRACE','SLUNCH1','SD4','ELL3',
							  'SDELL','IEP','B017001','B000905','B013801','B017101',
							  'B017201','B001151','B017451','B018101','B003501',
							  'B003601','B018201')]
plot.missing(g8read.toimpute, g8read3$FIPS02)
g8read.mice = mice(g8read.toimpute, m=1)
save(g8read.mice, file='data2009/g8read.mice.Rdata')
load('data2009/g8read.mice.Rdata')


##
g8read.complete = complete(g8read.mice, 1)
names(g8read.complete)
table(g8read.complete$PCHARTR, useNA='ifany')
g8read.complete$PCHARTR = as.numeric(g8read.complete$PCHARTR)
g8read.complete$PCHARTR = g8read.complete$PCHARTR %% 2
g8read.lr = glm(PCHARTR ~ ., data=g8read.complete, family='binomial')

readscore = apply(g8read3[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
df = data.frame(read=readscore, ps=fitted(g8read.lr), charter=g8read3$charter, state=g8read3$FIPS)
ggplot(df, aes(x=ps, y=read, colour=charter)) + geom_smooth()
ggplot(df, aes(x=ps, y=read, colour=charter)) + geom_smooth() + facet_wrap(~ state)

source('align.R')
pmain = ggplot(df, aes(x=ps, y=read, colour=charter))
pmain = pmain + geom_point(data=df[sample(which(df$charter), (.1 * length(which(df$charter))), replace=FALSE),], aes(x=ps, y=read, colour=charter), alpha=.2)
pmain = pmain + geom_point(data=df[sample(which(!df$charter), round(.01 * length(which(!df$charter))), replace=FALSE),], aes(x=ps, y=read, colour=charter), alpha=.2)
pmain = pmain + geom_smooth() + ylab('Grade 8 Reading Score') + xlab("Propensity Score") + opts(legend.position=c(.8,.8), legend.justification='left') + scale_colour_hue('Charter School')
ptop = ggplot(df, aes(x=ps, colour=charter)) + geom_density() + opts(legend.position=c(-1,-1)) + xlab(NULL) + ylab('Density')
pright = ggplot(df, aes(x=read, colour=charter)) + geom_density() + coord_flip() + opts(legend.position='none') + xlab(NULL) + ylab('Density')
grid_layout <- grid.layout(nrow=2, ncol=2, widths=c(3,1), heights=c(1,3))
grid.newpage()
pushViewport( viewport( layout=grid_layout ) )
align.plots(grid_layout, list(ptop, 1, 1), list(pmain, 2, 1), list(pright, 2, 2))

nStrata = 5
strata = cut(df$ps, breaks=quantile(df$ps, seq(0,1,1/nStrata), na.rm=TRUE), labels=paste('Strata', 1:nStrata, sep=''))
table(strata)
df = cbind(df, strata)
df = df[!is.na(df$strata),]
ggplot(df, aes(x=read, colour=charter)) + geom_density() + facet_wrap(~ strata, ncol=1)
ggplot(df, aes(y=read, x=charter, colour=charter)) + geom_boxplot() + facet_wrap(~ strata, ncol=1) + coord_flip() + xlab(NULL)

#Random forest
library(randomForest)
rf = dlply(g8read.complete)
fit <- randomForest(PCHARTR ~ ., data=g8read.complete) #TODO: Try other methods for na.action
print(fit) # view results 
importance(fit) # importance of each predictor


