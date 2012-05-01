setwd('~/Dropbox/School/Dissertation/R')

library(naep)
library(foreach)
library(plyr)
library(compiler)
library(mice)
library(multilevelPSA)

##### Grade 4 Reading
g4read.dir = getBaseDirectory()
g4read.dir = '/Volumes/NAEP2009/NAEP 2009 Reading G4G8'
g4read.catalog = getNAEPCatalog(year=2009, grade=4, subject='Reading', 
								directory=g4read.dir, type='Student', sample='AT')
if(file.exists('naep.2009.read4.vars.R')) {
	source('naep.2009.read4.vars.R')
} else {
	g4read.vars = selectVariables(year=2009, grade=4, subject='Reading', 
								  directory=g4read.dir, type='Student', sample='AT')
	cat(paste("g4read.vars = c(", 
			  paste("'", g4read.vars, "'", collapse=", ", sep=''), 
			  ")\n"), file='naep.2009.read.vars.R')
}
g4read.catalog[which(g4read.catalog$FieldName %in% g4read.vars), c('FieldName', 'Description')]
g4read = getNAEPData(year=2009, grade=4, subject='Reading', directory=g4read.dir,
					 vars=g4read.vars, type='Student', sample='AT')
g4read = g4read[which(g4read$SCHTYPE == 'Public'),] #Remove non-public schools
save(g4read, g4read.catalog, g4read.vars, file='data2009/g4read.Rdata')

load('data2009/g4read.Rdata')

names(g4read); nrow(g4read)

table(g4read$SCHTYPE, useNA='ifany')
table(g4read$CHRTRPT, useNA='ifany')

g4read$charter = ifelse(g4read$CHRTRPT == 'Charter school', TRUE, FALSE)
table(g4read$charter, useNA='ifany')

## Merge with the distances data (derived from he common core data)
distances = read.csv('data2009/SchoolDistances.csv')
head(g4read$NCESSCH)
head(distances$NCESSCH)
table(g4read$NCESSCH %in% distances$NCESSCH)
#distances$NCESSCH = as.character(distances$NCESSCH)
g4read$NCESSCH = as.character(g4read$NCESSCH)
g4read2 = merge(g4read, distances, by.x='NCESSCH', by.y='NCESSCH', all.x=TRUE)
table(is.na(g4read2$dist))
g4read2 = g4read2[g4read2$charter | !is.na(g4read2$dist),] #Distances of NA are from states with no charter schools

## Remove non-charter schools that are not close (i.e. > 5 miles)
g4read3 = g4read2[which(g4read2$charter | g4read2$dist < 5),]
nrow(g4read3) / nrow(g4read) * 100 #Percent remaining
table(g4read3$charter, useNA='ifany')
table(g4read3$FIPS, g4read3$charter, useNA='ifany')
stateXcharter = as.data.frame(table(g4read3$FIPS, g4read3$charter, useNA='ifany'))
stateXcharter$Var2 = as.logical(stateXcharter$Var2)
stateXcharter$Var1 = as.character(stateXcharter$Var1)
stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),c('Var1','Freq')]
states = stateXcharter[which(stateXcharter$Freq > 0 & stateXcharter$Var2),'Var1']
g4read3 = g4read3[g4read3$FIPS %in% states,]

## Impute missing values
g4read.toimpute = g4read3[, c('PCHARTR','DSEX','SRACE','SLUNCH1','SD4','ELL3',
							  'SDELL','IEP','B017001','B000905','B013801','B017101',
							  'B017201','B001151','B017451','B018101','B018201')]
plot.missing(g4read.toimpute, g4read3$FIPS02)
g4read.mice = mice(g4read.toimpute, m=1)
save(g4read.mice, file='data2009/g4read.mice.Rdata')
load('data2009/g4read.mice.Rdata')


##
g4read.complete = complete(g4read.mice, 1)
names(g4read.complete)
table(g4read.complete$PCHARTR, useNA='ifany')
g4read.complete$PCHARTR = as.numeric(g4read.complete$PCHARTR)
g4read.complete$PCHARTR = g4read.complete$PCHARTR %% 2
g4read.lr = glm(PCHARTR ~ ., data=g4read.complete, family='binomial')

readscore = apply(g4read3[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
df = data.frame(read=readscore, ps=fitted(g4read.lr), charter=g4read3$charter, state=g4read3$FIPS)
ggplot(df, aes(x=ps, y=read, colour=charter)) + geom_smooth()
ggplot(df, aes(x=ps, y=read, colour=charter)) + geom_smooth() + facet_wrap(~ state)

source('align.R')
pmain = ggplot(df, aes(x=ps, y=read, colour=charter))
pmain = pmain + geom_point(data=df[sample(which(df$charter), (.1 * length(which(df$charter))), replace=FALSE),], aes(x=ps, y=read, colour=charter), alpha=.2)
pmain = pmain + geom_point(data=df[sample(which(!df$charter), round(.01 * length(which(!df$charter))), replace=FALSE),], aes(x=ps, y=read, colour=charter), alpha=.2)
pmain = pmain + geom_smooth() + ylab('Grade 4 Reading Score') + xlab("Propensity Score") + opts(legend.position=c(.8,.8), legend.justification='left') + scale_colour_hue('Charter School')
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
rf = dlply(g4read.complete)
fit <- randomForest(PCHARTR ~ ., data=g4read.complete) #TODO: Try other methods for na.action
print(fit) # view results 
importance(fit) # importance of each predictor


