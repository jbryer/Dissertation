setwd('~/Dropbox/School/Dissertation/R')

library(naep)
library(foreach)
library(plyr)
library(compiler)
library(mice)

mice <- cmpfun(mice)

##### Grade 8 Math
g8math.dir = getBaseDirectory()
g8math.dir = '/Volumes/NAEP2009/NAEP 2009 Math G4G8'
g8math.catalog = getNAEPCatalog(year=2009, grade=8, subject='Math', directory=g8math.dir, type='Student')
g8math.vars = selectVariables(year=2009, grade=8, subject='Math', directory=g8math.dir, type='Student')
g8math.catalog[which(g8math.catalog$FieldName %in% g8math.vars), c('FieldName', 'Description')]
g8math = getNAEPData(year=2009, grade=8, subject='Math', directory=g8math.dir, vars=g8math.vars, type='Student')

save(g8math, g8math.catalog, g8math.vars, file='g8math.Rdata')
load('g8math.Rdata')

names(g8math); nrow(g8math)

table(g8math$SCHTYPE, useNA='ifany')
table(g8math[which(g8math$SCHTYPE == 'Public'),]$CHARTER, useNA='ifany')

g8math.toimpute = g8math[which(g8math$SCHTYPE == 'Public'), c('DSEX','SRACE','SLUNCH1','SD4','ELL3','NEWENRL','SDELL','IEP','IEP2009','B017001','B000905','B013801','B017101','B017201','B001151','B017451','B018101','B003501','B003601','B018201','M821401','M824901','M824902','M824903','M824904','M824905','M820901','M820904','M820905','M815401','M815501','M815601')]
missingPlot(g8math.toimpute, g8math.toimpute$CHARTER)
g8math.mice.plyr = dlply(g8math.toimpute, as.character(g8math[which(g8math$SCHTYPE == 'Public'),'FIPSLOC']), mice, .progress='text', .parallel=TRUE)
g8math.mice = mice(g8math.toimpute, m=1)
save(g8math.mice, file='g8math.mice.Rdata')



##### Grade 8 Reading
g8read.dir = getBaseDirectory()
g8read.dir = '/Volumes/NAEP2009/NAEP 2009 Reading G4G8'
g8read.catalog = getNAEPCatalog(year=2009, grade=8, subject='Reading', directory=g8read.dir, type='Student')
g8read.vars = selectVariables(year=2009, grade=8, subject='Reading', directory=g8read.dir, type='Student')
g8read.catalog[which(g8read.catalog$FieldName %in% g8read.vars), c('FieldName', 'Description')]
g8read = getNAEPData(year=2009, grade=8, subject='Reading', directory=g8read.dir, vars=g8read.vars, type='Student')

save(g8read, g8read.catalog, g8read.vars, file='g8read.Rdata')
load('g8read.Rdata')

names(g8read); nrow(g8read)

table(g8read$SCHTYPE, useNA='ifany')
table(g8read[which(g8read$SCHTYPE == 'Public'),]$CHARTER, useNA='ifany')

g8read.toimpute = g8read[which(g8read$SCHTYPE == 'Public'), c('DSEX','SRACE','SLUNCH1','SD4','ELL3','NEWENRL','SDELL','IEP','IEP2009','B017001','B000905','B013801','B017101','B017201','B001151','B017451','B018101','B003501','B003601','B018201','M821401','M824901','M824902','M824903','M824904','M824905','M820901','M820904','M820905','M815401','M815501','M815601')]
missingPlot(g8read.toimpute, g8read.toimpute$CHARTER)
g8read.mice = mice(g8read.toimpute, m=1)
save(g8read.mice, file='g8read.mice.Rdata')
