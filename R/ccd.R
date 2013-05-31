library(foreign)
library(fields)
library(plyr)

setwd('~/Dropbox/School/Dissertation/CCD')
#http://nces.ed.gov/ccd/pubschuniv.asp
ccd = read.spss('2008-09/sc081b.sav', to.data.frame=TRUE, use.value.labels=TRUE)
names(ccd); nrow(ccd)

ccd2 = ccd[,c('NCESSCH', 'SCHNO', 'SCHNAM08', 'MSTATE08', 'LCITY08', 'LSTATE08', 'LZIP08', 'TYPE08', 'LATCOD08', 'LONCOD08', 'CDCODE08', 'FTE08', 'LEVEL08', 'MAGNET08', 'CHARTR08', 'MEMBER08', 'PUPTCH08')]
ccd2$NCESSCH = as.character(ccd2$NCESSCH)
ccd2$LONCOD08 = as.numeric(ccd2$LONCOD08) / 1000
ccd2$LATCOD08 = as.numeric(ccd2$LATCOD08) / 1000
cpt = table(ccd2$LSTATE08, ccd2$CHARTR08, useNA='ifany')
cpt = as.data.frame(cpt)
charterStates = as.character(cpt[which(cpt$Var2 == 'Yes' & cpt$Freq > 0),'Var1'])
charters = ccd2[which(ccd2$CHARTR08 == 'Yes'),]
notCharters = ccd2[which(ccd2$CHARTR08 != 'Yes'),]
notCharters = notCharters[which(notCharters$LSTATE08 %in% charterStates),]
notCharters$LSTATE08 = as.character(notCharters$LSTATE08)

distances = data.frame(NCESSCH=character(), dist=numeric(), state=character())
for(s in unique(notCharters$LSTATE08)) {
	print(s)
	ids = notCharters[which(notCharters$LSTATE08 == s),]$NCESSCH
	p = notCharters[which(notCharters$LSTATE08 == s),c('LONCOD08', 'LATCOD08')]
	c = charters[which(charters$LSTATE08 == s),c('LONCOD08', 'LATCOD08')]
	distMatrix = rdist.earth(p, c)
	dist = apply(distMatrix, 1, min)
	distances = rbind(distances, data.frame(NCESSCH=ids, dist=dist, state=rep(s, length(ids))))
}
head(distances)
save(distances, file='../R/data2009/SchoolDistances.rda')

