library(foreign)
library(fields)
library(plyr)

setwd('~/Dropbox/School/Dissertation/CCD')
#http://nces.ed.gov/ccd/pubschuniv.asp
ccd = read.spss('sc061c.sav', to.data.frame=TRUE, use.value.labels=TRUE)
names(ccd); nrow(ccd)

ccd2 = ccd[,c('NCESSCH', 'SCHNO', 'SCHNAM06', 'MSTATE06', 'LCITY06', 'LSTATE06', 'LZIP06', 'TYPE06', 'LATCOD06', 'LONCOD06', 'CDCODE06', 'FTE06', 'LEVEL06', 'MAGNET06', 'CHARTR06', 'MEMBER06', 'PUPTCH06')]
ccd2$NCESSCH = as.character(ccd2$NCESSCH)
ccd2$LONCOD06 = as.numeric(ccd2$LONCOD06) / 1000
ccd2$LATCOD06 = as.numeric(ccd2$LATCOD06) / 1000
cpt = table(ccd2$LSTATE06, ccd2$CHARTR06, useNA='ifany')
cpt = as.data.frame(cpt)
charterStates = as.character(cpt[which(cpt$Var2 == 'Yes' & cpt$Freq > 0),'Var1'])
charters = ccd2[which(ccd2$CHARTR06 == 'Yes'),]
publics = ccd2[which(ccd2$CHARTR06 == 'No'),] #TODO: May want to include missing and/or not applicable
publics = publics[which(publics$LSTATE06 %in% charterStates),]
publics$LSTATE06 = as.character(publics$LSTATE06)

distances = data.frame(NCESSCH=character(), dist=numeric())
for(s in unique(publics$LSTATE06)) {
	print(s)
	ids = publics[which(publics$LSTATE06 == s),]$NCESSCH
	p = publics[which(publics$LSTATE06 == s),c('LONCOD06', 'LATCOD06')]
	c = charters[which(charters$LSTATE06 == s),c('LONCOD06', 'LATCOD06')]
	distMatrix = rdist.earth(p, c)
	dist = apply(distMatrix, 1, min)
	distances = rbind(distances, data.frame(NSESSCH=ids, dist=dist))
}
distances$close = ifelse(distances$dist < 5, TRUE, FALSE)
head(distances)
table(distances$close, useNA='ifany')
ggplot(distances[!distances$close,], aes(x=dist)) + geom_histogram()

g8formula = charter ~ gender + race + iep + ell + lunch + parented + books + newspapers + magazines + computer + encyclopedia + pagesread + talkstudies + daysabsent + langinhome

g8math2 = cbind(g8math[,c(6,7,8,56)], complete(g8math.mice, 5))
g8math2 = merge(g8math2, g8math.school.orig[,c('school', 'ncesschoolid')], by.x='school', by.y='school', all.x=TRUE)
g8math2 = merge(g8math2, distances, by.x='ncesschoolid', by.y='NSESSCH', all.x=TRUE)
table(g8math2$close, useNA='ifany')
table(g8math2$charter, g8math2$close, useNA='ifany')

g8math3 = g8math2[(g8math2$charter == 'Charter' | g8math2$close == TRUE),]
g8math3 = g8math3[!is.na(g8math3$charter),]
100 * (nrow(g8math2) - nrow(g8math3)) / (nrow(g8math2)) #Percentage of rows removed
table(g8math3$charter, g8math3$close, useNA='ifany')

lr = glm(g8formula, data=g8math.complete, family=binomial) #All publics
toplot = data.frame(ps=fitted(lr), math=g8math.complete$mathscore, charter=g8math.complete$charter)
ggplot(toplot, aes(x=ps, y=math, colour=charter)) + geom_smooth()

lr2 = glm(g8formula, data=g8math3, family=binomial) #Only 'close' publics
toplot = data.frame(ps=fitted(lr2), math=g8math3$mathscore, charter=g8math3$charter)
ggplot(toplot, aes(x=ps, y=math, colour=charter)) + geom_smooth()
ggplot(toplot, aes(x=ps, colour=charter)) + geom_density()

#Read the data dictionary
catalog = readLines(file('~/Dropbox/School/Dissertation/CCD/2007-08/psu071blay.txt'))
strsplit(catalog[25], split='\t')
strsplit(catalog[38], split='\t')

catalog = read.table('~/Dropbox/School/Dissertation/CCD/2007-08/sc071b.txt', header=TRUE, sep='\t', fill=TRUE, stringsAsFactors=FALSE)
nrow(catalog)
names(catalog)
