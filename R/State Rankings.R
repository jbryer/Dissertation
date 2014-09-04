setwd('./R')
getwd()

source('Functions/slopegraph.R')
source('../Data2009/states.r')
require(gdata)
require(reshape2)
require(gridExtra)
require(xtable)

rankings <- read.xls('../Data/StateRankings.xlsx')
g4math <- read.csv('../Data2009/g4math-level2-ctree.csv')
g4read <- read.csv('../Data2009/g4read-level2-ctree.csv')
g8math <- read.csv('../Data2009/g8math-level2-ctree.csv')
g8read <- read.csv('../Data2009/g8read-level2-ctree.csv')

meanByState <- read.csv('../Data2009/NAEPDescriptivesByState.csv')

rankings <- merge(rankings, states, by.x='State', by.y='name', all.x=TRUE)

g4math <- g4math[order(g4math$diffwtd, decreasing=TRUE),c('level2','diffwtd')]
g4read <- g4read[order(g4read$diffwtd, decreasing=TRUE),c('level2','diffwtd')]
g8math <- g8math[order(g8math$diffwtd, decreasing=TRUE),c('level2','diffwtd')]
g8read <- g8read[order(g8read$diffwtd, decreasing=TRUE),c('level2','diffwtd')]

g4math$g4math <- 1:nrow(g4math)
g4read$g4read <- 1:nrow(g4read)
g8math$g8math <- 1:nrow(g8math)
g8read$g8read <- 1:nrow(g8read)

##### For comparing scores #####################################################
scores <- merge(rankings, g4math[,c('level2','diffwtd')], by.x='abbr', by.y='level2', all.x=TRUE)
names(scores)[ncol(scores)] <- 'g4math'
scores <- merge(scores, g4read[,c('level2','diffwtd')], by.x='abbr', by.y='level2', all.x=TRUE)
names(scores)[ncol(scores)] <- 'g4read'
scores <- merge(scores, g8math[,c('level2','diffwtd')], by.x='abbr', by.y='level2', all.x=TRUE)
names(scores)[ncol(scores)] <- 'g8math'
scores <- merge(scores, g8read[,c('level2','diffwtd')], by.x='abbr', by.y='level2', all.x=TRUE)
names(scores)[ncol(scores)] <- 'g8read'

meanByState <- merge(meanByState, scores[,c('abbr','State')], by.x='group1', by.y='State', all.x=TRUE)


lm.g4math <- lm(g4math ~ NAPCS2010Score, data=scores)
lm.g4read <- lm(g4read ~ NAPCS2010Score, data=scores)
lm.g8math <- lm(g8math ~ NAPCS2010Score, data=scores)
lm.g8read <- lm(g8read ~ NAPCS2010Score, data=scores)

df.melted <- melt(scores[,c('abbr','NAPCS2010Score','g4math','g4read','g8math','g8read')],
				  id=c('abbr','NAPCS2010Score'))
df.melted <- merge(df.melted, meanByState[,c('abbr','zscore','Subject')], 
				   by.x=c('abbr','variable'), by.y=c('abbr','Subject'), all.x=TRUE)
df.melted$variable <- factor(df.melted$variable, levels=c('g4math','g4read','g8math','g8read'),
							 labels=c('Grade 4 Math', 'Grade 4 Reading',
							 		  'Grade 8 Math', 'Grade 8 Reading'))
df.cor <- data.frame(variable=c('g4math','g4read','g8math','g8read'),
					 cor=c(cor(scores$NAPCS2010Score, scores$g4math, use='pairwise.complete.obs'),
					 	   cor(scores$NAPCS2010Score, scores$g4read, use='pairwise.complete.obs'),
					 	   cor(scores$NAPCS2010Score, scores$g8math, use='pairwise.complete.obs'),
					 	   cor(scores$NAPCS2010Score, scores$g8read, use='pairwise.complete.obs')),
					 m=c(lm.g4math$coefficients[2],
					 	 lm.g4read$coefficients[2],
					 	 lm.g8math$coefficients[2],
					 	 lm.g8read$coefficients[2]),
					 b=c(lm.g4math$coefficients[1],
					 	 lm.g4read$coefficients[1],
					 	 lm.g8math$coefficients[1],
					 	 lm.g8read$coefficients[1]),
					 stringsAsFactors=FALSE)
df.cor$variable <- factor(df.cor$variable, levels=c('g4math','g4read','g8math','g8read'),
							 labels=c('Grade 4 Math', 'Grade 4 Reading',
							 		 'Grade 8 Math', 'Grade 8 Reading'))
df.cor$formula <- paste0('y = ', round(df.cor$m, digits=2), 'x', 
						 ifelse(df.cor$b >= 0, ' + ', ' - '), 
						 prettyNum(abs(df.cor$b), digits=2, drop0trailing=FALSE, nsmall=2))

p <- ggplot(df.melted, aes(x=NAPCS2010Score, y=value, label=abbr)) +
	geom_smooth(span=1.2, alpha=.2, method='loess') +
	geom_abline(data=df.cor, aes(intercept=b, slope=m), color='grey30', alhpa=.1) +
	geom_point(aes(size=zscore), alpha=.5) +
	geom_text(size=2.5, vjust=-1) +
 	geom_text(data=df.cor, x=40, y=23,
 			  aes(label=paste0('Correlation = ', prettyNum(cor, digits=2, nsmall=2))),
 			  hjust=0, size=4, parse=FALSE) +
	geom_text(data=df.cor, x=150, y=-30,
			  aes(label=formula), hjust=1, size=4, parse=FALSE) +
	scale_size_continuous('Z-Score') +
	ylim(c(-30, 25)) +
	xlab('NAPCS Score for Quality of State Charter Law (out of a maximum score of 208)') +
	ylab('NAEP Standardized Mean Difference (Effect Size)')

p + facet_wrap(~ variable)
ggsave('../Figures2009/LawScoresVsNAEPDifferences.pdf', width=8, height=7)

# This has one row and is used for the poster
p + facet_wrap(~ variable, nrow=1) 
ggsave('../Figures2009/LawScoresVsNAEPDifferences2.pdf', width=16, height=5)

##### Pairs plot for effect sizes
scores2 <- scores[,c('NAPCS2010Score','g4math','g4read','g8math','g8read')]
names(scores2) <- c('NAPCS Scores', 'Grade 4 Math', 'Grade 4 Reading', 
					'Grade 8 Math', 'Grade 8 Reading')
panel.hist <- function(x, ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r <- abs(cor(x, y, use='pairwise.complete.obs'))
	txt <- format(c(r, 0.123456789), digits = digits)[1]
	txt <- paste0(prefix, txt)
	text(0.5, 0.5, txt, cex=1.5)
}
panel.smooth.linear <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
		  cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
	points(x, y, pch = pch, col = col, bg = bg, cex = cex)
	ok <- is.finite(x) & is.finite(y)
	if (any(ok)) {
		lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
			  col = 'blue', ...)
	}
	data <- data.frame(x=x[ok], y=y[ok])
	lm.fit <- lm(y ~ x, data=data)
	lines(data$x, predict(lm.fit), col = "black")
}

pdf('../Figures2009/NAEPLawScoresMatrixPlot.pdf', width=10, height=10)
pairs(scores2, lower.panel=panel.smooth.linear, diag.panel=panel.hist, 
	  upper.panel=panel.cor, cex.labels=1.5, span=1.2)
dev.off()

##### For comparing rankings ###################################################
rankings <- merge(rankings, g4math[,c('level2','g4math')], by.x='abbr', by.y='level2', all.x=TRUE)
rankings <- merge(rankings, g4read[,c('level2','g4read')], by.x='abbr', by.y='level2', all.x=TRUE)
rankings <- merge(rankings, g8math[,c('level2','g8math')], by.x='abbr', by.y='level2', all.x=TRUE)
rankings <- merge(rankings, g8read[,c('level2','g8read')], by.x='abbr', by.y='level2', all.x=TRUE)

df.melted <- melt(rankings[,c('State','NAPCS2012Ranking','g4math')], id='State')
df.melted$value <- -1 * df.melted$value
levels(df.melted$variable) <- c('NAPCS\nRanking', 'Grade 4\nMath')
yvals <- df.melted[df.melted$variable == levels(df.melted$variable)[1],]
p.g4math <- ggplot(df.melted,aes(x=variable,y=value)) +
	geom_line(aes(group=State),colour="grey80") +
	geom_point(colour="white",size=6, alpha=.5) +
	geom_text(aes(label=abs(value)),size=2.5) +
	scale_y_continuous(name="", breaks=yvals$value, labels=yvals$State) +
	theme_slopegraph()
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', 
# 					   method='spaced', min.space=0.05)
# df <- df[df$y != 0,]
# p.g4math <- plot_slopegraph(df)

df.melted <- melt(rankings[,c('State','NAPCS2012Ranking','g4read')], id='State')
df.melted$value <- -1 * df.melted$value
levels(df.melted$variable) <- c('NAPCS\nRanking', 'Grade 4\nReading')
yvals <- df.melted[df.melted$variable == levels(df.melted$variable)[1],]
p.g4read <- ggplot(df.melted,aes(x=variable,y=value)) +
	geom_line(aes(group=State),colour="grey80") +
	geom_point(colour="white",size=6, alpha=.5) +
	geom_text(aes(label=abs(value)),size=2.5) +
	scale_y_continuous(name="", breaks=yvals$value, labels=yvals$State) +
	theme_slopegraph()
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', 
# 					   method='spaced', min.space=0.05)
# df <- df[df$y != 0,]
# p.g4read <- plot_slopegraph(df)

df.melted <- melt(rankings[,c('State','NAPCS2012Ranking','g8math')], id='State')
df.melted$value <- -1 * df.melted$value
levels(df.melted$variable) <- c('NAPCS\nRanking', 'Grade 8\nMath')
yvals <- df.melted[df.melted$variable == levels(df.melted$variable)[1],]
p.g8math <- ggplot(df.melted,aes(x=variable,y=value)) +
	geom_line(aes(group=State),colour="grey80") +
	geom_point(colour="white",size=6, alpha=.5) +
	geom_text(aes(label=abs(value)),size=2.5) +
	scale_y_continuous(name="", breaks=yvals$value, labels=yvals$State) +
	theme_slopegraph()
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', 
# 					   method='spaced', min.space=0.05)
# df <- df[df$y != 0,]
# p.g8math <- plot_slopegraph(df)

df.melted <- melt(rankings[,c('State','NAPCS2012Ranking','g8read')], id='State')
df.melted$value <- -1 * df.melted$value
levels(df.melted$variable) <- c('NAPCS\nRanking', 'Grade 8\nReading')
yvals <- df.melted[df.melted$variable == levels(df.melted$variable)[1],]
p.g8read <- ggplot(df.melted,aes(x=variable,y=value)) +
	geom_line(aes(group=State),colour="grey80") +
	geom_point(colour="white",size=6, alpha=.5) +
	geom_text(aes(label=abs(value)),size=2.5) +
	scale_y_continuous(name="", breaks=yvals$value, labels=yvals$State) +
	theme_slopegraph()
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', 
# 					   method='spaced', min.space=0.05)
# df <- df[df$y != 0,]
# p.g8read <-	plot_slopegraph(df)

pdf('../Figures2009/StateRankings.pdf', width=9, height=12)
grid.arrange(p.g4math, p.g4read, p.g8math, p.g8read)
dev.off()

##### Rubric to LaTeX ##########################################################
rubric <- read.xls('../Data/StateRankings.xlsx', sheet=2)

addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = c(3)
addtorow$command = c(paste0('\\hline & & \\multicolumn{5}{c}{Scoring} & \\\\ \\cline{3-7}',
							'& Component & \\multicolumn{1}{c}{0} & ',
							'\\multicolumn{1}{c}{1} & ',
							'\\multicolumn{1}{c}{2} & ',
							'\\multicolumn{1}{c}{3} & ',
							'\\multicolumn{1}{c}{4} & Weight \\\\ \\hline',
							'\\endfirsthead ',
							'\\multicolumn{8}{l}{{...continued from previous page}}\\\\ ',
							'\\hline & & \\multicolumn{5}{c}{Scoring} & \\\\ \\cline{3-7}',
							'& Component & \\multicolumn{1}{c}{0} & ',
							'\\multicolumn{1}{c}{1} & ',
							'\\multicolumn{1}{c}{2} & ',
							'\\multicolumn{1}{c}{3} & ',
							'\\multicolumn{1}{c}{4} & Weight \\\\ ',							
							'\\hline \\endhead '),
					 '\\hline \\pagebreak')
x <- xtable(rubric, 
			align='rrp{2.2in}p{0.9in}p{0.9in}p{0.9in}p{0.9in}p{0.9in}c',
			caption='NAPCS Rubric for Rating the Quality of State Charter Laws',
			label='NAPCSrubric')
print(x, 
	  file='../Tables/NAPCSRubric.tex',
	  hline.after=1:20,
	  tabular.environment='longtable',
	  floating=FALSE,
	  caption.placement='top',
	  size='smaller', add.to.row=addtorow,
	  include.rownames=FALSE, include.colnames=FALSE)
