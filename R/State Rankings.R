setwd('./R')
getwd()

source('Functions/slopegraph.R')
source('../Data2009/states.r')
require(gdata)
require(reshape2)
require(gridExtra)

rankings <- read.xls('../Data/StateRankings.xlsx')
g4math <- read.csv('../Data2009/g4math-level2-ctree.csv')
g4read <- read.csv('../Data2009/g4read-level2-ctree.csv')
g8math <- read.csv('../Data2009/g8math-level2-ctree.csv')
g8read <- read.csv('../Data2009/g8read-level2-ctree.csv')

rankings <- merge(rankings, states, by.x='State', by.y='name', all.x=TRUE)

g4math <- g4math[order(g4math$diffwtd, decreasing=TRUE),c('level2','diffwtd')]
g4read <- g4read[order(g4read$diffwtd, decreasing=TRUE),c('level2','diffwtd')]
g8math <- g8math[order(g8math$diffwtd, decreasing=TRUE),c('level2','diffwtd')]
g8read <- g8read[order(g8read$diffwtd, decreasing=TRUE),c('level2','diffwtd')]

g4math$g4math <- 1:nrow(g4math)
g4read$g4read <- 1:nrow(g4read)
g8math$g8math <- 1:nrow(g8math)
g8read$g8read <- 1:nrow(g8read)

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
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', method='spaced', min.space=0.05)
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
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', method='spaced', min.space=0.05)
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
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', method='spaced', min.space=0.05)
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
# df <- build_slopegraph(df.melted, x='variable', y='value', group='State', method='spaced', min.space=0.05)
# df <- df[df$y != 0,]
# p.g8read <-	plot_slopegraph(df)

pdf('../Figures2009/StateRankings.pdf', width=9, height=12)
grid.arrange(p.g4math, p.g4read, p.g8math, p.g8read)
dev.off()
