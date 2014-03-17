# Sensitivity analysis for grade 8 reading, one-to-one matched analysis
require(rbounds)

load("~/Dropbox/School/Dissertation/Data2009/g8read.analysis.Rdata")

summary(match1)
x <- g8read3[match1$index.treated,]$readscore
y <- g8read3[match1$index.control,]$readscore
psens(x, y, Gamma=1.5, GammaInc=.1)

mean(x - y)
sd(x - y)
