# Sensitivity analysis for grade 8 reading, one-to-one matched analysis
require(rbounds)
require(party)

load("~/Dropbox/School/Dissertation/Data2009/g8read.Rdata")
load("~/Dropbox/School/Dissertation/Data2009/g8read.analysis.Rdata")

summary(match1)
naep$readscore <- apply(naep[,c('RRPCM1','RRPCM2','RRPCM3','RRPCM4','RRPCM5')], 1, mean)
x <- naep[match1$index.treated,]$readscore
y <- naep[match1$index.control,]$readscore
psens(x, y, Gamma=1.5, GammaInc=.1)

mean(x - y)
sd(x - y)


load("~/Dropbox/School/Dissertation/Data2009/g8math.Rdata")
load("~/Dropbox/School/Dissertation/Data2009/g8math.analysis.Rdata")

mathscore <- apply(naep[,c('MRPCM1','MRPCM2','MRPCM3','MRPCM4','MRPCM5')], 1, mean)
table(is.na(mathscore))




##### lalonde ##################################################################
library(Matching)
data(lalonde)

# Estimate Propensity Score
DWglm <- glm(treat ~ age + I(age^2) + educ + I(educ^2) + black + hisp +
			 	married + nodegr + re74 + I(re74^2) + re75 + I(re75^2) +
			 	u74 + u75, family=binomial, data=lalonde)

# Save data objects
Y  <- lalonde$re78   #the outcome of interest
Tr <- lalonde$treat #the treatment of interest

# Match - without replacement
mDW  <- Match(Y=Y, Tr=Tr, X=DWglm$fitted, replace=FALSE)

# One should check balance, but let's skip that step for now.

# Sensitivity Test
psens(mDW, Gamma = 2, GammaInc = 0.1)
