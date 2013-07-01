getSimulatedData <- function(nvars = 3, ntreat = 100, treat.mean = 0.6, treat.sd = 0.5, 
							 ncontrol = 1000, control.mean = 0.4, control.sd = 0.5) {
	if (length(treat.mean) == 1) {
		treat.mean = rep(treat.mean, nvars)
	}
	if (length(treat.sd) == 1) {
		treat.sd = rep(treat.sd, nvars)
	}
	if (length(control.mean) == 1) {
		control.mean = rep(control.mean, nvars)
	}
	if (length(control.sd) == 1) {
		control.sd = rep(control.sd, nvars)
	}
	
	df <- c(rep(0, ncontrol), rep(1, ntreat))
	for (i in 1:nvars) {
		df <- cbind(df, c(rnorm(ncontrol, mean = control.mean[1], sd = control.sd[1]), 
						  rnorm(ntreat, mean = treat.mean[1], sd = treat.sd[1])))
	}
	df <- as.data.frame(df)
	names(df) <- c("treat", letters[1:nvars])
	return(df)
}

set.seed(2112)
df.psrange <- getSimulatedData(ncontrol = 1000, nvars=1,
							   treat.mean=0.6, treat.sd=0.4,
							   control.mean=0.4, control.sd=0.4)
psrange.test <- psrange(df.psrange, df.psrange$treat, treat ~ ., 
			    samples = seq(100, 1000, by = 100), nboot = 20)
plot(psrange.test)
ggsave('../Figures2009/PSRanges.pdf')

#Perfect overlap
df.overlap <- getSimulatedData(ncontrol = 1000, nvars=1,
							  treat.mean=0.5, treat.sd=0.4,
							  control.mean=0.5, control.sd=0.4)
psrange.overlap <- psrange(df.overlap, df.overlap$treat, treat ~ ., 
					 samples = seq(100, 1000, by = 100), nboot = 20)
plot(psrange.overlap)
ggsave('../Figures2009/PSRanges-Overlap.pdf')

#No overlap
df.nooverlap <- getSimulatedData(ncontrol = 1000, nvars=1,
							     treat.mean=0.2, treat.sd=0.4,
							     control.mean=0.8, control.sd=0.4)
psrange.nooverlap <- psrange(df.nooverlap, df.nooverlap$treat, treat ~ ., 
					 samples = seq(100, 1000, by = 100), nboot = 20)
plot(psrange.nooverlap)
ggsave('../Figures2009/PSRanges-NoOverlap.pdf')

