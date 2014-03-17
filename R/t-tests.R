#' Manual calculation of paired dependent t-test.
#' @param diff mean difference,
#' @param sd standard deviation of the difference.
#' @param n sample size.
#' @param mu test the difference other than zero.
t.dep.2 <- function(diff, sd, n, mu=0) {
	t <- (diff - mu) / (sd / sqrt(n))
	return(c(t = t, p = 1-pt(t, df=n-1)))
}

# Test it out
if(FALSE) {
	t.dep.2(2, 40, 3000)
	t.dep.2(3, 40, 3000)
	
	require(pwr)
	?pwr.t.test
	pwr.t.test(n=3000, d=.06, sig.level=0.05)
	pwr.t.test(n=6000, d=.06, sig.level=0.05)
	pwr.t.test(n=9000, d=.06, sig.level=0.05)
	pwr.t.test(n=10000, d=.06, sig.level=0.05)
	
}
