if(file.exists('../Data2009/g8math.mice.Rdata')) {
	load('../Data2009/g8math.mice.Rdata')
} else {
	g8math.toimpute = g8math3[, names(g8math3) %in% all.covars]
	g8math.mice = mice(g8math.toimpute, m=1)
	save(g8math.mice, file='../Data2009/g8math.mice.Rdata')
}
