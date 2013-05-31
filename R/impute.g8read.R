if(file.exists('data2009/g8read.mice.Rdata')) {
	load('data2009/g8read.mice.Rdata')
} else {
	g8read.toimpute = g8read3[, names(g8read3) %in% all.covars]
	g8read.mice = mice(g8read.toimpute, m=1)
	save(g8read.mice, file='data2009/g8read.mice.Rdata')
}
