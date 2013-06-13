if(file.exists('../Data2009/g4math.mice.Rdata')) {
	load('../Data2009/g4math.mice.Rdata')
} else {
	g4math.toimpute = g4math3[, names(g4math3) %in% all.covars]
	
#	tmp <- g4math$catalog[g4math$catalog$FieldName %in% names(g4math.toimpute),
#						  c('FieldName','Description')]
#	g4math.toimpute2 <- renameColumns(g4math.toimpute, tmp$FieldName, as.character(tmp$Description))
# 	pdf('../Figures2009/g4math-missing.pdf')
# 	missing.plot(g4math.toimpute2, g4math3$FIPS02, grid=TRUE, heights=c(1,2))
# 	dev.off()
	
	g4math.mice = mice(g4math.toimpute, m=1)
	save(g4math.mice, file='../Data2009/g4math.mice.Rdata')
}
