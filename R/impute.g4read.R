if(file.exists('../Data2009/g4read.mice.Rdata')) {
	load('../Data2009/g4read.mice.Rdata')
} else {
	g4read.toimpute = g4read3[, names(g4read3) %in% all.covars]

#	tmp <- g4read$catalog[g4read$catalog$FieldName %in% names(g4read.toimpute),
#						  c('FieldName','Description')]
#	g4read.toimpute2 <- renameColumns(g4read.toimpute, tmp$FieldName, as.character(tmp$Description))
	# 	pdf('../Figures2009/g4read-missing.pdf')
	# 	missing.plot(g4read.toimpute2, g4read3$FIPS02, grid=TRUE, heights=c(1,2))
	# 	dev.off()
	
	g4read.mice = mice(g4read.toimpute, m=1)
	save(g4read.mice, file='../Data2009/g4read.mice.Rdata')
}
