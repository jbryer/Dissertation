#' Returns descriptive statistics.
descriptives <- function(df, depVar) {
	cols = c('group1', 'group2', 'n', 'mean', 'sd', 'median', 'min', 'max')
	d1 = describeBy(df[,depVar], list(df$charter, rep('Overall', nrow(df))), mat=TRUE)[,cols]
	d1[,'n'] = as.integer(d1[,'n'])
	d2 = describeBy(df[,depVar], list(df$charter, df$FIPS), mat=TRUE)[,cols]
	d2[,'n'] = as.integer(d2[,'n'])
	d = rbind(d1, d2)
	r = unique(d$group2)
	for(i in cols[3:8]) {
		c = cast(d, group2 ~ group1, value=i)[,2:3]
		names(c) = paste(names(c), i, sep='.')
		r = cbind(r, c)
	}
	row.names(r) = r$r
	r$r = NULL
	r = r[,c(paste('TRUE', cols[3:length(cols)], sep='.'), paste('FALSE', cols[3:length(cols)], sep='.'))]
	names(r) <- gsub('TRUE', 'Charter', names(r))
	names(r) <- gsub('FALSE', 'Public', names(r))
	r <- na.omit(r)
	return(r)
}

xtableDescriptives <- function(df, caption=NULL, label=NULL, ...) {
	ul = unlist(strsplit(names(df), '.', fixed=TRUE))
	names(df) = ul[seq(2,2*ncol(df),2)]
	addtorow = list()
	addtorow$pos = list()
	addtorow$pos[[1]] = c(-1)
	addtorow$command = c(paste("\\hline & \\multicolumn{", ncol(df)/2, "}{c}{", ul[1], " Schools} & \\multicolumn{", ncol(df)/2, "}{c}{", ul[ncol(df)+1], " Schools} \\\\ \\cline{2-7} \\cline{8-13}", sep=''))
	x = xtable(df, digits=2, align=c('l', rep('r', ((ncol(df)/2)-1)), 'r@{\\extracolsep{10pt}}', rep('r', (ncol(df)/2))), caption=caption, label=label)
	print(x, add.to.row=addtorow, include.rownames=TRUE, include.colnames=TRUE, hline.after=c(0,nrow(x)), caption.placement='top', ...)
}
