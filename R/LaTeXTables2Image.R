require(tools)
require(Hmisc)

tabdir <- "/Users/jbryer/Dropbox/School/Dissertation/Tables2009"

# Adapted from
# http://stackoverflow.com/questions/9298765/print-latex-table-directly-to-an-image-png-or-other

dvipng.dvi <- function (object, file, res=600) {
	if (missing(file)){
		invisible(sys(
			paste("dvipng -T tight", "-D", res, shQuote(object$file)))
		)
	}
	else{
		invisible(sys(
			paste("dvipng -T tight", "-D", res, "-o", file, shQuote(object$file)))
		)
	}
}

setwd(tabdir)

textabs <- list.files('.', pattern='*.tex')
i <- 2

for(i in seq_along(textabs)) {
	latextab <- readChar(textabs[i], file.info(textabs[i])$size)
	filename <- substr(textabs[i], 1, nchar(textabs[i])-4)
	sink(file=paste0(filename, '2.tex'))
	cat('
	\\documentclass{report}
	\\usepackage[paperwidth=5.5in,paperheight=70in,noheadfoot,margin=0in]{geometry}
    \\usepackage{longtable}
    \\usepackage{rotating}
    \\newcommand{\\thickline}{\\hline\\hline\\hline}
	\\begin{document}\\pagestyle{empty}
	')
	cat(latextab)
	cat('
	\\end{document}
	')
	sink()
	texi2dvi(file=paste0(filename, '2.tex'))
	cmd <- paste0("dvipng -T tight -D 600 ", shQuote(paste0(filename,"2.dvi")))
	invisible(sys(cmd))
	cleaner <- c("2.tex","2.aux","2.log","2.dvi")
	invisible(file.remove(paste0(filename,cleaner)))
	tomove <- list.files('.', pattern=paste0(filename, '2[0-9]*.png'))
	for(j in tomove) {
		file.rename(j, paste0(filename, substr(j, nchar(filename) + 2, nchar(j))))
	}
}

