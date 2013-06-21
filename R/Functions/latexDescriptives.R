#' Generage LaTeX descriptives table. 
#' @seealso latexDescriptives
covariate.descriptive <- function(naep, all.covars, subject, grade, year=2009) {
	#' Creates a LaTeX table for the given categorical variables.
	#' 
	#' @param df the data frame.
	   #' @param cols the column names to use.
	   #' @param caption the caption of the table.
	   #' @param label the label of the table.
	   #' @param filename the name of the file to save the latex code to.
	   latexDescriptives <- function(df, cols, caption=NULL, label=NULL, filename="") {
	   	r = data.frame()
	   	for(i in cols) {
	   		t = cast(as.data.frame(table(df[,i], df$charter, useNA='always')), 
	   				 Var1 ~ Var2, value='Freq')[,1:3]
	   		p = cast(as.data.frame(prop.table(table(df[,i], df$charter, useNA='always'), 2)), 
	   				 Var1 ~ Var2, value='Freq')[,1:3]
	   		t[,1] = p[,1] = as.character(t[,1])
	   		t[nrow(t),1] = 'Unknown'
	   		p[nrow(p),1] = 'Unknown'
	   		r = rbind(r, cbind(rep(i, nrow(p)), t, p[,2:3]))
	   	}
	   	r = r[,c(1,2,3,5,4,6)]
	   	names(r) = c('Variable', 'Value', 'Public', 'PublicPercent', 'Charter', 'CharterPercent')
	   	r$CharterPercent = paste(round(100 * r$CharterPercent), '%', sep='')
	   	r$PublicPercent = paste(round(100 * r$PublicPercent), '%', sep='')
	   	as.numeric(rownames(r[!duplicated(r$Variable),]))
	   	addtorow = list()
	   	addtorow$pos = list()
	   	addtorow$pos[[1]] = c(0)
	   	pos = as.numeric(rownames(r[!duplicated(r$Variable),]))
	   	for(i in 1:length(pos)) { addtorow$pos[[(i+1)]] = c(pos[i]-1) }
	   	addtorow$command = c(
	   		paste('\\thickline & \\multicolumn{2}{c}{Traditional} & ',
	   			  '\\multicolumn{2}{c}{Charter} \\\\ ',
	   			  ' \\endfirsthead ',
	   			  '\\multicolumn{5}{l}{{...continued from previous page}}\\\\ ',
	   			  '\\hline & \\multicolumn{2}{c}{Charter} & ',
	   			  '\\multicolumn{2}{c}{Traditional}  \\\\ ',
	   			  '\\hline \\endhead ', 
	   			  '\\thickline \\multicolumn{5}{r}{continued on next page...} \\\\ ',
	   			  '\\endfoot \\multicolumn{5}{c}{} \\\\ \\endlastfoot ', sep=''),
	   		#See page 9 of the longtable document for how to suggest where page breaks go
	   		#http://www.cs.brown.edu/system/software/latex/doc/longtable.pdf
	   		paste('\\pagebreak[2] \\hline \\multicolumn{5}{c}{', r[!duplicated(r$Variable),'Variable'], 
	   			  '} \\\\ \\cline{1-5} ', sep='') )
	   	x = xtable(r[2:6], digits=2, caption=caption, label=label, 
	   			   align=c('l','l','r','r@{\\extracolsep{10pt}}','r','r'))
	   	print(x, add.to.row=addtorow, floating=FALSE, include.rownames=FALSE, 
	   		  include.colnames=FALSE, caption.placement='top', 
	   		  tabular.environment='longtable', file=filename, hline.after=NULL)
	   }
	   
	covars <- naep$catalog[naep$catalog$FieldName %in% all.covars,c('FieldName','Description')]
	covars <- covars[covars$FieldName %in% names(naep$data),]
	tmp <- naep$data[,covars$FieldName]
	names(tmp) <- covars$Description
	tmp$charter <- naep$data$charter
	latexDescriptives(tmp, cols=covars$Description, 
					  caption=paste0('Grade ', grade, ' ', subject, ' Descriptive Statistics'),
					  label=paste0('tab:g', grade, subject, '-desc'),
					  filename=paste0('../Tables', year, '/g', grade, subject, '-desc.tex'))
}
