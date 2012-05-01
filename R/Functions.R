dataLocation <- function(filename) {
	dataDir = ''
	if(Sys.info()['sysname'] == 'Windows') {
		dataDir = 'N:/'
	} else {
		dataDir = '/Volumes/NAEP/'
	}
	return(paste(dataDir, filename, sep=''))
}

##### Data Cleanup #############################################################
dataCleanup <- function(df) {
	df = df[which(df$private != 'Private'),] #Remove private school studnets
	df = df[!is.na(df$state),] #Remove students with missing state
	#This will only use students from states that have charter school counterparts
	charterStates = unique(df[which(df$charter=='Charter'),'state'])
	df = df[which(df$state %in% charterStates),]
	df$state = as.factor(as.character(df$state))
	df$region = as.factor(as.character(df$region))
	return(df)
}

cleanup <- function() {
	g4.math.school.complete <<- NULL
	g4.reading.school.complete <<- NULL
	g8.math.school.complete <<- NULL
	g8.reading.school.complete <<- NULL
	g4.math.student.complete <<- NULL
	g4.reading.student.complete <<- NULL
	g8.math.student.complete <<- NULL
	g8.reading.student.complete <<- NULL
	g4math.orig <<- NULL
	g4reading.orig <<- NULL
	g8math.orig <<- NULL
	g8reading.orig <<- NULL
	g4math.merged.orig <<- NULL
	g4reading.merged.orig <<- NULL
	g8math.merged.orig <<- NULL
	g8reading.merged.orig <<- NULL
}

##### Descriptive statistics ###################################################
descriptives <- function(df, depVar) {
	cols = c('group1', 'group2', 'n', 'mean', 'sd', 'median', 'min', 'max')
	d1 = describe.by(df[,depVar], list(df$charter, rep('Overall', nrow(df))), mat=TRUE)[,cols]
	d1[,'n'] = as.integer(d1[,'n'])
	d2 = describe.by(df[,depVar], list(df$charter, df$state), mat=TRUE)[,cols]
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
	r = r[,c(paste('Charter', cols[3:length(cols)], sep='.'), paste('Public', cols[3:length(cols)], sep='.'))]
	r
}

################################################################################
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

level1Tex <- function(psa, file='', caption='Level 1 Summary', label=NULL) {
	l1 = psa$level1.summary[,c(10,2,4,6,3,5,7,9)]
	l1[,2] = as.integer(l1[,2])
	l1[,5] = as.integer(l1[,5])
	addtorow.l1 = list()
	addtorow.l1$pos = list()
	addtorow.l1$pos[[1]] = c(0)
	addtorow.l1$command = c(paste("& \\multicolumn{3}{c}{", names(psa$level1.summary)[4], " Schools} & \\multicolumn{3}{c}{", names(psa$level1.summary)[5], " Schools} & \\\\ \\cline{2-4} \\cline{5-7} State & n & Score & SE & n & Score & SE & Diff \\\\ \\endfirsthead \\multicolumn{8}{l}{{...continued from previous page}}\\\\ \\hline & \\multicolumn{3}{c}{", names(psa$level1.summary)[4], " Schools} & \\multicolumn{3}{c}{", names(psa$level1.summary)[5], " Schools} & \\\\ \\cline{2-4} \\cline{5-7} State & n & Score & SE & n & Score & SE & Diff \\\\ \\hline \\endhead \\hline \\endfoot \\endlastfoot", sep=''))
	x = xtable(l1, caption=caption, label=label, align=c('l','l','r','r','r@{\\extracolsep{10pt}}','r','r','r','r'))
#	print(x, file=file, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow.l1, size='smaller', tabular.environment='longtable')
	print(x, file=file, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow.l1, floating=FALSE, tabular.environment='longtable', caption.placement='top')
}

level2Tex <- function(psa, file='', caption='Level 2 Summary', label=NULL) {
	l2 = psa$level2.summary[,c(1,2,4,5,3,7,8)]
	addtorow.l2 = list()
	addtorow.l2$pos = list()
	addtorow.l2$pos[[1]] = c(0)
	addtorow.l2$command = c(paste("State & n & ", psa$x.label, " & ", psa$y.label, " & Diff & \\multicolumn{2}{c}{Confidence Interval} \\\\", sep=''))
	x = xtable(l2, caption=caption, label=label, align='llrrrrrr')
	#print(x, file=file, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow.l2, size='smaller')
	print(x, file=file, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow.l2, caption.placement='top')
}

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
		paste('\\pagebreak[2] \\hline & \\multicolumn{4}{c}{', r[!duplicated(r$Variable),'Variable'], 
			  '} \\\\ \\cline{2-5} ', sep='') )
	x = xtable(r[2:6], digits=2, caption=caption, label=label, 
			   align=c('l','l','r','r@{\\extracolsep{10pt}}','r','r'))
	print(x, add.to.row=addtorow, floating=FALSE, include.rownames=FALSE, 
		  include.colnames=FALSE, caption.placement='top', 
		  tabular.environment='longtable', file=filename, hline.after=NULL)
}


################################################################################
abbreviateState <- function(stateCol) {
	s = factor(stateCol, 
		levels=c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Dist. of Columbia','Florida','Georgia','Guam','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming','DoDEA/DoDDS','DoDEA/DDESS','DoDEA'),
		labels=c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','GU','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY','DoDDS','DDESS','DoDEA')
	)
	return(s)
}

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

##### Balance Plots ############################################################
balancePlots <- function(vars, treat, strata, save=NULL) {
	#Balance plots
	for(c in names(vars)) {
		if(!is.null(save)) { pdf(paste(save, c, '.pdf', sep='')) }
		cat.psa(vars[,c], treat, strata, xlab="Stratum", ylab=paste("Proportion for", c), catnames=levels(vars[,c]), barnames=levels(treat), main=paste('Balance Plot for', c))
		ifelse(is.null(save), par(ask=TRUE), dev.off())
	}
}

theme_bryer <- function (base_size = 12) {
	#Started from theme_minimal from the ggExtra package
    structure(list(axis.line = theme_blank(), 
    	axis.text.x = theme_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1), 
    	axis.text.y = theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1), 
    	axis.ticks = theme_segment(colour = "black", size = 0.2), 
    	axis.title.x = theme_text(size = base_size, vjust = .5), 
    	axis.title.y = theme_text(size = base_size, angle = 90, vjust = 0.5), 
    	axis.ticks.length = unit(0.3, "lines"), 
    	axis.ticks.margin = unit(0.5, "lines"), 
    	legend.background = theme_rect(colour = NA), 
        legend.key = theme_rect(colour = NA), 
        legend.key.size = unit(1.2, "lines"), 
        legend.text = theme_text(size = base_size * 0.8), 
        legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0), 
        legend.position = "right", 
        panel.background = theme_rect(fill = "white", colour = NA), 
        panel.border = theme_rect(fill = NA, colour = 'grey50'), 
        panel.grid.major = theme_line(colour = "grey90", size = 0.2), 
        panel.grid.minor = theme_line(colour = "grey98", size = 0.5), 
        panel.margin = unit(0.25, "lines"), 
        strip.background = theme_rect(fill = NA, colour = "grey90"), 
        strip.label = function(variable, value) value, 
        strip.text.x = theme_text(size = base_size * 0.8), 
        strip.text.y = theme_text(size = base_size * 0.8, angle = -90), 
        plot.background = theme_rect(colour = NA), 
        plot.title = theme_text(size = base_size * 1.2), 
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")), 
        class = "options")
}

