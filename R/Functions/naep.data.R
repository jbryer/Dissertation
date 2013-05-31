#' This function will return the naep data for the given grade, subject, and year.
#' 
#' This function will create a cached R data object named dataYYYY/gGsubject.Rdata.
#' If the file exits, that will be loaded and returned. If not, the NAEP data
#' will be loaded using the naep R package. Additionally, the function will use
#' and/or create a naep.YYYY.subjectG.vars.R for the variables to be returned.
#' 
#' @param grade the grade.
#' @param subject the subject.
#' @param vars variables to read in.
#' @param year the year.
#' @return a list with three elements: data, catalog, and vars.
naep.data <- function(grade, subject, vars, year=2009, dir=getBaseDirectory()) {
	filename <- paste0('../Data', year, '/g', grade, subject, '.Rdata')
	if(file.exists(filename)) {
		#Load a cached version of the data frames
		load(filename)
	} else {
		catalog = getNAEPCatalog(year=year, grade=grade, subject=subject, 
								 directory=dir, type='Student', sample='AT')
		catalog[which(catalog$FieldName %in% vars), c('FieldName', 'Description')]
		naep.full = getNAEPData(year=year, grade=grade, subject=subject, directory=dir,
						   vars=vars, type='Student', sample='AT')
		naep = naep.full$data[which(naep.full$data$SCHTYPE == 'Public'),] #Remove non-public schools
		
		#Save the data frames for faster re-loading later.
		save(naep, catalog, file=filename)
	}
	return(list(data=naep, catalog=catalog))
}
