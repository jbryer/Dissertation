#' Removes publics schools more than the specified distance from a charater school.
#' 
#' See ccd.R for how the distances were calculated
#' @param naep naep data.
#' @param distance the distance in miles as the crow flies.
#' @param min.charters the minimum number of charter school students within each
#'        state to retain that state's data.
close.publics <- function(naep, distance=5, min.charters=50) {
	load('../Data2009/SchoolDistances.rda')
	
	naep$data$NCESSCH2 = as.numeric(as.character(naep$data$NCESSCH))
	distances$NCESSCH = as.numeric(as.character(distances$NCESSCH))
	
	data2 = merge(naep$data, distances, by.x='NCESSCH2', by.y='NCESSCH', all.x=TRUE)
	#There are a few schools (generally less than 5%) where a distance to a charter
	#school could not be calculated. See ccd.R
	#Distances of NA are from states with no charter schools
	data2 = data2[(data2$charter | !is.na(data2$dist)),]
	
	##### Remove non-charter schools that are not close (i.e. > 5 miles) ###########
	data3 = data2[which(data2$charter | data2$dist < distance),]
	#print(paste0(nrow(data3) / nrow(naep$data) * 100, '% rows remaining')) #Percent remaining
	stateXcharter = as.data.frame(table(data3$FIPS, data3$charter, useNA='ifany'))
	stateXcharter$Var2 = as.logical(stateXcharter$Var2)
	stateXcharter$Var1 = as.character(stateXcharter$Var1)
	states = stateXcharter[which(stateXcharter$Freq >= min.charters & stateXcharter$Var2),'Var1']
	data3 = data3[data3$FIPS %in% states,]
	data3$state <- NULL
	return(data3)
}
