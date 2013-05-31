#' Recodes the levels of covariates within NAEP to numeric values. Will also
#' recode missing values appropriately.
#' 
#' @param naep data frame with NAEP data.
naep.recode <- function(naep) {
	if('SLUNCH1' %in% names(naep)) {
		naep$SLUNCH1 = recode(naep$SLUNCH1, c('Not eligible','Eligible'), c(0,1))
	}
	if('SD4' %in% names(naep)) {
		naep$SD4 = recode(naep$SD4, 
						  c('Not IEP','Yes, 504 plan','Yes, 504 in process', 'Yes, IEP'), 
						  c(0,1,1,2))
	}
	if('ELL3' %in% names(naep)) {
		naep$ELL3 = recode(naep$ELL3, c('No','Formerly ELL','Yes'), c(0,1,1))
	}
	if('SDELL' %in% names(naep)) {
		naep$SDELL = recode(naep$SDELL, 
								   c('Neither SD nor ELL','Student with disabi',
								     'English language le','Both SD and ELL'), 
								   c(0,1,1,2))
	}
	if('IEP' %in% names(naep)) {
		naep$IEP = recode(naep$IEP, c('Not student with di','Student with disabi'), c(0,1))
	}
	if('B017001' %in% names(naep)) {
		naep$B017001 = recode(naep$B017001, c('No','Yes'), c(0,1))
	}
	if('B000905' %in% names(naep)) {
		naep$B000905 = recode(naep$B000905, c('No','Yes'), c(0,1))
	}
	if('B013801' %in% names(naep)) {
		naep$B013801 = recode(naep$B013801, levels(naep$B013801)[1:4], c(1:4))
	}
	if('B017101' %in% names(naep)) {
		naep$B017101 = recode(naep$B017101, c('No','Yes'), c(0,1))
	}
	if('B017201' %in% names(naep)) {
		naep$B017201 = recode(naep$B017201, c('No','Yes'), c(0,1))
	}
	if('B001151' %in% names(naep)) {
		naep$B001151 = recode(naep$B001151, levels(naep$B001151)[1:5], c(1:5))
	}
	if('B017451' %in% names(naep)) {
		naep$B017451 = recode(naep$B017451, levels(naep$B017451)[1:5], c(1:5))
	}
	if('B018101' %in% names(naep)) {
		naep$B018101 = recode(naep$B018101, levels(naep$B018101)[1:5], c(1:5))
	}
	if('B018201' %in% names(naep)) {
		naep$B018201 <- recode(naep$B018201, levels(naep$B018201)[1:4], c(1:4))
	}
	if('B003501' %in% names(naep)) {
		naep$B003501 <- recode(naep$B003501,levels(naep$B003501)[1:4], c(1:4))
	}
	if('B003601' %in% names(naep)) {
		naep$B003601 <- recode(naep$B003601, levels(naep$B003601)[1:4], c(1:4))
	}
	if('R846001' %in% names(naep)) {
		naep$R846001 <- recode(naep$R846001, levels(naep$R846001)[1:4], c(1:4))
	}
	if('R846101' %in% names(naep)) {
		naep$R846101 <- recode(naep$R846101, levels(naep$R846101)[1:4], c(1:4))
	}
	if('R846401' %in% names(naep)) {
		naep$R846401 <- recode(naep$R846401, c('No','Yes'), c(0,1))
	}
	if('R846501' %in% names(naep)) {
		naep$R846501 <- recode(naep$R846501, c('No','Yes'), c(0,1))
	}
	if('R831001' %in% names(naep)) {
		naep$R831001 <- recode(naep$R831001, levels(naep$R831001)[1:4], c(1:4))
	}
	if('R831101' %in% names(naep)) {
		naep$R831101 <- recode(naep$R831101, levels(naep$R831101)[1:4], c(1:4))
	}
	if('R847001' %in% names(naep)) {
		naep$R847001 <- recode(naep$R847001, levels(naep$R847001)[1:4], c(1:4))
	}
	if('R833101' %in% names(naep)) {
		naep$R833101 <- recode(naep$R833101, levels(naep$R833101)[1:4], c(1:4))
	}
	if('R833401' %in% names(naep)) {
		naep$R833401 <- recode(naep$R833401, levels(naep$R833401)[1:4], c(1:4))
	}
	if('R846301' %in% names(naep)) {
		naep$R846301 <- recode(naep$R846301, levels(naep$R846301)[1:4], c(1:4))
	}
	if('R847901' %in% names(naep)) {
		naep$R847901 <- recode(naep$R847901, c('No','Yes'), c(0,1))
	}
	if('R848001' %in% names(naep)) {
		naep$R848001 <- recode(naep$R848001, c('No','Yes'), c(0,1))
	}
	if('M821401' %in% names(naep)) {
		naep$M821401 <- recode(naep$M821401, c('No','Yes'), c(0,1))
	}
	if('M824201' %in% names(naep)) {
		naep$M824201 <- recode(naep$M824201, levels(naep$M824201)[1:4], c(1:4))
	}
	if('M824301' %in% names(naep)) {
		naep$M824301 <- recode(naep$M824301, levels(naep$M824301)[1:4], c(1:4))
	}
	if('M824701' %in% names(naep)) {
		naep$M824701 <- recode(naep$M824701, levels(naep$M824701)[1:4], c(1:4))
	}
	if('M824902' %in% names(naep)) {
		naep$M824902 <- recode(naep$M824902, levels(naep$M824902)[1:4], c(1:4))
	}
	if('M824903' %in% names(naep)) {
		naep$M824903 <- recode(naep$M824903, levels(naep$M824903)[1:4], c(1:4))
	}
	if('M824904' %in% names(naep)) {
		naep$M824904 <- recode(naep$M824904, levels(naep$M824904)[1:4], c(1:4))
	}
	if('M820901' %in% names(naep)) {
		naep$M820901 <- recode(naep$M820901, levels(naep$M820901)[1:4], c(1:4))
	}
	if('M820904' %in% names(naep)) {
		naep$M820904 <- recode(naep$M820904, levels(naep$M820904)[1:4], c(1:4))
	}
	if('M820905' %in% names(naep)) {
		naep$M820905 <- recode(naep$M820905, levels(naep$M820905)[1:4], c(1:4))
	}
	
	
	return(naep)
}
