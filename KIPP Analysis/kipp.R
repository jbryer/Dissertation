library(naep)

dir = getBaseDirectory()

#School data
catalog = getNAEPCatalog(year=2009, grade=8, subject='Math', directory=dir, type='School')
vars = selectVariables(year=2009, grade=8, subject='Math', directory=dir, type='School')
vars  = c(catalog[which(catalog$PreselectFlag == 1),'FieldName'], 'SNCESSC') #Default variables plus NCES school ID
g8mathschool = getNAEPData(year=2009, grade=8, subject='Math', directory=dir, type='School', vars=vars)
names(g8mathschool)
nrow(g8mathschool)

#Student data
catalog.student = getNAEPCatalog(year=2009, grade=8, subject='Math', directory=dir, type='Student')
vars = selectVariables(year=2009, grade=8, subject='Math', directory=dir, type='Student')
vars  = c(catalog.student[which(catalog.student$PreselectFlag == 1),'FieldName'], 'NCESSCH') #Default variables plus NCES school ID
g8mathstudent = getNAEPData(year=2009, grade=8, subject='Math', directory=dir, type='Student', vars=vars)
names(g8mathstudent)
nrow(g8mathstudent)

#List of KIPP schools from searching the NCES website http://nces.ed.gov/ccd/schoolsearch/index.asp
kipp = read.csv('~/Dropbox/School/Dissertation/KIPP Analysis/KIPPschoolsNCES.csv')
names(kipp)
nrow(kipp)

head(g8mathschool$SNCESSC)

naepKIPP = g8mathschool[which(g8mathschool$SNCESSC %in% kipp$NCES.School.ID),]
nrow(naepKIPP)

naepKIPP.student = g8mathstudent[which(g8mathstudent$NCESSCH %in% kipp$NCES.School.ID),]
nrow(naepKIPP.student)
