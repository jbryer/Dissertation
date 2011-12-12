g12.writing.school.codebook = read.csv("data/2007.G12.Writing.School.Codebook.csv", header=TRUE)
g12.writing.school.complete = read.spss("data/2007.G12.Writing.School.sav", use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)
g12.writing.student.codebook = read.csv("data/2007.G12.Writing.StudentTeacher.Codebook.csv", header=TRUE)
g12.writing.student.complete = read.spss("data/2007.G12.Writing.StudentTeacher.sav", use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)

#Subset the full dataset to simplify analysis. We are also removing rows with missing outcome variables.
g12writing.orig = g12.writing.student.complete[-which(
	is.na(g12.writing.student.complete$WRIRP1) | 
	is.na(g12.writing.student.complete$WRIRP2) |
	is.na(g12.writing.student.complete$WRIRP3) |
	is.na(g12.writing.student.complete$WRIRP4) |
	is.na(g12.writing.student.complete$WRIRP5) ), c(
	"WRIRP1", "WRIRP2", "WRIRP3", "WRIRP4", "WRIRP5", #NAEP scores
	"FIPSLOC",   #State
	"SCHID",
	"PCHARTR", #School identified as charter (National Public)
	"PUBPRIV", #School type (1=Public; 2=Private)
	"SEX",    #1=Male; 2=Female
	"SRACE",  #1=White; 2=Black; 3=Hispanic; 4=Asian American; 5=American Indian; 6=Other
	"IEP",    #1=yes; 2=no
	"ELL3",   #1=yes; 2=no; 3=formerly 
	"SLUNCH", #1=Not eligible; 2=Reduced; 3=Free; 4=N/A; 5=School Refused; 6=Not participating
	"PARED",  #1=Did not finish H.S.; 2=Graduated H.S.; 3=Some ed after H.S.; 4=Graduated college; 7=I don't know; 8=Omitted; 0=Multiple
	"B013801", #Books in house: 1=0-10; 2=11-25; 3=26-100; 4=>100; 8=Omitted; 0=Multiple
	"MOB",     #Birth month
	"YOB",     #Birth year
	"B017001",   #Newspaper (1=yes; 2=no)
	"B000905",   #Magazines (1=yes; 2=no)
	"B017101",   #Computer (1=yes; 2=no)
	"B017201",   #Encyclopedia (1=yes; 2=no)
	"B001151",   #Pages read in school and for homework (1=<5; 2=6-10; 3=11-15; 4=16-20; 5=>20)
	"B017451",   #Talk about studies at home (1=Never or hardly every; 2=1-2 times a month; 3=1 time a week; 4=2-3 times a week; 5=Every day)
	"B018101",   #Days absent from school last month (1=none; 2=1-2 days; 3=3-4 days; 4=5-10 days; 5=More than 10 days)
	"B018201"    #Language other than English in home (1=never; 2=once in a while; 3=half the time; 4=All or most of the time)
)]
names(g12writing.orig) = c('writing1', 'writing2', 'writing3', 'writing4', 'writing5', 'state', 'school', 'charter', 'private', 'gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'birthmonth', 'birthyear', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome')
g12writing.orig$writingscore = (g12writing.orig$writing1 + g12writing.orig$writing2 + g12writing.orig$writing3 + g12writing.orig$writing4 + g12writing.orig$writing5) / 5

#We'll consider formerly ELL as no
g12writing.orig[which(g12writing.orig$ell %in% c(3)),] = 2
#Mark appropriate categories as NA
g12writing.orig[which(g12writing.orig$lunch %in% 4:6),]$lunch = NA
g12writing.orig[which(g12writing.orig$parented %in% c(7,8,0)),]$parented = NA
g12writing.orig[which(g12writing.orig$books %in% c(8,0)),]$books = NA
g12writing.orig[which(g12writing.orig$newspapers %in% c(7,8,0)),]$newspapers = NA
g12writing.orig[which(g12writing.orig$magazines %in% c(7,8,0)),]$magazines = NA
g12writing.orig[which(g12writing.orig$computer %in% c(8,0)),]$computer = NA
g12writing.orig[which(g12writing.orig$encyclopedia %in% c(7,8,0)),]$encyclopedia = NA
g12writing.orig[which(g12writing.orig$pagesread %in% c(8,0)),]$pagesread = NA
g12writing.orig[which(g12writing.orig$talkstudies %in% c(8,0)),]$talkstudies = NA
g12writing.orig[which(g12writing.orig$daysabsent %in% c(8,0)),]$daysabsent = NA
g12writing.orig[which(g12writing.orig$langinhome %in% c(8,0)),]$langinhome = NA

#Convert the factor variables
g12writing.orig$charter = factor(g12writing.orig$charter, labels=c('Charter','Public'))
g12writing.orig$private = factor(g12writing.orig$private, labels=c('Public', 'Private'))
g12writing.orig$gender = factor(g12writing.orig$gender, labels=c('Male','Female'))
g12writing.orig$race = factor(g12writing.orig$race, labels=c('White','Black','Hispanic','Asian American','American Indian','Other'))
g12writing.orig$iep = factor(g12writing.orig$iep, labels=c('Yes','No'))
g12writing.orig$ell = factor(g12writing.orig$ell, labels=c('Yes','No'))
g12writing.orig$lunch = factor(g12writing.orig$lunch, labels=c('Not eligible','Reduced','Free'), ordered=TRUE)
g12writing.orig$parented = factor(g12writing.orig$parented, labels=c('Did not finish H.S.','Graduated H.S.','Some ed after H.S.','Graduated College'), ordered=TRUE)
g12writing.orig$books = factor(g12writing.orig$books, labels=c('0-10','11-25','26-100','>100'), ordered=TRUE)
g12writing.orig$newspapers = factor(g12writing.orig$newspapers, labels=c('yes', 'no'))
g12writing.orig$magazines = factor(g12writing.orig$magazines, labels=c('yes', 'no'))
g12writing.orig$computer = factor(g12writing.orig$computer, labels=c('yes', 'no'))
g12writing.orig$encyclopedia = factor(g12writing.orig$encyclopedia, labels=c('yes', 'no'))
g12writing.orig$pagesread = factor(g12writing.orig$pagesread, labels=c('<5', '5-10', '11-15', '16-20', '>20'), ordered=TRUE)
g12writing.orig$talkstudies = factor(g12writing.orig$talkstudies, labels=c('Never', '1-2 per month', '1 per week', '2-3 times per week', 'Every day'), ordered=TRUE)
g12writing.orig$daysabsent = factor(g12writing.orig$daysabsent, labels=c('None', '1-2', '3-4', '5-10', '>10'), ordered=TRUE)
g12writing.orig$langinhome = factor(g12writing.orig$langinhome, labels=c('Never', 'Once in a while', 'Half the time', 'All or most of the time'), ordered=TRUE)
g12writing.orig$state = factor(g12writing.orig$state, levels=c(1:2,4:42,44:56,60,72,97), labels=c(
'Alabama','Alaska','Arizona','Arkansas','California','Canal Zone','Colorado','Connecticut',
'Delaware','Dist. of Columbia','Florida','Georgia','Guam','Hawaii','Idaho','Illinois','Indiana',
'Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Virgin Islands',
'Washington','West Virginia','Wisconsin','Wyoming','American Samoa','Puerto Rico','DoDEA'))

#str(g12writing.orig)

g12writing.school.orig = g12.writing.school.complete[,c(
	'SSCHID', 
	'SNAEPRS', # NAEP Region (1=Northeast; 2=Southeast; 3=Central; 4=West; 9=DoDDS)
	'SFIPS',   # State
	'SMLOCAL', # Metro-centric type of locale (1=Large city; 8=Rural; 9=DoD Intl)
	'SSCHASN', # School percent Asian
	'SSCHBLK', # School Percent Black
	'SSCHHSP', # School Percent Hispanic
	'C038101', # Current enrollment
	'C033601', # Percent absent on average day (1=0-2%; 2=3-5%; 3=6-10%; 4=More than 10%; 8=Omitted)
	'C057701', # Hours of instruction at grade 12 as of Feb 1, 2007
	'C036501', # Percent of teachers absent on average day (1=0-2%; 2=3-5%; 3=6-10%; 4=More than 10%; 8=Omitted; 0=Multiple)
	'C037801', # Percent still enrolled at end of year (1=98-100%; 2=95-97%; 3=90-94%; 4=80-89%; 5=70-79%; 6=60-69%; 7=50-59%; 8=Less than 50%; 88=Omitted; 0=Multiple)
	'C051601', # Percent eligible National School Lunch Program (1=0%; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-34%; 6=35-50%; 7=51-75%; 8=76-99%; 9=100%; 88=Omitted)
	'C044006', # Percent receiving non-English instruction (1=None; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-50%; 6=51-75%; 7=76-90%; 8=Over 90%; 88=Omitted)
	'C044007', # Percent in special education (1=None; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-50%; 6=51-75%; 7=76-90%; 8=Over 90%; 88=Omitted)
	'C045709' #School type is charter (1=yes; 8=ommitted)
)]
names(g12writing.school.orig) = c('school', 'region', 'state', 'urbanicity', 'asian', 'black', 'hispanic', 'enrollment', 'absent', 'hoursinst', 'teacherabsent', 'stillenroll', 'lunch', 'esl', 'specialed', 'charter')


#Recode charter school so that 0=public school and 1=charter school
g12writing.school.orig[which(g12writing.school.orig$charter!=1),]$charter = 0
#Remove private schools
#g12writing.school.orig = g12writing.school.orig[-which(g12writing.school.orig$charter==9),]

#Convert NAEP codes that correspond to NA
#g12writing.school.orig[which(g12writing.school.orig$absent %in% c(8,0)),]$absent = NA
#g12writing.school.orig[which(g12writing.school.orig$teacherabsent %in% c(8,0)),]$teacherabsent = NA
#g12writing.school.orig[which(g12writing.school.orig$stillenroll %in% c(88,0)),]$stillenroll = NA
#g12writing.school.orig[which(g12writing.school.orig$lunch %in% c(88,0)),]$lunch = NA
#g12writing.school.orig[which(g12writing.school.orig$esl %in% c(88,0)),]$esl = NA
#g12writing.school.orig[which(g12writing.school.orig$specialed %in% c(88,0)),]$specialed = NA
#g12writing.school.orig[which(g12writing.school.orig$chartergranted %in% c(88)),]$chartergranted = NA

#g12writing.school.orig$region = factor(g12writing.school.orig$region, labels=c('Northeast','Southeast','Central','West','DoDDS'))
#g12writing.school.orig$urbanicity = factor(g12writing.school.orig$urbanicity, labels=c('Large city', 'Midsize city', 'Urban fringe of LC', 'Urban fringe of MC', 'Large town', 'Small town', 'Rural', 'Rural inside CBSA', 'DoD'))
#g12writing.school.orig$absent = factor(g12writing.school.orig$absent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
#g12writing.school.orig$teacherabsent = factor(g12writing.school.orig$teacherabsent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
#g12writing.school.orig$stillenroll = factor(g12writing.school.orig$stillenroll, labels=c('98-100%', '95-97%', '90-94%', '80-89%', '70-79%', '60-69%', '50-59%', '<50%'), ordered=TRUE)
#g12writing.school.orig$lunch = factor(g12writing.school.orig$lunch, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-34%', '35-50%', '51-75%', '76-99%', '100%'), ordered=TRUE)
#g12writing.school.orig$esl = factor(g12writing.school.orig$esl, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
#g12writing.school.orig$specialed = factor(g12writing.school.orig$specialed, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
#g12writing.school.orig$charter = factor(g12writing.school.orig$charter, levels=c(0,1), labels=c('Public', 'Charter'))
#g12writing.school.orig$chartergranted = factor(g12writing.school.orig$chartergranted)

#str(g12writing.school.orig)

#Merge the charter school variable to the student level table
#NOTE: There are 1982 students from Alaska for whom there is no corresponding data from the school file
#g12writing.merged.orig = merge(g12writing.orig, g12writing.school.orig, by.x='school', by.y='school')
