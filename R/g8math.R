g8.math.school.codebook = read.csv(dataLocation("2007.G8.Math.School.Codebook.csv"), header=TRUE)
g8.math.school.complete = read.spss(dataLocation("2007.G8.Math.School.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)
g8.math.student.codebook = read.csv(dataLocation("2007.G8.Math.StudentTeacher.Codebook.csv"), header=TRUE)
g8.math.student.complete = read.spss(dataLocation("2007.G8.Math.StudentTeacher.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)

#Subset the full dataset to simplify analysis. We are also removing rows with missing outcome variables.
g8math.orig = g8.math.student.complete[-which(
	is.na(g8.math.student.complete$MRPCM1) | 
	is.na(g8.math.student.complete$MRPCM2) |
	is.na(g8.math.student.complete$MRPCM3) |
	is.na(g8.math.student.complete$MRPCM4) |
	is.na(g8.math.student.complete$MRPCM5) ), c(
	"MRPCM1", "MRPCM2", "MRPCM3", "MRPCM4", "MRPCM5", #NAEP scores
	"FIPS",   #State
	"SCHID",
	"CHRTRPT", #School identified as charter (National Public)
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
	"B018201",   #Language other than English in home (1=never; 2=once in a while; 3=half the time; 4=All or most of the time)
	'M814301','M815301','M815401','M815501','M815601',
	'M815701','M815801','M815901','M816001','M816101','M816201','M816301','M816401',
	'M816501','M816601','M816701','M816801','M817401','M817501','M817601','M820101',
	'M820102','M820103','M820104','M820105','M820106','M820107','M820108',
	'REGION'
)]
names(g8math.orig) = c('math1', 'math2', 'math3', 'math4', 'math5', 'state', 'school', 'charter', 'private', 'gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'birthmonth', 'birthyear', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome',
	'Use computer at school for math',
	'Use calculator for math tests-student',
	'Difficulty of this math test',
	'Effort on this math test',
	'Importance of success on this math test',
	'Math class taking now',
	'Math class expected next year',
	'Time per day on computer for math work',
	'Use spreadsheet program for math assignments',
	'Use program to drill on math facts',
	'Use program for new lessons on problem-solving',
	'Use Internet to learn things for math class',
	'Use calculator program for math class',
	'Using graphing program for charts for math class',
	'Use statistical program for math class',
	'Use word processing program for math class',
	'Use drawing program for math class',
	'Use basic four-function calculator in math class',
	'Use scientific calculator in math class',
	'Use graphing calculator in math class',
	'Have clear understanding what teacher asking to do',
	'The math work is too easy',
	'The math work is boring',
	'I have done a good job on my homework',
	'I have done a good job on my classwork',
	'The math work is challenging',
	'The math work is engaging and interesting',
	'I am learning',
	'region')

g8math.orig$mathscore = (g8math.orig$math1 + g8math.orig$math2 + g8math.orig$math3 + g8math.orig$math4 + g8math.orig$math5) / 5

#We'll consider formerly ELL as no
g8math.orig[which(g8math.orig$ell %in% c(3)),] = 2
#Mark appropriate categories as NA
g8math.orig[which(g8math.orig$lunch %in% 4:6),]$lunch = NA
g8math.orig[which(g8math.orig$parented %in% c(7,8,0)),]$parented = NA
g8math.orig[which(g8math.orig$books %in% c(8,0)),]$books = NA
g8math.orig[which(g8math.orig$newspapers %in% c(7,8,0)),]$newspapers = NA
g8math.orig[which(g8math.orig$magazines %in% c(7,8,0)),]$magazines = NA
g8math.orig[which(g8math.orig$computer %in% c(8,0)),]$computer = NA
g8math.orig[which(g8math.orig$encyclopedia %in% c(7,8,0)),]$encyclopedia = NA
g8math.orig[which(g8math.orig$pagesread %in% c(8,0)),]$pagesread = NA
g8math.orig[which(g8math.orig$talkstudies %in% c(8,0)),]$talkstudies = NA
g8math.orig[which(g8math.orig$daysabsent %in% c(8,0)),]$daysabsent = NA
g8math.orig[which(g8math.orig$langinhome %in% c(8,0)),]$langinhome = NA

for(col in 27:54) {
	g8math.orig[which(g8math.orig[,col] %in% c(7,8,0)), col] = NA
}

#Recode charter school so that 0=public school and 1=charter school
g8math.orig[which(g8math.orig$charter==2),]$charter = 0

#Convert the factor variables
g8math.orig$charter = factor(g8math.orig$charter, labels=c('Public','Charter'))
g8math.orig$private = factor(g8math.orig$private, labels=c('Public', 'Private'))
g8math.orig$gender = factor(g8math.orig$gender, labels=c('Male','Female'))
g8math.orig$race = factor(g8math.orig$race, labels=c('White','Black','Hispanic','Asian American','American Indian','Other'))
g8math.orig$iep = factor(g8math.orig$iep, labels=c('Yes','No'))
g8math.orig$ell = factor(g8math.orig$ell, labels=c('Yes','No'))
g8math.orig$lunch = factor(g8math.orig$lunch, labels=c('Not eligible','Reduced','Free'), ordered=TRUE)
g8math.orig$parented = factor(g8math.orig$parented, labels=c('Did not finish H.S.','Graduated H.S.','Some ed after H.S.','Graduated College'), ordered=TRUE)
g8math.orig$books = factor(g8math.orig$books, labels=c('0-10','11-25','26-100','>100'), ordered=TRUE)
g8math.orig$newspapers = factor(g8math.orig$newspapers, labels=c('yes', 'no'))
g8math.orig$magazines = factor(g8math.orig$magazines, labels=c('yes', 'no'))
g8math.orig$computer = factor(g8math.orig$computer, labels=c('yes', 'no'))
g8math.orig$encyclopedia = factor(g8math.orig$encyclopedia, labels=c('yes', 'no'))
g8math.orig$pagesread = factor(g8math.orig$pagesread, labels=c('<5', '5-10', '11-15', '16-20', '>20'), ordered=TRUE)
g8math.orig$talkstudies = factor(g8math.orig$talkstudies, labels=c('Never', '1-2 per month', '1 per week', '2-3 times per week', 'Every day'), ordered=TRUE)
g8math.orig$daysabsent = factor(g8math.orig$daysabsent, labels=c('None', '1-2', '3-4', '5-10', '>10'), ordered=TRUE)
g8math.orig$langinhome = factor(g8math.orig$langinhome, labels=c('Never', 'Once in a while', 'Half the time', 'All or most of the time'), ordered=TRUE)
g8math.orig$state = factor(g8math.orig$state, levels=c(1:2,4:42,44:56,60,72,97), labels=c(
'Alabama','Alaska','Arizona','Arkansas','California','Canal Zone','Colorado','Connecticut',
'Delaware','Dist. of Columbia','Florida','Georgia','Guam','Hawaii','Idaho','Illinois','Indiana',
'Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Virgin Islands',
'Washington','West Virginia','Wisconsin','Wyoming','American Samoa','Puerto Rico','DoDEA'))

#str(g8math.orig)

g8math.school.orig = g8.math.school.complete[,c(
	'SSCHID', 
	'SNAEPRS', # NAEP Region (1=Northeast; 2=Southeast; 3=Central; 4=West; 9=DoDDS)
	'SFIPS',   # State
	'SMLOCAL', # Metro-centric type of locale (1=Large city; 8=Rural; 9=DoD Intl)
	'SSCHASN', # School percent Asian
	'SSCHBLK', # School Percent Black
	'SSCHHSP', # School Percent Hispanic
	'C038101', # Current enrollment
	'C033601', # Percent absent on average day (1=0-2%; 2=3-5%; 3=6-10%; 4=More than 10%; 8=Omitted)
	'C057301', # Hours of instruction at grade 8 as of Feb 1, 2007
	'C036501', # Percent of teachers absent on average day (1=0-2%; 2=3-5%; 3=6-10%; 4=More than 10%; 8=Omitted; 0=Multiple)
	'C037801', # Percent still enrolled at end of year (1=98-100%; 2=95-97%; 3=90-94%; 4=80-89%; 5=70-79%; 6=60-69%; 7=50-59%; 8=Less than 50%; 88=Omitted; 0=Multiple)
	'C051601', # Percent eligible National School Lunch Program (1=0%; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-34%; 6=35-50%; 7=51-75%; 8=76-99%; 9=100%; 88=Omitted)
	'C044006', # Percent receiving non-English instruction (1=None; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-50%; 6=51-75%; 7=76-90%; 8=Over 90%; 88=Omitted)
	'C044007', # Percent in special education (1=None; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-50%; 6=51-75%; 7=76-90%; 8=Over 90%; 88=Omitted)
	'SCHRTFL', #School type is charter (1=charter; 2=not a charter; 3=private)
	'CS01101'  #Number of years since charter was granted (1 to 12; 88=omitted)
	
)]
names(g8math.school.orig) = c('school', 'region', 'state', 'urbanicity', 'asian', 'black', 'hispanic', 'enrollment', 'absent', 'hoursinst', 'teacherabsent', 'stillenroll', 'lunch', 'esl', 'specialed', 'charter', 'chartergranted')


#Recode charter school so that 0=public school and 1=charter school
g8math.school.orig[which(g8math.school.orig$charter==2),]$charter = 0
#Remove private schools
g8math.school.orig = g8math.school.orig[-which(g8math.school.orig$charter==9),]

#Convert NAEP codes that correspond to NA
g8math.school.orig[which(g8math.school.orig$absent %in% c(8,0)),]$absent = NA
g8math.school.orig[which(g8math.school.orig$teacherabsent %in% c(8,0)),]$teacherabsent = NA
g8math.school.orig[which(g8math.school.orig$stillenroll %in% c(88,0)),]$stillenroll = NA
g8math.school.orig[which(g8math.school.orig$lunch %in% c(88,0)),]$lunch = NA
g8math.school.orig[which(g8math.school.orig$esl %in% c(88,0)),]$esl = NA
g8math.school.orig[which(g8math.school.orig$specialed %in% c(88,0)),]$specialed = NA
g8math.school.orig[which(g8math.school.orig$chartergranted %in% c(88)),]$chartergranted = NA

g8math.school.orig$region = factor(g8math.school.orig$region, labels=c('Northeast','Southeast','Central','West','DoDDS'))
g8math.school.orig$urbanicity = factor(g8math.school.orig$urbanicity, labels=c('Large city', 'Midsize city', 'Urban fringe of LC', 'Urban fringe of MC', 'Large town', 'Small town', 'Rural', 'Rural inside CBSA', 'DoD'))
g8math.school.orig$absent = factor(g8math.school.orig$absent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g8math.school.orig$teacherabsent = factor(g8math.school.orig$teacherabsent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g8math.school.orig$stillenroll = factor(g8math.school.orig$stillenroll, labels=c('98-100%', '95-97%', '90-94%', '80-89%', '70-79%', '60-69%', '50-59%', '<50%'), ordered=TRUE)
g8math.school.orig$lunch = factor(g8math.school.orig$lunch, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-34%', '35-50%', '51-75%', '76-99%', '100%'), ordered=TRUE)
g8math.school.orig$esl = factor(g8math.school.orig$esl, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g8math.school.orig$specialed = factor(g8math.school.orig$specialed, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g8math.school.orig$charter = factor(g8math.school.orig$charter, levels=c(0,1), labels=c('Public', 'Charter'))
g8math.school.orig$chartergranted = factor(g8math.school.orig$chartergranted)

#str(g8math.school.orig)

#Merge the charter school variable to the student level table
#NOTE: There are 1982 students from Alaska for whom there is no corresponding data from the school file
g8math.merged.orig = merge(g8math.orig, g8math.school.orig, by.x='school', by.y='school')
