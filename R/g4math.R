g4.math.school.codebook = read.csv(dataLocation("2007.G4.Math.School.Codebook.csv"), header=TRUE)
g4.math.school.complete = read.spss(dataLocation("2007.G4.Math.School.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)
g4.math.student.codebook = read.csv(dataLocation("2007.G4.Math.StudentTeacher.Codebook.csv"), header=TRUE)
g4.math.student.complete = read.spss(dataLocation("2007.G4.Math.StudentTeacher.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)

#Subset the full dataset to simplify analysis. We are also removing rows with missing outcome variables.
g4math.orig = g4.math.student.complete[-which(
	is.na(g4.math.student.complete$MRPCM1) | 
	is.na(g4.math.student.complete$MRPCM2) |
	is.na(g4.math.student.complete$MRPCM3) |
	is.na(g4.math.student.complete$MRPCM4) |
	is.na(g4.math.student.complete$MRPCM5) ), c(
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
	"B018201",    #Language other than English in home (1=never; 2=once in a while; 3=half the time; 4=All or most of the time)
	'M814301','M814601','M814701','M815101','M815301',
	'M815401','M815501','M815601','M820001','M820002','M820003','M820004','M820005',
	'REGIONS'
)]
names(g4math.orig) = c('math1', 'math2', 'math3', 'math4', 'math5', 'state', 'school', 'charter', 'private', 'gender', 'race', 'iep', 'ell', 'lunch', 'books', 'birthmonth', 'birthyear', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome',
	'Use computer at school for math',
	'Use computer to practice or drill on math',
	'Use computer to play math games',
	'Kind of calculator you normally use',
	'Use calculator for math tests-student',
	'Difficulty of this math test',
	'Effort on this math test',
	'Importance of success on this math test',
	'The math work is too hard',
	'I have done a good job on my homework',
	'I have done a good job in class',
	'The math work is too easy',
	'I like what we do in math class',
	'region'
)

g4math.orig$mathscore = (g4math.orig$math1 + g4math.orig$math2 + g4math.orig$math3 + g4math.orig$math4 + g4math.orig$math5) / 5

g4math.orig[which(g4math.orig$ell %in% c(3)),] = 2
#Mark appropriate categories as NA
g4math.orig[which(g4math.orig$lunch %in% 4:6),]$lunch = NA
g4math.orig[which(g4math.orig$books %in% c(8,0)),]$books = NA
g4math.orig[which(g4math.orig$newspapers %in% c(7,8,0)),]$newspapers = NA
g4math.orig[which(g4math.orig$magazines %in% c(7,8,0)),]$magazines = NA
g4math.orig[which(g4math.orig$computer %in% c(8,0)),]$computer = NA
g4math.orig[which(g4math.orig$encyclopedia %in% c(7,8,0)),]$encyclopedia = NA
g4math.orig[which(g4math.orig$pagesread %in% c(8,0)),]$pagesread = NA
g4math.orig[which(g4math.orig$talkstudies %in% c(8,0)),]$talkstudies = NA
g4math.orig[which(g4math.orig$daysabsent %in% c(8,0)),]$daysabsent = NA
g4math.orig[which(g4math.orig$langinhome %in% c(8,0)),]$langinhome = NA

for(col in 26:38) {
	g4math.orig[which(g4math.orig[,col] %in% c(7,8,0)), col] = NA
}

#Recode charter school so that 0=public school and 1=charter school
g4math.orig[which(g4math.orig$charter==2),]$charter = 0

#Convert the factor variables
g4math.orig$charter = factor(g4math.orig$charter, labels=c('Public','Charter'))
g4math.orig$private = factor(g4math.orig$private, labels=c('Public', 'Private'))
g4math.orig$gender = factor(g4math.orig$gender, labels=c('Male','Female'))
g4math.orig$race = factor(g4math.orig$race, labels=c('White','Black','Hispanic','Asian American','American Indian','Other'))
g4math.orig$iep = factor(g4math.orig$iep, labels=c('Yes','No'))
g4math.orig$ell = factor(g4math.orig$ell, labels=c('Yes','No'))
g4math.orig$lunch = factor(g4math.orig$lunch, labels=c('Not eligible','Reduced','Free'), ordered=TRUE)
g4math.orig$books = factor(g4math.orig$books, labels=c('0-10','11-25','26-100','>100'), ordered=TRUE)
g4math.orig$newspapers = factor(g4math.orig$newspapers, labels=c('yes', 'no'))
g4math.orig$magazines = factor(g4math.orig$magazines, labels=c('yes', 'no'))
g4math.orig$computer = factor(g4math.orig$computer, labels=c('yes', 'no'))
g4math.orig$encyclopedia = factor(g4math.orig$encyclopedia, labels=c('yes', 'no'))
g4math.orig$pagesread = factor(g4math.orig$pagesread, labels=c('<5', '5-10', '11-15', '16-20', '>20'), ordered=TRUE)
g4math.orig$talkstudies = factor(g4math.orig$talkstudies, labels=c('Never', '1-2 per month', '1 per week', '2-3 times per week', 'Every day'), ordered=TRUE)
g4math.orig$daysabsent = factor(g4math.orig$daysabsent, labels=c('None', '1-2', '3-4', '5-10', '>10'), ordered=TRUE)
g4math.orig$langinhome = factor(g4math.orig$langinhome, labels=c('Never', 'Once in a while', 'Half the time', 'All or most of the time'), ordered=TRUE)
g4math.orig$state = factor(g4math.orig$state, levels=c(1:2,4:42,44:56,60,72,97), labels=c(
'Alabama','Alaska','Arizona','Arkansas','California','Canal Zone','Colorado','Connecticut',
'Delaware','Dist. of Columbia','Florida','Georgia','Guam','Hawaii','Idaho','Illinois','Indiana',
'Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Virgin Islands',
'Washington','West Virginia','Wisconsin','Wyoming','American Samoa','Puerto Rico','DoDEA'))

#str(g4math.orig)

g4math.school.orig = g4.math.school.complete[,c(
	'SSCHID', 
	'SNAEPRG', # NAEP Region (1=Northeast; 2=Southeast; 3=Central; 4=West; 9=DoDDS)
	'SFIPS',   # State
	'SMLOCAL',   # Metro-centric type of locale 
	'SSCHASN', # School percent Asian
	'SSCHBLK', # School Percent Black
	'SSCHHSP', # School Percent Hispanic
	'C038101', # Current enrollment
	'C033601', # Percent absent on average day (1=0-2%; 2=3-5%; 3=6-10%; 4=More than 10%; 8=Omitted)
	'C056701', # Hours of instruction at grade 4 as of Feb 1, 2007
	'C036501', # Percent of teachers absent on average day (1=0-2%; 2=3-5%; 3=6-10%; 4=More than 10%; 8=Omitted; 0=Multiple)
	'C037801', # Percent still enrolled at end of year (1=98-100%; 2=95-97%; 3=90-94%; 4=80-89%; 5=70-79%; 6=60-69%; 7=50-59%; 8=Less than 50%; 88=Omitted; 0=Multiple)
	'C051601', # Percent eligible National School Lunch Program (1=0%; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-34%; 6=35-50%; 7=51-75%; 8=76-99%; 9=100%; 88=Omitted)
	'C044006', # Percent receiving non-English instruction (1=None; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-50%; 6=51-75%; 7=76-90%; 8=Over 90%; 88=Omitted)
	'C044007', # Percent in special education (1=None; 2=1-5%; 3=6-10%; 4=11-25%; 5=26-50%; 6=51-75%; 7=76-90%; 8=Over 90%; 88=Omitted)
	'SCHRTFL', #School type is charter (1=charter; 2=not a charter; 3=private)
	'CS01101'  #Number of years since charter was granted (1 to 12; 88=omitted)
)]
names(g4math.school.orig) = c('school', 'region', 'state', 'urbanicity', 'asian', 'black', 'hispanic', 'enrollment', 'absent', 'hoursinst', 'teacherabsent', 'stillenroll', 'lunch', 'esl', 'specialed', 'charter', 'chartergranted')


#Recode charter school so that 0=public school and 1=charter school
g4math.school.orig[which(g4math.school.orig$charter==2),]$charter = 0
#Remove private schools
g4math.school.orig = g4math.school.orig[-which(g4math.school.orig$charter==9),]

#Convert NAEP codes that correspond to NA
g4math.school.orig[which(g4math.school.orig$absent %in% c(8,0)),]$absent = NA
g4math.school.orig[which(g4math.school.orig$teacherabsent %in% c(8,0)),]$teacherabsent = NA
g4math.school.orig[which(g4math.school.orig$stillenroll %in% c(88,0)),]$stillenroll = NA
g4math.school.orig[which(g4math.school.orig$lunch %in% c(88,0)),]$lunch = NA
g4math.school.orig[which(g4math.school.orig$esl %in% c(88,0)),]$esl = NA
g4math.school.orig[which(g4math.school.orig$specialed %in% c(88,0)),]$specialed = NA
g4math.school.orig[which(g4math.school.orig$chartergranted %in% c(88)),]$chartergranted = NA

g4math.school.orig$region = factor(g4math.school.orig$region, labels=c('Northeast','Southeast','Central','West','DoDDS'))
g4math.school.orig$urbanicity = factor(g4math.school.orig$urbanicity, labels=c('Large city', 'Midsize city', 'Urban fringe of LC', 'Urban fringe of MC', 'Large town', 'Small town', 'Rural', 'Rural inside CBSA', 'DoD'))
g4math.school.orig$absent = factor(g4math.school.orig$absent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g4math.school.orig$teacherabsent = factor(g4math.school.orig$teacherabsent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g4math.school.orig$stillenroll = factor(g4math.school.orig$stillenroll, labels=c('98-100%', '95-97%', '90-94%', '80-89%', '70-79%', '60-69%', '50-59%', '<50%'), ordered=TRUE)
g4math.school.orig$lunch = factor(g4math.school.orig$lunch, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-34%', '35-50%', '51-75%', '76-99%', '100%'), ordered=TRUE)
g4math.school.orig$esl = factor(g4math.school.orig$esl, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g4math.school.orig$specialed = factor(g4math.school.orig$specialed, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g4math.school.orig$charter = factor(g4math.school.orig$charter, levels=c(0,1), labels=c('Public', 'Charter'))
g4math.school.orig$chartergranted = factor(g4math.school.orig$chartergranted)

#str(g4math.school.orig)

#Merge the charter school variable to the student level table
#NOTE: There are 1982 students from Alaska for whom there is no corresponding data from the school file
g4math.merged.orig = merge(g4math.orig, g4math.school.orig, by.x='school', by.y='school')
