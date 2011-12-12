g4.reading.school.codebook = read.csv(dataLocation("2007.G4.Reading.School.Codebook.csv"), header=TRUE)
g4.reading.school.complete = read.spss(dataLocation("2007.G4.Reading.School.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)
g4.reading.student.codebook = read.csv(dataLocation("2007.G4.Reading.StudentTeacher.Codebook.csv"), header=TRUE)
g4.reading.student.complete = read.spss(dataLocation("2007.G4.Reading.StudentTeacher.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)

#Subset the full dataset to simplify analysis. We are also removing rows with missing outcome variables.
g4reading.orig = g4.reading.student.complete[-which(
	is.na(g4.reading.student.complete$RRPCM1) | 
	is.na(g4.reading.student.complete$RRPCM2) |
	is.na(g4.reading.student.complete$RRPCM3) |
	is.na(g4.reading.student.complete$RRPCM4) |
	is.na(g4.reading.student.complete$RRPCM5) ), c(
	"RRPCM1", "RRPCM2", "RRPCM3", "RRPCM4", "RRPCM5", #NAEP scores
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
	"B018201",   #Language other than English in home (1=never; 2=once in a while; 3=half the time; 4=All or most of the time)
	'R830601','R830701','R830801','R830901','R831001','R831101','R831401','R831501','R831601',
	'R831701','R831801','R831901','R832001','R832101','R832201','R832301','R832401','R832501',
	'R832601','R832701','R832801','R836601','R836701','R836801',
	'REGIONS'
)]
names(g4reading.orig) = c('reading1', 'reading2', 'reading3', 'reading4', 'reading5', 'state', 'school', 'charter', 'private', 'gender', 'race', 'iep', 'ell', 'lunch', 'books', 'birthmonth', 'birthyear', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome',
	'Learn a lot when reading books',
	'Reading is a favorite activity',
	'Writing stories or letters is a favorite activity',
	'Writing helps share ideas',
	'Read for fun on own',
	'Talk with friends about what you read',
	'Write e-mails to friends or family',
	'Read stories or poems for fun',
	'Read to learn about real things',
	'Read stories on Internet for fun',
	'Class discussion about something class has read',
	'Work in groups to talk about something read',
	'Write in journal about something read',
	'Write a book report',
	'Make presentation to class about something read',
	'Do school project about something read',
	'Read books or magazines for reading',
	'Read books or magazines for science',
	'Read books or magazines for social studies/history',
	'Read books or magazines for math',
	'Write long answers on reading tests',
	'Difficulty of this reading test',
	'Effort on this reading test',
	'Importance of success on this reading test',
	'region')

g4reading.orig$readingscore = (g4reading.orig$reading1 + g4reading.orig$reading2 + g4reading.orig$reading3 + g4reading.orig$reading4 + g4reading.orig$reading5) / 5

g4reading.orig[which(g4reading.orig$ell %in% c(3)),] = 2
#Mark appropriate categories as NA
g4reading.orig[which(g4reading.orig$lunch %in% 4:6),]$lunch = NA
g4reading.orig[which(g4reading.orig$books %in% c(8,0)),]$books = NA
g4reading.orig[which(g4reading.orig$newspapers %in% c(7,8,0)),]$newspapers = NA
g4reading.orig[which(g4reading.orig$magazines %in% c(7,8,0)),]$magazines = NA
g4reading.orig[which(g4reading.orig$computer %in% c(8,0)),]$computer = NA
g4reading.orig[which(g4reading.orig$encyclopedia %in% c(7,8,0)),]$encyclopedia = NA
g4reading.orig[which(g4reading.orig$pagesread %in% c(8,0)),]$pagesread = NA
g4reading.orig[which(g4reading.orig$talkstudies %in% c(8,0)),]$talkstudies = NA
g4reading.orig[which(g4reading.orig$daysabsent %in% c(8,0)),]$daysabsent = NA
g4reading.orig[which(g4reading.orig$langinhome %in% c(8,0)),]$langinhome = NA

for(col in 26:49) {
	g4reading.orig[which(g4reading.orig[,col] %in% c(7,8,0)), col] = NA
}

#Recode charter school so that 0=public school and 1=charter school
g4reading.orig[which(g4reading.orig$charter==2),]$charter = 0

#Convert the factor variables
g4reading.orig$charter = factor(g4reading.orig$charter, labels=c('Public','Charter'))
g4reading.orig$private = factor(g4reading.orig$private, labels=c('Public', 'Private'))
g4reading.orig$gender = factor(g4reading.orig$gender, labels=c('Male','Female'))
g4reading.orig$race = factor(g4reading.orig$race, labels=c('White','Black','Hispanic','Asian American','American Indian','Other'))
g4reading.orig$iep = factor(g4reading.orig$iep, labels=c('Yes','No'))
g4reading.orig$ell = factor(g4reading.orig$ell, labels=c('Yes','No'))
g4reading.orig$lunch = factor(g4reading.orig$lunch, labels=c('Not eligible','Reduced','Free'), ordered=TRUE)
g4reading.orig$books = factor(g4reading.orig$books, labels=c('0-10','11-25','26-100','>100'), ordered=TRUE)
g4reading.orig$newspapers = factor(g4reading.orig$newspapers, labels=c('yes', 'no'))
g4reading.orig$magazines = factor(g4reading.orig$magazines, labels=c('yes', 'no'))
g4reading.orig$computer = factor(g4reading.orig$computer, labels=c('yes', 'no'))
g4reading.orig$encyclopedia = factor(g4reading.orig$encyclopedia, labels=c('yes', 'no'))
g4reading.orig$pagesread = factor(g4reading.orig$pagesread, labels=c('<5', '5-10', '11-15', '16-20', '>20'), ordered=TRUE)
g4reading.orig$talkstudies = factor(g4reading.orig$talkstudies, labels=c('Never', '1-2 per month', '1 per week', '2-3 times per week', 'Every day'), ordered=TRUE)
g4reading.orig$daysabsent = factor(g4reading.orig$daysabsent, labels=c('None', '1-2', '3-4', '5-10', '>10'), ordered=TRUE)
g4reading.orig$langinhome = factor(g4reading.orig$langinhome, labels=c('Never', 'Once in a while', 'Half the time', 'All or most of the time'), ordered=TRUE)
g4reading.orig$state = factor(g4reading.orig$state, levels=c(1:2,4:42,44:56,60,72,97), labels=c(
'Alabama','Alaska','Arizona','Arkansas','California','Canal Zone','Colorado','Connecticut',
'Delaware','Dist. of Columbia','Florida','Georgia','Guam','Hawaii','Idaho','Illinois','Indiana',
'Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Virgin Islands',
'Washington','West Virginia','Wisconsin','Wyoming','American Samoa','Puerto Rico','DoDEA'))

#str(g4reading.orig)

g4reading.school.orig = g4.reading.school.complete[,c(
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
names(g4reading.school.orig) = c('school', 'region', 'state', 'urbanicity', 'asian', 'black', 'hispanic', 'enrollment', 'absent', 'hoursinst', 'teacherabsent', 'stillenroll', 'lunch', 'esl', 'specialed', 'charter', 'chartergranted')


#Recode charter school so that 0=public school and 1=charter school
g4reading.school.orig[which(g4reading.school.orig$charter==2),]$charter = 0
#Remove private schools
g4reading.school.orig = g4reading.school.orig[-which(g4reading.school.orig$charter==9),]

#Convert NAEP codes that correspond to NA
g4reading.school.orig[which(g4reading.school.orig$absent %in% c(8,0)),]$absent = NA
g4reading.school.orig[which(g4reading.school.orig$teacherabsent %in% c(8,0)),]$teacherabsent = NA
g4reading.school.orig[which(g4reading.school.orig$stillenroll %in% c(88,0)),]$stillenroll = NA
g4reading.school.orig[which(g4reading.school.orig$lunch %in% c(88,0)),]$lunch = NA
g4reading.school.orig[which(g4reading.school.orig$esl %in% c(88,0)),]$esl = NA
g4reading.school.orig[which(g4reading.school.orig$specialed %in% c(88,0)),]$specialed = NA
g4reading.school.orig[which(g4reading.school.orig$chartergranted %in% c(88)),]$chartergranted = NA

g4reading.school.orig$region = factor(g4reading.school.orig$region, labels=c('Northeast','Southeast','Central','West','DoDDS'))
g4reading.school.orig$urbanicity = factor(g4reading.school.orig$urbanicity, labels=c('Large city', 'Midsize city', 'Urban fringe of LC', 'Urban fringe of MC', 'Large town', 'Small town', 'Rural', 'Rural inside CBSA', 'DoD'))
g4reading.school.orig$absent = factor(g4reading.school.orig$absent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g4reading.school.orig$teacherabsent = factor(g4reading.school.orig$teacherabsent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g4reading.school.orig$stillenroll = factor(g4reading.school.orig$stillenroll, labels=c('98-100%', '95-97%', '90-94%', '80-89%', '70-79%', '60-69%', '50-59%', '<50%'), ordered=TRUE)
g4reading.school.orig$lunch = factor(g4reading.school.orig$lunch, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-34%', '35-50%', '51-75%', '76-99%', '100%'), ordered=TRUE)
g4reading.school.orig$esl = factor(g4reading.school.orig$esl, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g4reading.school.orig$specialed = factor(g4reading.school.orig$specialed, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g4reading.school.orig$charter = factor(g4reading.school.orig$charter, levels=c(0,1), labels=c('Public', 'Charter'))
g4reading.school.orig$chartergranted = factor(g4reading.school.orig$chartergranted)

#str(g4reading.school.orig)

#Merge the charter school variable to the student level table
#NOTE: There are 1982 students from Alaska for whom there is no corresponding data from the school file
g4reading.merged.orig = merge(g4reading.orig, g4reading.school.orig, by.x='school', by.y='school')
