g8.reading.school.codebook = read.csv(dataLocation("2007.G8.Reading.School.Codebook.csv"), header=TRUE)
g8.reading.school.complete = read.spss(dataLocation("2007.G8.Reading.School.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)
g8.reading.student.codebook = read.csv(dataLocation("2007.G8.Reading.StudentTeacher.Codebook.csv"), header=TRUE)
g8.reading.student.complete = read.spss(dataLocation("2007.G8.Reading.StudentTeacher.sav"), use.value.labels=FALSE, to.data.frame=TRUE, use.missings=TRUE)

#Subset the full dataset to simplify analysis. We are also removing rows with missing outcome variables.
g8reading.orig = g8.reading.student.complete[-which(
	is.na(g8.reading.student.complete$RRPCM1) | 
	is.na(g8.reading.student.complete$RRPCM2) |
	is.na(g8.reading.student.complete$RRPCM3) |
	is.na(g8.reading.student.complete$RRPCM4) |
	is.na(g8.reading.student.complete$RRPCM5) ), c(
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
	'R832801','R833001','R833101','R833201','R833301','R833401','R833501','R833801',
	'R833901','R834001','R834101','R834201','R834301','R834401','R834501','R834601',
	'R834701','R834901','R835001','R835101','R835201','R835301','R835401','R835501',
	'R835601','R835701','R835801','R835901','R836001','R836101','R836201','R836401',
	'R836501','R836601','R836701','R836801',
	'REGIONS'
)]

names(g8reading.orig) = c('reading1', 'reading2', 'reading3', 'reading4', 'reading5', 'state', 'school', 'charter', 'private', 'gender', 'race', 'iep', 'ell', 'lunch', 'parented', 'books', 'birthmonth', 'birthyear', 'newspapers', 'magazines', 'computer', 'encyclopedia', 'pagesread', 'talkstudies', 'daysabsent', 'langinhome',
	'Write long answers on reading tests',
	'Learn a lot when reading books',
	'Reading is a favorite activity',
	'Writing stories or letters is a favorite activity',
	'Writing helps share ideas',
	'Read for fun on own',
	'Talk with friends about what you read',
	'Write e-mails to friends or family',
	'Read comic books or joke books outside school',
	'Read fiction books or stories outside school',
	'Read plays outside school',
	'Read poems outside school',
	'Read biographies/autobiographies outside school',
	'Read books on science outside school',
	'Read books on technology outside school',
	'Read books on other countries outside school',
	'Read books on history outside school',
	'Read other non-fiction books outside school',
	'Read newspaper articles or stories outside school',
	'Read magazine articles or stories outside school',
	'Read Internet articles or stories outside school',
	'Class discussion about something class has read',
	'Work in groups to talk about something read',
	'Write in journal about something read',
	'Write report or paper about something read',
	'Make presentation to class about something read',
	'Done project about something read',
	'Read other than textbook for English class',
	'Read other than textbook for science class',
	'Read other than textbook for social studies class',
	'Read other than textbook for math class',
	'Explain understanding of what you read',
	'Discuss interpretation of what you read',
	'Difficulty of this reading test',
	'Effort on this reading test',
	'Importance of success on this reading test',
	'region'
)

g8reading.orig$readingscore = (g8reading.orig$reading1 + g8reading.orig$reading2 + g8reading.orig$reading3 + g8reading.orig$reading4 + g8reading.orig$reading5) / 5

#We'll consider formerly ELL as no
g8reading.orig[which(g8reading.orig$ell %in% c(3)),] = 2
#Mark appropriate categories as NA
g8reading.orig[which(g8reading.orig$lunch %in% 4:6),]$lunch = NA
g8reading.orig[which(g8reading.orig$parented %in% c(7,8,0)),]$parented = NA
g8reading.orig[which(g8reading.orig$books %in% c(8,0)),]$books = NA
g8reading.orig[which(g8reading.orig$newspapers %in% c(7,8,0)),]$newspapers = NA
g8reading.orig[which(g8reading.orig$magazines %in% c(7,8,0)),]$magazines = NA
g8reading.orig[which(g8reading.orig$computer %in% c(8,0)),]$computer = NA
g8reading.orig[which(g8reading.orig$encyclopedia %in% c(7,8,0)),]$encyclopedia = NA
g8reading.orig[which(g8reading.orig$pagesread %in% c(8,0)),]$pagesread = NA
g8reading.orig[which(g8reading.orig$talkstudies %in% c(8,0)),]$talkstudies = NA
g8reading.orig[which(g8reading.orig$daysabsent %in% c(8,0)),]$daysabsent = NA
g8reading.orig[which(g8reading.orig$langinhome %in% c(8,0)),]$langinhome = NA

for(col in 27:62) {
	g8reading.orig[which(g8reading.orig[,col] %in% c(7,8,0)), col] = NA
}

#Recode charter school so that 0=public school and 1=charter school
g8reading.orig[which(g8reading.orig$charter==2),]$charter = 0

#Convert the factor variables
g8reading.orig$charter = factor(g8reading.orig$charter, labels=c('Public','Charter'))
g8reading.orig$private = factor(g8reading.orig$private, labels=c('Public', 'Private'))
g8reading.orig$gender = factor(g8reading.orig$gender, labels=c('Male','Female'))
g8reading.orig$race = factor(g8reading.orig$race, labels=c('White','Black','Hispanic','Asian American','American Indian','Other'))
g8reading.orig$iep = factor(g8reading.orig$iep, labels=c('Yes','No'))
g8reading.orig$ell = factor(g8reading.orig$ell, labels=c('Yes','No'))
g8reading.orig$lunch = factor(g8reading.orig$lunch, labels=c('Not eligible','Reduced','Free'), ordered=TRUE)
g8reading.orig$parented = factor(g8reading.orig$parented, labels=c('Did not finish H.S.','Graduated H.S.','Some ed after H.S.','Graduated College'), ordered=TRUE)
g8reading.orig$books = factor(g8reading.orig$books, labels=c('0-10','11-25','26-100','>100'), ordered=TRUE)
g8reading.orig$newspapers = factor(g8reading.orig$newspapers, labels=c('yes', 'no'))
g8reading.orig$magazines = factor(g8reading.orig$magazines, labels=c('yes', 'no'))
g8reading.orig$computer = factor(g8reading.orig$computer, labels=c('yes', 'no'))
g8reading.orig$encyclopedia = factor(g8reading.orig$encyclopedia, labels=c('yes', 'no'))
g8reading.orig$pagesread = factor(g8reading.orig$pagesread, labels=c('<5', '5-10', '11-15', '16-20', '>20'), ordered=TRUE)
g8reading.orig$talkstudies = factor(g8reading.orig$talkstudies, labels=c('Never', '1-2 per month', '1 per week', '2-3 times per week', 'Every day'), ordered=TRUE)
g8reading.orig$daysabsent = factor(g8reading.orig$daysabsent, labels=c('None', '1-2', '3-4', '5-10', '>10'), ordered=TRUE)
g8reading.orig$langinhome = factor(g8reading.orig$langinhome, labels=c('Never', 'Once in a while', 'Half the time', 'All or most of the time'), ordered=TRUE)
g8reading.orig$state = factor(g8reading.orig$state, levels=c(1:2,4:42,44:56,60,72,97), labels=c(
'Alabama','Alaska','Arizona','Arkansas','California','Canal Zone','Colorado','Connecticut',
'Delaware','Dist. of Columbia','Florida','Georgia','Guam','Hawaii','Idaho','Illinois','Indiana',
'Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Virgin Islands',
'Washington','West Virginia','Wisconsin','Wyoming','American Samoa','Puerto Rico','DoDEA'))
g8reading.orig$region = factor(g8reading.orig$region, levels=c(1,2,3,4,9), labels=c('Northeast', 'Southeast', 'Central', 'West', 'DoD'))

#str(g8reading.orig)

g8reading.school.orig = g8.reading.school.complete[,c(
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
names(g8reading.school.orig) = c('school', 'region', 'state', 'urbanicity', 'asian', 'black', 'hispanic', 'enrollment', 'absent', 'hoursinst', 'teacherabsent', 'stillenroll', 'lunch', 'esl', 'specialed', 'charter', 'chartergranted')


#Recode charter school so that 0=public school and 1=charter school
g8reading.school.orig[which(g8reading.school.orig$charter==2),]$charter = 0
#Remove private schools
g8reading.school.orig = g8reading.school.orig[-which(g8reading.school.orig$charter==9),]

#Convert NAEP codes that correspond to NA
g8reading.school.orig[which(g8reading.school.orig$absent %in% c(8,0)),]$absent = NA
g8reading.school.orig[which(g8reading.school.orig$teacherabsent %in% c(8,0)),]$teacherabsent = NA
g8reading.school.orig[which(g8reading.school.orig$stillenroll %in% c(88,0)),]$stillenroll = NA
g8reading.school.orig[which(g8reading.school.orig$lunch %in% c(88,0)),]$lunch = NA
g8reading.school.orig[which(g8reading.school.orig$esl %in% c(88,0)),]$esl = NA
g8reading.school.orig[which(g8reading.school.orig$specialed %in% c(88,0)),]$specialed = NA
g8reading.school.orig[which(g8reading.school.orig$chartergranted %in% c(88)),]$chartergranted = NA

g8reading.school.orig$region = factor(g8reading.school.orig$region, labels=c('Northeast','Southeast','Central','West','DoDDS'))
g8reading.school.orig$urbanicity = factor(g8reading.school.orig$urbanicity, labels=c('Large city', 'Midsize city', 'Urban fringe of LC', 'Urban fringe of MC', 'Large town', 'Small town', 'Rural', 'Rural inside CBSA', 'DoD'))
g8reading.school.orig$absent = factor(g8reading.school.orig$absent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g8reading.school.orig$teacherabsent = factor(g8reading.school.orig$teacherabsent, labels=c('0-2%', '3-5%', '6-10%', '>10%'), ordered=TRUE)
g8reading.school.orig$stillenroll = factor(g8reading.school.orig$stillenroll, labels=c('98-100%', '95-97%', '90-94%', '80-89%', '70-79%', '60-69%', '50-59%', '<50%'), ordered=TRUE)
g8reading.school.orig$lunch = factor(g8reading.school.orig$lunch, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-34%', '35-50%', '51-75%', '76-99%', '100%'), ordered=TRUE)
g8reading.school.orig$esl = factor(g8reading.school.orig$esl, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g8reading.school.orig$specialed = factor(g8reading.school.orig$specialed, labels=c('0%', '1-5%', '6-10%', '11-25%', '26-50%', '51-75%', '76-90%', '>90%'), ordered=TRUE)
g8reading.school.orig$charter = factor(g8reading.school.orig$charter, levels=c(0,1), labels=c('Public', 'Charter'))
g8reading.school.orig$chartergranted = factor(g8reading.school.orig$chartergranted)

#str(g8reading.school.orig)

#Merge the charter school variable to the student level table
#NOTE: There are 1982 students from Alaska for whom there is no corresponding data from the school file
g8reading.merged.orig = merge(g8reading.orig, g8reading.school.orig, by.x='school', by.y='school')
