setwd('/Users/jbryer/Dropbox/School/Dissertation')
setwd('c:/Dropbox/My Dropbox/School/Dissertation')

library(ggplot2)
library(maps)

schools = readLines('Data/CharterSchools.txt')
length(schools)/9

schools2 = data.frame(
	names = schools[seq(1, length(schools), by=9)],
	address1 = schools[seq(2, length(schools), by=9)],
	address2 = schools[seq(3, length(schools), by=9)],
	phone = schools[seq(4, length(schools), by=9)],
	opened = schools[seq(5, length(schools), by=9)],
	enrollment = schools[seq(6, length(schools), by=9)],
	grades = schools[seq(7, length(schools), by=9)],
	status = schools[seq(8, length(schools), by=9)],
	description = schools[seq(9, length(schools), by=9)],
	stringsAsFactors=FALSE)

schools2$phone = substr(schools2$phone, 8, nchar(schools2$phone))
schools2$opened = substr(schools2$opened, 8, nchar(schools2$opened))
schools2$enrollment = substr(schools2$enrollment, 13, nchar(schools2$enrollment))
schools2$grades = substr(schools2$grades, 9, nchar(schools2$grades))
schools2$status = substr(schools2$status, 9, nchar(schools2$status))
schools2$description = substr(schools2$description, 14, nchar(schools2$description))

str(schools2)
names(schools2); nrow(schools2)
head(schools2[,1:8])

table(schools2$status, useNA='ifany')

googlekey="ABQIAAAA7bgkuLvqAhe9zntD3TgmaRSEzMG93xTLm-NuYn_uyePMxeJfGxSTb0fCEC5GKYQCy6b4M0Tt9kwpIg"
yahookey="cg983VbV34EHOpu7ipokF7nj5EqBfW.U069s7kiHrS7fXsuVupmXAUf6EMxR7g60iQ--"

geocode.google <- function(key, address) {
	google.url = "http://maps.google.com/maps/geo?"
	u = url(URLencode( paste(google.url, "q=", address, "&output=csv&key=", key, sep="") ))
	r = readLines(u, n=1, warn=FALSE)
	split = strsplit(r, ",")[[1]]
	lat = split[3]
	lon = split[4]
	close(u)
	data.frame(longitude=lon, latitude=lat)
}

geocode.yahoo <- function(key, address) {
	yahoo.url = "http://local.yahooapis.com/MapsService/V1/geocode?"
	u = url(URLencode( paste(yahoo.url, "location=", address, "&output=xml&appid=", key)))
	r = readLines(u, n=-1, warn=FALSE)
	doc = xmlInternalTreeParse(r)
	root = xmlRoot(doc)
	lat = xmlValue(root[[1]]["Latitude"][[1]])
	lon = xmlValue(root[[1]]["Longitude"][[1]])
	close(u)
	data.frame(longitude=lon, latitude=lat)
}

coords = do.call('rbind', apply(schools2, 1, function(x) { geocode.google(googlekey, paste(x['address1'], x['address2'], sep=', ')) }))
coords[which(coords$longitude == 0 | coords$latitude == 0),c('longitude', 'latitude')] = NA
nrow(coords); nrow(schools2)

schools = cbind(schools2, coords)
write.csv(schools, file='Data/CharterSchools.csv', row.names=FALSE)

schools = read.csv('Data/CharterSchools.csv', stringsAsFactors=FALSE)
names(schools); nrow(schools)
head(schools)

tmp = strsplit(schools$address2, ',')
tmp2 = unlist(lapply(tmp, function(x) length(x)))
table(tmp2)
tmp3 = unlist(tmp)[seq(2,2*length(tmp),by=2)]
tmp3 = gsub("^\\s+|\\s+$", "", tmp3)
tmp4 = strsplit(tmp3, ' ')
tmp5 = ( unlist(lapply(tmp4, function(x) length(x))) )
table(tmp5)
schools[tmp5 == 3,]
schools[tmp5 == 1,'address2']

tmp1 = unlist(strsplit(schools$address2, ','))[seq(1,2*nrow(schools),by=2)]
tmp2 = unlist(strsplit(schools$address2, ','))[seq(2,2*nrow(schools),by=2)]
tmp2 = gsub("^\\s+|\\s+$", "", tmp2)
schools$state = unlist(strsplit(tmp2, ' '))[seq(1,2*length(tmp2), by=2)]
schools$zip = unlist(strsplit(tmp2, ' '))[seq(2,2*length(tmp2), by=2)]

table(schools$state, useNA='ifany')

usa = map_data('state')
ggplot(data=usa, aes_string(x='long', y='lat', group='group')) + geom_polygon(colour='grey', fill='white') + geom_point(data=schools, aes(x=longitude, y=latitude, group=status), alpha=.5) + coord_map() + xlab(NULL) + ylab(NULL) + opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank(), plot.background=theme_rect(colour = NA), panel.grid.major=theme_blank(), panel.background=theme_rect(fill = "white", colour = NA), panel.border=theme_blank())


