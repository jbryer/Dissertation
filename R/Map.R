require(maptools)
require(ggplot2)
require(RColorBrewer)
require(gdata)
require(maps)
require(gridExtra)

setwd('C:/Dropbox/School/Dissertation')
setwd('/Users/jbryer/Dropbox/School/Dissertation')

stateThematic <- function(df, stateCol, fillCol, ...) {
 	df[,stateCol] = tolower(df[,stateCol])
	usa = map_data('state')
	usa = merge(usa, df, by.x='region', by.y=stateCol, all.x=TRUE)
	usa = arrange(usa, order)
	usa.map = ggplot(data=usa, aes_string(x='long', y='lat', group='group', fill=fillCol)) + geom_polygon(colour='black') + coord_map() + xlab(NULL) + ylab(NULL) + opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank()) + scale_fill_gradient(...)

  	alaska = map_data('world', region='USA')
	alaska = alaska[which(alaska$subregion == 'Alaska'),]
	alaska[which(alaska$long > 0),]$long = alaska[which(alaska$long > 0),]$long * -1
	alaska$subregion = tolower(alaska$subregion)
	alaska = merge(alaska, df, by.x='subregion', by.y=stateCol, all.x=TRUE)
	alaska = arrange(alaska, order)
	alaska.map = ggplot(alaska, aes_string(x='long', y='lat', group='group', fill=fillCol)) + geom_polygon(colour='black') + coord_map() + xlab(NULL) + ylab(NULL) + opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank())

	hawaii = map_data('world', region='Hawaii')
	hawaii = hawaii[which(hawaii$long > -161),]
	hawaii$region = tolower(hawaii$region)
	hawaii = merge(hawaii, df, by.x='region', by.y=stateCol, all.x=TRUE)
	hawaii = arrange(hawaii, order)
	hawaii.map = ggplot(hawaii, aes_string(x='long', y='lat', group='group', fill=fillCol)) + geom_polygon(colour='black') + coord_map() + xlab(NULL) + ylab(NULL) + opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank())
	
	legend = usa.map + opts(keep="legend_box")

	Layout = grid.layout(nrow=2, ncol=4, 
		widths=unit(c(.2,.25,1,.2), c("null", "null", "null", "null")), 
		heights=unit(c(.6,.4), c("null", "null")),
		just=c('bottom'))
	grid.newpage()
	pushViewport(viewport(layout=Layout))
	print(usa.map + opts(legend.position="none", panel.grid.major=theme_blank(), panel.background=theme_rect(fill = "white", colour = NA), panel.border=theme_blank()), vp=viewport(layout.pos.row=1:2, layout.pos.col=1:3))
	print(hawaii.map + opts(legend.position="none", plot.background=theme_rect(colour = NA), panel.grid.major=theme_blank(), panel.background=theme_rect(fill = "white", colour = NA), panel.border=theme_blank()), vp=viewport(layout.pos.row=2,layout.pos.col=2))
	print(alaska.map + opts(legend.position="none", plot.background=theme_rect(colour = NA), panel.grid.major=theme_blank(), panel.background=theme_rect(fill = "white", colour = NA), panel.border=theme_blank()), vp=viewport(layout.pos.row=2,layout.pos.col=1))
	print(legend, vp=viewport(layout.pos.row=1:2,layout.pos.col=4))
}


#Source: http://www.edreform.com/download/CER_Charter_Survey_2010.pdf
en = read.xls('Tables/Charter School Enrollment.xls')
names(en); nrow(en)

pdf('Figures/CharterMapEnrollment.pdf', width=8,height=3.5)
stateThematic(en, stateCol='State', fillCol='Total.Enrollment', 'Total Enrollment', breaks=as.vector(quantile(en$Total.Enrollment, seq(0, 1, 1/5))))
dev.off()

pdf('Figures/CharterMapOperating.pdf', width=8,height=3.5)
stateThematic(en, stateCol='State', fillCol='Total.Operating', 'Total Operating\nCharter Schools')
dev.off()
