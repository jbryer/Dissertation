require(multilevelPSA)

#' Loess plot with density distributions for propensity scores and outcomes on
#' top and right, respectively.
#'
#' @param ps vector of propensity scores.
#' @param response the response variable.
#' @param treatment the treatment varaible as a logical type.
#' @param percentPoints.treat the percentage of treatment points to randomly plot.
#' @param percentPoints.control the percentage of control points to randomly plot.
#' @param responseTitle the label to use for the y-axis (i.e. the name of the response variable)
#' @param treatmentTitle the label to use for the treatment legend.
#' @return a ggplot2 figure
#' @export
plot.loess <- function(ps, response, treatment, responseTitle='', 
					   treatmentTitle='Treatment',
					   percentPoints.treat=.1, percentPoints.control=.01) {
	df = data.frame(ps=ps, response=response, treatment=treatment)
	df.points.treat = df[sample(which(df$treatment), 
					round(percentPoints.treat * length(which(df$treatment))), replace=FALSE),]
	df.points.control =  df[sample(which(!df$treatment), 
					round(percentPoints.control * length(which(!df$treatment))), replace=FALSE),]
	pmain = ggplot(df, aes(x=ps, y=response, colour=treatment))
	pmain = pmain + geom_point(data=df.points.treat, 
							   aes(x=ps, y=response, colour=treatment), alpha=.2)
	pmain = pmain + geom_point(data=df.points.control, 
							   aes(x=ps, y=response, colour=treatment), alpha=.2)
	pmain = pmain + geom_smooth() + ylab(responseTitle) + xlab("Propensity Score") + 
				opts(legend.position=c(.8,.8), legend.justification='left') + 
				scale_colour_hue(treatmentTitle)
	ptop = ggplot(df, aes(x=ps, colour=treatment)) + geom_density() + 
				opts(legend.position=c(-1,-1)) + xlab(NULL) + ylab('Density')
	pright = ggplot(df, aes(x=response, colour=treatment)) + geom_density() + coord_flip() + 
				opts(legend.position='none', axis.text.x=theme_text(size=0)) + 
				xlab(NULL) + ylab('Density')
	grid_layout <- grid.layout(nrow=2, ncol=2, widths=c(3,1), heights=c(1,3))
	grid.newpage()
	pushViewport( viewport( layout=grid_layout ) )
	multilevelPSA:::align.plots(grid_layout, 
								list(ptop, 1, 1), 
								list(pmain, 2, 1), 
								list(pright, 2, 2))
}
