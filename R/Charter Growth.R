require(ggplot2)

numCharters <- data.frame(
	year=c(1999:2013),
	numCharters=c(507,1524,1651,2009,2337,2632,3062,3472,3840,4220,4624,5043,5453,5714,6187)
)

ggplot(numCharters, aes(x=as.character(year), y=numCharters, label=prettyNum(numCharters, big.mark=','))) + 
	geom_bar(stat='identity', alpha=.7) + 
	geom_text(vjust=-0.5, size=3) +
	ylim(c(0,6500)) +
	xlab('Year') + ylab('Number of Charter Schools')

ggsave('Figures/CharterSchoolGrowth.pdf', width=6.82, height=3.48)
