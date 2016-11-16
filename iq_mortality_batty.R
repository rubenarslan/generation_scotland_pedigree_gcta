
IQMORTALITY = data.frame(
	iq = 1:9,
	inci = c(812, 1538, 1971, 2516, 2853, 2148, 1478, 824, 358),
total = c(28766, 66975, 103402, 150780, 219147, 171591, 129719, 80348, 43534))

library(ggplot2)
ggplot(IQMORTALITY, aes(iq, inci/total)) + geom_point() + scale_x_continuous(breaks = 1:9)
