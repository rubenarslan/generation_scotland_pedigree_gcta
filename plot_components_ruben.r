library(ggplot2)
x <- read.table("stack_plot_with_pers.csv", sep = ",", header = T, skip = 0)
x$Trait = factor(x$Trait, levels = c("g", "Education", "Vocabulary", "Digit symbol", "Verbal fluency","Logical memory", "Neuroticism", "Extraversion"))
x$Source = factor(x$Source, levels = c("Common genetic","Pedigree genetic","Family","Sibling","Couple"))

ggplot(x,aes(x = Trait, y = Variance,fill = Source)) +
    geom_bar(position = "stack",stat = "identity",width =0.5) +
	theme_bw() + theme(axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),  panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
        axis.title.x = element_text(face="bold", colour='black', size=28),
        axis.text.x  = element_text(size=28, colour='black'),
        axis.title.y = element_text(face="bold", colour='black', size=28),
        axis.text.y  = element_text(size=28, colour='black'),
        legend.position=c(0.9, 0.8),
        legend.title = element_text(colour="black", size=30),
        legend.text = element_text(colour="black", size =30),
        plot.title = element_text(face="bold", size=30))+
		theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1, size=30))+
		theme(axis.text.y  = element_text(vjust=1, hjust = 1, size=30))+
		ggtitle("Genetic and environmental contributions to phenotypic variance")+
		labs(x="Phenotypes", y="Variance explained %")+
	scale_fill_manual(values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2"))+
	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete()+
	coord_cartesian(ylim=c(0,100))

x$Trait = factor(x$Trait, levels = rev(c("g", "Education", "Vocabulary", "Digit symbol", "Verbal fluency","Logical memory", "Neuroticism", "Extraversion")))
x$Source = factor(x$Source, levels = rev(c("Common genetic","Pedigree genetic","Family","Sibling","Couple")))
library(dplyr)
y = x %>% bind_rows(x %>% group_by(Trait) %>%
	summarise(Variance = sum(Variance)) %>% mutate(Source = "Total"))
y$Source = factor(y$Source, levels = c("Common genetic","Pedigree genetic","Family","Sibling","Couple", "Total"))

barwidth = 0.7
ggplot(y,aes(x = Trait, y = Variance,fill = Source)) +
	geom_bar(position = position_dodge(width = barwidth),stat = "identity", width =barwidth) +
	geom_linerange(aes(ymin = Variance - Standard.Error, ymax = Variance + Standard.Error), position = position_dodge(width = barwidth),stat = "identity") +
	theme_bw() + theme(axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),  panel.border = element_blank(), panel.grid.major.y = element_blank(),
										 panel.grid.minor.y = element_blank(),
										 axis.title.x = element_text(face="bold", colour='black', size=25),
										 axis.text.x  = element_text(size=28, colour='black'),
										 axis.title.y = element_text(face="bold", colour='black', size=25),
										 axis.text.y  = element_text(size=28, colour='black'),
										 legend.position=c(0.8, 0.2),
										 legend.title = element_text(colour="black", size=25),
										 legend.text = element_text(colour="black", size =25),
										 plot.title = element_text(face="bold", size=25),
										 legend.key.size = unit(0.8,"cm"))+
	ggtitle("Genetic and environmental contributions to phenotypic variance")+
	labs(x="Phenotypes", y="Variance explained %")+
	scale_fill_manual(values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2", "Total" = "black"))+
	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete()+
	coord_flip(ylim = c(0,100))

ggsave(filename = "ind_components.pdf", width = 15, height = 15)

library(formr)
x %>% group_by(Trait) %>%
									filter(Source %contains% "genetic") %>%
									summarise(Variance = sum(Variance)) %>% mutate(Source = "Total")

x %>% group_by(Trait) %>%
	filter(!Source %contains% "Couple") %>%
	summarise(Variance = sum(Variance)) %>% mutate(Source = "Gen")


library(ggplot2)
x <- read.table("stack_plot_with_pers.csv", sep = ",", header = T, skip = 0)
x$Trait = factor(x$Trait, levels = c("g", "Education", "Vocabulary", "Digit symbol", "Verbal fluency","Logical memory", "Neuroticism", "Extraversion"))
x$Source = factor(x$Source, levels = c("Common genetic","Pedigree genetic","Family","Sibling","Couple"))
x$Trait = factor(x$Trait, levels = rev(c("g", "Education", "Vocabulary", "Digit symbol", "Verbal fluency","Logical memory", "Neuroticism", "Extraversion")))
x$Source = factor(x$Source, levels = rev(c("Common genetic","Pedigree genetic","Family","Sibling","Couple")))

barwidth = 0.7
ggplot(x,aes(x = Trait, y = Variance,fill = Source)) +
	geom_bar(position = position_stack(), stat = "identity", width = barwidth, alpha = 0.5) +
	geom_bar(position = position_dodge(width = barwidth),stat = "identity", width = barwidth) +
	geom_linerange(aes(ymin = Variance - Standard.Error, ymax = Variance + Standard.Error),
								 position = position_dodge(width = barwidth),stat = "identity") +
	theme_bw() + theme(axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),  panel.border = element_blank(), panel.grid.major.y = element_blank(),
										 panel.grid.minor.y = element_blank(),
										 axis.title.x = element_text(face="bold", colour='black', size=25),
										 axis.text.x  = element_text(size=28, colour='black'),
										 axis.title.y = element_text(face="bold", colour='black', size=25),
										 axis.text.y  = element_text(size=28, colour='black'),
										 legend.position=c(0.8, 0.2),
										 legend.title = element_text(colour="black", size=25),
										 legend.text = element_text(colour="black", size =25),
										 plot.title = element_text(face="bold", size=25),
										 legend.key.size = unit(0.8,"cm"))+
	ggtitle("Genetic and environmental contributions to phenotypic variance")+
	labs(x="Phenotypes", y="Variance explained %")+
	scale_fill_manual(values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2", "Total" = "black"))+
	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete()+
	coord_flip(ylim = c(0,100))

ggsave(filename = "ind_components_stack.pdf", width = 15, height = 15)

