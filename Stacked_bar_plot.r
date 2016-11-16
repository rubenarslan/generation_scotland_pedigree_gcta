x <- read.table("File_location\\stack_plot.csv", sep = ",", header = T, skip = 0)

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
		scale_fill_manual(values=c("red2", "blue4", "lightskyblue", "red4", "blue2"), breaks = c("Common genetic","Pedigree genetic","Family","Sibling","Couple"))+
	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete(limits=c("g","Vocabulary", "Education", "Digit symbol", "Executive functions","Logical memory"))+
	coord_cartesian(ylim=c(0,100))
	
	