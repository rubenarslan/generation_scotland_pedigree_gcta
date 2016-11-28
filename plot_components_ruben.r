library(dplyr)
library(ggplot2)
x <- readr::read_csv("stack_plot_with_pers.csv")
x$Trait = factor(recode(x$Trait,
			"g" = "italic(g)",
			"Digit symbol" = "Digit~Symbol~Test",
			"Education" = "Education",
			"Vocabulary" = "Vocabulary",
			"Verbal fluency" = "Verbal~Fluency",
			"Logical memory" = "Logical~Memory",
			"Neuroticism" = "Neuroticism",
			"Extraversion" = "Extraversion"), levels = c("italic(g)", "Education", "Vocabulary", "Digit~Symbol~Test", "Verbal~Fluency","Logical~Memory", "Neuroticism", "Extraversion"))
x$Source = factor(x$Source, levels = rev(c("Common genetic","Pedigree genetic","Family","Sibling","Couple")))
x$Phenotypes = x$Source
y = x
y$Phenotypes = "Total"
y$Standard.Error = NA
z = bind_rows(x, y)
z$Phenotypes = factor(z$Phenotypes, rev(c("Common genetic","Pedigree genetic","Family","Sibling","Couple","Total")))
str(y$Phenotypes)

blank_facets = theme(
	strip.background = element_blank(),
	strip.text.y = element_text(colour='black', size = 15, angle = 180, hjust = 1),
	strip.placement = "outside",
	strip.switch.pad.wrap	= unit(10, "mm"),
	axis.text.y = element_blank(),
	axis.ticks.length = unit(0, "mm"),
	panel.spacing.y = unit(4, "mm"),
	panel.spacing.x = unit(0, "mm"),
	panel.grid.major.x = element_blank(),
	axis.line.x = element_line(colour = "black"),
	axis.line.y = element_line(colour = "black"),
	panel.border = element_rect(colour = "#EEEEEE"),
	panel.grid.major.y = element_blank(),
	panel.grid.minor.y = element_blank(),
	axis.title.x = element_text(face="bold", colour='black', size=20),
	axis.title.y = element_text(face="bold", colour='black', size=20),
	axis.text.x  = element_text(size=20, colour='black'),
	legend.position=c(0.87, 0.125),
	legend.title = element_text(colour="black", size=20),
	legend.text = element_text(colour="black", size =16),
	plot.title = element_text(face="bold", size=22),
	legend.key.size = unit(0.6,"cm"))

barwidth = 0.7
stack_dodge = ggplot(z,aes(x = Phenotypes, y = Variance,fill = Source)) +
	geom_col(width = barwidth, alpha = 1) +
	geom_linerange(aes(ymin = Variance - Standard.Error, ymax = Variance + Standard.Error),
								 position = position_dodge(width = barwidth),stat = "identity") +
	facet_wrap(~ Trait, strip.position = "left", ncol = 1, scales = "free_y", drop = T, labeller = label_parsed) +
	theme_bw() +
	blank_facets +
	ggtitle("Genetic and environmental contributions to phenotypic variance")+
	scale_fill_manual("Variance components", values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2", "Total" = "black"))+
	scale_y_continuous("Variance explained %", breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete("")+
	coord_flip()
stack_dodge

ggsave(stack_dodge, filename = "ind_components_stack.pdf", width = 15, height = 15)

library(formr)
x %>% group_by(Trait) %>%
									filter(Source %contains% "genetic") %>%
									summarise(Variance = sum(Variance)) %>% mutate(Source = "Total")

x %>% group_by(Trait) %>%
	filter(!Source %contains% "Couple") %>%
	summarise(Variance = sum(Variance)) %>% mutate(Source = "Gen")


#
# barwidth = 0.7
# stack_dodge = ggplot(z,aes(x = Phenotypes, y = Variance,fill = Source)) +
# 	geom_col(width = barwidth, alpha = 0.7) +
# 	geom_linerange(aes(ymin = Variance - Standard.Error, ymax = Variance + Standard.Error),
# 								 position = position_dodge(width = barwidth),stat = "identity") +
# 	facet_wrap(~ Trait, strip.position = "left", ncol = 1) +
# 	theme_bw() +
# 	blankfacets +
# 	ggtitle("Genetic and environmental contributions to phenotypic variance")+
# 	scale_fill_manual("Variance components", values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2", "Total" = "black"))+
# 	scale_y_continuous("Variance explained %", breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
# 	scale_x_discrete()+
# 	coord_flip()





#### marioni
marioni = bind_rows(
	data.frame(
						Phenotypes = c("Family", "Family"),
						Trait = c("italic(g)", "Education"),
					 Variance = c(54, 41),
					 Standard.Error = c(2, 2),
						Source = c("ASReml-R", "ASReml-R")),
z %>% filter(Trait %in% c("italic(g)", "Education"), Source %in% c("Common genetic", "Pedigree genetic"), Phenotypes == "Total") %>% mutate(Source = recode(Source, "Common genetic" = "GRMg GREML", "Pedigree genetic" = "GRMkin GREML"),
																																																																						Phenotypes = "Molecular Genetic"																				)
)


marioni
marioni$Trait = factor(marioni$Trait, levels = c("italic(g)", "Education"))
marioni$Source = factor(marioni$Source, levels = rev(c("ASReml-R", "GRMg GREML", "GRMkin GREML")))
marioni$Phenotypes = factor(marioni$Phenotypes, levels = c( "Molecular Genetic", "Family"))

comp_pedi = ggplot(marioni,aes(x = Phenotypes, y = Variance,fill = Source)) +
	geom_col(width = barwidth, alpha = 1) +
	facet_wrap(~ Trait, strip.position = "top", nrow = 1,  drop = T, labeller = label_parsed) +
	theme_bw() +
	theme(
		strip.background = element_blank(),
		strip.text = element_text(colour='black', size = 20),
		strip.placement = "outside",
		strip.switch.pad.wrap	= unit(10, "mm"),
		panel.spacing.y = unit(4, "mm"),
		panel.spacing.x = unit(0, "mm"),
		panel.grid.major.x = element_blank(),
		axis.line.x = element_line(colour = "black"),
		axis.line.y = element_line(colour = "black"),
		panel.border = element_rect(colour = "#EEEEEE"),
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank(),
		axis.title = element_text(face="bold", colour='black', size=20),
		axis.text  = element_text(size=20, colour='black'),
		legend.position=c(0.87, 0.9),
		legend.title = element_text(colour="black", size=20),
		legend.text = element_text(colour="black", size =15),
		plot.title = element_text(face="bold", size=22),
		legend.key.size = unit(0.6,"cm")) +
	ggtitle("Comparison to the traditional pedigree estimate")+
	scale_fill_manual("Method", values=c("GRMg GREML" = "red2", "GRMkin GREML" = "red4", "ASReml-R" = "blue"))+
	scale_y_continuous("Variance explained %", breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete("Data used to derive estimate of relatedness")
comp_pedi

ggsave(comp_pedi, filename = "comp_to_marioni.pdf", width = 15, height = 10)



#
# stack_dodge + scale_fill_brewer("Phenotypes", palette = 1)
# stack_dodge + scale_fill_manual("Phenotypes", values=
# 		c(
# 			"Common genetic" = "black",
# 			"Pedigree genetic" = "#9F3B2B",
# 			"Family" = "#8A846C",
# 			"Sibling" = "#21523B",
# 			"Couple" = "#E2A674"
# 		)
# )
#
# ggplot(x,aes(x = Trait, y = Variance,fill = Source)) +
#     geom_bar(position = "stack",stat = "identity",width =0.5) +
# 	theme_bw() + theme(axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),  panel.border = element_blank(), panel.grid.major = element_blank(),
# panel.grid.minor = element_blank(),
#         axis.title.x = element_text(face="bold", colour='black', size=28),
#         axis.text.x  = element_text(size=28, colour='black'),
#         axis.title.y = element_text(face="bold", colour='black', size=28),
#         axis.text.y  = element_text(size=28, colour='black'),
#         legend.position=c(0.9, 0.8),
#         legend.title = element_text(colour="black", size=30),
#         legend.text = element_text(colour="black", size =30),
#         plot.title = element_text(face="bold", size=30))+
# 		theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1, size=30))+
# 		theme(axis.text.y  = element_text(vjust=1, hjust = 1, size=30))+
# 		ggtitle("Genetic and environmental contributions to phenotypic variance")+
# 		labs(x="Phenotypes", y="Variance explained %")+
# 	scale_fill_manual(values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2"))+
# 	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
# 	scale_x_discrete()+
# 	coord_cartesian(ylim=c(0,100))

# x$Source = factor(x$Source, levels = rev(c("Common genetic","Pedigree genetic","Family","Sibling","Couple")))
# y = x %>% bind_rows(x %>% group_by(Trait) %>%
# 	summarise(Variance = sum(Variance)) %>% mutate(Source = "Total"))
# y$Source = factor(y$Source, levels = c("Common genetic","Pedigree genetic","Family","Sibling","Couple", "Total"))

# barwidth = 0.7
# ggplot(y,aes(x = Trait, y = Variance,fill = Source)) +
# 	geom_bar(position = position_dodge(width = barwidth),stat = "identity", width =barwidth) +
# 	geom_linerange(aes(ymin = Variance - Standard.Error, ymax = Variance + Standard.Error), position = position_dodge(width = barwidth),stat = "identity") +
# 	theme_bw() + theme(axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),  panel.border = element_blank(), panel.grid.major.y = element_blank(),
# 										 panel.grid.minor.y = element_blank(),
# 										 axis.title.x = element_text(face="bold", colour='black', size=25),
# 										 axis.text.x  = element_text(size=28, colour='black'),
# 										 axis.title.y = element_text(face="bold", colour='black', size=25),
# 										 axis.text.y  = element_text(size=28, colour='black'),
# 										 legend.position=c(0.8, 0.2),
# 										 legend.title = element_text(colour="black", size=25),
# 										 legend.text = element_text(colour="black", size =25),
# 										 plot.title = element_text(face="bold", size=25),
# 										 legend.key.size = unit(0.8,"cm"))+
# 	ggtitle("Genetic and environmental contributions to phenotypic variance")+
# 	labs(x="Phenotypes", y="Variance explained %")+
# 	scale_fill_manual(values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2", "Total" = "black"))+
# 	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
# 	scale_x_discrete()+
# 	coord_flip(ylim = c(0,100))
#
# ggsave(filename = "ind_components.pdf", width = 15, height = 15)
