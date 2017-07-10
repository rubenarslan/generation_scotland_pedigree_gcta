library(dplyr)
library(ggplot2)
x <- readr::read_csv("data/stack_plot_with_pers.csv")
x$Trait = factor(recode(x$Trait,
			"g" = "italic(g)",
			"Digit symbol" = "Digit~Symbol~Test",
			"Education" = "Education",
			"Vocabulary" = "Vocabulary",
			"Verbal fluency" = "Verbal~Fluency",
			"Logical memory" = "Logical~Memory",
			"Neuroticism" = "Neuroticism",
			"Extraversion" = "Extraversion"), levels = c("italic(g)", "Education", "Vocabulary", "Digit~Symbol~Test", "Verbal~Fluency","Logical~Memory", "Neuroticism", "Extraversion"))
x$Source = factor(x$Source, levels = rev(c("Pedigree genetic", "Common genetic","Family","Sibling","Couple")))
x$Phenotypes = x$Source
y = x
y$Phenotypes = "Total"
y$Standard.Error = NA
z = bind_rows(x, y %>% filter(Source %in% c("Common genetic", "Pedigree genetic")))
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
	panel.border = element_blank(),
	panel.background = element_rect(fill = "#FEFEFE"),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
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



simple_facets = theme(
	strip.background = element_blank(),
	strip.text.y = element_text(colour='black', size = 15, angle = 180, hjust = 1),
	strip.placement = "outside",
	strip.switch.pad.wrap	= unit(3, "mm"),
	panel.spacing.y = unit(14, "mm"),
	panel.spacing.x = unit(0, "mm"),
	panel.grid.major.x = element_blank(),
	axis.line.x = element_line(colour = "black"),
	axis.line.y = element_line(colour = "black"),
	panel.border = element_blank(),
	panel.background = element_rect(fill = "#FEFEFE"),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	axis.title.x = element_text(face="bold", colour='black', size = 20),
	axis.title.y = element_text(face="bold", colour='black', size = 20),
	axis.text.x  = element_text(size = 20, colour='black'),
	axis.text.y  = element_text(size = 14, colour='black'),
	legend.position=c(0.87, 0.125),
	legend.title = element_text(colour="black", size=20),
	legend.text = element_text(colour="black", size =16),
	plot.title = element_text(face="bold", size=22),
	legend.key.size = unit(0.6,"cm"))

z$Phenotype_short = recode(z$Phenotypes,
		"Pedigree genetic" = "K",
		"Common genetic" = "G",
		"Couple" = "C",
		"Family" = "F",
		"Sibling" = "S",
		"Total" = "Total")

stack_dodge = ggplot(z %>% filter(Trait %in% c("italic(g)", "Education", "Neuroticism", "Extraversion")),aes(x = Phenotype_short, y = Variance,fill = Source)) +
	geom_col(width = barwidth, alpha = 1) +
	geom_linerange(aes(ymin = Variance - Standard.Error, ymax = Variance + Standard.Error),
								 position = position_dodge(width = barwidth),stat = "identity", na.rm = T) +
	facet_wrap(~ Trait, strip.position = "left", ncol = 1, scales = "free_y", drop = T, labeller = label_parsed) +
	theme_bw() +
	simple_facets +
	ggtitle("Genetic and environmental contributions to phenotypic variance in GREML-KIN") +
	scale_fill_manual("Variance components", values=c("Common genetic" = "red2", "Pedigree genetic" = "red4", "Family" = "blue4", "Sibling" = "lightskyblue",  "Couple" = "blue2"), breaks =
c("Common genetic", "Pedigree genetic", "Family", "Sibling", "Couple") ) +
	scale_y_continuous("Variance explained %", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
	scale_x_discrete("") +
	coord_flip(ylim = c(3.5,89), expand = T)
stack_dodge

ggsave(stack_dodge, filename = "plots/Figure 1.pdf", width = 17.8, height = 17.8*0.625)

library(formr)
x %>% group_by(Trait) %>%
									filter(Source %contains% "genetic") %>%
									summarise(Variance = sum(Variance)) %>% mutate(Source = "Total")

x %>% group_by(Trait) %>%
	filter(!Source %contains% "Couple") %>%
	summarise(Variance = sum(Variance)) %>% mutate(Source = "Gen")

library(dplyr)
library(ggplot2)
bins <- readxl::read_xlsx("data/6bins.xlsx", 2)
x = bins %>% filter(Bin != "Total")
x$trait_old = x$Trait
x$Trait = factor(recode(x$Trait,
												"g" = "italic(g)",
												"digit" = "Digit~Symbol~Test",
												"Education" = "Education",
												"vocabulary" = "Vocabulary",
												"verbal_total" = "Verbal~Fluency",
												"TOTAL_LM" = "Logical~Memory",
												"eysenck_N" = "Neuroticism",
												"eysenck_E" = "Extraversion"), levels = c("italic(g)", "Education", "Vocabulary", "Digit~Symbol~Test", "Verbal~Fluency","Logical~Memory", "Neuroticism", "Extraversion"))
x$Variance = x$Estimate * 100
x$Standard.Error = x$S.E * 100
x$Bin = stringr::str_sub(x$Bin, 4)

x$Phenotypes = factor(paste0(x$Bin, " (", x$SNPs %>% format(big.mark = " ") %>% stringr::str_trim(), " SNPs)"), rev(c("0.001-0.01 (3 898 626 SNPs)", "0.01-0.1 (3 320 146 SNPs)", "0.1-0.2 (1 413 929 SNPs)",
																																																											"0.2-0.3 (1 061 603 SNPs)", "0.3-0.4 (930 841 SNPs)", "0.4-0.5 (872 346 SNPs)"
)))
x$Source = x$Phenotypes


method = "ASReml-R mixed model"
#### marioni
marioni = bind_rows(
	data.frame(
						Phenotypes = c(method, method),
						Trait = c("italic(g)", "Education"),
					 Variance = c(54, 41),
					 Standard.Error = c(2, 2),
						Source = c("Family-based pedigree", "Family-based pedigree")),
z %>% filter(Trait %in% c("italic(g)", "Education"), Source %in% c("Common genetic", "Pedigree genetic"), Phenotypes == "Total") %>% mutate(Source = recode(Source, "Common genetic" = "GREML-KIN G", "Pedigree genetic" = "GREML-KIN K"),
																																																																						Phenotypes = "GREML-KIN"																				),
						x %>% filter(Trait %in% c("italic(g)", "Education")) %>% mutate(																											Phenotypes = "GREML-MS",
																					Source = paste0("GREML-MS ", Bin)																														 )
)


# marioni
marioni$Trait = factor(marioni$Trait, levels = c("italic(g)", "Education"))
marioni$Source = factor(marioni$Source, levels = rev(c("Family-based pedigree", paste0("GREML-MS ", c("0.001-0.01", 	"0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5")), "GREML-KIN K", "GREML-KIN G")))
marioni$Phenotypes = factor(marioni$Phenotypes, levels = c( method, "GREML-KIN", "GREML-MS"))

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
		legend.position=c(0.9, 0.88),
		legend.title = element_text(colour="black", size=20),
		legend.text = element_text(colour="black", size =14),
		plot.title = element_text(face="bold", size=22),
		legend.key.size = unit(0.5,"cm")) +
	ggtitle("Comparison between molecular genetic and family-based pedigree estimates")+
	scale_fill_manual("Relatedness information", values=c("GREML-KIN G" = "red2", "GREML-KIN K" = "red4", "Family-based pedigree" = "darkgreen",
																			 "GREML-MS 0.001-0.01"  = "#08306B",
																			 "GREML-MS 0.01-0.1" = "#08519C",
																			 "GREML-MS 0.1-0.2" = "#2171B5",
																			 "GREML-MS 0.2-0.3"  = "#4292C6",
																			 "GREML-MS 0.3-0.4"  = "#6BAED6",
																			 "GREML-MS 0.4-0.5"  = "#9ECAE1"),
										breaks = c("Family-based pedigree", "GREML-KIN G", "GREML-KIN K",
															 "GREML-MS 0.4-0.5", "GREML-MS 0.3-0.4","GREML-MS 0.2-0.3", "GREML-MS 0.1-0.2", "GREML-MS 0.01-0.1", "GREML-MS 0.001-0.01")) +
	scale_y_continuous("Variance explained %", breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete("Method")
comp_pedi

ggsave(comp_pedi, filename = "plots/comp_to_marioni.pdf", width = 17.8, height = 17.8*0.625)
