library(dplyr)
library(ggplot2)
bins <- readxl::read_xlsx("data/6bins.xlsx", 1)
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

x$Bin_thresh = as.numeric(stringr::str_split_fixed(x$Bin, "-", 2)[,2])
#ymin = if_else(Variance - Standard.Error < 0, 0, Variance - Standard.Error)/100, ymax = (Variance + Standard.Error)/100
x = x %>% arrange(Trait, Bin_thresh)
by_thresh = x %>%
	mutate(
		Bin_thresh = if_else(Bin_thresh == 0.01, 0.1, Bin_thresh)) %>%
	group_by(trait_old, Trait, Bin_thresh) %>%
	summarise(Variance = sum(Variance), Standard.Error = mean(Standard.Error)) %>%
	group_by(Trait) %>% arrange(Trait, Bin_thresh) %>%
	mutate(Variance = Variance/100,
				 Standard.Error = Standard.Error/100,
				 Var_cum = cumsum(Variance)/sum(Variance))

traits = unique(by_thresh$trait_old)
by_thresh$se_cum = NA
for (i in seq_along(traits)) {
	trait = traits[i]
	cov = readr::read_tsv(paste0("data/",trait, ".tab"), col_names = c(paste0("G",1:6), "E", "rm")) %>%
		select(starts_with("G")) %>%
		slice(1:6) %>%
		as.matrix()
	by_thresh[by_thresh$trait_old == trait & by_thresh$Bin_thresh == 0.1, "se_cum"] =
	msm::deltamethod(
		~((x1+x2)/(x1+x2+x3+x4+x5+x6)),
		x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	by_thresh[by_thresh$trait_old == trait & by_thresh$Bin_thresh == 0.2, "se_cum"] =
		msm::deltamethod(
			~((x1+x2+x3)/(x1+x2+x3+x4+x5+x6)),
			x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	by_thresh[by_thresh$trait_old == trait & by_thresh$Bin_thresh == 0.3, "se_cum"] =
		msm::deltamethod(
			~((x1+x2+x3+x4)/(x1+x2+x3+x4+x5+x6)),
			x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	by_thresh[by_thresh$trait_old == trait & by_thresh$Bin_thresh == 0.4, "se_cum"] =
		msm::deltamethod(
			~((x1+x2+x3+x4+x5)/(x1+x2+x3+x4+x5+x6)),
			x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	by_thresh[by_thresh$trait_old == trait & by_thresh$Bin_thresh == 0.5, "se_cum"] =
		msm::deltamethod(
			~((x1+x2+x3+x4+x5+x6)/(x1+x2+x3+x4+x5+x6)),
			x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)

	vars = x %>% filter(trait_old == trait) %>% .[["Variance"]]/100
	maf01 = sum(vars[1:2])
	se_maf01 =
	msm::deltamethod(
		~((x1+x2)/(x1+x2+x3+x4+x5+x6)),
		x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	maf05 = vars[6]
	se_maf05 = msm::deltamethod(
		~((x6)/(x1+x2+x3+x4+x5+x6)),
		x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	se_diff = msm::deltamethod(
		~((x1+x2-x6)/(x1+x2+x3+x4+x5+x6)),
		x %>% filter(trait_old == trait) %>% .[["Variance"]]/100, cov, ses = TRUE)
	(maf01-maf05)/se_diff
}

y = x
y$Bin = "Total"
y$Standard.Error = NA
z = bind_rows(x, y)
z$Phenotypes = factor(z$Bin, rev(c("0.001-0.01", "0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "Total")))

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

blank_facets2 = theme(
	strip.text.y = element_text(colour='black', size = 25),
	panel.grid.major.x = element_blank(),
	axis.line.x = element_line(colour = "black"),
	axis.line.y = element_line(colour = "black"),
	panel.background = element_rect(fill = "#FEFEFE", color = "#999999"),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	axis.title.x = element_text(face="bold", colour='black', size=15),
	axis.title.y = element_text(face="bold", colour='black', size=15),
	axis.text  = element_text(size=10, colour='black'),
	plot.title = element_text(face="bold", size=22),
	legend.key.size = unit(0.6,"cm"))

thresh_plot = ggplot(by_thresh,
										 aes(Bin_thresh, Var_cum, ymin = Var_cum - se_cum, ymax = Var_cum + se_cum)) +
	geom_point(position = position_dodge(width = 0.02)) +
	geom_linerange(position = position_dodge(width = 0.02)) +
	geom_abline(slope = 2, intercept = 0, color = "#888888") +
	blank_facets2 +
	facet_wrap(~ Trait, labeller = label_parsed) +
	scale_y_continuous("Cumulative contribution to genetic variance", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
	scale_x_continuous("MAF threshold", breaks = c(0.1, 0.2, 0.3, 0.4, 0.5)) +
	coord_cartesian(ylim=c(0,1), xlim = c(0,0.55))
thresh_plot
ggsave(thresh_plot, filename = "thresh_plot.pdf", width = 9.8, height = 9.8*0.625)


thresh_plot = ggplot(by_thresh %>% filter(trait_old == 'g'),
										 aes(Bin_thresh, Var_cum, ymin = Var_cum - se_cum, ymax = Var_cum + se_cum)) +
	geom_point(position = position_dodge(width = 0.02)) +
	geom_linerange(position = position_dodge(width = 0.02)) +
	geom_segment(x = 0, y = 0, yend = 1, xend = 0.5, color = "#888888") +
	blank_facets2 +
	facet_wrap(~ Trait, labeller = label_parsed) +
	scale_y_continuous("Cumulative contribution to genetic variance", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
	scale_x_continuous("MAF threshold", breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
	coord_cartesian(ylim=c(0,1), xlim = c(0,0.55))
thresh_plot
ggsave(thresh_plot, filename = "thresh_plot_g.pdf", width = 7.5, height = 7.5*0.625)


barwidth = 0.7
stack_dodge = ggplot(z,aes(x = Phenotypes, y = Variance,fill = Source)) +
	geom_col(width = barwidth, alpha = 1) +
	geom_linerange(aes(ymin = if_else(Variance - Standard.Error < 0, 0, Variance - Standard.Error), ymax = Variance + Standard.Error),
								 position = position_dodge(width = barwidth),stat = "identity") +
	facet_wrap(~ Trait, strip.position = "left", ncol = 1, scales = "free_y", drop = T, labeller = label_parsed) +
	theme_bw() +
	blank_facets +
	ggtitle("Genetic contributions to phenotypic variance by minor allele frequency (MAF)") +
	# scale_fill_brewer("MAF bin", palette = 1, direction = 1) +
	scale_fill_manual("MAF bin", values = c(
	"0.001-0.01 (3 898 626 SNPs)"  = "#08306B",
	"0.01-0.1 (3 320 146 SNPs)" = "#08519C",
	"0.1-0.2 (1 413 929 SNPs)" = "#2171B5",
	"0.2-0.3 (1 061 603 SNPs)"  = "#4292C6",
	"0.3-0.4 (930 841 SNPs)"  = "#6BAED6",
	"0.4-0.5 (872 346 SNPs)"  = "#9ECAE1"))	+
	scale_y_continuous("Variance explained %", breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete("")+
	coord_flip()
stack_dodge

ggsave(stack_dodge, filename = "maf_bins_nogridlines.pdf", width = 17.8, height = 17.8*0.625)

pas = function(x) {parse(text=x) }
stack_dodge2 = ggplot(x,aes(x = Trait, y = Variance,fill = Phenotypes)) +
	geom_col(width = barwidth, alpha = 1) +
	facet_wrap(~ Bin, strip.position = "left", ncol = 1, scales = "free_y", drop = T) +
	theme_bw() +
	ggtitle("Genetic and environmental contributions to phenotypic variance") +
	# scale_fill_brewer("MAF bin", palette = 1, direction = 1) +
	scale_fill_manual("MAF bin", values = c(
		"0.001-0.01 (3 898 626 SNPs)"  = "#08306B",
		"0.01-0.1 (3 320 146 SNPs)" = "#08519C",
		"0.1-0.2 (1 413 929 SNPs)" = "#2171B5",
		"0.2-0.3 (1 061 603 SNPs)"  = "#4292C6",
		"0.3-0.4 (930 841 SNPs)"  = "#6BAED6",
		"0.4-0.5 (872 346 SNPs)"  = "#9ECAE1"))	+
	scale_y_continuous("Variance explained %")+
	scale_x_discrete("", labels = pas)+
	coord_flip()
stack_dodge2
ggsave(stack_dodge2, filename = "mafs.pdf", width = 17.8, height = 17.8*0.625)


z$Phenotypes = factor(z$Bin, c("0.001-0.01", "0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "Total"))
stack_dodge = ggplot(z %>% filter(trait_old == 'g'),aes(x = Phenotypes, y = Variance,fill = Source)) +
	geom_col(width = barwidth, alpha = 1) +
	geom_linerange(aes(ymin = if_else(Variance - Standard.Error < 0, 0, Variance - Standard.Error), ymax = Variance + Standard.Error),
								 position = position_dodge(width = barwidth),stat = "identity") +
	theme_bw() +
	blank_facets2 +
	ggtitle("Genetic contributions by minor allele frequency (MAF)") +
	# scale_fill_brewer("MAF bin", palette = 1, direction = 1) +
	scale_fill_manual("MAF bin", values = c(
		"0.001-0.01 (3 898 626 SNPs)"  = "#08306B",
		"0.01-0.1 (3 320 146 SNPs)" = "#08519C",
		"0.1-0.2 (1 413 929 SNPs)" = "#2171B5",
		"0.2-0.3 (1 061 603 SNPs)"  = "#4292C6",
		"0.3-0.4 (930 841 SNPs)"  = "#6BAED6",
		"0.4-0.5 (872 346 SNPs)"  = "#9ECAE1"))	+
	scale_y_continuous("Variance explained %", breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_x_discrete("") +
	theme(	legend.position=c(0.5, 0.5))
stack_dodge
ggsave(stack_dodge, filename = "maf_g.pdf", width = 10, height = 10*0.625)

saveRDS(z, file="data/gremlms.rds")
