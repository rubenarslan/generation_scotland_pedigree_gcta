library(formr)
library(ggplot2)
library(dplyr)
library(pander)
library(readr)
library(stringr)
library(tidyr)
library(openxlsx)
load("data/models.rdata")

str(models)
models_long = models %>% gather(key, value)

comps = c("G", "K", "F", "S", "C", "V(e)", "V(p)", "Vp",
	"G%", "K%", "F%", "S%", "C%",
	"logL", "n")
comp_var_order = paste0(rep(comps, each = 2),"_", c("Variance", "SE"))

model_types = c("Full",
					 "G", "K", "F", "S", "C",
					 "GK", "GF", "GS", "GC",
					 "KF",  "KS", "KC",
					 "FS", "FC",
					 "SC",
					 "GFC", "GFS", "GKC",
					 "GKF", "GKS", "GSC",
					 "KFC", "KFS", "KSC",
					 "FSC",
					 "GFSC", "GKFC", "GKFS", "GKSC", "KFSC")
all_model_tables = list()
for(i in 1:length(model_types)) {
	model = model_types[i]
	tmp = models %>%
		filter_(paste0("model == '", model,"'"))
	if(nrow(tmp) > 0) {
	all_model_tables[[model]] = tmp %>%
		select(-model) %>%
		unite(trait, domain, aspect, sep = ": ") %>%
		mutate(Variance = format(Variance, digits = 2, nsmall = 2),
					 SE = ifelse(is.na(SE), NA_character_,
					 				str_c("(", format(SE, digits = 2, nsmall = 2),")"))) %>%
		gather(key, value, -trait, -Component) %>%
		unite(Component_var, Component, key, sep = "_") %>%
		mutate(
			trait = factor(trait, levels = c("Cognitive: g", "Cognitive: Education", "Cognitive: Vocabulary", "Cognitive: Digit symbol", "Cognitive: Verbal fluency","Cognitive: Logical memory", "Personality: Neuroticism", "Personality: Extraversion")),
			Component_var = factor(Component_var, levels = comp_var_order)) %>%
		spread(Component_var, value) %>%
		select(-logL_SE, -n_SE) %>%
		rename(logL = logL_Variance, n = n_Variance)
	}
}
workbook = write.xlsx(all_model_tables, file = "model_tables.xlsx", asTable = T, overwrite = T)
for (sheetnr in 1:length(all_model_tables)) {
	setColWidths(workbook, sheet = sheetnr, cols = 1:100, widths = "auto")
	setHeaderFooter(workbook, sheet = sheetnr,
		header = c("","Results of variance component analyses using all models on the GS20K", ""))
}
saveWorkbook(workbook, "data/model_tables.xlsx", overwrite = TRUE)
