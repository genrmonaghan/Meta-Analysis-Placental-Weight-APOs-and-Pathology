library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(meta)
library(metafor)
library(tidyr)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")


# View the default settings
default_settings <- settings.meta()
print(default_settings)
settings.meta()
settings.meta("reset")


SRS_PE_PW <- SRS_PE %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% arrange(GA_mean_case)
SRS_PE_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                    sd2i = PW_sd_control, sd1i = PW_sd_case,
                    n2i = Control_N, n1i = Case_N, data = SRS_PE_PW)
SRS_PE_PW <- SRS_PE_PW[!is.na(SRS_PE_PW$vi),]
head(SRS_PE_PW)
dim(SRS_PE_PW)
MA_PE_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                     n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                     studlab = study_id, data = SRS_PE_PW )
summary(MA_PE_PW)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("PE PW.png", width = 6000, height = 13000, res = 450)
{
  forest(MA_PE_PW,
        leftcols = c("study_id", "Country","GA_mean_case","GA_mean_control", "GA_mean_all"),
        leftlabs = c("Country", "Gestational Age \n(Cases)", "Gestational Age \n(Control)", "Gestational Age \n(Overall)"),
        xlab = "Placental weight (g)",
        print.tau2 = FALSE,
        print.Q = FALSE,
        print.pval.Q = TRUE,
        print.I2 = TRUE,
        print.pval.I2 = FALSE,
        col.diamond = "steelblue",
        col.diamond.lines = "steelblue",
        col.square = "steelblue",
        col.inside = "black")
}
dev.off()
