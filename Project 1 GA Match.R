library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(metaforest)
library(metafor)
library(meta)

setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")
load(file = "Project1.RData")

#### GA matched & restricted studies ####
#Step 1a
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")
GA_studies <- read_excel("Systematic review search.xlsx", sheet = "GA studies")
GA_studies <- merge(unique(data.frame(GA_studies)[,c(1,7,8)] %>% rename("study_n"=1) ), SRS[c(1,25,4:24)] %>% 
                    filter(Placenta == "PW"), by = "study_n")[c(1,4:6,10:25,9,2)] %>% rename("Total_N"="N") %>%
                    mutate(GA_diff = GA_mean_case - GA_mean_control) %>% arrange(study_id)
head(GA_studies)
table(GA_studies$Outcome, GA_studies$Adjusting)
# Step 1b.1 - GA matched (according to study)
(GA_matched <- GA_studies %>% filter(Adjusting == "GA matched") )
GA_matched[c(1:7,10:14,17:20,23)] %>% filter(GA_diff <= -1)
data.frame(table(GA_matched$Outcome)) %>% filter(Freq >= 3)
# Step 1b.2 - GA restricted
(GA_restrict <- GA_studies %>% filter(Adjusting == "GA restricted") )
GA_restrict[c(1:7,10:14,17:20,23)] %>% filter(GA_diff <= -1)
data.frame(table(GA_restrict$Outcome)) %>% filter(Freq >= 3)
# Step 1b.3 - GA matched (according to +-1wk GA diff)
nrow(SRS_EON %>% mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1) )
nrow(SRS_LON %>% mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1) )
data.frame(table(unique(SRS %>% mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1) %>%
          filter(Placenta=="PW:BW" | Placenta=="BW:PW") %>% select(c("study_id","Outcome")))$Outcome )) %>% filter(Freq >= 3)

data.frame(table((SRS %>% mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1) %>%
         filter(Placenta == "PW"))$Outcome)) %>% filter(Freq >= 3)

#### Placental Weight ####
#### PE ####
#Step 2 - create dataset
GA_PE_PW <- SRS_PE_PW %>% filter(GA_mean_case > 0, GA_mean_control > 0) %>% mutate(GA_diff = GA_mean_case - GA_mean_control)
head(GA_PE_PW)
dim(GA_PE_PW)
summary(GA_PE_PW$GA_diff)
GA_PE_PW <- GA_PE_PW %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_PE_PW)
dim(GA_PE_PW)
summary(GA_PE_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_PE <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                     sd.c = PW_sd_control, studlab = study_id, data = GA_PE_PW )
summary(MA_GA_PE)
forest(MA_GA_PE,
         leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_PE_GA_A <- metareg(MA_GA_PE, GA_mean_case, hakn = TRUE)
metareg_PE_GA_A$k
metareg_PE_GA_A$I2
metareg_PE_GA_A$pval[c(2)]
metareg_PE_GA_A$b[c(2)]
bubble(metareg_PE_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")
# Step 4b - gestational age difference
metareg(MA_GA_PE, GA_diff, hakn = TRUE)$pval[c(2)]


#### FGR ####
#Step 2 - create dataset
GA_FGR_PW <- SRS_FGR_PW %>% filter(GA_mean_case > 0,GA_mean_control > 0) %>% mutate(GA_diff = GA_mean_case - GA_mean_control)
head(GA_FGR_PW)
dim(GA_FGR_PW)
summary(GA_FGR_PW$GA_diff)
GA_FGR_PW <- GA_FGR_PW %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_FGR_PW)
dim(GA_FGR_PW)
summary(GA_FGR_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_FGR <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                     sd.c = PW_sd_control, studlab = study_id, data = GA_FGR_PW )
summary(MA_GA_FGR)
forest(MA_GA_FGR,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_FGR_GA_A <- metareg(MA_GA_FGR, GA_mean_case, hakn = TRUE)
metareg_FGR_GA_A$k
metareg_FGR_GA_A$I2
metareg_FGR_GA_A$pval[c(2)]
metareg_FGR_GA_A$b[c(2)]
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FGR MR PW GAM.png", width = 2000, height = 1500, res = 250)
bubble(metareg_FGR_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.7, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)",xlim = c(32,41))
dev.off()
# Step 4b - gestational age difference
metareg_FGR_GA_B <- metareg(MA_GA_FGR, GA_diff, hakn = TRUE) 
metareg_FGR_GA_B$I2
metareg_FGR_GA_B$pval[c(2)]
metareg_FGR_GA_B$b[c(2)]
bubble(metareg_FGR_GA_B, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")


MAR_FGR_GA <- data.frame(c("PW"), c(metareg_FGR_GA_A$pval[c(2)]),
                      c(metareg_FGR_GA_A$b[c(2)]), c(metareg_FGR_GA_A$se[c(2)]),
                      c(metareg_FGR_GA_A$k), c(metareg_FGR_GA_A$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_FGR_GA$beta <- format(round(MAR_FGR_GA$beta,1),nsmall=1,scientific=FALSE)
MAR_FGR_GA$UCI <- format(round(MAR_FGR_GA$UCI,1),nsmall=1,scientific=FALSE)
MAR_FGR_GA$LCI <- format(round(MAR_FGR_GA$LCI,1),nsmall=1,scientific=FALSE)
MAR_FGR_GA$i2 <- format(round(MAR_FGR_GA$i2,1),nsmall=1,scientific=FALSE)
MAR_FGR_GA$pval <- ifelse(MAR_FGR_GA$pval < 0.001, "<0.001",format(round(MAR_FGR_GA$pval,3), nsmall = 3, scientific = FALSE))
MAR_FGR_GA$beta_95CI <- paste0(MAR_FGR_GA$beta,"g (",MAR_FGR_GA$LCI,"g, ",MAR_FGR_GA$UCI,"g)")
MAR_FGR_GA[c(1,5,6,9,2)]

#### SGA ####
#Step 2 - create dataset
GA_SGA_PW <- SRS_SGA_PW %>% filter(GA_mean_case > 0,GA_mean_control > 0) %>% mutate(GA_diff = GA_mean_case - GA_mean_control)
head(GA_SGA_PW)
dim(GA_SGA_PW)
summary(GA_SGA_PW$GA_diff)
GA_SGA_PW <- GA_SGA_PW %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_SGA_PW)
dim(GA_SGA_PW)
summary(GA_SGA_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_SGA <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_SGA_PW )
summary(MA_GA_SGA)
forest(MA_GA_SGA,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_SGA_GA_A <- metareg(MA_GA_SGA, GA_mean_case, hakn = TRUE)
metareg_SGA_GA_A$k
metareg_SGA_GA_A$I2
metareg_SGA_GA_A$pval[c(2)]
metareg_SGA_GA_A$b[c(2)]
bubble(metareg_SGA_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")
# Step 4b - gestational age difference
metareg_SGA_GA_B <- metareg(MA_GA_SGA, GA_diff, hakn = TRUE) 
metareg_SGA_GA_B$I2
metareg_SGA_GA_B$pval[c(2)]
metareg_SGA_GA_B$b[c(2)]
bubble(metareg_SGA_GA_B, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")

#### LBW ####
#Step 2 - create dataset
GA_LBW_PW <- SRS_LBW_PW %>% filter(GA_mean_case > 0,GA_mean_control > 0) %>% mutate(GA_diff = GA_mean_case - GA_mean_control)
head(GA_LBW_PW)
dim(GA_LBW_PW)
summary(GA_LBW_PW$GA_diff)
GA_LBW_PW <- GA_LBW_PW %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_LBW_PW)
dim(GA_LBW_PW)
summary(GA_LBW_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_LBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_LBW_PW )
summary(MA_GA_LBW)
forest(MA_GA_LBW,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_LBW_GA_A <- metareg(MA_GA_LBW, GA_mean_case, hakn = TRUE)
metareg_LBW_GA_A$k
metareg_LBW_GA_A$I2
metareg_LBW_GA_A$pval[c(2)]
metareg_LBW_GA_A$b[c(2)]
bubble(metareg_LBW_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")
# Step 4b - gestational age difference
metareg_LBW_GA_B <- metareg(MA_GA_LBW, GA_diff, hakn = TRUE) 
metareg_LBW_GA_B$I2
metareg_LBW_GA_B$pval[c(2)]
metareg_LBW_GA_B$b[c(2)]
bubble(metareg_LBW_GA_B, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")


#### MVM ####
#Step 2 - create dataset
GA_MVM_PW <- SRS_MVM_PW %>% filter(GA_mean_case > 0,GA_mean_control > 0) %>% 
  mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1)
GA_MVM_PW
summary(GA_MVM_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_MVM <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_MVM_PW )
summary(MA_GA_MVM)
forest(MA_GA_MVM,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_MVM_GA_A <- metareg(MA_GA_MVM, GA_mean_case, hakn = TRUE)
metareg_MVM_GA_A$k
metareg_MVM_GA_A$I2
metareg_MVM_GA_A$pval[c(2)]
metareg_MVM_GA_A$b[c(2)]
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("MVM MR PW GAM.png", width = 2000, height = 1500, res = 250)
bubble(metareg_MVM_GA_A, xlab = "Gestational Age of Cases (weeks)", cex=0.5, col.line = "red", studlab = TRUE,ylim=c(-110,0),
       cex.studlab =0.9, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)",xlim=c(27,40))
dev.off()


#### PE EON ####
#Step 2 - create dataset
GA_EON_PW <- SRS_EON %>% filter(GA_mean_case > 0, GA_mean_control > 0) %>% 
  mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1)
GA_EON_PW
dim(GA_EON_PW)
summary(GA_EON_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_EON <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                     sd.c = PW_sd_control, studlab = study_id, data = GA_EON_PW )
summary(MA_GA_EON)
forest(MA_GA_EON,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_EON_GA_A <- metareg(MA_GA_EON, GA_mean_case, hakn = TRUE)
metareg_EON_GA_A$k
metareg_EON_GA_A$I2
metareg_EON_GA_A$pval[c(2)]
metareg_EON_GA_A$b[c(2)]
bubble(metareg_EON_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")

#### PE LON ####
#Step 2 - create dataset
GA_LON_PW <- SRS_LON %>% filter(GA_mean_case > 0, GA_mean_control > 0) %>% 
  mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1)
GA_LON_PW
dim(GA_LON_PW)
summary(GA_LON_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_LON <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_LON_PW )
summary(MA_GA_LON)
forest(MA_GA_LON,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_LON_GA_A <- metareg(MA_GA_LON, GA_mean_case, hakn = TRUE)
metareg_LON_GA_A$k
metareg_LON_GA_A$I2
metareg_LON_GA_A$pval[c(2)]
metareg_LON_GA_A$b[c(2)]
bubble(metareg_LON_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")

#### HDP ####
#Step 2 - create dataset
GA_HDP_PW <- SRS_HDP_PW %>% filter(GA_mean_case > 0, GA_mean_control > 0) %>% 
  mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_HDP_PW)
dim(SRS_HDP_PW)
dim(GA_HDP_PW)
# Step 3 - meta analysis
MA_GA_HDP <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_HDP_PW )
summary(MA_GA_HDP)
forest(MA_GA_HDP,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_HDP_GA_A <- metareg(MA_GA_HDP, GA_mean_case, hakn = TRUE)
metareg_HDP_GA_A$k
metareg_HDP_GA_A$I2
metareg_HDP_GA_A$pval[c(2)]
metareg_HDP_GA_A$b[c(2)]
bubble(metareg_HDP_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")

#### GH ####
#Step 2 - create dataset
GA_GH_PW <- SRS_GH_PW %>% filter(GA_mean_case > 0, GA_mean_control > 0) %>% 
  mutate(GA_diff = GA_mean_case - GA_mean_control) %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_GH_PW)
dim(SRS_GH_PW)
dim(GA_GH_PW)
# Step 3 - meta analysis
MA_GA_GH <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_GH_PW )
summary(MA_GA_GH)
forest(MA_GA_GH,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_GH_GA_A <- metareg(MA_GA_GH, GA_mean_case, hakn = TRUE)
metareg_GH_GA_A$k
metareg_GH_GA_A$I2
metareg_GH_GA_A$pval[c(2)]
metareg_GH_GA_A$b[c(2)]
bubble(metareg_GH_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")


#### Stillbirth ####
#Step 2 - create dataset
GA_Stillbirth_PW <- SRS_Stillbirth_PW %>% filter(GA_mean_case > 0,GA_mean_control > 0) %>% mutate(GA_diff = GA_mean_case - GA_mean_control)
head(GA_Stillbirth_PW)
dim(GA_Stillbirth_PW)
summary(GA_Stillbirth_PW$GA_diff)
GA_Stillbirth_PW <- GA_Stillbirth_PW %>% filter(GA_diff <= 1, GA_diff >= -1)
head(GA_Stillbirth_PW)
dim(GA_Stillbirth_PW)
summary(GA_Stillbirth_PW$GA_diff)
# Step 3 - meta analysis
MA_GA_Stillbirth <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                      sd.c = PW_sd_control, studlab = study_id, data = GA_Stillbirth_PW )
summary(MA_GA_Stillbirth)
forest(MA_GA_Stillbirth,
       leftcols = c("study_id", "Country","N", "GA_mean_case","GA_mean_control"),
       leftlabs = c("Country", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
       xlab = "Placental weight (g)", smlab = "Preeclampsia", comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
       print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
       col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
# Step 4 - meta regression
# Step 4a - gestational age of cases
metareg_Stillbirth_GA_A <- metareg(MA_GA_Stillbirth, GA_mean_case, hakn = TRUE)
metareg_Stillbirth_GA_A$k
metareg_Stillbirth_GA_A$I2
metareg_Stillbirth_GA_A$pval[c(2)]
metareg_Stillbirth_GA_A$b[c(2)]
bubble(metareg_Stillbirth_GA_A, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")
# Step 4b - gestational age difference
metareg_Stillbirth_GA_B <- metareg(MA_GA_Stillbirth, GA_diff, hakn = TRUE) 
metareg_Stillbirth_GA_B$I2
metareg_Stillbirth_GA_B$pval[c(2)]
metareg_Stillbirth_GA_B$b[c(2)]
bubble(metareg_Stillbirth_GA_B, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)")


#### Summary ####

#GA Matched results (MA and MR)
MA_GA <- list(c(MA_GA_PE),c(MA_GA_FGR),c(MA_GA_SGA),c(MA_GA_LBW),c(MA_GA_MVM),
              c(MA_GA_Stillbirth), c(MA_GA_EON), c(MA_GA_LON), c(MA_GA_HDP), c(MA_GA_GH))
MR_GA_A <- list(c(metareg_PE_GA_A),c(metareg_FGR_GA_A),c(metareg_SGA_GA_A),c(metareg_LBW_GA_A), c(metareg_MVM_GA_A),
              c(metareg_Stillbirth_GA_A),c(metareg_EON_GA_A),c(metareg_LON_GA_A),c(metareg_HDP_GA_A),c(metareg_GH_GA_A))
MA_GA_APOs <- data.frame(matrix(ncol = 11)) %>% rename(beta=1,lower=2,upper=3,I2_GAM=4,
                          n_GAM=5,p_GAM=6,MR_beta_GAM=7, MR_p_GAM=8,MR_I2_GAM=9,MR_se_GAM=10,N=11)
for (i in 1:length(MA_GA)) {
  MA_GA_APOs[c(nrow(MA_GA_APOs)+1),] <- 
    data.frame(c(MA_GA[[i]]$TE.random),c(MA_GA[[i]]$lower.random),
               c(MA_GA[[i]]$upper.random),c(MA_GA[[i]]$I2*100),c(MA_GA[[i]]$k),
               c(MA_GA[[i]]$pval.random), c(MR_GA_A[[i]]$b[c(2)]),c(MR_GA_A[[i]]$pval[c(2)]),
               c(MR_GA_A[[i]]$I2),c(MR_GA_A[[i]]$se[c(2)]))
  MA_GA_APOs$beta <- format(round(as.numeric(MA_GA_APOs$beta),2),nsmall=2,scientific=FALSE)
  MA_GA_APOs$lower <- format(round(as.numeric(MA_GA_APOs$lower),2),nsmall=2,scientific=FALSE)
  MA_GA_APOs$upper <- format(round(as.numeric(MA_GA_APOs$upper),2),nsmall=2,scientific=FALSE)
  MA_GA_APOs$I2_GAM <- format(round(as.numeric(MA_GA_APOs$I2_GAM),1),nsmall=1,scientific=FALSE)
  MA_GA_APOs$p_GAM <- ifelse(MA_GA_APOs$p_GAM < 0.001,"<0.001",format(round(as.numeric(MA_GA_APOs$p_GAM),3),nsmall=3))
  
  MA_GA_APOs$MR_upper_GAM <- as.numeric(MA_GA_APOs$MR_beta_GAM) + as.numeric(MA_GA_APOs$MR_se_GAM)*qnorm(0.975)
  MA_GA_APOs$MR_lower_GAM <- as.numeric(MA_GA_APOs$MR_beta_GAM) - as.numeric(MA_GA_APOs$MR_se_GAM)*qnorm(0.975)
  
  MA_GA_APOs$MR_upper_GAM <- format(round(as.numeric(MA_GA_APOs$MR_upper_GAM),1),nsmall=1,scientific=FALSE)
  MA_GA_APOs$MR_lower_GAM <- format(round(as.numeric(MA_GA_APOs$MR_lower_GAM),1),nsmall=1,scientific=FALSE)
  MA_GA_APOs$MR_beta_GAM <- format(round(as.numeric(MA_GA_APOs$MR_beta_GAM),1),nsmall=1,scientific=FALSE)
  MA_GA_APOs$MR_se_GAM <- format(round(as.numeric(MA_GA_APOs$MR_se_GAM),2),nsmall=2,scientific=FALSE)
  MA_GA_APOs$MR_I2_GAM <- format(round(as.numeric(MA_GA_APOs$MR_I2_GAM),1),nsmall=1,scientific=FALSE)
  MA_GA_APOs$MR_p_GAM <- format(round(as.numeric(MA_GA_APOs$MR_p_GAM),3),nsmall=3,scientific=FALSE)
  
  MA_GA_APOs[c(nrow(MA_GA_APOs)),]$N <- format(as.numeric(MA_GA[[i]]$n.c.pooled + MA_GA[[i]]$n.e.pooled),big.mark=",")
  MA_GA_APOs <- MA_GA_APOs %>% filter(!is.na(beta))
}
MA_GA_APOs$beta_GAM <- paste0(MA_GA_APOs$beta, " (",MA_GA_APOs$lower,",",MA_GA_APOs$upper,")")
MA_GA_APOs$MR_betaCI_GAM <- paste0(MA_GA_APOs$MR_beta_GAM, " (",MA_GA_APOs$MR_lower_GAM,",",
                                   MA_GA_APOs$MR_upper_GAM,")")
MA_GA_APOs <- (MA_GA_APOs[-c(1),] %>% mutate(APO=c("PE","FGR","SGA","LBW","MVM","Stillbirth","PE early onset",
                                                   "PE late onset","HDP","GH")))[,c(16,14,1:6,11,15,9,8)]
MA_GA_APOs


#MA results for all outcomes and exposures
aj <- anti_join(Table2[c(1,2)], (MA_GA_APOs[c(1)] %>% rename("Outcome"=1) %>% 
                  mutate(`Placenta Measure` = "Placental Weight") ))
All_MA_results <- {
  rbind( (rbind((MA_GA_APOs %>% mutate(`Placenta Measure` = "Placental Weight", 
                               Population="GA Matched"))[c(1,12,13,3:5,7,6,8)] %>% 
          rename("Mean"="beta","UCI"="upper","LCI"="lower","I2"="I2_GAM",
                 "N studies"="n_GAM","P value"="p_GAM"),
        (Table2 %>% mutate(Population="All") %>% rename("APO"="Outcome"))[c(1,2,11,3,5,6,7,10,8)] ) %>%
          mutate("Mean (95%CI)" = ifelse(`Placenta Measure` == "Placental Weight",
                 paste0(format(round(as.numeric(Mean),0),nsmall=0)," (", format(round(as.numeric(LCI),0),nsmall=0),",",
                        format(round(as.numeric(UCI),0),nsmall=0),")"), 
                 paste0(format(round(as.numeric(Mean),2),nsmall=2)," (",
                        format(round(as.numeric(LCI),2),nsmall=2),",", 
                        format(round(as.numeric(UCI),2),nsmall=2),")")) ) )[c(1:3,10,7:9)],
        cbind(aj %>% mutate(Population="GA Matched") %>% rename("APO"=1), matrix(nrow=nrow(aj),ncol=4)) %>% 
          rename("Mean (95%CI)"="1","I2"="3","N studies"="2","P value"="4") ) %>% 
    arrange(APO,(`Placenta Measure`),Population)  
  }
All_MA_results$APO <- factor(All_MA_results$APO, 
                             levels=(c("Stillbirth","HDP","GH","PE","PE early onset",
                                        "PE late onset","PTB","FGR","SGA","LBW","MVM","FVM","AI","CI")))
All_MA_results$`Placenta Measure` <- factor(All_MA_results$`Placenta Measure`,
                             levels=(c("Placental Weight","BW:PW ratio","PW:BW ratio")))
All_MA_results$`N studies` <- as.numeric(All_MA_results$`N studies`)
Table2_results <- (All_MA_results %>% rename("Outcome"="APO") %>%
    mutate(`Mean (95%CI)` = str_replace_all(`Mean (95%CI)`,"  "," ")) %>%
    mutate(`Mean (95%CI)` = str_replace_all(`Mean (95%CI)`,"  "," ")) %>%
    arrange(Outcome,`Placenta Measure`,Population) )[c(1:3,5,6,4,7)]
Table2_results
write.csv(Table2_results,"Table2 SR results.csv")



#MR results for model 3
GAM_MA_results <- (MA_GA_APOs %>% mutate("Placenta Measure"="Placental Weight","Included studies"="GA matched",
                                         "Meta Regression by"="GA") )[c(1,12:14,7,9,11)] %>%
  mutate(Type=c(rep("APO",4),"Path",rep("APO",5))) %>% arrange(Type,APO) %>% select(-c("Type")) %>%
  rename("Outcome"=1,"N studies"="n_GAM","Beta"="MR_betaCI_GAM","P value"="MR_p_GAM")
GAM_MA_results

#MR results for all outcomes for placental weight only
aj_2 <- data.frame(c(unique(anti_join(MR_results_All[c(1)],GAM_MA_results)[c(1)])),c("Placental Weight"),
                   c("GA matched"), c("GA"),c("NA"),c(""),c("")) %>% 
                  rename("Outcome"=1,"Placenta Measure"=2,"Included studies"=3,"Meta Regression by"=4,"N studies"=5,
                         "Beta"=6,"P value"=7)
Table3 <- rbind(GAM_MA_results,aj_2,MR_results_All) %>% arrange(Outcome,`Included studies`,`Meta Regression by`) %>%
               filter(`Placenta Measure` == "Placental Weight") %>% select(-c(`Placenta Measure`))
Table3$Beta <- Table3$Beta %>% str_remove_all("g") %>% str_replace_all("  "," ")  %>% 
               str_replace_all("  "," ") %>% str_replace_all(",1",", 1") 
Table3$Outcome<-factor(Table3$Outcome,levels=c("Stillbirth","HDP","GH","PE","PE early onset",
                                 "PE late onset","PTB","FGR","SGA","LBW","MVM","FVM","AI","CI"))
Table3 <- Table3 %>% arrange(Outcome,`Included studies`,`Meta Regression by`)
Table3$`Included studies`<- c(rep(c("All","","GA matched"),length(unique(Table3$Outcome))))
Table3$Outcome <- c("Stillbirth","","","HDP","","","GH","","","PE","","","PE early onset","","","PE late onset","","",
                    "PTB","","","FGR","","","SGA","","","LBW","","","MVM","","","FVM","","",
                    "AI","","","CI","","")
Table3
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
write.csv(Table3,"Table3 meta reg.csv")

