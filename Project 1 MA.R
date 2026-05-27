library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(metafor)
library(metaforest)
library(meta)
library(tidyr)

### Step 1 ####
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")
Systematic_review_search <- read_excel("Systematic review search.xlsx", sheet = "Meta Analysis 2.0")
SRS <- data.frame(Systematic_review_search[c(-1),c(1:13,22,23,32,33,42:44,53,54,63,64,75,76,78)]) %>% filter(A == "A") %>% 
  select(-c("A")) %>%
  rename("Pub"=4,"Placenta"=7,"N"=9,"Control_N"=10,"Case_N"=17, "PW_mean_control"=11,"PW_sd_control"=12,"BW_mean_control"=13,
         "BW_sd_control"=14,"GA_mean_control"=15, "GA_sd_control"=16,"PW_mean_case"=18,"PW_sd_case"=19,"BW_mean_case"=20,
         "BW_sd_case"=21,"GA_mean_case"=22, "GA_sd_case"=23, "study_n"=1, "GA_mean_all"=24, "GA_sd_all"=25) %>% 
  filter(Measure == "Mean" | Measure == "Median") %>% filter(is.na(Repeat)) %>% 
  separate(Authors, c("study_i", "bar"), ",") %>% separate(study_i, c("study_i", "j"), " and") %>% 
  select(-c("bar", "j","Repeat"))
SRS[c(11:16,18:25)] <- as.numeric(unlist(SRS[c(11:16,18:25)]))
SRS[c(9,10,17)] <- as.numeric(unlist(SRS[c(9,10,17)]))
SRS$study_i <- gsub(".*[.] ","",SRS$study_i)
SRS <- SRS %>% mutate(study_id = paste0(study_i, " et al. ", Pub)) %>% select(-c("study_i")) %>% arrange(study_id)
length(unique(SRS$study_id))
summary(SRS$GA_mean_all)
table(SRS$Outcome, SRS$Placenta)
head(SRS[-c(2)])
dim(SRS)
#repeat data check:
as.data.frame(count(SRS[-c(1:4,25)],SRS[-c(1:4,25)])) %>% filter(n>1)
#repeat check 2:
#SRS %>% filter(Placenta=="PW",Country=="Japan") %>% select(1,25,4,5,8:24)
sum((unique(SRS[c(1,6,9:15)] %>% filter(Placenta == "PW") ))$Control_N)
sum((unique(SRS[c(1,6,16:22)] %>% filter(Placenta == "PW") ))$Case_N)


### Preeclampsia ####
# Step 2
# Step 2a
SRS_PE <- SRS %>% filter(Outcome == "Preeclampsia") %>% select(1,25,4,6,8:24)
head(SRS_PE)
dim(SRS_PE)
table(SRS_PE$Country)
#SRS_PE %>% filter(Country == "0")
table(SRS_PE$Placenta)
#see `Project 1 PE Onset .R` for HDP, GH, early onset PE, and late onset PE

## PW
# Step 2b - PW
SRS_PE_PW <- SRS_PE %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
                        mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
                        arrange(GA_mean_case,GA_mean_control,study_id)
SRS_PE_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                    sd2i = PW_sd_control, sd1i = PW_sd_case,
                    n2i = Control_N, n1i = Case_N, data = SRS_PE_PW)
SRS_PE_PW <- SRS_PE_PW[!is.na(SRS_PE_PW$vi),]
head(SRS_PE_PW)
sum(SRS_PE_PW$Total_N)
sum(SRS_PE_PW$Case_N)
length(unique(SRS_PE_PW$study_id))
dim(SRS_PE_PW)
table(is.na(SRS_PE_PW$GA_mean_all))
# Step 3
MA_PE_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                     n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                     studlab = study_id, data = SRS_PE_PW )
summary(MA_PE_PW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Preeclampsia PW.png", width = 6000, height = 12000, res = 450)
{
  forest(MA_PE_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)", smlab = "Preeclampsia",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#Flipping ratios
SRS_PE_BWPW <- SRS_PE %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  arrange(GA_mean_case,GA_mean_control) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_PE_PWBW <- SRS_PE %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21) %>%
  arrange(GA_mean_case,GA_mean_control) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
table((rbind(data.frame(SRS_PE_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_PE_BWPW) %>% mutate(PW = "PWBW") ))$study_id)
Ratio_PE_flip <-
  (rbind(data.frame(SRS_PE_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_PE_BWPW) %>% mutate(PW = "PWBW") ) %>% 
     arrange(study_id) %>% filter(!(study_id == "Marques et al. 2018" | study_id == "Shallie et al. 2023")) %>%
     mutate(PW_1_mean_control = 1/PW_mean_control) %>% 
     mutate(PW_1_sd_control = (PW_sd_control/PW_mean_control)*PW_1_mean_control) %>% 
     select(-c("PW_mean_control","PW_sd_control")) %>% mutate(PW_1_mean_case = 1/PW_mean_case) %>% 
     mutate(PW_1_sd_case = (PW_sd_case/PW_mean_case)*PW_1_mean_case) %>% select(-c("PW_mean_case","PW_sd_case")) %>% 
     rename("PW_mean_control" = "PW_1_mean_control","PW_sd_control"="PW_1_sd_control","PW_mean_case"="PW_1_mean_case",
            "PW_sd_case"="PW_1_sd_case"))[c(1:4,15,16,5,17,18,6:14)]


## BW:PW ratio
# Step 2b - PW:BW ratio
SRS_PE_BWPW <- SRS_PE %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
                          mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_PE_BWPW <- rbind(SRS_PE_BWPW,Ratio_PE_flip %>% filter(PW == "BWPW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_PE_BWPW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                    sd2i = PW_sd_control, sd1i = PW_sd_case,
                    n2i = Control_N, n1i = Case_N, data = SRS_PE_BWPW)
table(is.na(SRS_PE_BWPW$GA_mean_all))
(SRS_PE_BWPW)

# Step 3
MA_PE_BWPW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control, studlab = study_id, data = SRS_PE_BWPW )
summary(MA_PE_BWPW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Preeclampsia BW:PW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_PE_BWPW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "BW:PW ratio", smlab = "Preeclampsia",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


## PW:BW ratio
# Step 2b - PW:BW ratio
SRS_PE_PWBW <- SRS_PE %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
                          mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_PE_PWBW <- rbind(SRS_PE_PWBW,Ratio_PE_flip %>% filter(PW == "PWBW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_PE_PWBW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                      sd2i = PW_sd_control, sd1i = PW_sd_case,
                      n2i = Control_N, n1i = Case_N, data = SRS_PE_PWBW)
table(is.na(SRS_PE_PWBW$vi))
(SRS_PE_PWBW)
# Step 3
MA_PE_PWBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control, studlab = study_id, data = SRS_PE_PWBW )
summary(MA_PE_PWBW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Preeclampsia PW:BW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_PE_PWBW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "PW:BW ratio", smlab = "Preeclampsia",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#

### FGR ####
# Step 2
# Step 2a
SRS_FGR <- SRS %>% filter(Outcome == "FGR") %>% select(1,25,4,6,8:24)
head(SRS_FGR)
dim(SRS_FGR)
table(SRS_FGR$Country)
#SRS_FGR %>% filter(Country == "0")
table(SRS_FGR$Placenta)

## PW
# Step 2b - PW
SRS_FGR_PW <- SRS_FGR %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_FGR_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                    sd2i = PW_sd_control, sd1i = PW_sd_case,
                    n2i = Control_N, n1i = Case_N, data = SRS_FGR_PW)
SRS_FGR_PW <- SRS_FGR_PW[!is.na(SRS_FGR_PW$vi),]
head(SRS_FGR_PW)
dim(SRS_FGR_PW)
table(is.na(SRS_FGR_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_FGR_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                     n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                     studlab = study_id, data = SRS_FGR_PW )
summary(MA_FGR_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FGR PW.png", width = 4000, height = 5000, res = 300)
{
  forest(MA_FGR_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "FGR",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#Flipping ratios
SRS_FGR_BWPW <- SRS_FGR %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_FGR_PWBW <- SRS_FGR %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21) %>%
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
data.frame(table((rbind(data.frame(SRS_FGR_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_FGR_BWPW) %>% 
                          mutate(PW = "PWBW") ))$study_id)) %>% filter (Freq > 1)
Ratio_FGR_flip <-
  (rbind(data.frame(SRS_FGR_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_FGR_BWPW) %>% mutate(PW = "PWBW") ) %>% 
     arrange(study_id) %>% mutate(PW_1_mean_control = 1/PW_mean_control) %>% 
     filter(!(study_id == "Biswas et al. 2008" | study_id == "Spinillo et al. 2019" | study_id == "Endo et al. 2022")) %>%
     mutate(PW_1_sd_control = (PW_sd_control/PW_mean_control)*PW_1_mean_control) %>% 
     select(-c("PW_mean_control","PW_sd_control")) %>% mutate(PW_1_mean_case = 1/PW_mean_case) %>% 
     mutate(PW_1_sd_case = (PW_sd_case/PW_mean_case)*PW_1_mean_case) %>% select(-c("PW_mean_case","PW_sd_case")) %>% 
     rename("PW_mean_control" = "PW_1_mean_control","PW_sd_control"="PW_1_sd_control","PW_mean_case"="PW_1_mean_case",
            "PW_sd_case"="PW_1_sd_case"))[c(1:4,15,16,5,17,18,6:14)]
nrow(Ratio_FGR_flip)

## BW:PW ratio
# Step 2b - PW:BW ratio
SRS_FGR_BWPW <- SRS_FGR %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_FGR_BWPW <- rbind(SRS_FGR_BWPW,Ratio_FGR_flip %>% filter(PW == "BWPW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_FGR_BWPW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_FGR_BWPW)
table(is.na(SRS_FGR_BWPW$GA_mean_all))
(SRS_FGR_BWPW)
# Step 3
MA_FGR_BWPW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_FGR_BWPW )
summary(MA_FGR_BWPW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FGR BWPW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_FGR_BWPW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "BW:PW ratio", smlab = "FGR",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


## PW:BW ratio
# Step 2b - PW:BW ratio
SRS_FGR_PWBW <- SRS_FGR %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_FGR_PWBW <- rbind(SRS_FGR_PWBW,Ratio_FGR_flip %>% filter(PW == "PWBW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_FGR_PWBW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_FGR_PWBW)
table(is.na(SRS_FGR_PWBW$vi))
(SRS_FGR_PWBW)
# Step 3
MA_FGR_PWBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_FGR_PWBW )
summary(MA_FGR_PWBW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FGR PWBW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_FGR_PWBW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "PW:BW ratio", smlab = "FGR",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#

### SGA ####
# Step 2
# Step 2a
SRS_SGA <- SRS %>% filter(Outcome == "SGA") %>% select(1,25,4,6,8:24)
head(SRS_SGA)
dim(SRS_SGA)
table(SRS_SGA$Country)
#SRS_SGA %>% filter(Country == "0")
table(SRS_SGA$Placenta)


## PW
# Step 2b - PW
SRS_SGA_PW <- SRS_SGA %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_SGA_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_SGA_PW)
SRS_SGA_PW <- SRS_SGA_PW[!is.na(SRS_SGA_PW$vi),]
head(SRS_SGA_PW)
dim(SRS_SGA_PW)
table(is.na(SRS_SGA_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_SGA_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_SGA_PW )
summary(MA_SGA_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("SGA PW.png", width = 4000, height = 3100, res = 300)
{
  forest(MA_SGA_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "SGA",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#Flipping ratios
SRS_SGA_BWPW <- SRS_SGA %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_SGA_PWBW <- SRS_SGA %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21) %>%
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
table((rbind(data.frame(SRS_SGA_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_SGA_BWPW) %>% mutate(PW = "PWBW") ))$study_id)
Ratio_SGA_flip <-
  (rbind(data.frame(SRS_SGA_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_SGA_BWPW) %>% mutate(PW = "PWBW") ) %>% 
     arrange(study_id)  %>% mutate(PW_1_mean_control = 1/PW_mean_control) %>% 
     mutate(PW_1_sd_control = (PW_sd_control/PW_mean_control)*PW_1_mean_control) %>% 
     select(-c("PW_mean_control","PW_sd_control")) %>% mutate(PW_1_mean_case = 1/PW_mean_case) %>% 
     mutate(PW_1_sd_case = (PW_sd_case/PW_mean_case)*PW_1_mean_case) %>% select(-c("PW_mean_case","PW_sd_case")) %>% 
     rename("PW_mean_control" = "PW_1_mean_control","PW_sd_control"="PW_1_sd_control","PW_mean_case"="PW_1_mean_case",
            "PW_sd_case"="PW_1_sd_case"))[c(1:4,15,16,5,17,18,6:14)]

## BW:PW ratio
# Step 2b - PW:BW ratio
SRS_SGA_BWPW <- SRS_SGA %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case,GA_mean_control)
SRS_SGA_BWPW <- rbind(SRS_SGA_BWPW,Ratio_SGA_flip %>% filter(PW == "BWPW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_SGA_BWPW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_SGA_BWPW)
table(is.na(SRS_SGA_BWPW$GA_mean_all))
(SRS_SGA_BWPW)
# Step 3
MA_SGA_BWPW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_SGA_BWPW )
summary(MA_SGA_BWPW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("SGA BWPW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_SGA_BWPW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "BW:PW ratio", smlab = "SGA",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


## PW:BW ratio
# Step 2b - PW:BW ratio
SRS_SGA_PWBW <- SRS_SGA %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) 
SRS_SGA_PWBW <- rbind(SRS_SGA_PWBW,Ratio_SGA_flip %>% filter(PW == "PWBW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_SGA_PWBW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_SGA_PWBW)
table(is.na(SRS_SGA_PWBW$vi))
(SRS_SGA_PWBW)
# Step 3
MA_SGA_PWBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_SGA_PWBW )
summary(MA_SGA_PWBW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("SGA PWBW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_SGA_PWBW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "PW:BW ratio", smlab = "SGA",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#

### PTB ####
# Step 2
# Step 2a
SRS_PTB <- SRS %>% filter(Outcome == "PTB") %>% select(1,25,4,6,8:24)
head(SRS_PTB)
dim(SRS_PTB)
table(SRS_PTB$Country)
#SRS_PTB %>% filter(Country == "0")
table(SRS_PTB$Placenta)


## PW
# Step 2b - PW
SRS_PTB_PW <- SRS_PTB %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_PTB_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_PTB_PW)
SRS_PTB_PW <- SRS_PTB_PW[!is.na(SRS_PTB_PW$vi),]
head(SRS_PTB_PW)
dim(SRS_PTB_PW)
table(is.na(SRS_PTB_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_PTB_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_PTB_PW )
summary(MA_PTB_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("PTB PW.png", width = 4000, height = 2500, res = 300)
{
  forest(MA_PTB_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "PTB",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#

### LBW ####
# Step 2
# Step 2a
SRS_LBW <- SRS %>% filter(Outcome == "LBW") %>% select(1,25,4,6,8:24)
head(SRS_LBW)
dim(SRS_LBW)
table(SRS_LBW$Country)
#SRS_LBW %>% filter(Country == "0")
table(SRS_LBW$Placenta)


## PW
# Step 2b - PW
SRS_LBW_PW <- SRS_LBW %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_LBW_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_LBW_PW)
SRS_LBW_PW <- SRS_LBW_PW[!is.na(SRS_LBW_PW$vi),]
head(SRS_LBW_PW)
dim(SRS_LBW_PW)
table(is.na(SRS_LBW_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_LBW_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_LBW_PW )
summary(MA_LBW_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("LBW PW.png", width = 4000, height = 1500, res = 300)
{
  forest(MA_LBW_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "LBW",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#Flipping ratios
SRS_LBW_BWPW <- SRS_LBW %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_LBW_PWBW <- SRS_LBW %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21) %>%
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
table((rbind(data.frame(SRS_LBW_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_LBW_BWPW) %>% mutate(PW = "PWBW") ))$study_id)


## BW:PW ratio
# Step 2b - PW:BW ratio
SRS_LBW_BWPW <- SRS_LBW %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case,GA_mean_control)
SRS_LBW_BWPW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_LBW_BWPW)
table(is.na(SRS_LBW_BWPW$GA_mean_all))
(SRS_LBW_BWPW)
# Step 3
MA_LBW_BWPW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_LBW_BWPW )
summary(MA_LBW_BWPW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("LBW BWPW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_LBW_BWPW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "BW:PW ratio", smlab = "LBW",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


## PW:BW ratio
# Step 2b - PW:BW ratio
SRS_LBW_PWBW <- SRS_LBW %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))  %>%  arrange(GA_mean_case,GA_mean_control)
SRS_LBW_PWBW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_LBW_PWBW)
table(is.na(SRS_LBW_PWBW$vi))
(SRS_LBW_PWBW)
# Step 3
MA_LBW_PWBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_LBW_PWBW )
summary(MA_LBW_PWBW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("LBW PWBW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_LBW_PWBW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "PW:BW ratio", smlab = "LBW",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#

### Stillbirth ####
# Step 2
# Step 2a
SRS_Stillbirth <- SRS %>% filter(Outcome == "Stillbirth") %>% select(1,25,4,6,8:24)
(SRS_Stillbirth)
dim(SRS_Stillbirth)
table(SRS_Stillbirth$Country)
#SRS_Stillbirth %>% filter(Country == "0")
table(SRS_Stillbirth$Placenta)


## PW
# Step 2b - PW
SRS_Stillbirth_PW <- SRS_Stillbirth %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_Stillbirth_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_Stillbirth_PW)
SRS_Stillbirth_PW <- SRS_Stillbirth_PW[!is.na(SRS_Stillbirth_PW$vi),]
head(SRS_Stillbirth_PW)
dim(SRS_Stillbirth_PW)
table(is.na(SRS_Stillbirth_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_Stillbirth_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_Stillbirth_PW )
summary(MA_Stillbirth_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Stillbirth PW.png", width = 4000, height = 1500, res = 300)
{
  forest(MA_Stillbirth_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "Stillbirth",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#Flipping ratios
SRS_Stillbirth_BWPW <- SRS_Stillbirth %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  arrange(GA_mean_case,GA_mean_control) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_Stillbirth_PWBW <- SRS_Stillbirth %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21) %>%
  arrange(GA_mean_case,GA_mean_control) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
table((rbind(data.frame(SRS_Stillbirth_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_Stillbirth_BWPW) %>% mutate(PW = "PWBW") ))$study_id)
Ratio_Stillbirth_flip <-
  (rbind(data.frame(SRS_Stillbirth_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_Stillbirth_BWPW) %>% mutate(PW = "PWBW") ) %>% 
     arrange(study_id) %>% mutate(PW_1_mean_control = 1/PW_mean_control) %>% 
     mutate(PW_1_sd_control = (PW_sd_control/PW_mean_control)*PW_1_mean_control) %>% 
     select(-c("PW_mean_control","PW_sd_control")) %>% mutate(PW_1_mean_case = 1/PW_mean_case) %>% 
     mutate(PW_1_sd_case = (PW_sd_case/PW_mean_case)*PW_1_mean_case) %>% select(-c("PW_mean_case","PW_sd_case")) %>% 
     rename("PW_mean_control" = "PW_1_mean_control","PW_sd_control"="PW_1_sd_control","PW_mean_case"="PW_1_mean_case",
            "PW_sd_case"="PW_1_sd_case"))[c(1:4,15,16,5,17,18,6:14)]


## BW:PW ratio
# Step 2b - PW:BW ratio
SRS_Stillbirth_BWPW <- SRS_Stillbirth %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_Stillbirth_BWPW <- rbind(SRS_Stillbirth_BWPW,Ratio_Stillbirth_flip %>% filter(PW == "BWPW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_Stillbirth_BWPW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                      sd2i = PW_sd_control, sd1i = PW_sd_case,
                      n2i = Control_N, n1i = Case_N, data = SRS_Stillbirth_BWPW)
table(is.na(SRS_Stillbirth_BWPW$GA_mean_all))
(SRS_Stillbirth_BWPW)
# Step 3
MA_Stillbirth_BWPW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control, studlab = study_id, data = SRS_Stillbirth_BWPW )
summary(MA_Stillbirth_BWPW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Stillbirth BW:PW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_Stillbirth_BWPW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "BW:PW ratio", smlab = "Stillbirth",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


## PW:BW ratio
# Step 2b - PW:BW ratio
SRS_Stillbirth_PWBW <- SRS_Stillbirth %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_Stillbirth_PWBW <- rbind(SRS_Stillbirth_PWBW,Ratio_Stillbirth_flip %>% filter(PW == "PWBW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_Stillbirth_PWBW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                      sd2i = PW_sd_control, sd1i = PW_sd_case,
                      n2i = Control_N, n1i = Case_N, data = SRS_Stillbirth_PWBW)
table(is.na(SRS_Stillbirth_PWBW$vi))
(SRS_Stillbirth_PWBW)
# Step 3
MA_Stillbirth_PWBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control, studlab = study_id, data = SRS_Stillbirth_PWBW )
summary(MA_Stillbirth_PWBW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Stillbirth PW:BW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_Stillbirth_PWBW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "PW:BW ratio", smlab = "Stillbirth",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()



### Placental Praevia ####
# Step 2
# Step 2a
SRS_PP <- SRS %>% filter(Outcome == "Placental praevia") %>% select(1,25,4,6,8:24)
head(SRS_PP)
dim(SRS_PP)
table(SRS_PP$Country)
table(SRS_PP$Placenta)
# Step 2b - PW
SRS_PP_PW <- SRS_PP %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_PP_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case, sd2i = PW_sd_control, sd1i = PW_sd_case,
                            n2i = Control_N, n1i = Case_N, data = SRS_PP_PW)
SRS_PP_PW <- SRS_PP_PW[!is.na(SRS_PP_PW$vi),]
head(SRS_PP_PW)
dim(SRS_PP_PW)
table(is.na(SRS_PP_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_PP_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, studlab = study_id, 
                     n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control, data = SRS_PP_PW )
summary(MA_PP_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Placental Praevia PW.png", width = 4000, height = 1500, res = 300)
{forest(MA_PP_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "PP",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


#
### Placental Abruption ####
# Step 2
# Step 2a
SRS_PA <- SRS %>% filter(Outcome == "Placental abruption") %>% select(1,25,4,6,8:24)
head(SRS_PA)
dim(SRS_PA)
table(SRS_PA$Country)
table(SRS_PA$Placenta)
# Step 2b - PW
SRS_PA_PW <- SRS_PA %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_PA_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case, sd2i = PW_sd_control, sd1i = PW_sd_case,
                    n2i = Control_N, n1i = Case_N, data = SRS_PA_PW)
SRS_PA_PW <- SRS_PA_PW[!is.na(SRS_PA_PW$vi),]
head(SRS_PA_PW)
dim(SRS_PA_PW)
table(is.na(SRS_PA_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_PA_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, studlab = study_id, 
                     n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control, data = SRS_PA_PW )
summary(MA_PA_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Placental Abruption PW.png", width = 4000, height = 1500, res = 300)
{forest(MA_PA_PW,
        leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
        leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
        xlab = "Placental Weight (g)", smlab = "PP",
        comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
        print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
        col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#
### MVM ####
# Step 2
# Step 2a
SRS_MVM <- data.frame(Systematic_review_search[c(-1),c(1:13,22,23,32,33,42:44,53,54,63,64,75,76,73,78)])   %>%
  rename("Pub"=4,"Placenta"=7,"Total_N"=10,"Control_N"=11,"Case_N"=18, "PW_mean_control"=12,"PW_sd_control"=13,
         "BW_mean_control"=14,"BW_sd_control"=15,"GA_mean_control"=16, "GA_sd_control"=17,"PW_mean_case"=19,"PW_sd_case"=20,
         "BW_mean_case"=21,"BW_sd_case"=22,"GA_mean_case"=23, "GA_sd_case"=24,"study_n"=1,"GA_mean_all"=25,"GA_sd_all"=26) %>%
  filter(Measure == "Mean" | Measure == "Median") %>% filter(Outcome == "MVM") %>% filter(Placenta == "PW")  %>% 
  separate(Authors, c("study_i", "bar"), ",") %>%  separate(study_i, c("study_i", "j"), " and") %>% filter(is.na(Repeat)) %>% 
  select(-c("bar", "j", "study_n", "Measure","Outcome","Title","Repeat"))
SRS_MVM$study_i <- gsub(".*[.] ","",SRS_MVM$study_i)
SRS_MVM <- SRS_MVM %>% mutate(study_id = paste0(study_i, " et al. ", Pub)) %>% select(-c("study_i")) %>% arrange(study_id) %>%
  select(c("study_id","Country","A","Control_N","PW_mean_control","PW_sd_control","GA_mean_control","GA_sd_control",
           "Case_N","PW_mean_case","PW_sd_case","GA_mean_case","GA_sd_case","GA_mean_all","GA_sd_all","Notes"))  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_MVM[c(4:15)] <- as.numeric(unlist(SRS_MVM[c(4:15)]))
SRS_MVM[-c(2)]
dim(SRS_MVM)
table(SRS_MVM$Country)
#SRS_MVM %>% filter(Country == "0")


## PW
# Step 2b - PW
SRS_MVM_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_MVM)
SRS_MVM_PW <- SRS_MVM_PW[!is.na(SRS_MVM_PW$vi),]
# Step 3 - Meta analysis
MA_MVM_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_MVM_PW )
summary(MA_MVM_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("MVM PW.png", width = 4000, height = 1500, res = 300)
{
  forest(MA_MVM_PW,
         leftcols = c("study_id", "Notes","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("MVM type", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "MVM",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


# Adding MOMI
#Step 2a part 1 - importing MOMI
MOMI_Data_P2 <- read_csv("~/Documents/Placenta/Project 2 - Non Linear Pathology/MOMI_Data_P2.csv")
MOMI_Data_P2_MVM_Control <- as.data.frame(MOMI_Data_P2) %>% filter(MVM_01 == "0")
MOMI_Data_P2_MVM_Case <- as.data.frame(MOMI_Data_P2) %>% filter(MVM_01 == "1")
#Step 2a part 2 - creating MOMI
MOMI <- data.frame(c("MOMI"),c("US"),c("A"),nrow(MOMI_Data_P2_MVM_Control),mean(MOMI_Data_P2_MVM_Control$PlacentaWeight),
                   sd(MOMI_Data_P2_MVM_Control$PlacentaWeight),mean(MOMI_Data_P2_MVM_Control$WksGestation),sd(MOMI_Data_P2_MVM_Control$WksGestation),
                   nrow(MOMI_Data_P2_MVM_Case),mean(MOMI_Data_P2_MVM_Case$PlacentaWeight),sd(MOMI_Data_P2_MVM_Case$PlacentaWeight),
                   mean(MOMI_Data_P2_MVM_Case$WksGestation),sd(MOMI_Data_P2_MVM_Case$WksGestation),mean(MOMI_Data_P2$WksGestation),
                   sd(MOMI_Data_P2$WksGestation),"MVM") %>% 
        rename("study_id"=1,"Country"=2,"A"=3,"Control_N"=4,"PW_mean_control"=5,"PW_sd_control"=6,"GA_mean_control"=7,"GA_sd_control"=8,"Case_N"=9,
               "PW_mean_case"=10,"PW_sd_case"=11,"GA_mean_case"=12,"GA_sd_case"=13,"GA_mean_all"=14,"GA_sd_all"=15,"Notes"=16)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_MVM_MOMI <- rbind(SRS_MVM,MOMI) %>% arrange(GA_mean_case)
# Step 2b - PW
SRS_MVM_MOMI <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_MVM_MOMI)
SRS_MVM_MOMI <- SRS_MVM_MOMI[!is.na(SRS_MVM_MOMI$vi),]
SRS_MVM_MOMI
# Step 3 - Meta analysis
MA_MVM_MOMI <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_MVM_MOMI )
summary(MA_MVM_MOMI)
{
  forest(MA_MVM_MOMI,
         leftcols = c("study_id", "Notes","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("MVM type", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "MVM",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}


### FVM ####
# Step 2
# Step 2a
SRS_FVM <- SRS %>% filter(Outcome == "FVM") %>% filter(Placenta == "PW") %>% select(1,25,4,6,8:24)
(SRS_FVM)
dim(SRS_FVM)
table(SRS_FVM$Country)
#SRS_FVM %>% filter(Country == "0")
table(SRS_FVM$Placenta)


## PW
# Step 2b - PW
SRS_FVM_PW <- SRS_FVM %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_FVM_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_FVM_PW)
SRS_FVM_PW <- SRS_FVM_PW[!is.na(SRS_FVM_PW$vi),]
head(SRS_FVM_PW)
dim(SRS_FVM_PW)
table(is.na(SRS_FVM_PW$GA_mean_all))
# Step 3 - Meta analysis
MA_FVM_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_FVM_PW )
summary(MA_FVM_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FVM PW.png", width = 4000, height = 1500, res = 300)
{
  forest(MA_FVM_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "FVM",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


# Adding MOMI
#Step 2a part 1 - importing MOMI
MOMI_Data_P2 <- read_csv("~/Documents/Placenta/Project 2 - Non Linear Pathology/MOMI_Data_P2.csv")
MOMI_Data_P2_FVM_Control <- as.data.frame(MOMI_Data_P2) %>% filter(FVM_01 == "0")
MOMI_Data_P2_FVM_Case <- as.data.frame(MOMI_Data_P2) %>% filter(FVM_01 == "1")
#Step 2a part 2 - creating MOMI
MOMI_FVM <- data.frame(c("MOMI"),c("US"),c("A"),nrow(MOMI_Data_P2_FVM_Control),mean(MOMI_Data_P2_FVM_Control$PlacentaWeight),
                   sd(MOMI_Data_P2_FVM_Control$PlacentaWeight),mean(MOMI_Data_P2_FVM_Control$WksGestation),sd(MOMI_Data_P2_FVM_Control$WksGestation),
                   nrow(MOMI_Data_P2_FVM_Case),mean(MOMI_Data_P2_FVM_Case$PlacentaWeight),sd(MOMI_Data_P2_FVM_Case$PlacentaWeight),
                   mean(MOMI_Data_P2_FVM_Case$WksGestation),sd(MOMI_Data_P2_FVM_Case$WksGestation),mean(MOMI_Data_P2$WksGestation),
                   sd(MOMI_Data_P2$WksGestation),"FVM") %>% 
  rename("study_id"=1,"Country"=2,"A"=3,"Control_N"=4,"PW_mean_control"=5,"PW_sd_control"=6,"GA_mean_control"=7,"GA_sd_control"=8,"Case_N"=9,
         "PW_mean_case"=10,"PW_sd_case"=11,"GA_mean_case"=12,"GA_sd_case"=13,"GA_mean_all"=14,"GA_sd_all"=15,"Notes"=16)
SRS_FVM_MOMI <- rbind(SRS_FVM %>% select(-c(BW_mean_case,BW_sd_case,BW_mean_control,BW_sd_control,study_n,N,Placenta)),MOMI_FVM %>% select(-c(A,Notes))) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
# Step 2b - PW
SRS_FVM_MOMI <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_FVM_MOMI)
SRS_FVM_MOMI <- SRS_FVM_MOMI[!is.na(SRS_FVM_MOMI$vi),]
SRS_FVM_MOMI
# Step 3 - Meta analysis
MA_FVM_MOMI <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                        n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                        studlab = study_id, data = SRS_FVM_MOMI )
summary(MA_FVM_MOMI)
{
  forest(MA_FVM_MOMI,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "FVM",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = FALSE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}


### AI ####
# Step 2
# Step 2a
SRS_AI <- data.frame(Systematic_review_search[c(-1),c(1:13,22,23,32,33,42:44,53,54,63,64,75,76,73,78)])   %>%
  rename("Pub"=4,"Placenta"=7,"Total_N"=10,"Control_N"=11,"Case_N"=18, "PW_mean_control"=12,"PW_sd_control"=13,
         "BW_mean_control"=14,"BW_sd_control"=15,"GA_mean_control"=16, "GA_sd_control"=17,"PW_mean_case"=19,"PW_sd_case"=20,
         "BW_mean_case"=21,"BW_sd_case"=22,"GA_mean_case"=23, "GA_sd_case"=24,"study_n"=1,"GA_mean_all"=25,"GA_sd_all"=26) %>%
  filter(Measure == "Mean" | Measure == "Median") %>% filter(Outcome == "AI") %>% filter(Placenta == "PW")  %>% 
  separate(Authors, c("study_i", "bar"), ",") %>%  separate(study_i, c("study_i", "j"), " and") %>% filter(is.na(Repeat)) %>% 
  select(-c("bar", "j", "study_n", "Measure","Outcome","Title","Repeat"))
SRS_AI$study_i <- gsub(".*[.] ","",SRS_AI$study_i)
SRS_AI <- SRS_AI %>% mutate(study_id = paste0(study_i, " et al. ", Pub)) %>% select(-c("study_i")) %>% arrange(study_id) %>%
  select(c("study_id","Country","A","Control_N","PW_mean_control","PW_sd_control","GA_mean_control","GA_sd_control",
           "Case_N","PW_mean_case","PW_sd_case","GA_mean_case","GA_sd_case","GA_mean_all","GA_sd_all","Notes"))  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_AI[c(4:15)] <- as.numeric(unlist(SRS_AI[c(4:15)]))
(SRS_AI)
dim(SRS_AI)
table(SRS_AI$Country)
#SRS_AI %>% filter(Country == "0")


## PW
# Step 2b - PW
SRS_AI_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_AI)
SRS_AI_PW <- SRS_AI_PW[!is.na(SRS_AI_PW$vi),]
# Step 3 - Meta analysis
MA_AI_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_AI_PW )
summary(MA_AI_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("AI PW.png", width = 4000, height = 1500, res = 300)
{
  forest(MA_AI_PW,
         leftcols = c("study_id", "Notes","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("AI type", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "AI",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


# Adding MOMI
#Step 2a part 1 - importing MOMI
MOMI_Data_P2 <- read_csv("~/Documents/Placenta/Project 2 - Non Linear Pathology/MOMI_Data_P2.csv")
MOMI_Data_P2_AI_Control <- as.data.frame(MOMI_Data_P2) %>% filter(AI_01 == "0")
MOMI_Data_P2_AI_Case <- as.data.frame(MOMI_Data_P2) %>% filter(AI_01 == "1")
#Step 2a part 2 - creating MOMI
MOMI <- data.frame(c("MOMI"),c("US"),c("A"),nrow(MOMI_Data_P2_AI_Control),mean(MOMI_Data_P2_AI_Control$PlacentaWeight),
                   sd(MOMI_Data_P2_AI_Control$PlacentaWeight),mean(MOMI_Data_P2_AI_Control$WksGestation),sd(MOMI_Data_P2_AI_Control$WksGestation),
                   nrow(MOMI_Data_P2_AI_Case),mean(MOMI_Data_P2_AI_Case$PlacentaWeight),sd(MOMI_Data_P2_AI_Case$PlacentaWeight),
                   mean(MOMI_Data_P2_AI_Case$WksGestation),sd(MOMI_Data_P2_AI_Case$WksGestation),mean(MOMI_Data_P2$WksGestation),
                   sd(MOMI_Data_P2$WksGestation),"AI") %>% 
  rename("study_id"=1,"Country"=2,"A"=3,"Control_N"=4,"PW_mean_control"=5,"PW_sd_control"=6,"GA_mean_control"=7,"GA_sd_control"=8,"Case_N"=9,
         "PW_mean_case"=10,"PW_sd_case"=11,"GA_mean_case"=12,"GA_sd_case"=13,"GA_mean_all"=14,"GA_sd_all"=15,"Notes"=16)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_AI_MOMI <- rbind(SRS_AI,MOMI) %>% arrange(GA_mean_case)
# Step 2b - PW
SRS_AI_MOMI <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_AI_MOMI)
SRS_AI_MOMI <- SRS_AI_MOMI[!is.na(SRS_AI_MOMI$vi),]
SRS_AI_MOMI
# Step 3 - Meta analysis
MA_AI_MOMI <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                        n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                        studlab = study_id, data = SRS_AI_MOMI )
summary(MA_AI_MOMI)
{
  forest(MA_AI_MOMI,
         leftcols = c("study_id", "Notes","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("AI type", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "AI",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}

#
### CI ####
# Step 2
# Step 2a
SRS_CI <- data.frame(Systematic_review_search[c(-1),c(1:13,22,23,32,33,42:44,53,54,63,64,75,76,73,78)])   %>%
  rename("Pub"=4,"Placenta"=7,"Total_N"=10,"Control_N"=11,"Case_N"=18, "PW_mean_control"=12,"PW_sd_control"=13,
         "BW_mean_control"=14,"BW_sd_control"=15,"GA_mean_control"=16, "GA_sd_control"=17,"PW_mean_case"=19,"PW_sd_case"=20,
         "BW_mean_case"=21,"BW_sd_case"=22,"GA_mean_case"=23, "GA_sd_case"=24,"study_n"=1,"GA_mean_all"=25,"GA_sd_all"=26) %>%
  filter(Measure == "Mean" | Measure == "Median") %>% filter(Outcome == "CI") %>% filter(Placenta == "PW")  %>% 
  separate(Authors, c("study_i", "bar"), ",") %>%  separate(study_i, c("study_i", "j"), " and") %>% filter(is.na(Repeat)) %>% 
  select(-c("bar", "j", "study_n", "Measure","Outcome","Title","Repeat"))
SRS_CI$study_i <- gsub(".*[.] ","",SRS_CI$study_i)
SRS_CI <- SRS_CI %>% mutate(study_id = paste0(study_i, " et al. ", Pub)) %>% select(-c("study_i")) %>% arrange(study_id) %>%
  select(c("study_id","Country","A","Control_N","PW_mean_control","PW_sd_control","GA_mean_control","GA_sd_control",
           "Case_N","PW_mean_case","PW_sd_case","GA_mean_case","GA_sd_case","GA_mean_all","GA_sd_all","Notes"))  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_CI[c(4:15)] <- as.numeric(unlist(SRS_CI[c(4:15)]))
(SRS_CI)
dim(SRS_CI)
table(SRS_CI$Country)
#SRS_CI %>% filter(Country == "0")


## PW
# Step 2b - PW
SRS_CI_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_CI)
SRS_CI_PW <- SRS_CI_PW[!is.na(SRS_CI_PW$vi),]
# Step 3 - Meta analysis
MA_CI_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_CI_PW )
summary(MA_CI_PW)
# Step 4 - Forest plot
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("CI PW.png", width = 4000, height = 1500, res = 300)
{
  forest(MA_CI_PW,
         leftcols = c("study_id", "Notes","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("CI type", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "CI",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


# Adding MOMI
#Step 2a part 1 - importing MOMI
MOMI_Data_P2 <- read_csv("~/Documents/Placenta/Project 2 - Non Linear Pathology/MOMI_Data_P2.csv")
MOMI_Data_P2_CI_Control <- as.data.frame(MOMI_Data_P2) %>% filter(CI_01 == "0")
MOMI_Data_P2_CI_Case <- as.data.frame(MOMI_Data_P2) %>% filter(CI_01 == "1")
#Step 2a part 2 - creating MOMI
MOMI <- data.frame(c("MOMI"),c("US"),c("A"),nrow(MOMI_Data_P2_CI_Control),mean(MOMI_Data_P2_CI_Control$PlacentaWeight),
                   sd(MOMI_Data_P2_CI_Control$PlacentaWeight),mean(MOMI_Data_P2_CI_Control$WksGestation),
                   sd(MOMI_Data_P2_CI_Control$WksGestation),nrow(MOMI_Data_P2_CI_Case),
                   mean(MOMI_Data_P2_CI_Case$PlacentaWeight),sd(MOMI_Data_P2_CI_Case$PlacentaWeight),
                   mean(MOMI_Data_P2_CI_Case$WksGestation),sd(MOMI_Data_P2_CI_Case$WksGestation),
                   mean(MOMI_Data_P2$WksGestation), sd(MOMI_Data_P2$WksGestation),"CI") %>% 
  rename("study_id"=1,"Country"=2,"A"=3,"Control_N"=4,"PW_mean_control"=5,"PW_sd_control"=6,"GA_mean_control"=7,
         "GA_sd_control"=8,"Case_N"=9,
         "PW_mean_case"=10,"PW_sd_case"=11,"GA_mean_case"=12,"GA_sd_case"=13,"GA_mean_all"=14,"GA_sd_all"=15,"Notes"=16)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case)
SRS_CI_MOMI <- rbind(SRS_CI,MOMI) %>% arrange(GA_mean_case)
# Step 2b - PW
SRS_CI_MOMI <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_CI_MOMI)
SRS_CI_MOMI <- SRS_CI_MOMI[!is.na(SRS_CI_MOMI$vi),]
SRS_CI_MOMI
# Step 3 - Meta analysis
MA_CI_MOMI <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                        n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                        studlab = study_id, data = SRS_CI_MOMI )
summary(MA_CI_MOMI)
{
  forest(MA_CI_MOMI,
         leftcols = c("study_id", "Notes","N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("CI type", "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental Weight (g)", smlab = "CI",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}

### Summary ####

# Supp Table 1
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
Table_1 <- unique(SRS %>% select("study_id","Country","Outcome","Case_N","GA_mean_case","GA_sd_case","Control_N",
                                 "GA_mean_control","GA_sd_control")) %>% mutate(study_p = paste0(study_id,"_",Outcome))
head(Table_1)
dim(Table_1)
nrow(unique(Table_1[c(1,2)]))
TB <- data.frame(matrix(nrow=1,ncol=7))%>%rename(study_id=1,Country=2,Outcome=3,Case_N=4,GA_case=5,Control_N=6,GA_control=7)
for (i in 1:nrow(unique(Table_1[c(1)]))) {
  j <- unique(Table_1[c(10)])[c(i),]
  TB[c(i),1] <- (Table_1 %>% filter(study_p == j))[c(1)]
  TB[c(i),2] <- (Table_1 %>% filter(study_p == j))[c(2)]
  TB[c(i),3] <- as.character(as.list((Table_1 %>% filter(study_p == j))[c(3)]))
  TB[c(i),4] <- (Table_1 %>% filter(study_p == j))[c(4)]
  TB[c(i),5] <- ifelse((Table_1 %>% filter(study_p == j))[c(5)]<0,"NA",
                       paste0(format(round(as.numeric((Table_1 %>% filter(study_p == j))[c(5)]),2),nsmall=1)," ± ",
                       format(round(as.numeric((Table_1 %>% filter(study_p == j))[c(6)]),2),nsmall=1)) )
  TB[c(i),6] <- (Table_1 %>% filter(study_p == j))[c(7)]
  TB[c(i),7] <- ifelse((Table_1 %>% filter(study_p == j))[c(8)]<0,"NA",
                       paste0(format(round(as.numeric((Table_1 %>% filter(study_p == j))[c(8)]),2),nsmall=1)," ± ",
                              format(round(as.numeric((Table_1 %>% filter(study_p == j))[c(9)]),2),nsmall=1)) )
}
unique(TB) -> TB
head(TB)
dim(TB)
write.csv(TB[c(-2)],"Table1 all studies.csv")


#run PE Onset code
# Table 1: Mean +- SD
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
P <- list(MA_PE_PW,MA_PE_BWPW, MA_PE_PWBW, MA_FGR_PW, MA_FGR_BWPW, MA_FGR_PWBW, MA_SGA_PW, MA_SGA_BWPW, MA_SGA_PWBW,
          MA_LBW_PW, MA_LBW_BWPW, MA_LBW_PWBW,MA_PTB_PW,MA_MVM_PW, MA_FVM_PW,MA_AI_PW,MA_CI_PW, MA_Stillbirth_PW,
          MA_Stillbirth_BWPW, MA_Stillbirth_PWBW,PE_PW_EON, PE_PW_LON,MA_HDP_PW,MA_HDP_BWPW,MA_GH_PW,MA_PP_PW,MA_PA_PW)
Q <- data.frame(c(rep("PE",3),rep("FGR",3),rep("SGA",3),rep("LBW",3),rep("PTB",1),rep("MVM",1),rep("FVM",1),rep("AI",1),
                     rep("CI",1),rep("Stillbirth",3),"PE early onset","PE late onset",rep("HDP",2),"GH","PP","PA"),
                c(rep(c("Placental Weight","BW:PW ratio","PW:BW ratio"),4),rep("Placental Weight",5),"Placental Weight",
                     "BW:PW ratio","PW:BW ratio",rep("Placental Weight",3),"BW:PW ratio",rep("Placental Weight",3)),
                c(rep("APO",13),rep("Path",4),rep("APO",10)) ) %>% rename("Outcome"=1,"Placenta Measure"=2,"Type"=3)
R <- data.frame(matrix(ncol=10)) %>% 
  rename(Mean=1,SD=2,UCI=4,LCI=3,"N studies"=5,"Mean ± SD"=8,"Mean (95%CI)"=7,"P value"=6,"I2"=9,"N"=10)
for (i in P) {
  Mean <- format(round(i$TE.random,2), nsmall = 2)
  N <- as.numeric(i$k.all)
  UCI <- format(round(i$upper.random,2), nsmall = 2)
  LCI <- format(round(i$lower.random,2), nsmall = 2)
  SE <- (i$upper.random - i$lower.random)/(2*qnorm(0.975))
  SD <- SE * sqrt(N)
  SD <- format(round(SD,2), nsmall = 2)
  I <- i$I2 *100
  R[nrow(R)+1,1] <- Mean
  R[nrow(R),2] <- SD
  R[nrow(R),3] <- LCI
  R[nrow(R),4] <- UCI
  R[nrow(R),5] <- as.numeric(N)
  R[nrow(R),6] <- format(round(i$pval.random,3),nsmall=3)
  R[,c(6)] <- ifelse(R[,c(6)] < 0.001,"<0.001",R[,c(6)])
  R[nrow(R),7] <- paste0(Mean," (",LCI,",",UCI,")")
  R[nrow(R),8] <- paste0(Mean," ± ",SD)
  R[nrow(R),9] <- format(round(I,1),nsmall=1)
  R[nrow(R),10] <- format(as.numeric(i$n.c.pooled + i$n.e.pooled),big.mark=",")
}
Q$Outcome <- factor(Q$Outcome, levels=(c("Stillbirth","HDP","GH","PE","PE early onset","PE late onset","PTB","FGR",
                                         "SGA","LBW","PP","PA","MVM","FVM","AI","CI")))
Table2 <- unique(cbind(Q,R[-c(1),])%>% arrange(Outcome) %>% select(-c("Type","Mean ± SD"))) 
Table2$`N studies` <- as.numeric(Table2$`N studies`)
Table2[c(1,2,7,11,10,9,8)]
write.csv(Table2[c(1,2,7,11,10,9,8)], file = "Table2.csv")

# Saving data
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")
save(SRS_PE, SRS_PE_PW, SRS_PE_BWPW, SRS_PE_PWBW, MA_PE_PW, MA_PE_BWPW, MA_PE_PWBW,
     SRS_FGR, SRS_FGR_PW, SRS_FGR_BWPW, SRS_FGR_PWBW, MA_FGR_PW, MA_FGR_BWPW, MA_FGR_PWBW,
     SRS_SGA, SRS_SGA_PW, SRS_SGA_BWPW, SRS_SGA_PWBW, MA_SGA_PW, MA_SGA_BWPW, MA_SGA_PWBW,
     SRS_LBW, SRS_LBW_PW, SRS_LBW_BWPW, SRS_LBW_PWBW, MA_LBW_PW, MA_LBW_BWPW, MA_LBW_PWBW,
     SRS_HDP, SRS_HDP_PW, SRS_HDP_BWPW, SRS_HDP_PWBW, MA_HDP_PW, MA_HDP_BWPW, MA_HDP_PWBW,
     SRS_PTB, SRS_PTB_PW,MA_PTB_PW, SRS_MVM, SRS_MVM_PW, MA_MVM_PW, SRS_FVM, SRS_FVM_PW, MA_FVM_PW,
     SRS_AI, SRS_AI_PW, MA_AI_PW, SRS_CI, SRS_CI_PW, MA_CI_PW, SRS_Stillbirth, SRS_Stillbirth_PW, 
     SRS_Stillbirth_PWBW, MA_Stillbirth_PW, MA_Stillbirth_BWPW, MA_Stillbirth_PWBW, SRS, SRS_Stillbirth_BWPW,
     SRS_PA, SRS_PA_PW, MA_PA_PW, SRS_PP, SRS_PP_PW, MA_PP_PW,
     SRS, SRS_ON, SRS_EON, PE_PW_EON, SRS_LON, PE_PW_LON, SRS_CON, PE_CON, Table_1, Table2,
     file="Project1.RData")


#EPPEC Poster forest plot
#uses Table 3 and new figure
library(ggforestplot)
(S_fp <- rbind( ((S %>% filter(`Placenta Measure` == "Placental Weight"))[c(1,3:8,11,9)] %>% mutate(Population="All") %>%
  mutate(SE=as.numeric(as.numeric(SD)/sqrt(as.numeric(`N studies`)))) )[c(1,10,2,11,4:8)], 
  (MA_GA_APOs %>% mutate(SE=(as.numeric(upper)-as.numeric(lower))/(2*1.96)) %>% mutate(Population="GA Matched") %>% 
     rename(Mean=beta,I2=I2_GAM,"N studies"=n_GAM,"P value"=p_GAM,"UCI"=upper,
            "LCI"=lower))[c(1,13,9,12,10,11,4,5,3)] ) %>%
 mutate(Type=ifelse(APO=="MVM","Path",ifelse(APO=="FVM","Path",ifelse(APO=="AI","Path",ifelse(APO=="CI","Path","APO"))))) %>%
    arrange(Type,APO,(Population)) %>% mutate(Population=factor(Population,levels = c("GA Matched","All"))) %>%
    mutate("Info"= paste0(format(round(as.numeric(Mean),0),nsmall=0),"g (", format(round(as.numeric(LCI),0),nsmall=0),"g,",
                         format(round(as.numeric(UCI),0),nsmall=0),"g); N = ",`N studies`,"; I^2 = ",I2,"%")))
(plot1<-forestplot(df = S_fp, name = APO, estimate = as.numeric(Mean), se = SE, pvalue = `P value`, col = Population, 
           xlab="Mean Difference in Placental Weight (g)", leftcols = c("APO", "N studies"), xlim=c(-220,175)) +
 geom_text(aes(x=60,y=rev(c(0.75,1.25,2,3,4,4.75,5.25,5.75,6.25,7,8,seq(8.75,12.25,0.5))),label=S_fp$Info), 
             hjust = 0,cex=2.7) + theme_test() + scale_colour_manual(values = c("All" = "black","GA Matched"= "orange")) 
  ) 
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Drafts/EPPEC 2025/")
png("EPPEC forestplot.png", width = 4000, height = 2750, res = 450)
plot1
dev.off()



