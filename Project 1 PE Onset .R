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

#### Step 1 ####
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")
Systematic_review_search <- read_excel("Systematic review search.xlsx", sheet = "Meta Analysis 2.0")
SRS_ON <- data.frame(Systematic_review_search[c(-1),c(1:13,22,23,32,33,42:44,53,54,63,64,75,76,73,78)])   %>%
  rename("Pub"=4,"Placenta"=7,"Total_N"=10,"Control_N"=11,"Case_N"=18, "PW_mean_control"=12,"PW_sd_control"=13,
         "BW_mean_control"=14,"BW_sd_control"=15,"GA_mean_control"=16, "GA_sd_control"=17,"PW_mean_case"=19,"PW_sd_case"=20,
         "BW_mean_case"=21,"BW_sd_case"=22,"GA_mean_case"=23, "GA_sd_case"=24,"study_n"=1,"GA_mean_all"=25,"GA_sd_all"=26) %>%
  filter(Measure == "Mean" | Measure == "Median") %>% filter(Outcome == "Preeclampsia")  %>% 
  separate(Authors, c("study_i", "bar"), ",") %>%  separate(study_i, c("study_i", "j"), " and") %>% filter(is.na(Repeat)) %>% 
  select(-c("bar", "j", "study_n", "Measure","Outcome","Title","Repeat"))
SRS_ON$study_i <- gsub(".*[.] ","",SRS_ON$study_i)
SRS_ON <- SRS_ON %>% mutate(study_id = paste0(study_i, " et al. ", Pub)) %>% select(-c("study_i")) %>% arrange(study_id) %>%
  select(c("study_id","Country","A","Control_N","PW_mean_control","PW_sd_control","GA_mean_control","GA_sd_control",
           "Case_N","PW_mean_case","PW_sd_case","GA_mean_case","GA_sd_case","GA_mean_all","GA_sd_all","Notes","Placenta"))
table(SRS_ON$Placenta)
SRS_ON[c(4:15)] <- as.numeric(unlist(SRS_ON[c(4:15)]))


#### Early onset ####
SRS_EON_ids_A <- unique(SRS_ON %>% filter(Notes == "early onset PE", is.na(A), Placenta=="PW") %>% select("study_id")) 
SRS_EON_ids_B <- unique(SRS_ON %>% filter(Notes == "early onset PE", !is.na(A), Placenta=="PW") %>% select("study_id"))
dim(unique(SRS_ON %>% filter(Notes == "early onset PE", Placenta=="PW") %>% select("study_id")))
dim(unique(SRS_ON %>% filter(Notes == "early onset PE", Placenta=="BW:PW" | Placenta=="PW:BW") %>% select("study_id")))
SRS_EON<- data.frame(matrix(NA, nrow = 1, ncol = 12)) %>% rename(study_id=1,Country=2,Control_N=3, PW_mean_control=4,
               Case_N=8,PW_sd_control=5,GA_mean_control=6,GA_sd_control=7,PW_mean_case=9,PW_sd_case=10, GA_mean_case=11,
               GA_sd_case=12)
for (i in 1:nrow(SRS_EON_ids_A)) {
  SRS_EON[c(i),c(1)] <- SRS_EON_ids_A[c(i),]
  SRS_EON[c(i),c(2:7)]  <- SRS_ON %>% filter(study_id==SRS_EON_ids_A[c(i),], A =="A", Placenta=="PW") %>% select(2,4:8)
  a <- max(SRS_ON %>% filter(study_id==SRS_EON_ids_A[c(i),], Notes == "early onset PE", Placenta=="PW") %>% select(Case_N))
  SRS_EON[c(i),c(8:12)] <- SRS_ON %>% filter(study_id==SRS_EON_ids_A[c(i),], Notes == "early onset PE", Case_N==a, 
                                             Placenta=="PW") %>% select(9:13)
}
for (i in 1:nrow(SRS_EON_ids_B)) {
  SRS_EON[c(i+nrow(SRS_EON_ids_A)),c(1)] <- SRS_EON_ids_B[c(i),]
  SRS_EON[c(i+nrow(SRS_EON_ids_A)),c(2:12)]  <- SRS_ON %>% 
    filter(study_id==SRS_EON_ids_B[c(i),], !is.na(A), Placenta=="PW", Notes == "early onset PE") %>% select(2,4:13)
}
SRS_EON <- SRS_EON %>% mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N, " / ",Total_N)) %>% 
  arrange(GA_mean_case)
SRS_EON
#SRS_EON <- SRS_EON %>% mutate(GA_diff=GA_mean_control-GA_mean_case) %>% arrange(GA_diff)
PE_PW_EON <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control, 
                      studlab = study_id, data = SRS_EON )
summary(PE_PW_EON)
{
  forest(PE_PW_EON,
         leftcols = c("study_id",  "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N", "Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)", 
         print.tau2 = FALSE, print.Q = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue",col.square = "steelblue",col.inside = "black")
}


#### Late onset ####
SRS_LON_ids_A <- unique(SRS_ON %>% filter(Notes == "late onset PE", is.na(A), Placenta=="PW") %>% select("study_id"))
SRS_LON_ids_B <- unique(SRS_ON %>% filter(Notes == "late onset PE", !is.na(A), Placenta=="PW") %>% select("study_id"))
dim(unique(SRS_ON %>% filter(Notes == "late onset PE", Placenta=="PW") %>% select("study_id")))
dim(unique(SRS_ON %>% filter(Notes == "late onset PE", Placenta=="BW:PW" | Placenta=="PW:BW") %>% select("study_id")))
SRS_LON<- data.frame(matrix(NA, nrow = 1, ncol = 11)) %>% rename(study_id=1,Control_N=2,PW_mean_control=3,Case_N=7,
                PW_sd_control=4,GA_mean_control=5,GA_sd_control=6,PW_mean_case=8,PW_sd_case=9,GA_mean_case=10,
                GA_sd_case=11)
for (i in 1:nrow(SRS_LON_ids_A)) {
  SRS_LON[c(i),c(1)] <- SRS_LON_ids_A[c(i),]
  SRS_LON[c(i),c(2:6)]  <- SRS_ON %>% filter(study_id==SRS_LON_ids_A[c(i),], A =="A", Placenta=="PW") %>% select(4:8)
  a <- max(SRS_ON %>% filter(study_id==SRS_LON_ids_A[c(i),], Notes == "late onset PE", Placenta=="PW") %>% select(Case_N))
  SRS_LON[c(i),c(7:11)] <- SRS_ON %>% filter(study_id==SRS_LON_ids_A[c(i),], Notes == "late onset PE", Case_N==a, Placenta=="PW") %>%
    select(9:13)
}
for (i in 1:nrow(SRS_LON_ids_B)) {
  SRS_LON[c(i+nrow(SRS_LON_ids_A)),c(1)] <- SRS_LON_ids_B[c(i),]
  SRS_LON[c(i+nrow(SRS_LON_ids_A)),c(2:11)]  <- SRS_ON %>% filter(study_id==SRS_LON_ids_B[c(i),], !is.na(A), 
                                                          Notes == "late onset PE", Placenta=="PW") %>% select(4:13)
}
SRS_LON <- SRS_LON %>% mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N, " / ",Total_N)) %>% 
  arrange(GA_mean_case)
SRS_LON
(SRS_LON %>% mutate(PW_diff = PW_mean_control - PW_mean_case))[c(1,13,10,5,14)]
#SRS_LON <- SRS_LON %>% mutate(GA_diff=GA_mean_control-GA_mean_case) %>% arrange(GA_diff)
PE_PW_LON <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control, 
                      studlab = study_id, data = SRS_LON )
summary(PE_PW_LON)
{
  forest(PE_PW_LON,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c("Cases N / Total N", "Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)",
         print.tau2 = FALSE, print.Q = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         col.diamond = "steelblue",
         col.diamond.lines = "steelblue",
         col.square = "steelblue",
         col.inside = "black")
}


#### Combined ####
SRS_EON$Onset <- "early onset PE"
SRS_LON$Onset <- "late onset PE"
SRS_CON <- rbind(SRS_EON,SRS_LON)
SRS_CON$Onset <- factor(SRS_CON$Onset, levels = c("early onset PE", "late onset PE"))
SRS_CON[c(4:7,9:12)] <- as.numeric(format(round(as.numeric(unlist(SRS_CON[c(4:7,9:12)])), digits=1)))
SRS_CON
PE_CON <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                   n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control, studlab = study_id, data = SRS_CON,
                   subgroup = Onset, subgroup.name = "Onset")
summary(PE_CON)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Preeclampsia PW Onset.png", width = 5750, height =4500, res = 450)
{
  forest(PE_CON,
         leftcols = c("study_id",  "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N", "Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)", print.subgroup.name = TRUE,
         print.tau2 = FALSE, print.Q = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue", col.subgroup = "black", comb.fixed = FALSE, comb.random = TRUE,
         col.diamond.lines = "steelblue", common = FALSE, col.inside = "black")
}
dev.off()


#### HDP ####
# Step 2
# Step 2a
SRS_HDP <- SRS %>% filter(Outcome == "HDP") %>% select(1,25,4,6,8:24)
head(SRS_HDP)
dim(SRS_HDP)
table(SRS_HDP$Placenta)

## PW
# Step 2b - PW
SRS_HDP_PW <- SRS_HDP %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_HDP_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                    sd2i = PW_sd_control, sd1i = PW_sd_case,
                    n2i = Control_N, n1i = Case_N, data = SRS_HDP_PW)
SRS_HDP_PW <- SRS_HDP_PW[!is.na(SRS_HDP_PW$vi),]
SRS_HDP_PW
length(unique(SRS_HDP_PW$study_id))
dim(SRS_HDP_PW)
table(is.na(SRS_HDP_PW$GA_mean_all))
# Step 3
MA_HDP_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                     n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                     studlab = study_id, data = SRS_HDP_PW )
summary(MA_HDP_PW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("HDP PW.png", width = 5500, height = 2500, res = 450)
{
  forest(MA_HDP_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)", smlab = "HDP",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#Flipping ratios
SRS_HDP_BWPW <- SRS_HDP %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
SRS_HDP_PWBW <- SRS_HDP %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21) %>%
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N))
table((rbind(data.frame(SRS_HDP_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_HDP_BWPW) %>% mutate(PW = "PWBW") ))$study_id)
Ratio_HDP_flip <-
  (rbind(data.frame(SRS_HDP_PWBW) %>% mutate(PW = "BWPW"), data.frame(SRS_HDP_BWPW) %>% mutate(PW = "PWBW") ) %>% 
     arrange(study_id)  %>% mutate(PW_1_mean_control = 1/PW_mean_control) %>% 
     filter(!study_id == "Marques et al. 2018") %>%
     mutate(PW_1_sd_control = (PW_sd_control/PW_mean_control)*PW_1_mean_control) %>% 
     select(-c("PW_mean_control","PW_sd_control")) %>% mutate(PW_1_mean_case = 1/PW_mean_case) %>% 
     mutate(PW_1_sd_case = (PW_sd_case/PW_mean_case)*PW_1_mean_case) %>% select(-c("PW_mean_case","PW_sd_case")) %>% 
     rename("PW_mean_control" = "PW_1_mean_control","PW_sd_control"="PW_1_sd_control","PW_mean_case"="PW_1_mean_case",
            "PW_sd_case"="PW_1_sd_case"))[c(1:4,15,16,5,17,18,6:14)]

## BW:PW ratio
# Step 2b - PW:BW ratio
SRS_HDP_BWPW <- SRS_HDP %>% filter(Placenta == "BW:PW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% arrange(GA_mean_case,GA_mean_control)
SRS_HDP_BWPW <- rbind(SRS_HDP_BWPW,Ratio_HDP_flip %>% filter(PW == "BWPW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_HDP_BWPW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_HDP_BWPW)
table(is.na(SRS_HDP_BWPW$GA_mean_all))
(SRS_HDP_BWPW)
# Step 3
MA_HDP_BWPW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_HDP_BWPW )
summary(MA_HDP_BWPW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("HDP BWPW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_HDP_BWPW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "BW:PW ratio", smlab = "HDP",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()


## PW:BW ratio
# Step 2b - PW:BW ratio
SRS_HDP_PWBW <- SRS_HDP %>% filter(Placenta == "PW:BW") %>% select(1:3,6:8,13:15,11,12,18:21)  %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) 
SRS_HDP_PWBW <- rbind(SRS_HDP_PWBW,Ratio_HDP_flip %>% filter(PW == "PWBW") %>% select(-c("PW"))) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_HDP_PWBW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                       sd2i = PW_sd_control, sd1i = PW_sd_case,
                       n2i = Control_N, n1i = Case_N, data = SRS_HDP_PWBW)
table(is.na(SRS_HDP_PWBW$vi))
(SRS_HDP_PWBW)
# Step 3
MA_HDP_PWBW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control, studlab = study_id, data = SRS_HDP_PWBW )
summary(MA_HDP_PWBW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("HDP PWBW.png", width = 3500, height = 1500, res = 250)
{
  forest(MA_HDP_PWBW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "PW:BW ratio", smlab = "HDP",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

#

#### GH ####
SRS_GH <- SRS %>% filter(Outcome == "GH") %>% select(1,25,4,6,8:24)
(SRS_GH)
dim(SRS_GH)
table(SRS_GH$Placenta)

## PW
# Step 2b - PW
SRS_GH_PW <- SRS_GH %>% filter(Placenta == "PW") %>% select(1:3,6:8,13:15,11,12,18:21) %>% 
  mutate(Total_N=Case_N+Control_N) %>% mutate(N=paste0(Case_N," / ",Total_N)) %>% 
  arrange(GA_mean_case,GA_mean_control,study_id)
SRS_GH_PW <- escalc(measure = "MD", m2i = PW_mean_control, m1i = PW_mean_case,
                     sd2i = PW_sd_control, sd1i = PW_sd_case,
                     n2i = Control_N, n1i = Case_N, data = SRS_GH_PW)
SRS_GH_PW <- SRS_GH_PW[!is.na(SRS_GH_PW$vi),]
SRS_GH_PW
length(unique(SRS_GH_PW$study_id))
dim(SRS_GH_PW)
table(is.na(SRS_GH_PW$GA_mean_all))
# Step 3
MA_GH_PW <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case,
                      n.c = Control_N, mean.c = PW_mean_control, sd.c = PW_sd_control,
                      studlab = study_id, data = SRS_GH_PW )
summary(MA_GH_PW)
# Step 4
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("GH PW.png", width = 5500, height = 2000, res = 450)
{
  forest(MA_GH_PW,
         leftcols = c("study_id", "N", "GA_mean_case","GA_mean_control"),
         leftlabs = c( "Cases N / Total N","Gestational Age \n(Cases)", "Gestational Age \n(Control)"),
         xlab = "Placental weight (g)", smlab = "GH",
         comb.fixed = FALSE, comb.random = TRUE, common = FALSE,
         print.Q = FALSE, print.tau2 = FALSE, print.pval.Q = TRUE, print.I2 = TRUE, print.pval.I2 = FALSE,
         col.diamond = "steelblue",col.diamond.lines = "steelblue", col.inside = "black")
}
dev.off()

nrow(unique((SRS %>% filter(Placenta=="PW") %>% filter(Outcome=="Preeclampsia"|Outcome=="HDP"|Outcome=="GH"))[c(1)] ))
{sum((unique((SRS %>% filter(Placenta=="PW") %>% 
                filter(Outcome=="Preeclampsia"|Outcome=="HDP"|Outcome=="GH"))[c(1,9:15)]))$Control_N) +
    sum((unique(merge(anti_join(unique( (SRS %>% filter(Placenta=="PW") %>% 
                                           filter(Outcome=="Preeclampsia"|Outcome == "GH"))[c(1)] ),
          unique( (SRS %>% filter(Placenta == "PW") %>% filter(Outcome == "HDP"))[c(1)] ) ),
      unique( (SRS %>% filter(Placenta == "PW") %>% 
                 filter(Outcome == "Preeclampsia" | Outcome == "GH"))[c(1,16:22)] ) )))$Case_N) }

#
#### PIH ####
SRS_PIH <- SRS %>% filter(Outcome == "PIH") %>% select(1,25,4,6,8:24)
(SRS_PIH)
dim(SRS_PIH)
table(SRS_PIH$Placenta)
