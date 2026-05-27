library(meta)
library(metafor)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/")
load(file = "Project1.RData")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### Gestational Age of Cases ####
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Preeclampsia ####

dim(SRS_PE_PW)
head(SRS_PE_PW)
MA_PE_PW
metareg.PE.PW.GA <- metareg(MA_PE_PW, GA_mean_case, hakn = TRUE) 
metareg.PE.PW.GA$I2
metareg.PE.PW.GA$pval[c(2)]
metareg.PE.PW.GA$b[c(2)]
bubble(metareg.PE.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
             cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Preeclampsia MR PW.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PE.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         xlim = c(28,41))
}
dev.off()


dim(SRS_PE_BWPW)
head(SRS_PE_BWPW)
MA_PE_BWPW
metareg.PE.BWPW.GA <- metareg(MA_PE_BWPW, GA_mean_case, hakn = TRUE) 
metareg.PE.BWPW.GA$I2
metareg.PE.BWPW.GA$pval[c(2)]
exp(metareg.PE.BWPW.GA$b[c(2)])
bubble(metareg.PE.BWPW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_PE_PWBW)
head(SRS_PE_PWBW)
MA_PE_PWBW
metareg.PE.PWBW.GA <- metareg(MA_PE_PWBW, GA_mean_case, hakn = TRUE) 
metareg.PE.PWBW.GA$I2
metareg.PE.PWBW.GA$pval[c(2)]
metareg.PE.PWBW.GA$b[c(2)]
bubble(metareg.PE.PWBW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_PE <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.PE.PW.GA$pval[c(2)], metareg.PE.BWPW.GA$pval[c(2)],
                                                metareg.PE.PWBW.GA$pval[c(2)]),
                      c(metareg.PE.PW.GA$b[c(2)],metareg.PE.BWPW.GA$b[c(2)],metareg.PE.PWBW.GA$b[c(2)]),
                      c(metareg.PE.PW.GA$se[c(2)],metareg.PE.BWPW.GA$se[c(2)],metareg.PE.PWBW.GA$se[c(2)]),
                      c(metareg.PE.PW.GA$k,metareg.PE.BWPW.GA$k,metareg.PE.PWBW.GA$k),
                      c(metareg.PE.PW.GA$I2,metareg.PE.BWPW.GA$I2,metareg.PE.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PE$beta <- format(round(MAR_PE$beta,1),nsmall=1,scientific=FALSE)
MAR_PE$UCI <- format(round(MAR_PE$UCI,1),nsmall=1,scientific=FALSE)
MAR_PE$LCI <- format(round(MAR_PE$LCI,1),nsmall=1,scientific=FALSE)
MAR_PE$i2 <- format(round(MAR_PE$i2,1),nsmall=1,scientific=FALSE)
MAR_PE$pval <- ifelse(MAR_PE$pval < 0.001, "<0.001",format(round(MAR_PE$pval,3), nsmall = 3, scientific = FALSE))
MAR_PE$beta_95CI <- paste0(MAR_PE$beta," (",MAR_PE$LCI,", ",MAR_PE$UCI,")")
MAR_PE[c(1,5,6,9,2)] 

#### Preeclampsia Onset ####

dim(SRS_EON)
(SRS_EON)
PE_PW_EON
(metareg.PE.EON.PW.GA <- metareg(PE_PW_EON, GA_mean_case, hakn = TRUE) )
metareg.PE.EON.PW.GA$k
metareg.PE.EON.PW.GA$pval[c(2)]
metareg.PE.EON.PW.GA$b[c(2)]
bubble(metareg.PE.EON.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_LON)
(SRS_LON)
PE_PW_LON
(metareg.PE.LON.PW.GA <- metareg(PE_PW_LON, GA_mean_case, hakn = TRUE) )
metareg.PE.LON.PW.GA$k
metareg.PE.LON.PW.GA$pval[c(2)]
metareg.PE.LON.PW.GA$b[c(2)]
bubble(metareg.PE.LON.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)



MAR_PE_ON<-(data.frame(c("PE early onset","PE late onset"),c(metareg.PE.EON.PW.GA$pval[c(2)],metareg.PE.LON.PW.GA$pval[c(2)]),
                     c(metareg.PE.EON.PW.GA$b[c(2)],metareg.PE.LON.PW.GA$b[c(2)]),c(metareg.PE.EON.PW.GA$k,
                       metareg.PE.LON.PW.GA$k), c("PW"), c(metareg.PE.EON.PW.GA$I2,metareg.PE.LON.PW.GA$I2 ),
                     c(metareg.PE.EON.PW.GA$se[c(2)],metareg.PE.LON.PW.GA$se[c(2)]) ) %>% 
              rename(Exposure=1,pval=2,beta=3,n=4,Placenta=5,i2=6,se=7)) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PE_ON$beta <- format(round(MAR_PE_ON$beta,1),nsmall=1,scientific=FALSE)
MAR_PE_ON$UCI <- format(round(MAR_PE_ON$UCI,1),nsmall=1,scientific=FALSE)
MAR_PE_ON$LCI <- format(round(MAR_PE_ON$LCI,1),nsmall=1,scientific=FALSE)
MAR_PE_ON$beta_95CI <- paste0(MAR_PE_ON$beta," (",MAR_PE_ON$LCI,", ",MAR_PE_ON$UCI,")")
MAR_PE_ON$i2 <- format(round(MAR_PE_ON$i2,1),nsmall=1,scientific=FALSE)
MAR_PE_ON$pval <- ifelse(MAR_PE_ON$pval < 0.001, "<0.001",format(round(MAR_PE_ON$pval,3), nsmall = 3, scientific = FALSE))
MAR_PE_ON[c(1,4,6,10,2,7)]



#

#### HDP ####

dim(SRS_HDP_PW)
head(SRS_HDP_PW)
MA_HDP_PW
(metareg.HDP.PW.GA <- metareg(MA_HDP_PW, GA_mean_case, hakn = TRUE) )
metareg.HDP.PW.GA$k
metareg.HDP.PW.GA$pval[c(2)]
metareg.HDP.PW.GA$b[c(2)]
bubble(metareg.HDP.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_HDP <- data.frame(c("PW"), c(metareg.HDP.PW.GA$pval[c(2)]),c(metareg.HDP.PW.GA$b[c(2)]),
                      c(metareg.HDP.PW.GA$k),c(metareg.HDP.PW.GA$I2), c(metareg.HDP.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_HDP$beta <- format(round(MAR_HDP$beta,1),nsmall=1,scientific=FALSE)
MAR_HDP$i2 <- format(round(MAR_HDP$i2,1),nsmall=1,scientific=FALSE)
MAR_HDP$pval <- ifelse(MAR_HDP$pval < 0.001, "<0.001",format(round(MAR_HDP$pval,3), nsmall = 3, scientific = FALSE))
MAR_HDP$UCI <- format(round(MAR_HDP$UCI,1),nsmall=1,scientific=FALSE)
MAR_HDP$LCI <- format(round(MAR_HDP$LCI,1),nsmall=1,scientific=FALSE)
MAR_HDP$beta_95CI <- paste0(MAR_HDP$beta," (",MAR_HDP$LCI,", ",MAR_HDP$UCI,")")
MAR_HDP[c(1,4,5,9,2)]


#

#### GH ####

dim(SRS_GH_PW)
head(SRS_GH_PW)
MA_GH_PW
(metareg.GH.PW.GA <- metareg(MA_GH_PW, GA_mean_case, hakn = TRUE) )
metareg.GH.PW.GA$k
metareg.GH.PW.GA$pval[c(2)]
metareg.GH.PW.GA$b[c(2)]
bubble(metareg.GH.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("GH MR PW.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.GH.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.8, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-275,0), xlim = c(28,38))
}
dev.off()


MAR_GH <- data.frame(c("PW"), c(metareg.GH.PW.GA$pval[c(2)]),c(metareg.GH.PW.GA$b[c(2)]),
                      c(metareg.GH.PW.GA$k),c(metareg.GH.PW.GA$I2), c(metareg.GH.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_GH$beta <- format(round(MAR_GH$beta,1),nsmall=1,scientific=FALSE)
MAR_GH$i2 <- format(round(MAR_GH$i2,1),nsmall=1,scientific=FALSE)
MAR_GH$pval <- ifelse(MAR_GH$pval < 0.001, "<0.001",format(round(MAR_GH$pval,3), nsmall = 3, scientific = FALSE))
MAR_GH$UCI <- format(round(MAR_GH$UCI,1),nsmall=1,scientific=FALSE)
MAR_GH$LCI <- format(round(MAR_GH$LCI,1),nsmall=1,scientific=FALSE)
MAR_GH$beta_95CI <- paste0(MAR_GH$beta," (",MAR_GH$LCI,", ",MAR_GH$UCI,")")
MAR_GH[c(1,4,5,9,2)]


#

#### FGR ####

dim(SRS_FGR_PW)
head(SRS_FGR_PW)
MA_FGR_PW
metareg.FGR.PW.GA <- metareg(MA_FGR_PW, GA_mean_case, hakn = TRUE) 
metareg.FGR.PW.GA
metareg.FGR.PW.GA$pval[c(2)]
metareg.FGR.PW.GA$b[c(2)]
bubble(metareg.FGR.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FGR MR PW.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.FGR.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.5, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9)
}
dev.off()

dim(SRS_FGR_BWPW)
head(SRS_FGR_BWPW)
MA_FGR_BWPW
metareg.FGR.BWPW.GA <- metareg(MA_FGR_BWPW, GA_mean_case, hakn = TRUE) 
metareg.FGR.BWPW.GA
metareg.FGR.BWPW.GA$pval[c(2)]
metareg.FGR.BWPW.GA$b[c(2)]
bubble(metareg.FGR.BWPW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_FGR_PWBW)
head(SRS_FGR_PWBW)
MA_FGR_PWBW
metareg.FGR.PWBW.GA <- metareg(MA_FGR_PWBW, GA_mean_case, hakn = TRUE) 
metareg.FGR.PWBW.GA
metareg.FGR.PWBW.GA$pval[c(2)]
metareg.FGR.PWBW.GA$b[c(2)]
bubble(metareg.FGR.PWBW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_FGR <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.FGR.PW.GA$pval[c(2)], metareg.FGR.BWPW.GA$pval[c(2)],
                                               metareg.FGR.PWBW.GA$pval[c(2)]),
                     c(metareg.FGR.PW.GA$b[c(2)],metareg.FGR.BWPW.GA$b[c(2)],metareg.FGR.PWBW.GA$b[c(2)]),
                     c(metareg.FGR.PW.GA$se[c(2)],metareg.FGR.BWPW.GA$se[c(2)],metareg.FGR.PWBW.GA$se[c(2)]),
                     c(metareg.FGR.PW.GA$k,metareg.FGR.BWPW.GA$k,metareg.FGR.PWBW.GA$k),
                     c(metareg.FGR.PW.GA$I2,metareg.FGR.BWPW.GA$I2,metareg.FGR.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_FGR$beta <- format(round(MAR_FGR$beta,1),nsmall=1,scientific=FALSE)
MAR_FGR$UCI <- format(round(MAR_FGR$UCI,1),nsmall=1,scientific=FALSE)
MAR_FGR$LCI <- format(round(MAR_FGR$LCI,1),nsmall=1,scientific=FALSE)
MAR_FGR$i2 <- format(round(MAR_FGR$i2,1),nsmall=1,scientific=FALSE)
MAR_FGR$pval <- ifelse(MAR_FGR$pval < 0.001, "<0.001",format(round(MAR_FGR$pval,3), nsmall = 3, scientific = FALSE))
MAR_FGR$beta_95CI <- paste0(MAR_FGR$beta," (",MAR_FGR$LCI,", ",MAR_FGR$UCI,")")
MAR_FGR[c(1,5,6,9,2)]

#### SGA ####

dim(SRS_SGA_PW)
head(SRS_SGA_PW)
MA_SGA_PW
(metareg.SGA.PW.GA <- metareg(MA_SGA_PW, GA_mean_case, hakn = TRUE) )
metareg.SGA.PW.GA$k
metareg.SGA.PW.GA$pval[c(2)]
metareg.SGA.PW.GA$b[c(2)]
bubble(metareg.SGA.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_SGA_BWPW)
head(SRS_SGA_BWPW)
MA_SGA_BWPW
(metareg.SGA.BWPW.GA <- metareg(MA_SGA_BWPW, GA_mean_case, hakn = TRUE) )
metareg.SGA.BWPW.GA$k
metareg.SGA.BWPW.GA$pval[c(2)]
metareg.SGA.BWPW.GA$b[c(2)]
bubble(metareg.SGA.BWPW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

MA_SGA_PWBW
dim(SRS_SGA_PWBW)
metareg.SGA.PWBW.GA <- metareg(MA_SGA_PWBW, GA_mean_case, hakn = TRUE)
metareg.SGA.PWBW.GA$k
metareg.SGA.PWBW.GA$pval[c(2)]
metareg.SGA.PWBW.GA$b[c(2)]
bubble(metareg.SGA.PWBW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_SGA <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.SGA.PW.GA$pval[c(2)], metareg.SGA.BWPW.GA$pval[c(2)],
                                               metareg.SGA.PWBW.GA$pval[c(2)]),
                     c(metareg.SGA.PW.GA$b[c(2)],metareg.SGA.BWPW.GA$b[c(2)],metareg.SGA.PWBW.GA$b[c(2)]),
                     c(metareg.SGA.PW.GA$se[c(2)],metareg.SGA.BWPW.GA$se[c(2)],metareg.SGA.PWBW.GA$se[c(2)]),
                     c(metareg.SGA.PW.GA$k,metareg.SGA.BWPW.GA$k,metareg.SGA.PWBW.GA$k),
                     c(metareg.SGA.PW.GA$I2,metareg.SGA.BWPW.GA$I2,metareg.SGA.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_SGA$beta <- format(round(MAR_SGA$beta,1),nsmall=1,scientific=FALSE)
MAR_SGA$UCI <- format(round(MAR_SGA$UCI,1),nsmall=1,scientific=FALSE)
MAR_SGA$LCI <- format(round(MAR_SGA$LCI,1),nsmall=1,scientific=FALSE)
MAR_SGA$i2 <- format(round(MAR_SGA$i2,1),nsmall=1,scientific=FALSE)
MAR_SGA$pval <- ifelse(MAR_SGA$pval < 0.001, "<0.001",format(round(MAR_SGA$pval,3), nsmall = 3, scientific = FALSE))
MAR_SGA$beta_95CI <- paste0(MAR_SGA$beta," (",MAR_SGA$LCI,", ",MAR_SGA$UCI,")")
MAR_SGA[c(1,5,6,9,2)]


#

#### PTB ####

dim(SRS_PTB_PW)
head(SRS_PTB_PW)
MA_PTB_PW
(metareg.PTB.PW.GA <- metareg(MA_PTB_PW, GA_mean_case, hakn = TRUE) )
metareg.PTB.PW.GA$k
metareg.PTB.PW.GA$pval[c(2)]
metareg.PTB.PW.GA$b[c(2)]
bubble(metareg.PTB.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("PTB MR PW.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PTB.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.8, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-275,0), xlim = c(28,38))
}
dev.off()


MAR_PTB <- data.frame(c("PW"), c(metareg.PTB.PW.GA$pval[c(2)]),c(metareg.PTB.PW.GA$b[c(2)]),
                      c(metareg.PTB.PW.GA$k),c(metareg.PTB.PW.GA$I2), c(metareg.PTB.PW.GA$se)[c(2)]) %>% 
                        rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PTB$beta <- format(round(MAR_PTB$beta,1),nsmall=1,scientific=FALSE)
MAR_PTB$i2 <- format(round(MAR_PTB$i2,1),nsmall=1,scientific=FALSE)
MAR_PTB$pval <- ifelse(MAR_PTB$pval < 0.001, "<0.001",format(round(MAR_PTB$pval,3), nsmall = 3, scientific = FALSE))
MAR_PTB$UCI <- format(round(MAR_PTB$UCI,1),nsmall=1,scientific=FALSE)
MAR_PTB$LCI <- format(round(MAR_PTB$LCI,1),nsmall=1,scientific=FALSE)
MAR_PTB$beta_95CI <- paste0(MAR_PTB$beta," (",MAR_PTB$LCI,", ",MAR_PTB$UCI,")")
MAR_PTB[c(1,4,5,9,2)]


#

#### LBW ####

dim(SRS_LBW_PW)
head(SRS_LBW_PW)
MA_LBW_PW
metareg.LBW.PW.GA <- metareg(MA_LBW_PW, GA_mean_case, hakn = TRUE) 
metareg.LBW.PW.GA
metareg.LBW.PW.GA$pval[c(2)]
metareg.LBW.PW.GA$b[c(2)]
bubble(metareg.LBW.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_LBW_BWPW)
head(SRS_LBW_BWPW)
MA_LBW_BWPW
metareg.LBW.BWPW.GA <- metareg(MA_LBW_BWPW, GA_mean_case, hakn = TRUE) 
metareg.LBW.BWPW.GA
metareg.LBW.BWPW.GA$pval[c(2)]
metareg.LBW.BWPW.GA$b[c(2)]
bubble(metareg.LBW.BWPW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_LBW_PWBW)
head(SRS_LBW_PWBW)
MA_LBW_PWBW
metareg.LBW.PWBW.GA <- metareg(MA_LBW_PWBW, GA_mean_case, hakn = TRUE) 
metareg.LBW.PWBW.GA
metareg.LBW.PWBW.GA$pval[c(2)]
metareg.LBW.PWBW.GA$b[c(2)]
bubble(metareg.LBW.PWBW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_LBW <- data.frame(c("PW"), c(metareg.LBW.PW.GA$pval[c(2)]),c(metareg.LBW.PW.GA$b[c(2)]),
                      c(metareg.LBW.PW.GA$k),c(metareg.LBW.PW.GA$I2), c(metareg.LBW.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_LBW$beta <- format(round(MAR_LBW$beta,1),nsmall=1,scientific=FALSE)
MAR_LBW$i2 <- format(round(MAR_LBW$i2,1),nsmall=1,scientific=FALSE)
MAR_LBW$pval <- ifelse(MAR_LBW$pval < 0.001, "<0.001",format(round(MAR_LBW$pval,3), nsmall = 3, scientific = FALSE))
MAR_LBW$UCI <- format(round(MAR_LBW$UCI,1),nsmall=1,scientific=FALSE)
MAR_LBW$LCI <- format(round(MAR_LBW$LCI,1),nsmall=1,scientific=FALSE)
MAR_LBW$beta_95CI <- paste0(MAR_LBW$beta," (",MAR_LBW$LCI,", ",MAR_LBW$UCI,")")
MAR_LBW[c(1,4,5,9,2)]

#

#### Stillbirth #### 

dim(SRS_Stillbirth_PW)
head(SRS_Stillbirth_PW)
MA_Stillbirth_PW
(metareg.Stillbirth.PW.GA <- metareg(MA_Stillbirth_PW, GA_mean_case, hakn = TRUE) )
metareg.Stillbirth.PW.GA$k
metareg.Stillbirth.PW.GA$pval[c(2)]
metareg.Stillbirth.PW.GA$b[c(2)]
bubble(metareg.Stillbirth.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, 
       col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_Stillbirth_BWPW)
head(SRS_Stillbirth_BWPW)
MA_Stillbirth_BWPW
(metareg.Stillbirth.BWPW.GA <- metareg(MA_Stillbirth_BWPW, GA_mean_case, hakn = TRUE) )
metareg.Stillbirth.BWPW.GA$k
metareg.Stillbirth.BWPW.GA$pval[c(2)]
metareg.Stillbirth.BWPW.GA$b[c(2)]
bubble(metareg.Stillbirth.BWPW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, 
       col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

MA_Stillbirth_PWBW
dim(SRS_Stillbirth_PWBW)
metareg.Stillbirth.PWBW.GA <- metareg(MA_Stillbirth_PWBW, GA_mean_case, hakn = TRUE)
metareg.Stillbirth.PWBW.GA$k
metareg.Stillbirth.PWBW.GA$pval[c(2)]
metareg.Stillbirth.PWBW.GA$b[c(2)]
bubble(metareg.Stillbirth.PWBW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, 
       col.line = "red", studlab = TRUE,cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_Stillbirth <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.Stillbirth.PW.GA$pval[c(2)], 
                                                       metareg.Stillbirth.BWPW.GA$pval[c(2)],
                                                metareg.Stillbirth.PWBW.GA$pval[c(2)]),
                      c(metareg.Stillbirth.PW.GA$b[c(2)],metareg.Stillbirth.BWPW.GA$b[c(2)],
                        metareg.Stillbirth.PWBW.GA$b[c(2)]),
                      c(metareg.Stillbirth.PW.GA$se[c(2)],metareg.Stillbirth.BWPW.GA$se[c(2)],
                        metareg.Stillbirth.PWBW.GA$se[c(2)]),
                      c(metareg.Stillbirth.PW.GA$k,metareg.Stillbirth.BWPW.GA$k,
                        metareg.Stillbirth.PWBW.GA$k),
                      c(metareg.Stillbirth.PW.GA$I2,metareg.Stillbirth.BWPW.GA$I2,
                        metareg.Stillbirth.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_Stillbirth$beta <- format(round(MAR_Stillbirth$beta,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth$UCI <- format(round(MAR_Stillbirth$UCI,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth$LCI <- format(round(MAR_Stillbirth$LCI,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth$i2 <- format(round(MAR_Stillbirth$i2,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth$pval <- ifelse(MAR_Stillbirth$pval < 0.001, "<0.001",
                              format(round(MAR_Stillbirth$pval,3), nsmall = 3, 
                                     scientific = FALSE))
MAR_Stillbirth$beta_95CI <- 
  paste0(MAR_Stillbirth$beta," (",MAR_Stillbirth$LCI,", ",MAR_Stillbirth$UCI,")")
MAR_Stillbirth[c(1,5,6,9,2)]

#


#### Placental Praevia ####
dim(SRS_PP_PW)
head(SRS_PP_PW)
MA_PP_PW
(metareg.PP.PW.GA <- metareg(MA_PP_PW, GA_mean_case, hakn = TRUE) )
metareg.PP.PW.GA$k
metareg.PP.PW.GA$pval[c(2)]
metareg.PP.PW.GA$b[c(2)]
bubble(metareg.PP.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("PP MR PW.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PP.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.8, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-275,0), xlim = c(28,38))
}
dev.off()


MAR_PP <- data.frame(c("PW"), c(metareg.PP.PW.GA$pval[c(2)]),c(metareg.PP.PW.GA$b[c(2)]),
                      c(metareg.PP.PW.GA$k),c(metareg.PP.PW.GA$I2), c(metareg.PP.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PP$beta <- format(round(MAR_PP$beta,1),nsmall=1,scientific=FALSE)
MAR_PP$i2 <- format(round(MAR_PP$i2,1),nsmall=1,scientific=FALSE)
MAR_PP$pval <- ifelse(MAR_PP$pval < 0.001, "<0.001",format(round(MAR_PP$pval,3), nsmall = 3, scientific = FALSE))
MAR_PP$UCI <- format(round(MAR_PP$UCI,1),nsmall=1,scientific=FALSE)
MAR_PP$LCI <- format(round(MAR_PP$LCI,1),nsmall=1,scientific=FALSE)
MAR_PP$beta_95CI <- paste0(MAR_PP$beta," (",MAR_PP$LCI,", ",MAR_PP$UCI,")")
MAR_PP[c(1,4,9,2)]


#


#### Placental Abruption ####

MA_PA_PW
dim(SRS_PA_PW)
SRS_PA_PW
#no gestational age data

#

#### MVM #### 

dim(SRS_MVM_PW)
head(SRS_MVM_PW)
MA_MVM_PW
(metareg.MVM.PW.GA <- metareg(MA_MVM_PW, GA_mean_case, hakn = TRUE) )
metareg.MVM.PW.GA$k
metareg.MVM.PW.GA$pval[c(2)]
metareg.MVM.PW.GA$b[c(2)]
bubble(metareg.MVM.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, 
       col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("MVM MR PW.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.MVM.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, 
         col.line = "red", studlab = TRUE,
         cex.studlab =0.8, pos.studlab = 4,offset = 0.2, 
         ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-110,0),xlim = c(28,41))
}
dev.off()


MAR_MVM <- data.frame(c("PW"), c(metareg.MVM.PW.GA$pval[c(2)]),c(metareg.MVM.PW.GA$b[c(2)]),
                      c(metareg.MVM.PW.GA$k),c(metareg.MVM.PW.GA$I2), 
                      c(metareg.MVM.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_MVM$beta <- format(round(MAR_MVM$beta,1),nsmall=1,scientific=FALSE)
MAR_MVM$i2 <- format(round(MAR_MVM$i2,1),nsmall=1,scientific=FALSE)
MAR_MVM$pval <- ifelse(MAR_MVM$pval < 0.001, "<0.001",format(round(MAR_MVM$pval,3), 
                                                             nsmall = 3, scientific = FALSE))
MAR_MVM$UCI <- format(round(MAR_MVM$UCI,1),nsmall=1,scientific=FALSE)
MAR_MVM$LCI <- format(round(MAR_MVM$LCI,1),nsmall=1,scientific=FALSE)
MAR_MVM$beta_95CI <- paste0(MAR_MVM$beta," (",MAR_MVM$LCI,", ",MAR_MVM$UCI,")")
MAR_MVM[c(1,4,5,9,2)]


#


#### FVM #### 

dim(SRS_FVM_PW)
head(SRS_FVM_PW)
MA_FVM_PW
(metareg.FVM.PW.GA <- metareg(MA_FVM_PW, GA_mean_case, hakn = TRUE) )
metareg.FVM.PW.GA$k
metareg.FVM.PW.GA$pval[c(2)]
metareg.FVM.PW.GA$b[c(2)]
bubble(metareg.FVM.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, 
       col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_FVM <- data.frame(c("PW"), c(metareg.FVM.PW.GA$pval[c(2)]),c(metareg.FVM.PW.GA$b[c(2)]),
                      c(metareg.FVM.PW.GA$k),c(metareg.FVM.PW.GA$I2), 
                      c(metareg.FVM.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_FVM$beta <- format(round(MAR_FVM$beta,1),nsmall=1,scientific=FALSE)
MAR_FVM$i2 <- format(round(MAR_FVM$i2,1),nsmall=1,scientific=FALSE)
MAR_FVM$pval <- ifelse(MAR_FVM$pval < 0.001, "<0.001",format(round(MAR_FVM$pval,3), nsmall = 3, 
                                                             scientific = FALSE))
MAR_FVM$UCI <- format(round(MAR_FVM$UCI,1),nsmall=1,scientific=FALSE)
MAR_FVM$LCI <- format(round(MAR_FVM$LCI,1),nsmall=1,scientific=FALSE)
MAR_FVM$beta_95CI <- paste0(MAR_FVM$beta," (",MAR_FVM$LCI,", ",MAR_FVM$UCI,")")
MAR_FVM[c(1,4,5,9,2)]

#

#### AI #### 

dim(SRS_AI_PW)
head(SRS_AI_PW)
MA_AI_PW
(metareg.AI.PW.GA <- metareg(MA_AI_PW, GA_mean_case, hakn = TRUE) )
metareg.AI.PW.GA$k
metareg.AI.PW.GA$pval[c(2)]
metareg.AI.PW.GA$b[c(2)]
bubble(metareg.AI.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", 
       studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_AI <- data.frame(c("PW"), c(metareg.AI.PW.GA$pval[c(2)]),c(metareg.AI.PW.GA$b[c(2)]),
                      c(metareg.AI.PW.GA$k),c(metareg.AI.PW.GA$I2),
                      c(metareg.AI.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_AI$beta <- format(round(MAR_AI$beta,1),nsmall=1,scientific=FALSE)
MAR_AI$i2 <- format(round(MAR_AI$i2,1),nsmall=1,scientific=FALSE)
MAR_AI$pval <- ifelse(MAR_AI$pval < 0.001, "<0.001",format(round(MAR_AI$pval,3), nsmall = 3,
                                                             scientific = FALSE))
MAR_AI$UCI <- format(round(MAR_AI$UCI,1),nsmall=1,scientific=FALSE)
MAR_AI$LCI <- format(round(MAR_AI$LCI,1),nsmall=1,scientific=FALSE)
MAR_AI$beta_95CI <- paste0(MAR_AI$beta," (",MAR_AI$LCI,", ",MAR_AI$UCI,")")
MAR_AI[c(1,4,5,9,2)]

#

#### CI #### 

dim(SRS_CI_PW)
head(SRS_CI_PW)
MA_CI_PW
(metareg.CI.PW.GA <- metareg(MA_CI_PW, GA_mean_case, hakn = TRUE) )
metareg.CI.PW.GA$k
metareg.CI.PW.GA$pval[c(2)]
metareg.CI.PW.GA$b[c(2)]
bubble(metareg.CI.PW.GA, xlab = "Gestational Age of Cases (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_CI <- data.frame(c("PW"), c(metareg.CI.PW.GA$pval[c(2)]),c(metareg.CI.PW.GA$b[c(2)]),
                      c(metareg.CI.PW.GA$k),c(metareg.CI.PW.GA$I2), c(metareg.CI.PW.GA$se)[c(2)]) %>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_CI$beta <- format(round(MAR_CI$beta,1),nsmall=1,scientific=FALSE)
MAR_CI$i2 <- format(round(MAR_CI$i2,1),nsmall=1,scientific=FALSE)
MAR_CI$pval <- ifelse(MAR_CI$pval < 0.001, "<0.001",format(round(MAR_CI$pval,3), nsmall = 3, scientific = FALSE))
MAR_CI$UCI <- format(round(MAR_CI$UCI,1),nsmall=1,scientific=FALSE)
MAR_CI$LCI <- format(round(MAR_CI$LCI,1),nsmall=1,scientific=FALSE)
MAR_CI$beta_95CI <- paste0(MAR_CI$beta," (",MAR_CI$LCI,", ",MAR_CI$UCI,")")
MAR_CI[c(1,4,5,9,2)]

#

#### Summary ####

(MR_results <- (data.frame(rbind(MAR_PE %>% mutate(Exposure = "PE"),  MAR_PE_ON, 
                                 MAR_HDP %>% mutate(Exposure = "HDP"), MAR_GH %>% mutate(Exposure = "GH"),
                      MAR_SGA %>% mutate(Exposure = "SGA"), MAR_LBW %>% mutate(Exposure = "LBW"),
                      MAR_Stillbirth %>% mutate(Exposure = "Stillbirth"),
                      MAR_FGR %>% mutate(Exposure = "FGR"), MAR_PTB %>% mutate(Exposure = "PTB"),
                      MAR_FVM %>% mutate(Exposure = "FVM"), MAR_MVM %>% mutate(Exposure = "MVM"),
                    MAR_AI %>% mutate(Exposure = "AI"), MAR_CI %>% mutate(Exposure = "CI"))) %>% 
                  mutate(Type = c(rep("APO",18),rep("Path",4))) %>% arrange(Type,Exposure,Placenta))[c(10,1,5,6,9,2,11)] 
 %>% rename("Placenta Measure"=2,"APO"=1,"P value"=6,"Beta"=5,"N"=3,"I2"=4) %>% 
 mutate(`Placenta Measure` = str_replace_all(`Placenta Measure`,c("PW"="Placental Weight"))) %>% 
   mutate(`Placenta Measure` = str_replace_all(`Placenta Measure`,c("Placental WeightBW"="PW:BW ratio"))) %>% 
   mutate(`Placenta Measure` = str_replace_all(`Placenta Measure`,c("BWPlacental Weight"="BW:PW ratio"))) )
MR_results$APO <- factor(MR_results$APO, 
                             levels=(c("Stillbirth","HDP","GH","PE","PE early onset",
                                       "PE late onset","PTB","FGR","SGA","LBW","MVM","FVM","AI","CI")))
MR_results$`Placenta Measure` <- factor(MR_results$`Placenta Measure`, 
                                            levels=(c("Placental Weight","BW:PW ratio","PW:BW ratio")))
MR_results <- MR_results %>% arrange(APO,`Placenta Measure`) %>% filter(!`Placenta Measure` == "PW:BW ratio")
MR_results %>% filter(`P value` < 0.05) 
MR_results %>% filter(`Placenta Measure` == "Placental Weight")

########
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #### Gestational Age Difference ####
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Preeclampsia ####

dim(SRS_PE_PW)
SRS_PE_PW_d <- SRS_PE_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_PE_PW_d)
MA_PE_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control,studlab = study_id, data = SRS_PE_PW_d )
metareg.PE.PW.GA.d <- metareg(MA_PE_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.PE.PW.GA.d$k
metareg.PE.PW.GA.d$pval[c(2)]
metareg.PE.PW.GA.d$b[c(2)]
bubble(metareg.PE.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Preeclampsia MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PE.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         xlim = c(-10,2))
}
dev.off()


dim(SRS_PE_BWPW)
SRS_PE_BWPW_d <- SRS_PE_BWPW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_PE_BWPW_d)
MA_PE_BWPW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                         sd.c = PW_sd_control,studlab = study_id, data = SRS_PE_BWPW_d )
metareg.PE.BWPW.GA <- metareg(MA_PE_BWPW_d, GA_mean_diff, hakn = TRUE) 
metareg.PE.BWPW.GA$k
metareg.PE.BWPW.GA$pval[c(2)]
metareg.PE.BWPW.GA$b[c(2)]
bubble(metareg.PE.BWPW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-1,1.5))


dim(SRS_PE_PWBW)
SRS_PE_PWBW_d <- SRS_PE_PWBW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_PE_PWBW_d)
MA_PE_PWBW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                         sd.c = PW_sd_control,studlab = study_id, data = SRS_PE_PWBW_d )
metareg.PE.PWBW.GA <- metareg(MA_PE_PWBW_d, GA_mean_diff, hakn = TRUE) 
metareg.PE.PWBW.GA$k
metareg.PE.PWBW.GA$pval[c(2)]
metareg.PE.PWBW.GA$b[c(2)]
bubble(metareg.PE.PWBW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-0.035,0.02))



MAR_PE_GAD <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.PE.PW.GA.d$pval[c(2)], metareg.PE.BWPW.GA$pval[c(2)],
                                               metareg.PE.PWBW.GA$pval[c(2)]),
                     c(metareg.PE.PW.GA.d$b[c(2)],metareg.PE.BWPW.GA$b[c(2)],metareg.PE.PWBW.GA$b[c(2)]),
                     c(metareg.PE.PW.GA.d$se[c(2)],metareg.PE.BWPW.GA$se[c(2)],metareg.PE.PWBW.GA$se[c(2)]),
                     c(metareg.PE.PW.GA.d$k,metareg.PE.BWPW.GA$k,metareg.PE.PWBW.GA$k),
                     c(metareg.PE.PW.GA.d$I2,metareg.PE.BWPW.GA$I2,metareg.PE.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PE_GAD$beta <- format(round(MAR_PE_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_PE_GAD$UCI <- format(round(MAR_PE_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_PE_GAD$LCI <- format(round(MAR_PE_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_PE_GAD$i2 <- format(round(MAR_PE_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_PE_GAD$pval <- ifelse(MAR_PE_GAD$pval < 0.001, "<0.001",format(round(MAR_PE_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_PE_GAD$beta_95CI <- paste0(MAR_PE_GAD$beta," (",MAR_PE_GAD$LCI,", ",MAR_PE_GAD$UCI,")")
MAR_PE_GAD[c(1,5,6,9,2)]


#

#### Preeclampsia Onset ####


dim(SRS_EON)
SRS_EON_PW_d <- SRS_EON %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
SRS_EON_PW_d
MA_EON_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_EON_PW_d )
metareg.EON.PW.GA.d <- metareg(MA_EON_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.EON.PW.GA.d$k
metareg.EON.PW.GA.d$pval[c(2)]
metareg.EON.PW.GA.d$b[c(2)]
bubble(metareg.EON.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
png("PE EON MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.EON.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         xlim = c(-9,2))
}
dev.off()


dim(SRS_LON)
SRS_LON_PW_d <- SRS_LON %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
SRS_LON_PW_d
MA_LON_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_LON_PW_d )
metareg.LON.PW.GA.d <- metareg(MA_LON_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.LON.PW.GA.d$k
metareg.LON.PW.GA.d$pval[c(2)]
metareg.LON.PW.GA.d$b[c(2)]
bubble(metareg.LON.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-150,0))


MAR_PE_ON_GAD<-(data.frame(c("PE early onset","PE late onset"),c(metareg.EON.PW.GA.d$pval[c(2)],metareg.LON.PW.GA.d$pval[c(2)]),
                           c(metareg.EON.PW.GA.d$b[c(2)],metareg.LON.PW.GA.d$b[c(2)]),
                           c(metareg.EON.PW.GA.d$se[c(2)],metareg.LON.PW.GA.d$se[c(2)]),
                           c(metareg.EON.PW.GA.d$k,metareg.LON.PW.GA.d$k),c(metareg.EON.PW.GA.d$I2,metareg.LON.PW.GA.d$I2),
                           c("PW")) %>%  rename(Exposure=1,pval=2,beta=3,se=4,n=5,Placenta=7,i2=6))[c(1,7,3:5,6,2)] %>%
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PE_ON_GAD$beta <- format(round(MAR_PE_ON_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_PE_ON_GAD$i2 <- format(round(MAR_PE_ON_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_PE_ON_GAD$pval <- ifelse(MAR_PE_ON_GAD$pval<0.001,"<0.001",format(round(MAR_PE_ON_GAD$pval,3),nsmall=3,scientific=FALSE))
MAR_PE_ON_GAD$UCI <- format(round(MAR_PE_ON_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_PE_ON_GAD$LCI <- format(round(MAR_PE_ON_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_PE_ON_GAD$beta_95CI <- paste0(MAR_PE_ON_GAD$beta," (",MAR_PE_ON_GAD$LCI,", ",MAR_PE_ON_GAD$UCI,")")
MAR_PE_ON_GAD[c(2,5,6,10,7,1)]


#

#### HDP ####

dim(SRS_HDP_PW)
SRS_HDP_PW_d <- SRS_HDP_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_HDP_PW_d)
MA_HDP_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control,studlab = study_id, data = SRS_HDP_PW_d )
metareg.PE.PW.GA.d <- metareg(MA_HDP_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.PE.PW.GA.d$k
metareg.PE.PW.GA.d$pval[c(2)]
metareg.PE.PW.GA.d$b[c(2)]
bubble(metareg.PE.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("HDP MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PE.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         xlim = c(-10,2))
}
dev.off()


MAR_HDP_GAD <- data.frame(c("PW"), c(metareg.PE.PW.GA.d$pval[c(2)]),
                     c(metareg.PE.PW.GA.d$b[c(2)]),
                     c(metareg.PE.PW.GA.d$se[c(2)]),
                     c(metareg.PE.PW.GA.d$k),
                     c(metareg.PE.PW.GA.d$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_HDP_GAD$beta <- format(round(MAR_HDP_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_HDP_GAD$UCI <- format(round(MAR_HDP_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_HDP_GAD$LCI <- format(round(MAR_HDP_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_HDP_GAD$i2 <- format(round(MAR_HDP_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_HDP_GAD$pval <- ifelse(MAR_HDP_GAD$pval < 0.001, "<0.001",format(round(MAR_HDP_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_HDP_GAD$beta_95CI <- paste0(MAR_HDP_GAD$beta," (",MAR_HDP_GAD$LCI,", ",MAR_HDP_GAD$UCI,")")
MAR_HDP_GAD[c(1,5,6,9,2)]


#


#### GH ####

dim(SRS_GH_PW)
SRS_GH_PW_d <- SRS_GH_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_GH_PW_d)
MA_GH_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_GH_PW_d )
metareg.PE.PW.GA.d <- metareg(MA_GH_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.PE.PW.GA.d$k
metareg.PE.PW.GA.d$pval[c(2)]
metareg.PE.PW.GA.d$b[c(2)]
bubble(metareg.PE.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_GH_GAD <- data.frame(c("PW"), c(metareg.PE.PW.GA.d$pval[c(2)]),c(metareg.PE.PW.GA.d$b[c(2)]),
                         c(metareg.PE.PW.GA.d$se[c(2)]), c(metareg.PE.PW.GA.d$k), c(metareg.PE.PW.GA.d$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_GH_GAD$beta <- format(round(MAR_GH_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_GH_GAD$UCI <- format(round(MAR_GH_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_GH_GAD$LCI <- format(round(MAR_GH_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_GH_GAD$i2 <- format(round(MAR_GH_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_GH_GAD$pval <- ifelse(MAR_GH_GAD$pval < 0.001, "<0.001",format(round(MAR_GH_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_GH_GAD$beta_95CI <- paste0(MAR_GH_GAD$beta," (",MAR_GH_GAD$LCI,", ",MAR_GH_GAD$UCI,")")
MAR_GH_GAD[c(1,5,6,9,2)]


#

#### FGR ####

dim(SRS_FGR_PW)
SRS_FGR_PW_d <- SRS_FGR_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_FGR_PW_d)
MA_FGR_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_FGR_PW_d )

metareg.FGR.PW.GA.d <- metareg(MA_FGR_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.FGR.PW.GA.d$k
metareg.FGR.PW.GA.d$pval[c(2)]
metareg.FGR.PW.GA.d$b[c(2)]
bubble(metareg.FGR.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FGR MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.FGR.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         xlim = c(-10,5))
}
dev.off()


dim(SRS_FGR_BWPW)
SRS_FGR_BWPW_d <- SRS_FGR_BWPW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_FGR_BWPW_d)
MA_FGR_BWPW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                          sd.c = PW_sd_control,studlab = study_id, data = SRS_FGR_BWPW_d )

metareg.FGR.BWPW.GA <- metareg(MA_FGR_BWPW_d, GA_mean_diff, hakn = TRUE) 
metareg.FGR.BWPW.GA$k
metareg.FGR.BWPW.GA$pval[c(2)]
metareg.FGR.BWPW.GA$b[c(2)]
bubble(metareg.FGR.BWPW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-1,1.5))


dim(SRS_FGR_PWBW)
SRS_FGR_PWBW_d <- SRS_FGR_PWBW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_FGR_PWBW_d)
MA_FGR_PWBW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                          sd.c = PW_sd_control,studlab = study_id, data = SRS_FGR_PWBW_d )

metareg.FGR.PWBW.GA <- metareg(MA_FGR_PWBW_d, GA_mean_diff, hakn = TRUE) 
metareg.FGR.PWBW.GA$k
metareg.FGR.PWBW.GA$pval[c(2)]
metareg.FGR.PWBW.GA$b[c(2)]
bubble(metareg.FGR.PWBW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-0.035,0.02))


MAR_FGR_GAD <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.FGR.PW.GA.d$pval[c(2)], metareg.FGR.BWPW.GA$pval[c(2)],
                                                   metareg.FGR.PWBW.GA$pval[c(2)]),
                         c(metareg.FGR.PW.GA.d$b[c(2)],metareg.FGR.BWPW.GA$b[c(2)],metareg.FGR.PWBW.GA$b[c(2)]),
                         c(metareg.FGR.PW.GA.d$se[c(2)],metareg.FGR.BWPW.GA$se[c(2)],metareg.FGR.PWBW.GA$se[c(2)]),
                         c(metareg.FGR.PW.GA.d$k,metareg.FGR.BWPW.GA$k,metareg.FGR.PWBW.GA$k),
                         c(metareg.FGR.PW.GA.d$I2,metareg.FGR.BWPW.GA$I2,metareg.FGR.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_FGR_GAD$beta <- format(round(MAR_FGR_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_FGR_GAD$UCI <- format(round(MAR_FGR_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_FGR_GAD$LCI <- format(round(MAR_FGR_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_FGR_GAD$i2 <- format(round(MAR_FGR_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_FGR_GAD$pval <- ifelse(MAR_FGR_GAD$pval < 0.001, "<0.001",format(round(MAR_FGR_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_FGR_GAD$beta_95CI <- paste0(MAR_FGR_GAD$beta," (",MAR_FGR_GAD$LCI,", ",MAR_FGR_GAD$UCI,")")
MAR_FGR_GAD[c(1,5,6,9,2)]

#

#### SGA ####

dim(SRS_SGA_PW)
SRS_SGA_PW_d <- SRS_SGA_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_SGA_PW_d)
MA_SGA_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_SGA_PW_d )

metareg.SGA.PW.GA.d <- metareg(MA_SGA_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.SGA.PW.GA.d$k
metareg.SGA.PW.GA.d$pval[c(2)]
metareg.SGA.PW.GA.d$b[c(2)]
bubble(metareg.SGA.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("SGA MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.SGA.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-260,0), xlim = c(-10,4))
}
dev.off()


dim(SRS_SGA_BWPW)
SRS_SGA_BWPW_d <- SRS_SGA_BWPW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_SGA_BWPW_d)
MA_SGA_BWPW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                          sd.c = PW_sd_control,studlab = study_id, data = SRS_SGA_BWPW_d )

metareg.SGA.BWPW.GA <- metareg(MA_SGA_BWPW_d, GA_mean_diff, hakn = TRUE) 
metareg.SGA.BWPW.GA$k
metareg.SGA.BWPW.GA$pval[c(2)]
metareg.SGA.BWPW.GA$b[c(2)]
bubble(metareg.SGA.BWPW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-1,1.5))


dim(SRS_SGA_PWBW)
SRS_SGA_PWBW_d <- SRS_SGA_PWBW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_SGA_PWBW_d)
MA_SGA_PWBW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                          sd.c = PW_sd_control,studlab = study_id, data = SRS_SGA_PWBW_d )

metareg.SGA.PWBW.GA <- metareg(MA_SGA_PWBW_d, GA_mean_diff, hakn = TRUE) 
metareg.SGA.PWBW.GA$k
metareg.SGA.PWBW.GA$pval[c(2)]
metareg.SGA.PWBW.GA$b[c(2)]
bubble(metareg.SGA.PWBW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-0.035,0.02))


MAR_SGA_GAD <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.SGA.PW.GA.d$pval[c(2)], metareg.SGA.BWPW.GA$pval[c(2)],
                                                    metareg.SGA.PWBW.GA$pval[c(2)]),
                          c(metareg.SGA.PW.GA.d$b[c(2)],metareg.SGA.BWPW.GA$b[c(2)],metareg.SGA.PWBW.GA$b[c(2)]),
                          c(metareg.SGA.PW.GA.d$se[c(2)],metareg.SGA.BWPW.GA$se[c(2)],metareg.SGA.PWBW.GA$se[c(2)]),
                          c(metareg.SGA.PW.GA.d$k,metareg.SGA.BWPW.GA$k,metareg.SGA.PWBW.GA$k),
                          c(metareg.SGA.PW.GA.d$I2,metareg.SGA.BWPW.GA$I2,metareg.SGA.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>%
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_SGA_GAD$beta <- format(round(MAR_SGA_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_SGA_GAD$UCI <- format(round(MAR_SGA_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_SGA_GAD$LCI <- format(round(MAR_SGA_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_SGA_GAD$i2 <- format(round(MAR_SGA_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_SGA_GAD$pval <- ifelse(MAR_SGA_GAD$pval < 0.001, "<0.001",format(round(MAR_SGA_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_SGA_GAD$beta_95CI <- paste0(MAR_SGA_GAD$beta," (",MAR_SGA_GAD$LCI,", ",MAR_SGA_GAD$UCI,")")
MAR_SGA_GAD[c(1,5,6,9,2)]

#

#### LBW ####

dim(SRS_LBW_PW)
SRS_LBW_PW_d <- SRS_LBW_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_LBW_PW_d)
MA_LBW_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_LBW_PW_d )
metareg.LBW.PW.GA.d <- metareg(MA_LBW_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.LBW.PW.GA.d$k
metareg.LBW.PW.GA.d$pval[c(2)]
metareg.LBW.PW.GA.d$b[c(2)]
bubble(metareg.LBW.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


dim(SRS_LBW_BWPW)
SRS_LBW_BWPW_d <- SRS_LBW_BWPW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_LBW_BWPW_d)
MA_LBW_BWPW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                          sd.c = PW_sd_control,studlab = study_id, data = SRS_LBW_BWPW_d )
metareg.LBW.BWPW.GA <- metareg(MA_LBW_BWPW_d, GA_mean_diff, hakn = TRUE) 
metareg.LBW.BWPW.GA$k
metareg.LBW.BWPW.GA$pval[c(2)]
metareg.LBW.BWPW.GA$b[c(2)]
bubble(metareg.LBW.BWPW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-1,1.5))


dim(SRS_LBW_PWBW)
SRS_LBW_PWBW_d <- SRS_LBW_PWBW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_LBW_PWBW_d)
MA_LBW_PWBW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                          sd.c = PW_sd_control,studlab = study_id, data = SRS_LBW_PWBW_d )

metareg.LBW.PWBW.GA <- metareg(MA_LBW_PWBW_d, GA_mean_diff, hakn = TRUE) 
metareg.LBW.PWBW.GA$k
metareg.LBW.PWBW.GA$pval[c(2)]
metareg.LBW.PWBW.GA$b[c(2)]
bubble(metareg.LBW.PWBW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2, ylim = c(-0.035,0.02))


MAR_LBW_GAD <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.LBW.PW.GA.d$pval[c(2)], metareg.LBW.BWPW.GA$pval[c(2)],
                                                   metareg.LBW.PWBW.GA$pval[c(2)]),
                         c(metareg.LBW.PW.GA.d$b[c(2)],metareg.LBW.BWPW.GA$b[c(2)],metareg.LBW.PWBW.GA$b[c(2)]),
                         c(metareg.LBW.PW.GA.d$se[c(2)],metareg.LBW.BWPW.GA$se[c(2)],metareg.LBW.PWBW.GA$se[c(2)]),
                         c(metareg.LBW.PW.GA.d$k,metareg.LBW.BWPW.GA$k,metareg.LBW.PWBW.GA$k),
                         c(metareg.LBW.PW.GA.d$I2,metareg.LBW.BWPW.GA$I2,metareg.LBW.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% filter(n > 2) %>%
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_LBW_GAD$beta <- format(round(MAR_LBW_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_LBW_GAD$UCI <- format(round(MAR_LBW_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_LBW_GAD$LCI <- format(round(MAR_LBW_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_LBW_GAD$i2 <- format(round(MAR_LBW_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_LBW_GAD$pval <- ifelse(MAR_LBW_GAD$pval < 0.001, "<0.001",format(round(MAR_LBW_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_LBW_GAD$beta_95CI <- paste0(MAR_LBW_GAD$beta," (",MAR_LBW_GAD$LCI,", ",MAR_LBW_GAD$UCI,")")
MAR_LBW_GAD[c(1,5,6,9,2)]

#

#### Stillbirth ####

dim(SRS_Stillbirth_PW)
SRS_Stillbirth_PW_d <- SRS_Stillbirth_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_Stillbirth_PW_d)
MA_Stillbirth_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, 
                               mean.c = PW_mean_control,
                               sd.c = PW_sd_control,studlab = study_id, data = SRS_Stillbirth_PW_d )

metareg.Stillbirth.PW.GA.d <- metareg(MA_Stillbirth_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.Stillbirth.PW.GA.d$k
metareg.Stillbirth.PW.GA.d$pval[c(2)]
metareg.Stillbirth.PW.GA.d$b[c(2)]
bubble(metareg.Stillbirth.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", 
       studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)

dim(SRS_Stillbirth_BWPW)
SRS_Stillbirth_BWPW_d <- SRS_Stillbirth_BWPW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_Stillbirth_BWPW_d)
MA_Stillbirth_BWPW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, 
                                 mean.c = PW_mean_control,
                                 sd.c = PW_sd_control,studlab = study_id, data = SRS_Stillbirth_BWPW_d )
metareg.Stillbirth.BWPW.GA <- metareg(MA_Stillbirth_BWPW_d, GA_mean_diff, hakn = TRUE) 
metareg.Stillbirth.BWPW.GA$k
metareg.Stillbirth.BWPW.GA$pval[c(2)]
metareg.Stillbirth.BWPW.GA$b[c(2)]
bubble(metareg.Stillbirth.BWPW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", 
       studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


dim(SRS_Stillbirth_PWBW)
SRS_Stillbirth_PWBW_d <- SRS_Stillbirth_PWBW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_Stillbirth_PWBW_d)
MA_Stillbirth_PWBW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, 
                                 mean.c = PW_mean_control,
                                 sd.c = PW_sd_control,studlab = study_id, data = SRS_Stillbirth_PWBW_d )
metareg.Stillbirth.PWBW.GA <- metareg(MA_Stillbirth_PWBW_d, GA_mean_diff, hakn = TRUE) 
metareg.Stillbirth.PWBW.GA$k
metareg.Stillbirth.PWBW.GA$I2
metareg.Stillbirth.PWBW.GA$pval[c(2)]
metareg.Stillbirth.PWBW.GA$b[c(2)]
bubble(metareg.Stillbirth.PWBW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", 
       studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("Stillbirth MR PWBW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.Stillbirth.PWBW.GA, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", 
         studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in PW:BW ratio", cex.lab = 0.9,
         ylim = c(0,0.04),xlim = c(-8,0))
}
dev.off()


MAR_Stillbirth_GAD <- data.frame(c("PW", "BWPW","PWBW"), c(metareg.Stillbirth.PW.GA.d$pval[c(2)], 
                                                           metareg.Stillbirth.BWPW.GA$pval[c(2)],
                                                    metareg.Stillbirth.PWBW.GA$pval[c(2)]),
                          c(metareg.Stillbirth.PW.GA.d$b[c(2)],metareg.Stillbirth.BWPW.GA$b[c(2)],
                            metareg.Stillbirth.PWBW.GA$b[c(2)]),
                          c(metareg.Stillbirth.PW.GA.d$se[c(2)],metareg.Stillbirth.BWPW.GA$se[c(2)],
                            metareg.Stillbirth.PWBW.GA$se[c(2)]),
                          c(metareg.Stillbirth.PW.GA.d$k,metareg.Stillbirth.BWPW.GA$k,metareg.Stillbirth.PWBW.GA$k),
                          c(metareg.Stillbirth.PW.GA.d$I2,metareg.Stillbirth.BWPW.GA$I2,
                            metareg.Stillbirth.PWBW.GA$I2)) %>% 
  rename(Placenta=1,pval=2,beta=3,se=4,n=5,i2=6) %>% filter(n > 2) %>%
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_Stillbirth_GAD$beta <- format(round(MAR_Stillbirth_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth_GAD$UCI <- format(round(MAR_Stillbirth_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth_GAD$LCI <- format(round(MAR_Stillbirth_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth_GAD$i2 <- format(round(MAR_Stillbirth_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_Stillbirth_GAD$pval <- ifelse(MAR_Stillbirth_GAD$pval < 0.001, "<0.001",format(round(MAR_Stillbirth_GAD$pval,3), 
                                                                                   nsmall = 3, scientific = FALSE))
MAR_Stillbirth_GAD$beta_95CI <- paste0(MAR_Stillbirth_GAD$beta," (",MAR_Stillbirth_GAD$LCI,", ",MAR_Stillbirth_GAD$UCI,")")
MAR_Stillbirth_GAD[c(1,5,6,9,2)]

#


#### PTB ####

dim(SRS_PTB_PW)
SRS_PTB_PW_d <- SRS_PTB_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_PTB_PW_d)
MA_PTB_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_PTB_PW_d )

metareg.PTB.PW.GA.d <- metareg(MA_PTB_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.PTB.PW.GA.d$k
metareg.PTB.PW.GA.d$pval[c(2)]
metareg.PTB.PW.GA.d$b[c(2)]
bubble(metareg.PTB.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
       ylim = c(-275,0),xlim = c(-12,0))
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("PTB MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PTB.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-275,0),xlim = c(-12,0))
}
dev.off()


MAR_PTB_GAD <- data.frame(c("PW"), c(metareg.PTB.PW.GA.d$pval[c(2)]), c(metareg.PTB.PW.GA.d$b[c(2)]),
                          c(metareg.PTB.PW.GA.d$k), c(metareg.PTB.PW.GA.d$I2),c(metareg.PTB.PW.GA.d$se[c(2)]) )%>% 
                            rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PTB_GAD$beta <- format(round(MAR_PTB_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_PTB_GAD$i2 <- format(round(MAR_PTB_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_PTB_GAD$pval <- ifelse(MAR_PTB_GAD$pval < 0.001, "<0.001",format(round(MAR_PTB_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_PTB_GAD$UCI <- format(round(MAR_PTB_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_PTB_GAD$LCI <- format(round(MAR_PTB_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_PTB_GAD$beta_95CI <- paste0(MAR_PTB_GAD$beta," (",MAR_PTB_GAD$LCI,", ",MAR_PTB_GAD$UCI,")")
MAR_PTB_GAD[c(1,4,5,9,2)]


#

#### PP ####

dim(SRS_PP_PW)
SRS_PP_PW_d <- SRS_PP_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
SRS_PP_PW_d
MA_PP_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_PP_PW_d )

metareg.PP.PW.GA.d <- metareg(MA_PP_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.PP.PW.GA.d$k
metareg.PP.PW.GA.d$pval[c(2)]
metareg.PP.PW.GA.d$b[c(2)]
bubble(metareg.PP.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
       ylim = c(-275,0),xlim = c(-12,0))
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("PP MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.PP.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         ylim = c(-275,0),xlim = c(-12,0))
}
dev.off()


MAR_PP_GAD <- data.frame(c("PW"), c(metareg.PP.PW.GA.d$pval[c(2)]), c(metareg.PP.PW.GA.d$b[c(2)]),
                          c(metareg.PP.PW.GA.d$k), c(metareg.PP.PW.GA.d$I2),c(metareg.PP.PW.GA.d$se[c(2)]) )%>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_PP_GAD$beta <- format(round(MAR_PP_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_PP_GAD$i2 <- format(round(MAR_PP_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_PP_GAD$pval <- ifelse(MAR_PP_GAD$pval < 0.001, "<0.001",format(round(MAR_PP_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_PP_GAD$UCI <- format(round(MAR_PP_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_PP_GAD$LCI <- format(round(MAR_PP_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_PP_GAD$beta_95CI <- paste0(MAR_PP_GAD$beta," (",MAR_PP_GAD$LCI,", ",MAR_PP_GAD$UCI,")")
MAR_PP_GAD[c(1,4,9,2)]


#

#### MVM ####

dim(SRS_MVM_PW)
SRS_MVM_PW_d <- SRS_MVM_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_MVM_PW_d)
MA_MVM_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_MVM_PW_d )
metareg.MVM.PW.GA.d <- metareg(MA_MVM_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.MVM.PW.GA.d$k
metareg.MVM.PW.GA.d$I2
metareg.MVM.PW.GA.d$pval[c(2)]
metareg.MVM.PW.GA.d$b[c(2)]
bubble(metareg.MVM.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.8, pos.studlab = 4,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
       ylim = c(-90,0),xlim = c(-3.25,2))


MAR_MVM_GAD <- data.frame(c("PW"), c(metareg.MVM.PW.GA.d$pval[c(2)]), c(metareg.MVM.PW.GA.d$b[c(2)]),
                          c(metareg.MVM.PW.GA.d$k), c(metareg.MVM.PW.GA.d$I2),c(metareg.MVM.PW.GA.d$se[c(2)]) )%>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_MVM_GAD$beta <- format(round(MAR_MVM_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_MVM_GAD$i2 <- format(round(MAR_MVM_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_MVM_GAD$pval <- ifelse(MAR_MVM_GAD$pval < 0.001, "<0.001",format(round(MAR_MVM_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_MVM_GAD$UCI <- format(round(MAR_MVM_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_MVM_GAD$LCI <- format(round(MAR_MVM_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_MVM_GAD$beta_95CI <- paste0(MAR_MVM_GAD$beta," (",MAR_MVM_GAD$LCI,", ",MAR_MVM_GAD$UCI,")")
MAR_MVM_GAD[c(1,4,5,9,2)]

#

#### FVM ####

dim(SRS_FVM_PW)
SRS_FVM_PW_d <- SRS_FVM_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
(SRS_FVM_PW_d)
MA_FVM_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                        sd.c = PW_sd_control,studlab = study_id, data = SRS_FVM_PW_d )
metareg.FVM.PW.GA.d <- metareg(MA_FVM_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.FVM.PW.GA.d$k
metareg.FVM.PW.GA.d$pval[c(2)]
metareg.FVM.PW.GA.d$b[c(2)]
bubble(metareg.FVM.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)
setwd("/Users/ah22023/Documents/Placenta/Project 1 - Systematic review/Figures/")
png("FVM MR PW GAD.png", width = 2000, height = 1500, res = 250)
{
  bubble(metareg.FVM.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
         cex.studlab =0.6, pos.studlab = 1,offset = 0.2, ylab = "Mean Difference in Placental Weight (g)", cex.lab = 0.9,
         xlim = c(-2,8), ylim = c(-75,200))
}
dev.off()


MAR_FVM_GAD <- data.frame(c("PW"), c(metareg.FVM.PW.GA.d$pval[c(2)]), c(metareg.FVM.PW.GA.d$b[c(2)]),
                          c(metareg.FVM.PW.GA.d$k), c(metareg.FVM.PW.GA.d$I2),c(metareg.FVM.PW.GA.d$se[c(2)]) )%>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_FVM_GAD$beta <- format(round(MAR_FVM_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_FVM_GAD$i2 <- format(round(MAR_FVM_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_FVM_GAD$pval <- ifelse(MAR_FVM_GAD$pval < 0.001, "<0.001",format(round(MAR_FVM_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_FVM_GAD$UCI <- format(round(MAR_FVM_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_FVM_GAD$LCI <- format(round(MAR_FVM_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_FVM_GAD$beta_95CI <- paste0(MAR_FVM_GAD$beta," (",MAR_FVM_GAD$LCI,", ",MAR_FVM_GAD$UCI,")")
MAR_FVM_GAD[c(1,4,5,9,2)]

#

#### AI ####

dim(SRS_AI_PW)
SRS_AI_PW_d <- SRS_AI_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_AI_PW_d)
MA_AI_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control,studlab = study_id, data = SRS_AI_PW_d )
metareg.AI.PW.GA.d <- metareg(MA_AI_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.AI.PW.GA.d$k
metareg.AI.PW.GA.d$pval[c(2)]
metareg.AI.PW.GA.d$b[c(2)]
bubble(metareg.AI.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_AI_GAD <- data.frame(c("PW"), c(metareg.AI.PW.GA.d$pval[c(2)]), c(metareg.AI.PW.GA.d$b[c(2)]),
                          c(metareg.AI.PW.GA.d$k), c(metareg.AI.PW.GA.d$I2),c(metareg.AI.PW.GA.d$se[c(2)]) )%>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_AI_GAD$beta <- format(round(MAR_AI_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_AI_GAD$i2 <- format(round(MAR_AI_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_AI_GAD$pval <- ifelse(MAR_AI_GAD$pval < 0.001, "<0.001",format(round(MAR_AI_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_AI_GAD$UCI <- format(round(MAR_AI_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_AI_GAD$LCI <- format(round(MAR_AI_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_AI_GAD$beta_95CI <- paste0(MAR_AI_GAD$beta," (",MAR_AI_GAD$LCI,", ",MAR_AI_GAD$UCI,")")
MAR_AI_GAD[c(1,4,5,9,2)]

#

#### CI ####

dim(SRS_CI_PW)
SRS_CI_PW_d <- SRS_CI_PW %>% mutate(GA_mean_diff = GA_mean_case - GA_mean_control)
head(SRS_CI_PW_d)
MA_CI_PW_d <- metacont(n.e = Case_N, mean.e = PW_mean_case, sd.e = PW_sd_case, n.c = Control_N, mean.c = PW_mean_control,
                       sd.c = PW_sd_control,studlab = study_id, data = SRS_CI_PW_d )
metareg.CI.PW.GA.d <- metareg(MA_CI_PW_d, GA_mean_diff, hakn = TRUE) 
metareg.CI.PW.GA.d$k
metareg.CI.PW.GA.d$pval[c(2)]
metareg.CI.PW.GA.d$b[c(2)]
bubble(metareg.CI.PW.GA.d, xlab = "Gestational Age Difference (weeks)", cex = 0.5, col.line = "red", studlab = TRUE,
       cex.studlab =0.3, pos.studlab = 1,offset = 0.2)


MAR_CI_GAD <- data.frame(c("PW"), c(metareg.CI.PW.GA.d$pval[c(2)]), c(metareg.CI.PW.GA.d$b[c(2)]),
                          c(metareg.CI.PW.GA.d$k), c(metareg.CI.PW.GA.d$I2),c(metareg.CI.PW.GA.d$se[c(2)]) )%>% 
  rename(Placenta=1,pval=2,beta=3,n=4,i2=5,se=6) %>% 
  mutate(LCI = beta - se*qnorm(0.975)) %>% mutate(UCI = beta + se*qnorm(0.975))
MAR_CI_GAD$beta <- format(round(MAR_CI_GAD$beta,1),nsmall=1,scientific=FALSE)
MAR_CI_GAD$i2 <- format(round(MAR_CI_GAD$i2,1),nsmall=1,scientific=FALSE)
MAR_CI_GAD$pval <- ifelse(MAR_CI_GAD$pval < 0.001, "<0.001",format(round(MAR_CI_GAD$pval,3), nsmall = 3, scientific = FALSE))
MAR_CI_GAD$UCI <- format(round(MAR_CI_GAD$UCI,1),nsmall=1,scientific=FALSE)
MAR_CI_GAD$LCI <- format(round(MAR_CI_GAD$LCI,1),nsmall=1,scientific=FALSE)
MAR_CI_GAD$beta_95CI <- paste0(MAR_CI_GAD$beta," (",MAR_CI_GAD$LCI,", ",MAR_CI_GAD$UCI,")")
MAR_CI_GAD[c(1,4,5,9,2)]

#



#### Summary ####
(MR_results_GAD <- (data.frame(rbind(MAR_PE_GAD %>% mutate(Exposure = "PE"), MAR_FGR_GAD %>% mutate(Exposure = "FGR"),
                                     MAR_SGA_GAD %>% mutate(Exposure = "SGA"), MAR_LBW_GAD %>% mutate(Exposure = "LBW"),
                                     MAR_PTB_GAD %>% mutate(Exposure = "PTB"), MAR_PE_ON_GAD,
                                     MAR_Stillbirth_GAD %>% mutate(Exposure = "Stillbirth"), 
                                     MAR_HDP_GAD %>% mutate(Exposure = "HDP"), MAR_GH_GAD %>% mutate(Exposure = "GH"), 
                                     MAR_FVM_GAD %>% mutate(Exposure = "FVM"),  MAR_MVM_GAD %>% mutate(Exposure = "MVM"),
                                     MAR_AI_GAD %>% mutate(Exposure = "AI"), MAR_CI_GAD %>% mutate(Exposure = "CI"))) %>% 
               mutate(Type = c(rep("APO",18),rep("Path",4))) %>% arrange(Type,Exposure,Placenta))[c(10,1,5,6,9,2,11)] %>% 
   rename("Placenta Measure"=2,"APO"=1,"P value"=6,"Beta"=5,"N"=3,"I2"=4)%>% 
   mutate(`Placenta Measure` = str_replace_all(`Placenta Measure`,c("PW"="Placental Weight"))) %>% 
   mutate(`Placenta Measure` = str_replace_all(`Placenta Measure`,c("Placental WeightBW"="PW:BW ratio"))) %>% 
   mutate(`Placenta Measure` = str_replace_all(`Placenta Measure`,c("BWPlacental Weight"="BW:PW ratio"))) )
MR_results_GAD$APO <- factor(MR_results_GAD$APO, 
                             levels=(c("Stillbirth","HDP","GH","PE","PE early onset",
                                       "PE late onset","PTB","FGR","SGA","LBW","MVM","FVM","AI","CI")))
MR_results_GAD$`Placenta Measure` <- factor(MR_results_GAD$`Placenta Measure`, 
                             levels=(c("Placental Weight","BW:PW ratio","PW:BW ratio")))
MR_results_GAD <- MR_results_GAD %>% arrange(APO,`Placenta Measure`) %>% filter(!`Placenta Measure` == "PW:BW ratio")
MR_results_GAD %>% filter(`P value` < 0.05) 
MR_results_GAD %>% filter(`Placenta Measure` == "Placental Weight") %>% select(-c(I2))


(MR_results_All <- (rbind(MR_results %>% mutate("Meta Regression by"= "GA of cases") %>% select(-c(I2, Type)), 
                         MR_results_GAD %>% mutate("Meta Regression by"= "ΔGA between cases and controls") %>% 
                           select(-c(I2,Type))) %>%
    rename("Outcome"="APO","N studies"="N") %>% mutate("Included studies"="All"))[c(1,2,7,6,3:5)] %>% 
    arrange(Outcome,`Placenta Measure`) )
MR_results_All$Outcome <- factor(MR_results_All$Outcome, 
                             levels=(c("Stillbirth","HDP","GH","PE","PE early onset",
                                       "PE late onset","PTB","FGR","SGA","LBW","MVM","FVM","AI","CI")))
MR_results_All$`Placenta Measure` <- factor(MR_results_All$`Placenta Measure`, 
                                            levels=(c("Placental Weight","BW:PW ratio","PW:BW ratio")))
MR_results_All <- MR_results_All %>% arrange(Outcome,`Placenta Measure`)
(MR_results_All %>% filter(`Placenta Measure`=="Placental Weight"))[-c(2,3)]
