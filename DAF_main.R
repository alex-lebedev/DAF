# "Psychedelic use and alternative beliefs", 2022
# OSF entry: https://osf.io/53jm2/

rm(list=ls())
# Load libraries:
library(dplyr)
library(ggpubr)
library(bestNormalize)
library(effsize)
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)
library(coefplot)
library(psych)
library(effsize)
library(lm.beta)
library(mlogit)
library(jtools)
library(rockchalk)
library(xlsx)
library(stats)
library(compareGroups)

load('/Users/alebedev/Dropbox/DAF_anonymized.rda')

# Creating a binary factor defining psychedelic user group {False;True}
ALLFU_ocean_wDemogr$PSY_group <- ALLFU_ocean_wDemogr$PSY_prox > 1
ALLEBS_wDemogr$PSY_group <- ALLEBS_wDemogr$PSY_prox > 1

# Convert "education" variable to numeric:
ALLEBS_wDemogr$education <- as.numeric(ALLEBS_wDemogr$education)
ALLFU_ocean_wDemogr$education <- as.numeric(ALLFU_ocean_wDemogr$education)
########################
# Density and QQ-plots #
########################

# CONS5 (ok)
ggdensity(ALLFU_ocean_wDemogr$CONS5, fill = "lightgray")
ggqqplot(ALLFU_ocean_wDemogr$CONS5)
# EBS-feel (ok)
ggdensity(ALLEBS_wDemogr$EBS_feel, fill = "lightgray")
ggqqplot(ALLEBS_wDemogr$EBS_feel)
# EBS-evid (non-normal)
ggdensity(ALLEBS_wDemogr$EBS_evid, fill = "lightgray")
ggqqplot(ALLEBS_wDemogr$EBS_evid)
# EBS-polit (non-normal)
ggdensity(ALLEBS_wDemogr$EBS_polit, fill = "lightgray")
ggqqplot(ALLEBS_wDemogr$EBS_polit)

####################
# Apply transforms #
####################
# EBS_evid: choose best normalization method (orderNorm):
bestNormalize(ALLEBS_wDemogr$EBS_evid)$chosen_transform
ALLEBS_wDemogr$EBS_evid_transformed <- bestNormalize(ALLEBS_wDemogr$EBS_evid)$x.t
# EBS_polit: choose best normalization method (Standardized I):
bestNormalize(ALLEBS_wDemogr$EBS_polit)$chosen_transform
ALLEBS_wDemogr$EBS_polit_transformed <- bestNormalize(ALLEBS_wDemogr$EBS_polit)$x.t



############
# ANALYSIS #
############

#########
# CONS5 #
#########
mydata <- data.frame(Age = ALLFU_ocean_wDemogr$age, Sex = ALLFU_ocean_wDemogr$sex,
                     Diagnosis = ALLFU_ocean_wDemogr$PsychDiagAny,
                     CMQ = ALLFU_ocean_wDemogr$CONS5,
                           Alcohol = ALLFU_ocean_wDemogr$ALC_prox,
                           Cannabis = ALLFU_ocean_wDemogr$CAN_prox,
                           MDMA = ALLFU_ocean_wDemogr$MDMA_prox,
                           Opiates = ALLFU_ocean_wDemogr$OPI_prox,
                           Psychedelics = ALLFU_ocean_wDemogr$PSY_prox,
                            PsychedelicsG = as.factor(ALLFU_ocean_wDemogr$PSY_prox>1),
                           Stimulants = ALLFU_ocean_wDemogr$STIM_prox,
                           Tobacco = ALLFU_ocean_wDemogr$TOB_prox
                           )

Model1 <- (lm(CMQ~Psychedelics, data=mydata))
beta(Model1)
# Users Only:
mydataB <- subset(mydata, mydata$PsychedelicsG==T)
Model1b <- (lm(CMQ~Psychedelics, data=mydataB))
beta(Model1b)

# The effect is still there even if we regress-out demographics and user/non-user group effects:
Model2 <- (lm(CMQ~Age+Sex+Diagnosis+Psychedelics, data=mydata))
# Users Only:
beta(Model2)
Model2b <- (lm(CMQ~Age+Sex+Diagnosis+Psychedelics, data=mydataB))
beta(Model2b)


# With adjustments for other drugs:
Model3 <- (lm(CMQ~Age+Sex+Diagnosis+Alcohol+Cannabis+MDMA+Opiates+Psychedelics+Stimulants+Tobacco, data=mydata))
beta(Model3)
# Users Only:
Model3b <- (lm(CMQ~Age+Sex+Diagnosis+Alcohol+Cannabis+MDMA+Opiates+Psychedelics+Stimulants+Tobacco, data=mydataB))
beta(Model3b)

multiplot(Model1, Model2,Model3, intercept = F, decreasing = T, title = NULL,
          xlab = "Estimate", numeric = F, zeroColor = "grey", plot.shapes = F,
          lwdInner=2,pointSize = 5, cex=7,outerCI=0)

#multiplot((Model3), (Model2),(Model1), intercept = F, decreasing = T, title = NULL,
#          xlab = "Estimate", numeric = F, zeroColor = "grey", plot.shapes = TRUE,
#          lwdInner=2,pointSize = 5, cex=7)+
  #scale_color_manual(values=c("black", "black", "black"))+
#  theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1),outerCI=0)


mydata2 <- data.frame(Age = ALLFU_ocean_wDemogr$age, Sex = ALLFU_ocean_wDemogr$sex,
                     Diagnosis = ALLFU_ocean_wDemogr$PsychDiagAny,
                     CMQ = ALLFU_ocean_wDemogr$CONS5,
                     Alcohol = ALLFU_ocean_wDemogr$ALC_freq,
                     Cannabis = ALLFU_ocean_wDemogr$CAN_freq,
                     MDMA = ALLFU_ocean_wDemogr$MDMA_freq,
                     Opiates = ALLFU_ocean_wDemogr$OPI_freq,
                     Psychedelics = ALLFU_ocean_wDemogr$PSY_freq,
                     Stimulants = ALLFU_ocean_wDemogr$STIM_freq,
                     Tobacco = ALLFU_ocean_wDemogr$TOB_freq
)
Model4 <- lm(CMQ~Age+Sex+Diagnosis+Alcohol+Cannabis+MDMA+Opiates+Psychedelics+Stimulants+Tobacco, data=mydata2)
beta(Model4)
coefplot.glm(Model4, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black",outerCI=0)


#######
# EBS #
#######
mydata <- data.frame(Age = ALLEBS_wDemogr$age, Sex = ALLEBS_wDemogr$sex,
                     Diagnosis = ALLEBS_wDemogr$PsychDiagAny,
                     EBS = ALLEBS_wDemogr$EBS_polit,
                     Alcohol = ALLEBS_wDemogr$ALC_prox,
                     Cannabis = ALLEBS_wDemogr$CAN_prox,
                     MDMA = ALLEBS_wDemogr$MDMA_prox,
                     Opiates = ALLEBS_wDemogr$OPI_prox,
                     Psychedelics = ALLEBS_wDemogr$PSY_prox,
                     PsychedelicsG = as.factor(ALLEBS_wDemogr$PSY_prox>1),
                     Stimulants = ALLEBS_wDemogr$STIM_prox,
                     Tobacco = ALLEBS_wDemogr$TOB_prox
)
Model1 <- (lm(EBS~Psychedelics, data=mydata))
beta(Model1)
# Users Only:
mydataB <- subset(mydata, mydata$PsychedelicsG==T)
Model1b <- (lm(EBS~Psychedelics, data=mydataB))
beta(Model1b)

# The effect is still there even if we regress-out demographics and user/non-user group effects:
Model2 <- (lm(EBS~Age+Sex+Diagnosis+Psychedelics, data=mydata))
# Users Only:
beta(Model2)
Model2b <- (lm(EBS~Age+Sex+Diagnosis+Psychedelics, data=mydataB))
beta(Model2b)

# With adjustments for other drugs:
Model3 <- (lm(EBS~Age+Sex+Diagnosis+Alcohol+Cannabis+MDMA+Opiates+Psychedelics+Stimulants+Tobacco, data=mydata))
beta(Model3)
# Users Only:
Model3b <- (lm(EBS~Age+Sex+Diagnosis+Alcohol+Cannabis+MDMA+Opiates+Psychedelics+Stimulants+Tobacco, data=mydataB))
beta(Model3b)

multiplot(Model1, Model2,Model3, intercept = F, decreasing = T, title = NULL,
          xlab = "Estimate", numeric = F, zeroColor = "grey", plot.shapes = F,
          lwdInner=2,pointSize = 5, cex=7,outerCI=0)




# CorrPlots
library(corrplot)
dat2corr <- ALLEBS_wDemogr[,c('CONS5', 'EBS_feel','EBS_evid', 'EBS_polit',
                                    'PDI_total', 'OLIFE_UE', 'OLIFE_CD', 'OLIFE_IA',
                                    'OLIFE_IN','ASRS', 'raads_any')]

corTab <- cor(dat2corr, use='complete.obs')
corrplot(corTab, method='ellipse')

# T-tests
t.test(ALLEBS_wDemogr$EBS_evid_transformed[ALLEBS_wDemogr$PSY_freq==1],ALLEBS_wDemogr$EBS_evid_transformed[ALLEBS_wDemogr$PSY_freq>1])
effsize::cohen.d(ALLEBS_wDemogr$EBS_evid_transformed[ALLEBS_wDemogr$PSY_freq==1],ALLEBS_wDemogr$EBS_evid_transformed[ALLEBS_wDemogr$PSY_freq>1])

t.test(ALLEBS_wDemogr$EBS_polit[ALLEBS_wDemogr$PSY_prox==1],ALLEBS_wDemogr$EBS_polit[ALLEBS_wDemogr$PSY_prox>1])
effsize::cohen.d(ALLEBS_wDemogr$EBS_polit[ALLEBS_wDemogr$PSY_prox==1],ALLEBS_wDemogr$EBS_polit[ALLEBS_wDemogr$PSY_prox>1])

# CMQ
t.test(ALLFU_ocean_wDemogr$CONS5[ALLFU_ocean_wDemogr$PSY_prox==1],ALLFU_ocean_wDemogr$CONS5[ALLFU_ocean_wDemogr$PSY_prox>1])
effsize::cohen.d(ALLFU_ocean_wDemogr$CONS5[ALLFU_ocean_wDemogr$PSY_prox==1],ALLFU_ocean_wDemogr$CONS5[ALLFU_ocean_wDemogr$PSY_prox>1])

t.test(ALLFU_ocean_wDemogr$CONS_public[ALLFU_ocean_wDemogr$PSY_prox==1],ALLFU_ocean_wDemogr$CONS_public[ALLFU_ocean_wDemogr$PSY_prox>1])
effsize::cohen.d(ALLFU_ocean_wDemogr$CONS_public[ALLFU_ocean_wDemogr$PSY_prox==1],ALLFU_ocean_wDemogr$CONS_public[ALLFU_ocean_wDemogr$PSY_prox>1])

t.test(ALLFU_ocean_wDemogr$CONS_org[ALLFU_ocean_wDemogr$PSY_prox==1],ALLFU_ocean_wDemogr$CONS_org[ALLFU_ocean_wDemogr$PSY_prox>1])
cohen.d(ALLFU_ocean_wDemogr$CONS_org[ALLFU_ocean_wDemogr$PSY_prox==1],ALLFU_ocean_wDemogr$CONS_org[ALLFU_ocean_wDemogr$PSY_prox>1])



# Radar Plots:
library(radarchart)
df <- ALLFU_ocean_wDemogr
df$group <- df$PSY_prox>1
dfp <- t(aggregate(df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],by=list(df$group), FUN=mean))
dfp <- data.frame(labels = c('Misinformation','Politics', 'Monitoring','Hidden Connections', 'Secret Organisations'), NonUsers=round(as.numeric(dfp[2:6,1]),2),
                  Users=round(as.numeric(dfp[2:6,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=15,height=700,
             main = paste('Consipracy Mentality and Psychedelic Drug Use (', 'n1 = ', table(df$group)['FALSE'],', n2 = ',
                          table(df$group)['TRUE'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

vars <- c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')
pv = rep(NA,length(vars))
names(pv) <- vars

t.test(df[df$group==F,'CONS5'],df[df$group==T,'CONS5'])
effsize::cohen.d(df[df$group==F,'CONS5'],df[df$group==T,'CONS5'])



for (i in 1:length(vars)){
  v=vars[i]
  print(paste(i,vars[i]))
  print(t.test(df[df$group==F,vars[i]],df[df$group==T,vars[i]]))
  print(sd(df[df$group==F,vars[i]], na.rm=T))
  print(sd(df[df$group==T,vars[i]], na.rm=T))
  print(effsize::cohen.d(df[df$group==F,vars[i]],df[df$group==T,vars[i]]))
  pv[i] <- t.test(df[df$group==F,vars[i]],df[df$group==T,vars[i]])$p.val
}


p.adjust(pv)



df <- ALLEBS_wDemogr
df$group <- df$PSY_prox>1
dfp <- t(aggregate(df[,c('EBS_feel','EBS_evid', 'EBS_polit')],by=list(df$group), FUN=mean))
dfp <- data.frame(labels = c('FI-facts','Need for evidence', 'Truth is political'), NonUsers=round(as.numeric(dfp[2:4,1]),2),
                  Users=round(as.numeric(dfp[2:4,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=15,height=700,
             main = paste('Consipracy Mentality and Psychedelic Drug Use (', 'n1 = ', table(df$group)['FALSE'],', n2 = ',
                          table(df$group)['TRUE'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

vars <- c('EBS_feel','EBS_evid', 'EBS_polit')
pv = rep(NA,length(vars))
names(pv) <- vars

for (i in 1:length(vars)){
  v=vars[i]
  print(paste(i,vars[i]))
  print(t.test(df[df$group==F,vars[i]],df[df$group==T,vars[i]]))
  print(sd(df[df$group==F,vars[i]], na.rm=T))
  print(sd(df[df$group==T,vars[i]], na.rm=T))
  print(effsize::cohen.d(df[df$group==F,vars[i]],df[df$group==T,vars[i]]))
  pv[i] <- t.test(df[df$group==F,vars[i]],df[df$group==T,vars[i]])$p.val
}

p.adjust(pv)
