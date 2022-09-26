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

load('~/Downloads/DAF_data.rda')


##############
# Regression #
##############

# CONS5:
mydata <- mydataCONS

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


mydata2 <- mydataCONS2

Model4 <- lm(CMQ~Age+Sex+Diagnosis+Alcohol+Cannabis+MDMA+Opiates+Psychedelics+Stimulants+Tobacco, data=mydata2)
beta(Model4)
coefplot.glm(Model4, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black",outerCI=0)


# EBS:
mydata <- mydataEBS

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


# CorrPlot:
library(corrplot)
corTab <- cor(dat2corr, use='complete.obs')
corrplot(corTab, method='ellipse')


########### 
# Groups: #
###########


# Radar Plot, CMQ:
dfp <- dfpCONS
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=15,height=700,
             main = 'Consipracy Mentality and Psychedelic Drug Use',
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)


# Radar Plot, EBS:
dfp <- dfpEBS
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=2,height=700,
             main = 'Epistemic Belief Scale',
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

