
################################################# packages
library(data.table)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(randomForest)
library(rfPermute)
library(MuMIn)
library(emmeans)
library(plotbiomes)
library(gghalves)

##################################################### CAE vs. CUE between different ecosystems ############################
############################################################ CAE in different ecosystems
rm(list = ls())
############################# CAE 
data_CAE<-data.table(read.csv("data_CAE.csv"))
data_CAE$Eco<-as.factor(data_CAE$Eco)
data_CAE$Study<-as.factor(data_CAE$Study)
options(na.action = na.fail)

################################# eliminated data with no ecosystems records
data_eco<-data_CAE[-(48:49)]

#################### Convert to percentage
data_eco$CAE<-data_eco$CAE*100

Box_CAE<-ggplot(data_eco,aes(Eco,CAE,color=Eco,fill=Eco))+
  geom_boxplot(width=0.1,fill="transparent",outlier.shape = NA,linewidth=0.4)+
  geom_half_violin(alpha = 0.5, side = 'top',trim = F)+
  geom_half_point(side  = "l",alpha = 0.3, size  = 1)+
  scale_color_manual(values = c("#F19759", "#5891C5", "#8DBD6C"))+
  scale_fill_manual(values = c("#F19759", "#5891C5", "#8DBD6C"))+
  coord_flip(clip = 'off')+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background= element_rect(fill = "white", color = "white"),
        axis.ticks.y = element_blank(),
        panel.grid.major.y= element_line(linetype="dotted",linewidth=0.3,color='gray'),
        panel.grid.minor.y= element_blank(), 
        panel.grid.major.x= element_blank(),
        panel.grid.minor.x= element_blank(),
        legend.position="none",
        panel.spacing=unit(1,'lines'),
        axis.line.x = element_line(color = "black"),
        axis.text.x = element_text(family = "serif",size = 12),
        axis.text.y = element_text(family = "serif",size = 12),
        axis.title.y = element_text(family = "serif",size=15),
        axis.title.x = element_text(family = "serif",size=15),
        panel.grid =element_blank())+ylab('CAE (%)')+xlab('')

Box_CAE

#################################################### linear mixed model of CAE in different ecosystems
############################## missing value elimination
lmm_box<-dplyr::filter(data_eco,  !is.na(Eco))
lmm_box<-dplyr::filter(lmm_box,  !is.na(Time))
lmm_box<-dplyr::filter(lmm_box,  !is.na(Rate))

############################## logarithmic transformation
lmm_box$CAE<-log10(lmm_box$CAE)

############################## lmm
fit_eco<-lmer(CAE~scale(Rate)+scale(Time)+Eco+(1|Study),data=lmm_box,weights = W)
fit_eco
summary(fit_eco)
anova(fit_eco)
ranova(fit_eco)
EMM1<-emmeans(fit_eco,~Eco,adjust='bonferroni') 
pairs(EMM1)

################################################################ CUE in different ecosystems
rm(list = ls())
############################# CUE
data_CUE<-data.table(read.csv("data_CUE.csv"))
data_CUE$Eco<-as.factor(data_CUE$Eco)
data_CUE$Study<-as.factor(data_CUE$Study)
options(na.action = na.fail)

################################# eliminated data with no ecosystems records
data_eco<-data_CUE[-c(18:19)]

#################### Convert to percentage
data_eco$CUE<-data_eco$CUE*100

Box_CUE<-ggplot(data_eco,aes(Eco,CUE,color=Eco,fill=Eco))+
  geom_boxplot(width=0.1,fill="transparent",outlier.shape = NA,linewidth=0.4)+
  geom_half_violin(alpha = 0.5, side = 'top',trim = F)+
  geom_half_point(side  = "l",alpha = 0.3, size  = 1)+
  scale_color_manual(values = c("#F19759", "#5891C5", "#8DBD6C"))+
  scale_fill_manual(values = c("#F19759", "#5891C5", "#8DBD6C"))+
  coord_flip(clip = 'off')+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background= element_rect(fill = "white", color = "white"),
        axis.ticks.y = element_blank(),
        panel.grid.major.y= element_line(linetype="dotted",linewidth=0.3,color='gray'),
        panel.grid.minor.y= element_blank(), 
        panel.grid.major.x= element_blank(),
        panel.grid.minor.x= element_blank(),
        legend.position="none",
        panel.spacing=unit(1,'lines'),
        axis.line.x = element_line(color = "black"),
        axis.text.x = element_text(family = "serif",size = 12),
        axis.text.y = element_text(family = "serif",size = 12),
        axis.title.y = element_text(family = "serif",size=15),
        axis.title.x = element_text(family = "serif",size=15),
        panel.grid =element_blank())+ylab('CAE (%)')+xlab('')

Box_CUE

####################################################### linear mixed model of CUE in different ecosystems
############################## missing value elimination
lmm_box<-dplyr::filter(data_eco,  !is.na(Eco))
lmm_box<-dplyr::filter(lmm_box,  !is.na(Time))
lmm_box<-dplyr::filter(lmm_box,  !is.na(Rate))

############################## logarithmic transformation
lmm_box$CUE<-log10(lmm_box$CUE)

############################## lmm
fit_eco<-lmer(CUE~scale(Time)+scale(Rate)+Eco+(1|Study),data=lmm_box,weights = W)
fit_eco
summary(fit_eco)
anova(fit_eco)
ranova(fit_eco)
EMM1<-emmeans(fit_eco,~Eco,adjust='bonferroni')
pairs(EMM1)

########################################################### influencing factors of CAE vs. CUE ('CN' represents 'SOC:N') ################################
rm(list = ls())
############################# CAE 
data_CAE<-data.table(read.csv("data_CAE.csv"))
data_CAE$Eco<-as.factor(data_CAE$Eco)
data_CAE$Study<-as.factor(data_CAE$Study)
options(na.action = na.fail)

############################# CUE
data_CUE<-data.table(read.csv("data_CUE.csv"))
data_CUE$Eco<-as.factor(data_CUE$Eco)
data_CUE$Study<-as.factor(data_CUE$Study)
options(na.action = na.fail)

######################################################### Randomforest
########################################### CAE
set.seed(123)
rf_CAE1<-randomForest(CAE~CN+MBC+pH+Clay+Rate+Time+fc,data=data_CAE,importance=TRUE,ntree=500,na.action = na.omit)
rf_CAE1
rf_CAE2<-rfPermute(CAE~CN+MBC+pH+Clay+Rate+Time+fc,data=data_CAE,importance=TRUE,ntree=500,na.action = na.omit)
importance_rf<- data.frame(randomForest::importance(rf_CAE2, scale = TRUE), check.names = TRUE)
importance_rf

############################################ CUE
set.seed(123)
rf_CUE1<-randomForest(CUE~CN+MBC+pH+Clay+Rate+Time+fc,data=data_CUE,importance=TRUE,ntree=500,na.action = na.omit)
rf_CUE1
rf_CUE2<-rfPermute(CUE~CN+MBC+pH+Clay+Rate+Time+fc,data=data_CUE,importance=TRUE,ntree=500,na.action = na.omit)
importance_rf<- data.frame(randomForest::importance(rf_CUE2, scale = TRUE), check.names = TRUE)
importance_rf

##################################################### logarithmic transformation & missing value elimination
data_CAE_lmm<-data_CAE
data_CAE_lmm$CAE<-log10(data_CAE_lmm$CAE) 
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(CAE))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(Rate))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(Time))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(Clay))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(pH))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(CN))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(MBC))
data_CAE_lmm<-dplyr::filter(data_CAE_lmm,  !is.na(fc))

############################################ CUE
data_CUE_lmm<-data_CUE
data_CUE_lmm$CUE<-log10(data_CUE_lmm$CUE) 
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(CUE))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(Rate))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(Time))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(Clay))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(pH))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(CN))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(MBC))
data_CUE_lmm<-dplyr::filter(data_CUE_lmm,  !is.na(fc))

#################################################################### linear mixed model
############################################ CAE
fit_CAE<-lmer(CAE~scale(Clay)+scale(fc)+scale(MBC)+scale(pH)+scale(CN)+scale(Rate)+scale(Time)+(1|Study),data=data_CAE_lmm,weights = W)
fit_CAE
summary(fit_CAE)
anova(fit_CAE)
ranova(fit_CAE)

############################################ CUE
fit_CUE<-lmer(CUE~scale(Clay)+scale(MBC)+scale(CN)+scale(fc)+scale(pH)+scale(Time)+scale(Rate)+(1|Study),data=data_CUE_lmm,weights = W)
fit_CUE
summary(fit_CUE)
anova(fit_CUE)
ranova(fit_CUE)


