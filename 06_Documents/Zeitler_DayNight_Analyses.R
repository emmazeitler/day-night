library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(car)
library(readr)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

##REMOVAL EVENT##

##Set up data
setwd("C:/Users/zeitl/OneDrive/Desktop/Lashley Lab (backup)-LAPTOP-8342CO1L-LAPTOP-8342CO1L-LAPTOP-8342CO1L/Data/Dung Beetle")

d1 <- read.csv("Zeitler_DayNight_Analyses.csv")

##Initial histogram

ggplot(d1, aes(x = Rem)) + geom_histogram(bins=12, color="white") + facet_wrap(~TTrtmnt, scales="free") + theme_bw(base_size = 16)

##Model
modRem1 <- glmmTMB(data = d1, Rem ~ TTrtmnt + STrtmnt + (1|SiteID), family=binomial)

##Simulate residuals

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

##Analyses

Anova(modRem1)
summary(modRem1)
emmeans(modRem1, ~TTrtmnt, type = "response")

r.squaredGLMM(modRem1)

##Plot

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

emRem <- emmeans(modRem1, ~TTrtmnt, type="response") %>% as.data.frame()

plotRem <- ggplot() + labs(x = "Time", y = "Probability of removal") + geom_jitter(data=d1 %>% filter(TTrtmnt =='DAY'), aes(x=TTrtmnt, y=Rem),  height=0, width=.25, size=1, alpha=.1, color="#56B4E9") +
  geom_errorbar(data=emRem ,aes(x=TTrtmnt, y=prob, ymin=(prob-SE), ymax=(prob+SE), color=TTrtmnt), width=.2, lwd=1.25, position=position_dodge(width=0.5)) + 
  geom_point(data=emRem , aes(x=TTrtmnt, y=prob, color=TTrtmnt), size=5, position=position_dodge(width=0.5)) +
  scale_color_manual(values=cbPalette) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA, size=2)) + 
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) + theme(text = element_text(size=20))

plotRem

## NUMBER OF PELLETS REMOVED ##

d1$RemNo1 = gsub("NA", "", d1$RemNo) %>% as.numeric()

##Initial histogram

ggplot(d1, aes(x = RemNo1)) + geom_histogram(bins=12, color="white") + facet_wrap(~TTrtmnt, scales="free") + theme_bw(base_size = 16)

##Model
modRemNo <- glmmTMB(data = d1, RemNo1 ~ TTrtmnt + STrtmnt + (1|SiteID), family=nbinom2)

res_RemNo <- simulateResiduals(fittedModel = modRemNo, n = 250)
hist(res_RemNo)
plot(res_RemNo)

Anova(modRemNo)
summary(modRemNo)
emmeans(modRemNo, ~TTrtmnt, type="response")

r.squaredGLMM(modRemNo)

##Plot

emRemNo <- emmeans(modRemNo, ~TTrtmnt, type="response") %>% as.data.frame()

plotRemNo <- ggplot() + labs(x = "Time", y = "Count of removed scat") + geom_jitter(data=d1 %>% filter(TTrtmnt=='DAY'), aes(x=TTrtmnt, y=Rem),  height=0, width=.25, size=1, alpha=.1, color="#56B4E9") +
  geom_errorbar(data=emRemNo ,aes(x=TTrtmnt, y=response, ymin=(response-SE), ymax=(response+SE), color=TTrtmnt), width=.2, lwd=1.25, position=position_dodge(width=0.5)) + 
  geom_point(data=emRemNo , aes(x=TTrtmnt, y=response, color=TTrtmnt), size=5, position=position_dodge(width=0.5)) +
  scale_color_manual(values=cbPalette) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA, size=2)) + 
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +
  theme(text = element_text(size=20))
plotRemNo
