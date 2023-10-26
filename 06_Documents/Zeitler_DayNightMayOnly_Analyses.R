library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(car)
library(readr)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

setwd("C:/Users/zeitl/OneDrive/Desktop/Lashley Lab (backup)-LAPTOP-8342CO1L-LAPTOP-8342CO1L-LAPTOP-8342CO1L/Data/Dung Beetle")

d2 <- read_csv("Zeitler_DayNightMayOnly_Analyses.csv")

##Removal Event##

modRem1 <- glmmTMB(data = d1, Rem ~ TTrtmnt * STrtmnt + (1|SiteID), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)
emmeans(modRem1, ~STrtmnt, type = "response")

##Number of pellets removed##

d1$RemNo1 = gsub("NA", "", d1$RemNo) %>% as.numeric()

##Initial histogram

ggplot(d1, aes(x = RemNo1)) + geom_histogram(bins=12, color="white") + facet_wrap(~STrtmnt, scales="free") + theme_bw(base_size = 16)

##Model
modRemNo <- glmmTMB(data = d1, RemNo1 ~ TTrtmnt * STrtmnt + (1|SiteID), family=nbinom2)

res_RemNo <- simulateResiduals(fittedModel = modRemNo, n = 250)
hist(res_RemNo)
plot(res_RemNo)

Anova(modRemNo)
summary(modRemNo)
emmeans(modRemNo, ~STrtmnt, type="response")
