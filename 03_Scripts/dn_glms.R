##### SET UP WORKSPACE ####
library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

d2 <- read_csv("02_Clean_Data/dn_clean.csv")

d3 <- d2 %>% 
  filter(norempair == 0)

#### Removal event model ####

modRem <- glmmTMB(data = d2, rem ~ dep_time + (1|site) + (1|pair), family=binomial)

resRem <- simulateResiduals(fittedModel = modRem, n = 250)
hist(resRem)
plot(resRem)

Anova(modRem)
summary(modRem)

emmeans(modRem, ~dep_time, type = "response")
remev.mod <- emmeans(modRem, ~dep_time, type = "response") %>% as.data.frame()

#### Remprob figure ####

ggplot(data=remev.mod) +
  geom_point(aes(x = dep_time,
                 y = prob,
                 color = dep_time),
             size = 5)+
  geom_errorbar(aes(x = dep_time,
                    y = prob,
                    ymin = asymp.LCL,
                    ymax= asymp.UCL,
                    width = .1,
                    color = dep_time))+
  scale_color_manual(values=c("blue", "black"))+
  scale_y_continuous(limits=c(0,1))+
  labs(x = "Deposition time",
       y = "Probability of removal")

#### Removal amount model ####

modRemno <- glmmTMB(data = d3, remno ~ dep_time + (1|site) + (1|pair), family=nbinom2())

resRemno <- simulateResiduals(fittedModel = modRemno, n = 250)
hist(resRemno)
plot(resRemno)

Anova(modRemno)
summary(modRemno)

emmeans(modRemno, ~dep_time, type = "response")
remno.mod <- emmeans(modRemno, ~dep_time, type = "response") %>% as.data.frame()

#### Remno figure ####
ggplot(data=remno.mod) +
  geom_point(aes(x = dep_time,
                 y = response,
                 color = dep_time),
             size = 5)+
  geom_errorbar(aes(x = dep_time,
                    y = response,
                    ymin = asymp.LCL,
                    ymax= asymp.UCL,
                    width = .1,
                    color = dep_time))+
  scale_color_manual(values=c("blue", "black"))+
  scale_y_continuous(limits=c(0,8))+
  labs(x = "Deposition time",
       y = "Amount of dung removed")
