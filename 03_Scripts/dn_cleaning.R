#### SET UP WORKSPACE ####
library(tidyverse)

dn_raw <- read.csv("01_Raw_Data/Zeitler_DayNight_Analyses.csv")

#### Design data ####

x <- dn_raw %>% 
  select(SiteID, Pair.ID, TTrtmnt, STrtmnt, Rem, RemNo, Latency) %>% 
  rename(site = SiteID,
         pair = Pair.ID,
         dep_time = TTrtmnt,
         dung_type = STrtmnt,
         rem = Rem,
         remno = RemNo,
         lat = Latency) %>% 
  filter(!pair == 0)

#### Convert to factors ####
x$site <- as.factor(x$site)
x$pair <- as.factor(x$pair)
x$dep_time <- as.factor(x$dep_time)
x$dung_type <- as.factor(x$dung_type)

#### Correct latency ####
# d1$Latency <- as.numeric(d1$Latency)
# lat <- d1[, c("Latency")]
# lat[is.na(lat)] <- 24
# d1[, c("Latency")] <- lat

#### No Rem column ####

day_rem <- x %>% 
  select(pair, dep_time, rem) %>% 
  filter(dep_time == "DAY") %>% 
  select(-dep_time) %>% 
  rename(day = rem)

night_rem <- x %>% 
  select(pair, dep_time, rem) %>% 
  filter(dep_time == "NIGHT") %>% 
  select(-dep_time) %>% 
  rename(night = rem)

norem <- day_rem %>% 
  merge(night_rem, by="pair") %>% 
  unite("norempair", 2:3, sep = "") 

norem$norempair[norem$norempair == '11'] <- '0'
norem$norempair[norem$norempair == '01'] <- '0'
norem$norempair[norem$norempair == '10'] <- '0'
norem$norempair[norem$norempair == '00'] <- '1'

x <- merge(x, norem, by="pair")

#### Finished ####

write_csv(x, "02_Clean_Data/dn_clean.csv")
