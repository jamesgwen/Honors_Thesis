## James Wen
## QSS Thesis
## combine pre 1945 and post 1945 data

library(dplyr)
library(plm)
library(glm)
setwd("~/Desktop/Thesis")

pre_1945 <- read.csv("pre_1945_data.csv") %>% 
  mutate(decade = ifelse(fourty == 1, "forty", 0),
         decade = ifelse(fifty == 1, "fifty", decade),
         decade = ifelse(sixty == 1, "sixty", decade),
         decade = ifelse(seventy == 1, "seventy", decade),
         decade = ifelse(eighty == 1, "eighty", decade), 
         decade = ifelse(ninety == 1, "ninety", decade))



%>% 
  select(ccode, cname, country, 
         ISO_3, year, western, 
         eeurop, lamerica, ssafrica,
         asia.x, nafrme, colbrit, 
         colfra, mtnest, lmtnest,
         elevdiff, Oil, ncontig,
         ethfrac, ef, plural,
         second, numlang, relfrac, 
         plurrel, minrelpc, muslim,
         lat_abst,
         malfal94, avexpr, logem4,
         yellow, baseco, leb95, 
         imr95, meantemp, lt100km,
         latabs, polity2, instab, intensity_level, 
         start_year, onset, tpop, 
         pec, nmc_pop, ind_year)
  


post_1945 <- read.csv("post_1945_data.csv")  %>% 
  mutate(decade = ifelse(forty == 1, "forty", 0),
         decade = ifelse(fifty == 1, "fifty", decade),
         decade = ifelse(sixty == 1, "sixty", decade),
         decade = ifelse(seventy == 1, "seventy", decade),
         decade = ifelse(eighty == 1, "eighty", decade), 
         decade = ifelse(ninety == 1, "ninety", decade),
         decade = ifelse(year >= 2000, "two-thousands", decade),
         cname = as.factor(cname))

%>% 
  select(ccode, cname, country, 
         ISO_3, year, western, 
         eeurop, lamerica, ssafrica,
         asia.x, nafrme, colbrit, 
         colfra, mtnest, lmtnest,
         elevdiff, Oil, ncontig,
         ethfrac, ef, plural,
         second, numlang, relfrac, 
         plurrel, minrelpc, muslim,
         lat_abst,
         malfal94, avexpr, logem4,
         yellow, baseco, leb95, 
         imr95, meantemp, lt100km,
         latabs, polity2, instab, intensity_level, 
         start_year, onset, tpop, 
         pec, nmc_pop, ind_year)

combined <- rbind(pre_1945, post_1945)


## table of obs w/ dif levels of civil war 

total_obs <- nrow(combined)
no <- nrow(subset(combined, is.na(intensity_level)))
minor <- nrow(subset(combined, intensity_level =="1"))
major <- nrow(subset(combined, intensity_level == "2"))

obs_type <- data.frame(no, minor, major)

## table for obs geography

combined_single <- combined %>% 
  filter(year == ind_year)

number_countries <- nrow(combined_single)
west <- nrow(subset(combined_single, western == "1"))
eeu <- nrow(subset(combined_single, eeurop == "1"))
lamerica <- nrow(subset(combined_single,  lamerica== "1"))
ssafrica <- nrow(subset(combined_single,  ssafrica== "1"))
asia <- nrow(subset(combined_single,  asia.x== "1"))
nafrme <- nrow(subset(combined_single,  nafrme== "1"))

country <- data.frame(west, eeu, lamerica, ssafrica, asia, nafrme)


## summary stats
str(combined)

summary(combined$logem4)

summary(combined$avexpr)

summary(combined$ethfrac)

summary(combined$relfrac)

summary(combined$tpop)

summary(combined$mtnest)

## summary stats

summary(combined_single$logem4)

summary(combined_single$avexpr)

summary(combined_single$ethfrac)

summary(combined_single$relfrac)

summary(combined_single$tpop)

summary(combined_single$mtnest)


test <- post_1945 %>% 
  filter(cname == "BURMA")



model.pool<-plm(formula = as.factor(intensity_level) ~ mtnest + Oil, model="within", 
                index = c("ccode", "year"), data = test)
probit_model <- glm(as.factor(intensity_level) ~mtnest + Oil,
                    family=binomial(link="probit"),
                    data = post_1945)

probit_model

test <- post_1945 %>% 
  filter(is.na(tpop))
