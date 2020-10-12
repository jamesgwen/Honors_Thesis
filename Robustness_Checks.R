## James Wen
## Thesis
## Robustness Checks


library(readstata13)
library(ggplot2)
library(gdata)
library(tidyverse)
library(stringr)
library(lubridate)
library(stargazer)
library(corrplot)
library(ivprobit)
library(lme4)
setwd("~/Desktop/Thesis")
# Correlation between 2 depen var -----------------------------------------

setwd("~/Desktop/Thesis/Acemoglu_Johnson_Robinson/maketable7")
mort_expro <- read.dta13("maketable7.dta")

cor(mort_expro$avexpr, mort_expro$logem4, use = "complete.obs")

## produces value of -0.5517566

cor(post_1945$avexpr, post_1945$instab, use = "complete.obs")

cor(post_1945$lat_abst, post_1945$ef, use = "complete.obs")

cor(post_1945$avexpr, post_1945$ncontig, use = "complete.obs")
# Just Regressing protection from expropriation -----------------------------------------------------

test_1 <- glm(minor ~ avexpr,
                   family=binomial(link="logit"),
                   data = combine)

test_2 <- glm(major ~ avexpr,
              family=binomial(link="logit"),
              data = combine)

test_3 <- glm(onset ~ avexpr,
              family=binomial(link="logit"),
              data = combine)

## all negative coeff 

# Just regressing settler deaths ------------------------------------------

test_4 <- glm(minor ~ logem4,
              family=binomial(link="logit"),
              data = combine)

test_5 <- glm(major ~ logem4,
              family=binomial(link="logit"),
              data = combine)

test_6 <- glm(onset ~ logem4,
              family=binomial(link="logit"),
              data = combine)

## test 4 produced a negative coef but not stat sig
## test 5 produced a negative coef and stat sig
## test 6 produced a postive coef and stat sig


# graph of protection from expropriation and settler deaths ---------------

ggplot(data = mort_expro, aes(x = logem4, y = avexpr)) +
  geom_point() +
  labs(x = "Log of Settler Deaths",
       y = "Protection from Expropriation",
       title = "Settler Death vs. Protection from Expropriation",
       subtitle = "Source: Acemoglu, Johnson, and Robinson (2001)")


# Graph of # years of civil war vs DV -------------------------------------

num_years <- post_1945 %>% 
  select(cname, logem4, avexpr, intensity_level) %>% 
  mutate(civil_war = ifelse(intensity_level == 2, 1, intensity_level)) %>% 
  select(cname, logem4, avexpr, civil_war) %>% 
  group_by(cname, logem4, avexpr) %>% 
  summarize(year_civil_war = sum(civil_war)) %>% 
  mutate(binary_civil_war = ifelse(year_civil_war > 0, 1, 0))

ggplot(data = num_years, aes(x = logem4, y = year_civil_war)) +
  geom_point() +
  labs(x = "Log of Settler Deaths",
       y = "Years of Civil War post 1945")

ggplot(data = num_years, aes(x = avexpr, y = year_civil_war)) +
  geom_point() +
  labs(x = "Protection Against Expropriation",
       y = "Years of Civil War post 1945")

high_years <- num_years %>% 
  filter(year_civil_war > 30)


# Single country = observation model --------------------------------------

single <- post_1945 %>% 
  filter(year == ind_year)

civil_single <- post_1945 %>% 
  select(cname, logem4, avexpr, intensity_level) %>% 
  mutate(civil_war = ifelse(intensity_level == 2, 1, intensity_level)) %>% 
  select(cname, logem4, avexpr, civil_war) %>% 
  group_by(cname, logem4, avexpr) %>% 
  summarize(year_civil_war = sum(civil_war)) %>% 
  mutate(binary_civil_war = ifelse(year_civil_war > 0, 1, 0)) %>% 
  ungroup()#%>% 
  #select(cname, binary_civil_war)

single_obs <- left_join(single, civil_single, by = "cname") %>% 
  rename(independence_year = year)
write.csv(single_obs, file = "single.csv", row.names=FALSE)

low_death <- single_obs %>% 
  filter(logem4 <=4)

single_regression <- glm(binary_civil_war ~ 
                            mtnest + ncontig + tpop + logem4 +
                            ef + relfrac,
                          family=binomial(link="logit"),
                          data = single_obs)

single_regression_exp <- glm(binary_civil_war ~ 
                           mtnest + ncontig + tpop + avexpr + 
                           ef + relfrac,
                         family=binomial(link="logit"),
                          data = single_obs)

stargazer(single_regression, single_regression_exp,
           title="Robustness Checks, 1 country = 1 observation",
           dep.var.labels=c("Occurance of Civil War, countries ind. post 1945"),
          covariate.labels=c("Percent Mountainous", "Noncontiguous", "Population, 1000s",
                             "Settler Deaths, log","Expropiation",
                             "Ethnic Fractionalization", "Religous Fractionalization"),
           omit.stat=c("LL","f"),
           star.cutoffs = c(0.10, 0.05, 0.01),
           type = "latex",
           digits = 5,
           column.labels = c("Model 1", "Model 2"))

single_regression_no_pop <- glm(binary_civil_war ~ 
                           mtnest + ncontig  + logem4 +
                           ef + relfrac,
                         family=binomial(link="logit"),
                         data = single_obs)

single_regression_exp_no_pop <- glm(binary_civil_war ~ 
                               mtnest + ncontig  + avexpr + 
                               ef + relfrac,
                             family=binomial(link="logit"),
                             data = single_obs)

stargazer(single_regression_no_pop, single_regression_exp_no_pop,
          title="Robustness Checks, 1 country = 1 observation",
          dep.var.labels=c("Occurance of Civil War, countries ind. post 1945"),
          covariate.labels=c("Percent Mountainous", "Noncontiguous", 
                             "Settler Deaths, log","Expropiation",
                             "Ethnic Fractionalization", "Religous Fractionalization"),
          omit.stat=c("LL","f"),
          star.cutoffs = c(0.10, 0.05, 0.01),
          type = "latex",
          digits = 5,
          column.labels = c("Model 1", "Model 2"))



# Correlation Matrix ------------------------------------------------------

post_1945_new_name <- post_1945 %>% 
  select(lat_abst, mtnest, ncontig ,
         tpop , logem4, avexpr, 
         ef , relfrac) %>% 
  # rename(`absolute latitude` = lat_abst, `percent mountain` = mtnest,
  #        noncontiguous = ncontig, population = tpop, `setter death` = logem4, 
  #        `protection from expropriation` = avexpr, `ethnic fractionalization` = ef,
  #        `religious fractionalization` = relfrac)


test_all <- post_1945_new_name %>%
  na.omit()

test_1_single <- single_obs %>% 
  select(lat_abst, mtnest, ncontig ,
          tpop , logem4.x  , avexpr.x,
          ef , relfrac) %>% 
  # rename(`absolute latitude` = lat_abst, `percent mountain` = mtnest,
  #        noncontiguous = ncontig, population = tpop, `setter death` = logem4.x, 
  #        `protection from expropriation` = avexpr.x, `ethnic fractionalization` = ef,
  #        `religious fractionalization` = relfrac)%>% 
  na.omit()
  

cor_post_1945_all <-cor(test_all)
corrplot(cor_post_1945_all, method = "number", number.cex=0.5)

cor_single<- cor(test_1_single)
corrplot(cor_single, method = "number", number.cex=0.5)


# Average of Civil War b/w Acemoglu Data ----------------------------------

num_years <- combine%>% 
  select(cname, logem4, avexpr, intensity_level) %>% 
  mutate(civil_war = ifelse(intensity_level == 2, 1, intensity_level)) %>% 
  select(cname, logem4, avexpr, civil_war) %>% 
  group_by(cname, logem4, avexpr) %>% 
  summarize(year_civil_war = sum(civil_war)) %>% 
  mutate(binary_civil_war = ifelse(year_civil_war > 0, 1, 0)) %>% 
  mutate(category = ifelse(is.na(logem4) & is.na(avexpr), "missing_data", "data"))

t.test(num_years$binary_civil_war ~ num_years$category)
# pvalue of .03328 
# 95% interval 0.0438316 0.8269549

t.test(num_years$year_civil_war~ num_years$category)


# Average war b/w death and settling   -------------------------------------
expro <- civil_single %>% 
  select(avexpr, binary_civil_war) %>% 
  filter(!is.na(avexpr))

mean(expro$binary_civil_war)
# .66666

death <- civil_single %>% 
  select(logem4, binary_civil_war) %>% 
  filter(!is.na(logem4))
mean(death$binary_civil_war)

# .714

a <- expro %>% 
  rename(category = avexpr) %>% 
  mutate(category = 1)

b <- death %>% 
  rename(category = logem4) %>% 
  mutate(category = 2)

c <- rbind(a, b) %>% 
  mutate(category = as.factor(category))

t.test(c$binary_civil_war ~ cat$category, alt = "two.sided")



# Average number of years -------------------------------------------------
num_years <- post_1945%>% 
  select(cname, logem4, avexpr, intensity_level) %>% 
  mutate(civil_war = ifelse(intensity_level == 2, 1, intensity_level)) %>% 
  select(cname, logem4, avexpr, civil_war) %>% 
  group_by(cname, logem4, avexpr) %>% 
  summarize(year_civil_war = sum(civil_war)) %>% 
  mutate(binary_civil_war = ifelse(year_civil_war > 0, 1, 0)) %>% 
  ungroup()

expro_year <- num_years %>% 
  select(avexpr, year_civil_war) %>% 
  filter(!is.na(avexpr))

mean(expro_year$year_civil_war)
# 10.24074

death_year <- num_years %>% 
  select(logem4, year_civil_war) %>% 
  filter(!is.na(logem4))
mean(death_year$year_civil_war)
# 9.489796

expro_cat <- expro_year %>% 
  mutate(avexpr = "expro") %>% 
  rename(category = avexpr) 

death_cat <- death_year %>% 
  mutate(logem4 = "death") %>% 
  rename(category = logem4)

cat <- rbind(expro_cat, death_cat) %>% 
  mutate(category = as.factor(category))

t.test(cat$year_civil_war ~ cat$category, alt = "two.sided")


# random effects logit ----------------------------------------------------

glmer(formula = onset ~ 
        mtnest + ncontig +
        tpop + avexpr  + 
        ef + relfrac, 
      data = post_1945,
      family = "binomial")
