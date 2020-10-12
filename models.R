## James Wen
## QSS Thesis
## R script for running and checking models


# rm(list=ls())
setwd("~/Desktop/Thesis")
library(dplyr)
library(plm)




# Load Data and Combine ---------------------------------------------------

post_1945 <- read.csv("post_1945_data.csv") %>% 
  mutate(decade = ifelse(forty == 1, "1_forty", 0),
         decade = ifelse(fifty == 1, "2_fifty", decade),
         decade = ifelse(sixty == 1, "3_sixty", decade),
         decade = ifelse(seventy == 1, "4_seventy", decade),
         decade = ifelse(eighty == 1, "5_eighty", decade), 
         decade = ifelse(ninety == 1, "6_ninety", decade)) %>% 
  mutate(onset = ifelse(intensity_level == 1 | intensity_level == 2, NA, 0),
         onset = ifelse(intensity_level == 1 & lag(intensity_level, 1) == 0, 1, onset),
         onset = ifelse(intensity_level == 2 & lag(intensity_level, 1) == 0, 1, onset)) 
         #decade = factor(decade, levels = c("forty", "fifty", "sixty", "seventy", "eighty", "ninety")))

##write.csv(post_1945, file = "post1945.csv", row.names=FALSE)

pre_1945 <- read.csv("pre_1945_data.csv") %>% 
  mutate(decade = ifelse(forty == 1, "1_forty", 0),
         decade = ifelse(fifty == 1, "2_fifty", decade),
         decade = ifelse(sixty == 1, "3_sixty", decade),
         decade = ifelse(seventy == 1, "4_seventy", decade),
         decade = ifelse(eighty == 1, "5_eighty", decade), 
         decade = ifelse(ninety == 1, "6_ninety", decade)) %>% 
  mutate(onset = ifelse(intensity_level == 1 | intensity_level == 2, NA, 0),
       onset = ifelse(intensity_level == 1 & lag(intensity_level, 1) == 0, 1, onset),
       onset = ifelse(intensity_level == 2 & lag(intensity_level, 1) == 0, 1, onset)) 
         #decade = factor(decade, levels = c("forty", "fifty", "sixty","seventy", "eighty", "ninety")))

combine <- rbind(pre_1945, post_1945) %>% 
  mutate(onset = ifelse(intensity_level == 1 | intensity_level == 2, NA, 0),
         onset = ifelse(intensity_level == 1 & lag(intensity_level, 1) == 0, 1, onset),
         onset = ifelse(intensity_level == 2 & lag(intensity_level, 1) == 0, 1, onset))
         

# Post-1945 Minor Models --------------------------------------------------------

# Model 1: latitude, mountain, noncontigous 
post_minor_1 <- glm(minor ~ 
                     lat_abst + mtnest + ncontig,
                   family=binomial(link="logit"),
                   data = post_1945)
# Model 2: population,  mountain, noncontigous, expropiation,
post_minor_2 <- glm(minor ~ 
                 mtnest + ncontig +
                tpop + avexpr + logem4,
              family=binomial(link="logit"),
              data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation,
# ethnic and relig fractionalization 
post_minor_3 <- glm(minor ~ 
                 mtnest + ncontig +
                tpop + avexpr + logem4 +
                ef + relfrac,
              family=binomial(link="logit"),
              data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation,
# ethnic, relig fractionalization, decade of war 
post_minor_4 <- glm(minor ~ 
                mtnest + ncontig +
                tpop + avexpr + logem4 + 
                ef + relfrac + as.factor(decade),
              family=binomial(link="logit"),
              data = post_1945)

# Model 5: population,  mountain, noncontigous, expropiation,
# ethnic, relig fractionalization, time since independence 
post_minor_5 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac + years_since,
                    family=binomial(link="logit"),
                    data = post_1945)



# Post 1945 Major Models --------------------------------------------------

# Model 1: latitude, mountain, noncontigous 
post_major_1 <- glm(major ~ 
                      lat_abst + mtnest + ncontig,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 2: population,  mountain, noncontigous, expropiation, deaths
post_major_2 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
post_major_3 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
post_major_4 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac + as.factor(decade),
                    family=binomial(link="logit"),
                    data = post_1945)

post_major_5 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac + years_since,
                    family=binomial(link="logit"),
                    data = post_1945)





# Pre and Post Minor Model ------------------------------------------------

# Model 1: latitude, mountain, noncontigous 
minor_1 <- glm(minor ~ 
                      lat_abst + mtnest + ncontig,
                    family=binomial(link="logit"),
                    data = combine)
# Model 2: population,  mountain, noncontigous, expropiation, deaths
minor_2 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4,
                    family=binomial(link="logit"),
                    data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
minor_3 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac,
                    family=binomial(link="logit"),
                    data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
minor_4 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac + as.factor(decade),
                    family=binomial(link="logit"),
                    data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
minor_5 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr + logem4 + 
                      ef + relfrac + years_since,
                    family=binomial(link="logit"),
                    data = combine)


# Pre and Post Major Models -----------------------------------------------

major_1 <- glm(major ~ 
                 lat_abst + mtnest + ncontig,
               family=binomial(link="logit"),
               data = combine)
# Model 2: population,  mountain, noncontigous, expropiation, deaths
major_2 <- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4,
               family=binomial(link="logit"),
               data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
major_3 <- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac,
               family=binomial(link="logit"),
               data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
major_4 <- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac + as.factor(decade),
               family=binomial(link="logit"),
               data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
major_5 <- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac + years_since,
               family=binomial(link="logit"),
               data = combine)


# Onset Everything -------------------------------------------------------------------

onset_1 <- glm(onset ~ 
                 lat_abst + mtnest + ncontig,
               family=binomial(link="logit"),
               data = combine)
# Model 2: population,  mountain, noncontigous, expropiation, deaths
onset_2 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4,
               family=binomial(link="logit"),
               data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
onset_3 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac,
               family=binomial(link="logit"),
               data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
onset_4 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac + as.factor(decade),
               family=binomial(link="logit"),
               data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
onset_5 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac + years_since,
               family=binomial(link="logit"),
               data = combine)


# Onset Post 1945 ---------------------------------------------------------

post_onset_1 <- glm(onset ~ 
                 lat_abst + mtnest + ncontig,
               family=binomial(link="logit"),
               data = post_1945)

# Model 2: population,  mountain, noncontigous, expropiation, deaths
post_onset_2 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4,
               family=binomial(link="logit"),
               data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
post_onset_3 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac,
               family=binomial(link="logit"),
               data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
post_onset_4 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac + as.factor(decade),
               family=binomial(link="logit"),
               data = post_1945)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
post_onset_5 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + logem4 + 
                 ef + relfrac + years_since,
               family=binomial(link="logit"),
               data = post_1945)

# Post-1945 Minor Models with just expropiation--------------------------------------------------------

post_minor <-  glm(minor ~ 
                     lat_abst +  mtnest + ncontig +
                     tpop,
                   family=binomial(link="logit"),
                   data = post_1945)
# Model 2: population,  mountain, noncontigous, expropiation,
post_minor_ex_1 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation,
# ethnic and relig fractionalization 
post_minor_ex_2 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr +
                      ef + relfrac,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation,
# ethnic, relig fractionalization, decade of war 
post_minor_ex_3 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr  + 
                      ef + relfrac + as.factor(decade),
                    family=binomial(link="logit"),
                    data = post_1945)

# Model 5: population,  mountain, noncontigous, expropiation,
# ethnic, relig fractionalization, time since independence 
post_minor_ex_4 <- glm(minor ~ 
                      mtnest + ncontig +
                      tpop + avexpr +  
                      ef + relfrac + years_since,
                    family=binomial(link="logit"),
                    data = post_1945)

post_minor_ex_5 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + avexpr + 
                         ef + relfrac + ssafrica + 
                         asia.x + nafrme,
                       family=binomial(link="logit"),
                       data = post_1945)


# Post-1945 Minor Models with just death ----------------------------------

post_minor_death_1 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + logem4,
                       family=binomial(link="logit"),
                       data = post_1945)
# Model 3: population,  mountain, noncontigous, death,
# ethnic and relig fractionalization 
post_minor_death_2 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + logem4 +
                         ef + relfrac,
                       family=binomial(link="logit"),
                       data = post_1945)
# Model 4: population,  mountain, noncontigous, death,
# ethnic, relig fractionalization, decade of war 
post_minor_death_3 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + logem4  + 
                         ef + relfrac + as.factor(decade),
                       family=binomial(link="logit"),
                       data = post_1945)

# Model 5: population,  mountain, noncontigous, death,
# ethnic, relig fractionalization, time since independence 
post_minor_death_4 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + logem4 +  
                         ef + relfrac + years_since,
                       family=binomial(link="logit"),
                       data = post_1945)


post_minor_death_5 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + logem4 + 
                         ef + relfrac + ssafrica + 
                         asia.x + nafrme,
                       family=binomial(link="logit"),
                       data = post_1945)

post_minor_death_6 <- glm(minor ~ 
                         mtnest + ncontig +
                         tpop + logem4 + 
                         ef + relfrac + ssafrica + 
                         asia.x + nafrme,
                       family=binomial(link="logit"),
                       data = post_1945)







# Pre and Post Minor with just expropiation -------------------------------

minor_ex_1 <- glm(minor ~ 
                    mtnest + ncontig +
                    tpop + avexpr,
                  family=binomial(link="logit"),
                  data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
minor_ex_2 <- glm(minor ~ 
                    mtnest + ncontig +
                    tpop + avexpr  + 
                    ef + relfrac,
                  family=binomial(link="logit"),
                  data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
minor_ex_3 <- glm(minor ~ 
                    mtnest + ncontig +
                    tpop + avexpr  + 
                    ef + relfrac + as.factor(decade),
                  family=binomial(link="logit"),
                  data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
minor_ex_4 <- glm(minor ~ 
                    mtnest + ncontig +
                    tpop + avexpr + 
                    ef + relfrac + years_since,
                  family=binomial(link="logit"),
                  data = combine)

# Pre and Post Minor with just death --------------------------------------

minor_death_1 <- glm(minor ~ 
                       mtnest + ncontig +
                       tpop + logem4,
                     family=binomial(link="logit"),
                     data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
minor_death_2 <- glm(minor ~ 
                       mtnest + ncontig +
                       tpop + logem4  + 
                       ef + relfrac,
                     family=binomial(link="logit"),
                     data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
minor_death_3 <- glm(minor ~ 
                       mtnest + ncontig +
                       tpop + logem4  + 
                       ef + relfrac + as.factor(decade),
                     family=binomial(link="logit"),
                     data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
minor_death_4 <- glm(minor ~ 
                       mtnest + ncontig +
                       tpop + logem4 + 
                       ef + relfrac + years_since,
                     family=binomial(link="logit"),
                     data = combine)

# Post 1945 Major Models with just expropriation  -------------------------

# Model 2: population,  mountain, noncontigous, expropiation, deaths

post_major <-  glm(major ~ 
                     lat_abst+mtnest + ncontig +
                     tpop ,
                   family=binomial(link="logit"),
                   data = post_1945)

post_major_ex_1 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr ,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
post_major_ex_2 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr  + 
                      ef + relfrac,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
post_major_ex_3 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr + 
                      ef + relfrac + as.factor(decade),
                    family=binomial(link="logit"),
                    data = post_1945)

post_major_ex_4 <- glm(major ~ 
                      mtnest + ncontig +
                      tpop + avexpr +  
                      ef + relfrac + years_since,
                    family=binomial(link="logit"),
                    data = post_1945)

post_major_ex_5 <- glm(major ~ 
                         mtnest + ncontig +
                         tpop + avexpr  + 
                         ef + relfrac + 
                         ssafrica + 
                         asia.x + nafrme,
                       family=binomial(link="logit"),
                       data = post_1945)


# Post 1945 Major Models with just death ----------------------------------
post_major_death_1 <- glm(major ~ 
                         mtnest + ncontig +
                         tpop + logem4 ,
                       family=binomial(link="logit"),
                       data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
post_major_death_2 <- glm(major ~ 
                         mtnest + ncontig +
                         tpop + logem4  + 
                         ef + relfrac,
                       family=binomial(link="logit"),
                       data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
post_major_death_3 <- glm(major ~ 
                         mtnest + ncontig +
                         tpop + logem4 + 
                         ef + relfrac + as.factor(decade),
                       family=binomial(link="logit"),
                       data = post_1945)

post_major_death_4 <- glm(major ~ 
                         mtnest + ncontig +
                         tpop + logem4 +  
                         ef + relfrac + years_since,
                       family=binomial(link="logit"),
                       data = post_1945)

post_major_death_5 <- glm(major ~ 
                            mtnest + ncontig +
                            tpop + logem4  + 
                            ef + relfrac + ssafrica + 
                            asia.x + nafrme,
                          family=binomial(link="logit"),
                          data = post_1945)


# Pre and Post Major with just expropiation -------------------------------

# Model 2: population,  mountain, noncontigous, expropiation, deaths
major_ex_1 <- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr ,
               family=binomial(link="logit"),
               data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
major_ex_2<- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr +  
                 ef + relfrac,
               family=binomial(link="logit"),
               data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
major_ex_3 <- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr + 
                 ef + relfrac + as.factor(decade),
               family=binomial(link="logit"),
               data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
major_ex_4<- glm(major ~ 
                 mtnest + ncontig +
                 tpop + avexpr + 
                 ef + relfrac + years_since,
               family=binomial(link="logit"),
               data = combine)
# Pre and Post Major with just death --------------------------------------

major_death_1 <- glm(major ~ 
                    mtnest + ncontig +
                    tpop + logem4 ,
                  family=binomial(link="logit"),
                  data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
major_death_2<- glm(major ~ 
                   mtnest + ncontig +
                   tpop + logem4 +  
                   ef + relfrac,
                 family=binomial(link="logit"),
                 data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
major_death_3 <- glm(major ~ 
                    mtnest + ncontig +
                    tpop + logem4 + 
                    ef + relfrac + as.factor(decade),
                  family=binomial(link="logit"),
                  data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
major_death_4<- glm(major ~ 
                   mtnest + ncontig +
                   tpop + logem4 + 
                   ef + relfrac + years_since,
                 family=binomial(link="logit"),
                 data = combine)

# Onset Everything with just expropriation --------------------------------

# Model 2: population,  mountain, noncontigous, expropiation, deaths

onset_1 <- glm(onset ~ 
                 lat_abst + mtnest + ncontig,
               family=binomial(link="logit"),
               data = combine)


onset_ex_1 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr ,
               family=binomial(link="logit"),
               data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
onset_ex_2 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + 
                 ef + relfrac,
               family=binomial(link="logit"),
               data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
onset_ex_3 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr + 
                 ef + relfrac + as.factor(decade),
               family=binomial(link="logit"),
               data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
onset_ex_4 <- glm(onset ~ 
                 mtnest + ncontig +
                 tpop + avexpr +  
                 ef + relfrac + years_since,
               family=binomial(link="logit"),
               data = combine)

onset_ex_5 <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + avexpr  + 
                         ef + relfrac + 
                         ssafrica + 
                         asia.x + nafrme,
                       family=binomial(link="logit"),
                       data = combine)

# Onset Everything with just death --------------------------------

onset_death_1 <- glm(onset ~ 
                    mtnest + ncontig +
                    tpop + logem4 ,
                  family=binomial(link="logit"),
                  data = combine)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
onset_death_2 <- glm(onset ~ 
                    mtnest + ncontig +
                    tpop + logem4 + 
                    ef + relfrac,
                  family=binomial(link="logit"),
                  data = combine)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
onset_death_3 <- glm(onset ~ 
                    mtnest + ncontig +
                    tpop + logem4 + 
                    ef + relfrac + as.factor(decade),
                  family=binomial(link="logit"),
                  data = combine)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
onset_death_4 <- glm(onset ~ 
                    mtnest + ncontig +
                    tpop + logem4 +  
                    ef + relfrac + years_since,
                  family=binomial(link="logit"),
                  data = combine)

onset_death_5 <- glm(onset ~ 
                            mtnest + ncontig +
                            tpop + logem4  + 
                            ef + relfrac + ssafrica + 
                            asia.x + nafrme,
                          family=binomial(link="logit"),
                          data = combine)



# Onset post 1945 with just expropriation  --------------------------------

# Model 2: population,  mountain, noncontigous, expropiation, deaths
post_onset_ex_1 <- glm(onset ~ 
                      mtnest + ncontig +
                      tpop + avexpr,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
post_onset_ex_2 <- glm(onset ~ 
                      mtnest + ncontig +
                      tpop + avexpr  + 
                      ef + relfrac,
                    family=binomial(link="logit"),
                    data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
post_onset_ex_3 <- glm(onset ~ 
                      mtnest + ncontig +
                      tpop + avexpr  + 
                      ef + relfrac + as.factor(decade),
                    family=binomial(link="logit"),
                    data = post_1945)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
post_onset_ex_4 <- glm(onset ~ 
                      mtnest + ncontig +
                      tpop + avexpr  + 
                      ef + relfrac + years_since,
                    family=binomial(link="logit"),
                    data = post_1945)

post_onset_ex_5 <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + avexpr  + 
                         ef + relfrac + 
                         ssafrica + 
                         asia.x + nafrme,
                       family=binomial(link="logit"),
                       data = post_1945)

# Onset post 1945 with just death -----------------------------------------

post_onset_death_1 <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + logem4,
                       family=binomial(link="logit"),
                       data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
post_onset_death_2 <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + logem4  + 
                         ef + relfrac,
                       family=binomial(link="logit"),
                       data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 
post_onset_death_3 <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + logem4  + 
                         ef + relfrac + as.factor(decade),
                       family=binomial(link="logit"),
                       data = post_1945)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
post_onset_death_4 <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + logem4  + 
                         ef + relfrac + years_since,
                       family=binomial(link="logit"),
                       data = post_1945)

post_onset_death_5 <- glm(onset ~ 
                            mtnest + ncontig +
                            tpop + logem4  + 
                            ef + relfrac + ssafrica + 
                            asia.x + nafrme,
                          family=binomial(link="logit"),
                          data = post_1945)

# Random Effects Model expro -----------------------------------------------------

post_onset_random <- plm(formula = onset ~ 
                           lat_abst + mtnest + ncontig, 
                         data = post_1945, 
                         model = "random", 
                         index = c("cname", "year"))

post_onset_ex_random_1 <- plm(formula = onset ~ 
                               mtnest + ncontig +
                               tpop + avexpr, 
                             data = post_1945, 
                             model = "random", 
                             index = c("cname", "year"))

post_onset_ex_random_2 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + avexpr  + 
                                ef + relfrac, 
                             data = post_1945, 
                             model = "random", 
                             index = c("cname", "year"))

post_onset_ex_random_3 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + avexpr  + 
                                ef + relfrac + 
                                as.factor(decade), 
                              data = post_1945,
                              model = "random", 
                              index = c("cname", "year"))

post_onset_ex_random_4 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + avexpr  + 
                                ef + relfrac + years_since, 
                              data = post_1945,
                              model = "random", 
                              index = c("cname", "year"))

post_onset_ex_random_5 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + avexpr  + 
                                ef + relfrac + 
                                ssafrica + 
                                asia.x + nafrme, 
                              data = post_1945,
                              model = "random", 
                              index = c("cname", "year"))


# Random Effects Death ----------------------------------------------------
post_onset_death_random_1 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + logem4, 
                              data = post_1945, 
                              model = "random", 
                              index = c("cname", "year"))

post_onset_death_random_2 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + logem4  + 
                                ef + relfrac, 
                              data = post_1945, 
                              model = "random", 
                              index = c("cname", "year"))

post_onset_death_random_3 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + logem4  + 
                                ef + relfrac + 
                                as.factor(decade), 
                              data = post_1945,
                              model = "random", 
                              index = c("cname", "year"))

post_onset_death_random_4 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + logem4  + 
                                ef + relfrac + years_since, 
                              data = post_1945,
                              model = "random", 
                              index = c("cname", "year"))

post_onset_death_random_5 <- plm(formula = onset ~ 
                                mtnest + ncontig +
                                tpop + logem4  + 
                                ef + relfrac + 
                                ssafrica + 
                                asia.x + nafrme, 
                              data = post_1945,
                              model = "random", 
                              index = c("cname", "year"))







#cluster.im.glm(post_onset_ex_1, post_1945, ~ ccode, ci.level = 0.95, report = TRUE,
               #drop = FALSE, truncate = FALSE, return.vcv = FALSE)

cluster.im.glm(post_onset_ex_1, post_1945, ~ ccode, ci.level = 0.95, report = TRUE)
               #drop = FALSE, truncate = FALSE, return.vcv = FALSE)

# Extra Interest Model ----------------------------------------------------
interest_model_exp <- glm(onset ~ 
                         mtnest + ncontig +
                         tpop + avexpr  + 
                         ef + relfrac + polity2 + instab + pec,
                       family=binomial(link="logit"),
                       data = post_1945)

interest_model_death <- glm(onset ~ 
                            mtnest + ncontig +
                            tpop + logem4  + 
                            ef + relfrac + polity2 + instab + pec,
                          family=binomial(link="logit"),
                          data = post_1945)

summary(interest_model)
