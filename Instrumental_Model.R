## James Wen 
## QSS Thesis

library(ivprobit)
library(stargazer)

#setwd("~/Desktop/Thesis")
library(dplyr)




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



# Models where settler death is the instrument ------------------------------------------------------------------


# Model 2: population,  mountain, noncontigous, expropiation, deaths
instrument_1 <- ivprobit(onset ~ 
                           mtnest + ncontig +
                           tpop  | avexpr |mtnest + ncontig +
                           tpop + logem4,
                         data = post_1945)
# Model 3: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic and relig fractionalization 
instrument_2 <- ivprobit(onset ~ 
                         mtnest + ncontig +
                         tpop + ef + relfrac | avexpr |mtnest + ncontig +
                      tpop + logem4 + ef + relfrac,
                       data = post_1945)
# Model 4: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, decade of war 

# model below doest not work
instrument_3 <- ivprobit(onset ~ 
                         mtnest + ncontig +
                         tpop + ef + relfrac + as.factor(decade) | avexpr |mtnest + ncontig +
                      tpop + logem4 + ef + relfrac + as.factor(decade),
                       data = post_1945)

# Model 5: population,  mountain, noncontigous, expropiation, deaths, 
# ethnic, relig fractionalization, time since independence 
instrument_4 <- ivprobit(onset ~ 
                         mtnest + ncontig +
                         tpop + ef + relfrac + years_since| avexpr |mtnest + ncontig +
                      tpop + logem4 + ef + relfrac + years_since,
                       data = post_1945)




# Models where latitude is the instrument ---------------------------------

instrument_lat <- ivprobit(onset ~  
                  mtnest + relfrac  + ef + tpop | avexpr | lat_abst  + mtnest + relfrac  + ef + tpop, 
                data = post_1945)
