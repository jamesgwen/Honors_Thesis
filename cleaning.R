# James Wen
# QSS Thesis Code
# data conversion + rangling 


# Packages and working directories ------------------------

library(readstata13)
library(readxl)
library(gdata)
library(tidyverse)
library(stringr)
library(lubridate)

# rm(list=ls())

# setwd("~/Desktop/Thesis")

# setwd("~/Desktop/Thesis/Fearon_Laitin")

# setwd("~/Desktop/Thesis/Acemoglu_Johnson_Robinson")

# setwd("~/Desktop/Thesis/Acemoglu_Johnson_Robinson/maketable7")

# setwd("~/Desktop/Thesis/UDCP")

# setwd("~/Desktop/Thesis/expgdpv6.0")

# setwd("~/Desktop/Thesis/National_Material_Capabilities")

# setwd("~/Desktop/Thesis/ICOW Colonial History 1.1")

# Converting Feron and Laitin Data to CSV from DTA ------------------------

# data <- read.dta13("Fearon_Laitin.dta")

# write.table(data, file = "Fearon_Laitin.csv", sep = ",")

# Pre-1999 Data set-------------------------------

setwd("~/Desktop/Thesis") 

COW <- read.csv("COW_country_codes.csv") %>% 
  rename(CStateAbb = StateAbb,
        CStateName = StateNme)

setwd("~/Desktop/Thesis/Fearon_Laitin")

data <- read.csv("Fearon_Laitin.csv") %>% 
  mutate(cname = as.character(cname)) %>% 
  mutate(cname = str_trim(cname)) %>% 
  filter(cname == "JAMAICA" | cname == "TRINIDAD" | cname == "GUYANA" 
         | cname == "CYPRUS" | cname == "GUINEA B" | cname =="GAMBIA"
         | cname == "MALI" | cname == "SENEGAL" | cname =="BENIN"
         | cname == "MAURITAN" | cname == "NIGER" | cname == "IVORY CO"
         | cname == "GUINEA" | cname == "BURKINA" | cname == "SIERRA L"
         | cname == "GHANA" | cname == "TOGO" | cname == "CAMEROON" 
         | cname == "NIGERIA" | cname == "GABON" | cname == "CENTRAL"
         | cname == "CHAD" | cname == "CONGO" | cname == "DEM. REP" 
         | cname == "UGANDA" | cname == "KENYA" | cname == "TANZANIA"
         | cname == "BURUNDI" | cname == "RWANDA" | cname == "SOMALIA"
         | cname == "DJIBOUTI" | cname == "ANGOLA" | cname == "MOZAMBIQ" 
         | cname == "ZAMBIA" | cname == "ZIMBABWE" | cname == "MALAWI" 
         | cname == "NAMIBIA" | cname == "LESOTHO" | cname == "BOTSWANA" 
         | cname == "SWAZILAN" | cname == "MADAGASC" | cname == "MAURITIU"
         | cname == "MOROCCO" | cname == "ALGERIA" | cname == "TUNISIA"
         | cname == "LIBYA" | cname == "SUDAN" | cname == "EGYPT"
         | cname == "SYRIA" | cname == "JORDAN" | cname == "ISRAEL" 
         | cname == "KUWAIT" | cname == "BAHRAIN" | cname == "U. ARAB" 
         | cname == "OMAN" | cname == "N. KOREA" | cname == "KOREA, S"
         | cname == "INDIA" | cname == "PAKISTAN"| cname == "BANGLADE"
         | cname == "BURMA" | cname == "SRI LANK" | cname == "CAMBODIA"
         | cname == "LAOS" | cname == "VIETNAM" | cname == "MALAYSIA"
         | cname == "PHILIPPI" | cname == "INDONESI" | cname == "PAPUA N."
         | cname == "FIJI" | cname == "BHUTAN") %>% 
  mutate(country_lower = tolower(country)) %>% 
  select(ccode, country, country_lower, cname, cmark, 
         year, pop, western, eeurop, 
         lamerica,ssafrica, asia, 
         nafrme, colbrit, colfra,
         mtnest, lmtnest, elevdiff, 
         Oil, ncontig, ethfrac, 
         ef,  plural, second,
         numlang, relfrac, plurrel,
         minrelpc, muslim)


# Post 1999 Data set ------------------------------------------------------

setwd("~/Desktop/Thesis") 


post_data <- read.csv("Fearon_Laitin.csv") %>% 
  mutate(cname = str_trim(cname)) %>% 
  filter(cname == "JAMAICA" | cname == "TRINIDAD" | cname == "GUYANA" 
         | cname == "CYPRUS" | cname == "GUINEA B" | cname =="GAMBIA"
         | cname == "MALI" | cname == "SENEGAL" | cname =="BENIN"
         | cname == "MAURITAN" | cname == "NIGER" | cname == "IVORY CO"
         | cname == "GUINEA" | cname == "BURKINA" | cname == "SIERRA L"
         | cname == "GHANA" | cname == "TOGO" | cname == "CAMEROON" 
         | cname == "NIGERIA" | cname == "GABON" | cname == "CENTRAL"
         | cname == "CHAD" | cname == "CONGO" | cname == "DEM. REP" 
         | cname == "UGANDA" | cname == "KENYA" | cname == "TANZANIA"
         | cname == "BURUNDI" | cname == "RWANDA" | cname == "SOMALIA"
         | cname == "DJIBOUTI" | cname == "ANGOLA" | cname == "MOZAMBIQ" 
         | cname == "ZAMBIA" | cname == "ZIMBABWE" | cname == "MALAWI" 
         | cname == "NAMIBIA" | cname == "LESOTHO" | cname == "BOTSWANA" 
         | cname == "SWAZILAN" | cname == "MADAGASC" | cname == "MAURITIU"
         | cname == "MOROCCO" | cname == "ALGERIA" | cname == "TUNISIA"
         | cname == "LIBYA" | cname == "SUDAN" | cname == "EGYPT"
         | cname == "SYRIA" | cname == "JORDAN" | cname == "ISRAEL" 
         | cname == "KUWAIT" | cname == "BAHRAIN" | cname == "U. ARAB" 
         | cname == "OMAN" | cname == "N. KOREA" | cname == "KOREA, S"
         | cname == "INDIA" | cname == "PAKISTAN"| cname == "BANGLADE"
         | cname == "BURMA" | cname == "SRI LANK" | cname == "CAMBODIA"
         | cname == "LAOS" | cname == "VIETNAM" | cname == "MALAYSIA"
         | cname == "PHILIPPI" | cname == "INDONESI" | cname == "PAPUA N."
         | cname == "FIJI" | cname == "BHUTAN") %>%
  filter(year == 1990) %>% 
  mutate(country_lower = tolower(country)) %>% 
  select(ccode, country, country_lower, cname, cmark, 
         year, pop, western, eeurop, 
         lamerica,ssafrica, asia, 
         nafrme, colbrit, colfra,
         mtnest, lmtnest, elevdiff, 
         Oil, ncontig, ethfrac, 
         ef,  plural, second,
         numlang, relfrac, plurrel,
         minrelpc, muslim)

setwd("~/Desktop/Thesis")

# export to add 2000-2011 years

setwd("~/Desktop/Thesis") 
##write.csv(post_data, file = "post_data.csv", row.names=FALSE)

# bring in new data

post_data <- read.csv("post_data_1.csv") %>%
  mutate(cname = as.character(cname)) %>% 
  mutate(cname = str_trim(cname))


# Combine post 1945-1999 data with post 1999 data -------------------------


combined_data <- rbind(data, post_data) %>% 
  arrange(ccode, country, year) 




# Add ISO_3 codes -----------------------------------------

setwd("~/Desktop/Thesis")

iso_3 <- read.xls("iso_3digit_alpha_country_codes.xls") %>% 
  mutate(Definition = tolower(Definition)) %>% 
  mutate(Code_Value = as.character(Code_Value))

data_2 <- left_join(combined_data, iso_3, by = c("country_lower" = "Definition")) %>%
  mutate(Code_Value = ifelse(country_lower == "korea, s.", "KOR", Code_Value)) %>% 
  rename(ISO_3 = Code_Value) %>% 
  mutate(ISO_3 = as.character(ISO_3))

# Add COW codes -----------------------------------------------------------

COW <- read.csv("COW_country_codes.csv") %>% 
  rename(CStateAbb = StateAbb,
         CStateName = StateNme)

data_3 <- left_join(data_2, COW, by = c("ccode" = "CCode")) %>% 
  rename(cowstatename = CStateName,
         cowstateabb = CStateAbb) 



# Add GDP/Pop Data --------------------------------------------------------


# setwd("~/Desktop/Thesis/expgdpv6.0")
# 
# gdp_pop <- read.delim("gdpv6.txt") %>% 
#   mutate(stateid = as.character(stateid)) %>% 
#   rename(pop_g = pop)
# 
# 
# gdp_pop_2 <- left_join(gdp_pop, COW, by = c("statenum" = "CCode"))
# 
# gdp_pop_3 <- semi_join(gdp_pop_2, data_3, by = c("statenum" = "ccode")) 
# 
# data_4 <- left_join(data_3, gdp_pop_3, by = c("year" = "year", "ccode" = "statenum", 
#                                               "cowstatename" = "CStateName"))


# Add Expropiation/Mortality Data -----------------------------------------


setwd("~/Desktop/Thesis/Acemoglu_Johnson_Robinson/maketable7")

mort_expro <- read.dta13("maketable7.dta")

data_4 <- left_join(data_3, mort_expro, by = c("ISO_3" = "shortnam"))


# Add Polity/Instability Data ---------------------------------------------------------

setwd("~/Desktop/Thesis")

polity <- read.xls("p4v2017.xls") %>% 
  select(ccode, year, polity2) 

data_5 <- left_join(data_4, polity, by = c("ccode" ="ccode", "year" = "year"))

data_6 <- data_5 %>% 
  mutate(instab = ifelse(abs(lag(polity2, 1) - (polity2)) >= 3 & ccode == lag(ccode, 1), 1, 0),
         instab = ifelse(abs(lag(polity2, 2) - (polity2)) >= 3 & ccode == lag(ccode, 2), 1, instab),
         instab = ifelse(abs(lag(polity2, 3) - (polity2)) >= 3 & ccode == lag(ccode, 3), 1, instab))

         


# UCDP Prio Data ----------------------------------------------------------

## location on UCDP = Cname

setwd("~/Desktop/Thesis/UDCP")

ucdp <- read.csv("ucdp-prio-acd-181.csv")  %>% 
  mutate(location = as.character(location)) %>% 
  mutate(location = ifelse(location == "Myanmar (Burma)", "BURMA", location),
         location = ifelse(location == "Yemen (North Yemen)", "YEMEN", location),
         location = ifelse(location == "Trinidad and Tobago", "TRINIDAD", location),
         location = ifelse(location == "Guinea-Bissau", "GUINEA B", location),
         location = ifelse(location == "Mauritania", "MAURITAN", location),
         location = ifelse(location == "Burkina Faso", "BURKINA ", location),
         location = ifelse(location == "Sierra Leone", "SIERRA L", location),
         location = ifelse(location == "Central African Republic", "CENTRAL ", location),
         location = ifelse(location == "DR Congo (Zaire)", "DEM. REP", location),
         location = ifelse(location == "Mozambique", "MOZAMBIQ", location),
         location = ifelse(location == "Zimbabwe (Rhodesia)", "ZIMBABWE", location),
         location = ifelse(location == "Madagascar (Malagasy)", "MADAGASC", location),
         location = ifelse(location == "Bangladesh", "BANGLADE", location),
         location = ifelse(location == "Sri Lanka", "SRI LANK", location),
         location = ifelse(location == "Cambodia (Kampuchea)", "CAMBODIA", location),
         location = ifelse(location == "Philippines", "PHILIPPI", location),
         location = ifelse(location == "Indonesia", "INDONESI", location),
         location = ifelse(location == "Papua New Guinea", "PAPUA N.", location),
         location = toupper(location)) %>% 
  mutate(location = as.character(location)) %>% 
  filter(type_of_conflict == 3) %>% 
  select(conflict_id, location, year, start_date, ep_end, intensity_level)

ucdp_1 <- ucdp %>% 
  select(location, year, start_date, intensity_level) %>% 
  group_by(location, year) %>% 
  summarize(intensity_level = sum(intensity_level)) %>% 
  mutate(intensity_level = ifelse(intensity_level > 2, 2, intensity_level)) %>% 
  ungroup() %>% 
  mutate(location = as.character(location)) %>% 
  arrange(location)
  

data_7 <- left_join(data_6, ucdp_1, by = c("cname" = "location", "year" = "year"))


# NMC Data ----------------------------------------------------------------

setwd("~/Desktop/Thesis/National_Material_Capabilities")

nmc <- read.csv("NMC_5_0.csv") %>%
  select(ccode, year, tpop, pec, upop) %>%
  mutate(nmc_pop = upop) %>%
  select(ccode, year, tpop, pec, nmc_pop)

data_8 <- left_join(data_7, nmc, by = c("ccode" = "ccode", "year" = "year"))


# add onset and end codes -------------------------------------------------
start_year <- ucdp  %>% 
  mutate(start_year = year(start_date)) %>% 
  filter(year == start_year) %>% 
  select(location, year, start_year, start_date) %>% 
  arrange(location, year) %>% 
  filter(start_date != "1948-02-29") %>% 
  select(location, year, start_year)

data_9 <- left_join(data_8, start_year, by = c("cname" = "location" , "year" = "year"))  %>% 
  mutate(onset = ifelse(year == start_year, 1, 0),
         onset = ifelse(intensity_level == 1 & is.na(start_year), NA, onset),
         onset = ifelse(intensity_level == 2 & is.na(start_year), NA, onset)) %>% 
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
         pec, nmc_pop)


# add independence year and time since ind---------------------------------------------------
setwd("~/Desktop/Thesis/ICOW Colonial History 1.1")
ind <- read.csv("coldata110.csv") %>% 
  select(State, IndDate) %>%
  mutate(IndDate = as.character(IndDate),
         testdate = paste(IndDate,"01", sep=""),
         ind_date = ymd(testdate),
         ind_year = year(ind_date)) %>% 
  select(State, ind_year)

data_10 <- left_join(data_9, ind, by = c("ccode" = "State")) %>%
  mutate(years_since = year - ind_year,
         forty = ifelse(year >= 1940 & year <= 1949,  1, 0),
         fifty = ifelse(year >= 1950 & year <= 1959,  1, 0),
         sixty = ifelse(year >= 1960 & year <= 1969,  1, 0),
         seventy = ifelse(year >= 1970 & year <= 1979,  1, 0),
         eighty = ifelse(year >= 1980 & year <= 1989,  1, 0),
         ninety = ifelse(year >= 1990 & year <= 1999,  1, 0))


# change intensity levels -------------------------------------------------


data_11 <- data_10 %>% 
  mutate(intensity_level = ifelse(cname == "ANGOLA" & year == 1991, 1, intensity_level),
         intensity_level = ifelse(cname == "ANGOLA" & year == 1994, 2, intensity_level),
         intensity_level = ifelse(cname == "ANGOLA" & year == 1998, 2, intensity_level),
         # Dem Rep Congo
         intensity_level = ifelse(cname == "DEM. REP" & year == 1961, 1, intensity_level),
         intensity_level = ifelse(cname == "DEM. REP" & year == 1962, 1, intensity_level),
         intensity_level = ifelse(cname == "DEM. REP" & year == 2007, 1, intensity_level),
         intensity_level = ifelse(cname == "DEM. REP" & year == 2008, 1, intensity_level),
         # India
         intensity_level = ifelse(cname == "INDIA" & year == 1966, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1967, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1968, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1982, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1983, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1984, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1985, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1986, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1987, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1988, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1989, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1990, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1991, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1992, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1993, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1994, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1995, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1996, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1997, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1998, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 1999, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2000, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2001, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2002, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2003, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2004, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2005, 2, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2006, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2007, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2008, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2009, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2010, 1, intensity_level),
         intensity_level = ifelse(cname == "INDIA" & year == 2011, 1, intensity_level),
         # Indonesia
         intensity_level = ifelse(cname == "INDONESI" & year == 1976, 2, intensity_level),
         intensity_level = ifelse(cname == "INDONESI" & year == 1977, 2, intensity_level),
         intensity_level = ifelse(cname == "INDONESI" & year == 1978, 2, intensity_level),
         intensity_level = ifelse(cname == "INDONESI" & year == 1981, 2, intensity_level),
         intensity_level = ifelse(cname == "INDONESI" & year == 1984, 1, intensity_level),
         intensity_level = ifelse(cname == "INDONESI" & year == 1999, 1, intensity_level),
         # Israel
         intensity_level = ifelse(cname == "ISRAEL" & year == 1990, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 1991, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 1992, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 1993, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 1994, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 1995, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 1996, 1, intensity_level),
         intensity_level = ifelse(cname == "ISRAEL" & year == 2006, 1, intensity_level),
         # Mali
         intensity_level = ifelse(cname == "MALI" & year == 2009, 1, intensity_level),
         # Burma
         intensity_level = ifelse(cname == "BURMA" & year == 1948, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1949, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1950, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1951, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1952, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1953, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1954, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1955, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1956, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1957, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1958, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1959, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1960, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1961, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1962, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1963, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1964, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1965, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1966, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1967, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1968, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1969, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1970, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1971, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1972, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1973, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1974, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1975, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1976, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1977, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1978, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1979, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1980, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1981, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1982, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1983, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1984, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1985, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1986, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1987, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1988, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1989, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1990, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1992, 2, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1993, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1994, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1995, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1996, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1997, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 1998, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2000, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2001, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2002, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2005, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2006, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2007, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2008, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2009, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2010, 1, intensity_level),
         intensity_level = ifelse(cname == "BURMA" & year == 2011, 1, intensity_level),
         # Nigeria
         intensity_level = ifelse(cname == "NIGERIA" & year == 2004, 1, intensity_level),
         # Pakistan
         intensity_level = ifelse(cname == "PAKISTAN" & year == 2007, 1, intensity_level),
         intensity_level = ifelse(cname == "PAKISTAN" & year == 2008, 2, intensity_level),
         intensity_level = ifelse(cname == "PAKISTAN" & year == 2009, 2, intensity_level),
         intensity_level = ifelse(cname == "PAKISTAN" & year == 2011, 2, intensity_level),
         # Philippines
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1970, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1971, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1972, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1973, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1974, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1975, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1976, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1977, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1978, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1979, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1980, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1981, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1982, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1983, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1984, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1985, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1986, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1987, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1988, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1989, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1990, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1993, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1994, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1995, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1997, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 1999, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2000, 2, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2001, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2002, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2003, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2004, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2005, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2006, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2007, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2008, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2009, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2010, 1, intensity_level),
         intensity_level = ifelse(cname == "PHILIPPI" & year == 2011, 1, intensity_level),
         # Sri Lanka
         intensity_level = ifelse(cname == "SRI LANK" & year == 1989, 1, intensity_level),
         intensity_level = ifelse(cname == "SRI LANK" & year == 1990, 2, intensity_level),
         # Sudan 
         intensity_level = ifelse(cname == "SUDAN" & year == 1971, 2, intensity_level),
         intensity_level = ifelse(cname == "SUDAN" & year == 2011, 2, intensity_level))


# Add mulitiple conflict variable -----------------------------------------

data_12 <- data_11 %>% 
  mutate(multiple = ifelse(is.na(intensity_level), NA, 0),
         multiple = ifelse(cname == "ANGOLA" & year == 1991, 1, multiple),
         multiple = ifelse(cname == "ANGOLA" & year == 1994, 1, multiple),
         multiple = ifelse(cname == "ANGOLA" & year == 1998, 1, multiple),
         # Dem Rep Congo
         multiple = ifelse(cname == "DEM. REP" & year == 1961, 1, multiple),
         multiple = ifelse(cname == "DEM. REP" & year == 1962, 1, multiple),
         multiple = ifelse(cname == "DEM. REP" & year == 2007, 1, multiple),
         multiple = ifelse(cname == "DEM. REP" & year == 2008, 1, multiple),
         # India
         multiple = ifelse(cname == "INDIA" & year == 1966, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1967, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1968, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1982, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1983, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1984, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1985, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1986, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1987, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1988, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1989, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1990, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1991, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1992, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1993, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1994, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1995, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1996, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1997, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1998, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 1999, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2000, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2001, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2002, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2003, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2004, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2005, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2006, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2007, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2008, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2009, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2010, 1, multiple),
         multiple = ifelse(cname == "INDIA" & year == 2011, 1, multiple),
         # Indonesia
         multiple = ifelse(cname == "INDONESI" & year == 1976, 1, multiple),
         multiple = ifelse(cname == "INDONESI" & year == 1977, 1, multiple),
         multiple = ifelse(cname == "INDONESI" & year == 1978, 1, multiple),
         multiple = ifelse(cname == "INDONESI" & year == 1981, 1, multiple),
         multiple = ifelse(cname == "INDONESI" & year == 1984, 1, multiple),
         multiple = ifelse(cname == "INDONESI" & year == 1999, 1, multiple),
         # Israel
         multiple = ifelse(cname == "ISRAEL" & year == 1990, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 1991, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 1992, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 1993, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 1994, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 1995, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 1996, 1, multiple),
         multiple = ifelse(cname == "ISRAEL" & year == 2006, 1, multiple),
         # Mali
         multiple = ifelse(cname == "MALI" & year == 2009, 1, multiple),
         # Burma
         multiple = ifelse(cname == "BURMA" & year == 1948, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1949, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1950, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1951, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1952, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1953, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1954, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1955, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1956, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1957, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1958, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1959, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1960, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1961, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1962, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1963, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1964, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1965, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1966, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1967, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1968, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1969, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1970, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1971, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1972, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1973, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1974, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1975, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1976, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1977, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1978, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1979, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1980, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1981, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1982, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1983, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1984, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1985, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1986, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1987, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1988, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1989, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1990, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1992, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1993, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1994, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1995, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1996, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1997, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 1998, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2000, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2001, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2002, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2005, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2006, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2007, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2008, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2009, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2010, 1, multiple),
         multiple = ifelse(cname == "BURMA" & year == 2011, 1, multiple),
         # Nigeria
         multiple = ifelse(cname == "NIGERIA" & year == 2004, 1, multiple),
         # Pakistan
         multiple = ifelse(cname == "PAKISTAN" & year == 2007, 1, multiple),
         multiple = ifelse(cname == "PAKISTAN" & year == 2008, 1, multiple),
         multiple = ifelse(cname == "PAKISTAN" & year == 2009, 1, multiple),
         multiple = ifelse(cname == "PAKISTAN" & year == 2011, 1, multiple),
         # Philippines
         multiple = ifelse(cname == "PHILIPPI" & year == 1970, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1971, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1972, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1973, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1974, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1975, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1976, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1977, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1978, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1979, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1980, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1981, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1982, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1983, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1984, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1985, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1986, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1987, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1988, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1989, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1990, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1993, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1994, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1995, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1997, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 1999, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2000, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2001, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2002, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2003, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2004, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2005, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2006, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2007, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2008, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2009, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2010, 1, multiple),
         multiple = ifelse(cname == "PHILIPPI" & year == 2011, 1, multiple),
         # Sri Lanka
         multiple = ifelse(cname == "SRI LANK" & year == 1989, 1, multiple),
         multiple = ifelse(cname == "SRI LANK" & year == 1990, 1, multiple),
         # Sudan 
         multiple = ifelse(cname == "SUDAN" & year == 1971, 1, multiple),
         multiple = ifelse(cname == "SUDAN" & year == 2011, 1, multiple))


# make no civil war 0, make binaries for minor and major -----------------------------------------------------

data_13 <- data_12 %>% 
  mutate(intensity_level = ifelse(is.na(intensity_level), 0, intensity_level),
         minor = ifelse(intensity_level == 1, 1, 0),
         major = ifelse(intensity_level == 2, 1, 0))



setwd("~/Desktop/Thesis")
write.csv(data_13, file = "post_1945_data.csv", row.names=FALSE)

