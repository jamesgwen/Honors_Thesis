# James Wen 
# QSS Code
# This code is to create the dataset for countries
# that gained independence pre-1945


library(readstata13)
library(readxl)
library(gdata)
library(tidyverse)
library(stringr)
library(lubridate)

post_1945_fxn <- function(data_frame_name, COW_code, COW_name, 
                          country_name, iso_code, independence_year, 
                          number_year_ind) {
  
  ccode <- as.vector(rep(COW_code, number_year_ind))
  cname <- as.vector(rep(COW_name, number_year_ind))
  country <- as.vector(rep(country_name, number_year_ind))
  ISO_3 <- as.vector(rep(iso_code, number_year_ind))
  year <- as.vector(independence_year:2011)
  
  # https://stackoverflow.com/questions/47516904/use-function-argument-as-name-for-new-data-frame-in-r
  assign(deparse(substitute(data_frame_name)), data.frame(ccode, cname, country, ISO_3, year), envir=.GlobalEnv)
}

#  Cuba
post_1945_fxn(cuba, 42, "CUBA", 
              "CUBA", "CUB", 1902,
              110)

# Haiti
post_1945_fxn(haiti, 41, "HAITI", 
              "HAITI", "HTI", 1804,
              208)

# Dominican Republic
post_1945_fxn(domiminican_rep, 42, "DOMINICA", 
              "DOMINICAN REP.", "DOM", 1844,
              168)

# Mexico
post_1945_fxn(mexico, 70, "MEXICO", 
              "MEXICO", "MEX", 1821,
              191)

# Guatemala
post_1945_fxn(guatemala, 90, "GUATEMAL", 
              "GUATEMALA", "GTM", 1839,
              173)

# Honduras
post_1945_fxn(honduras, 91, "HONDURAS", 
              "HONDURAS", "HND", 1838,
              174)

# El Salvador
post_1945_fxn(el_salvador, 92, "EL SALVA", 
              "EL SALVADOR", "SLV", 1840,
              172)

# Nicaragua
post_1945_fxn(nicaragua, 93, "NICARAGU", 
              "NICARAGUA", "NIC", 1838,
              174)

# Costa Rica
post_1945_fxn(costa_rica, 94, "COSTARIC", 
              "COSTA RICA", "CRI", 1838,
              174)

# Panama
post_1945_fxn(panama, 95, "PANAMA", 
              "PANAMA", "PAN", 1903,
              109)

# Columbia
post_1945_fxn(colombia, 100, "COLOMBIA", 
              "COLOMBIA", "COL", 1819,
              193)

# Venezuela 
post_1945_fxn(venezuela, 101, "VENEZUEL", 
              "VENEZUELA", "VEN", 1830,
              182)

# Ecuador
post_1945_fxn(ecuador, 130, "ECUADOR", 
              "ECUADOR", "ECU", 1830,
              182)

# Peru
post_1945_fxn(peru, 135, "PERU", 
              "PERU", "PER", 1824,
              188)

# Brazil
post_1945_fxn(brazil, 135, "BRAZIL", 
              "BRAZIL", "BRA", 1822,
              190)

# Bolivia
post_1945_fxn(bolivia, 145, "BOLIVIA", 
              "BOLIVIA", "BOL", 1825,
              187)

# Paraguay
post_1945_fxn(paraguay, 150, "PARAGUAY", 
              "PARAGUAY", "PRY", 1811,
              201)

# Chile
post_1945_fxn(chile, 155, "CHILE", 
              "CHILE", "CHL", 1818,
              194)

# Argentina
post_1945_fxn(argentina, 160, "ARGENTIN", 
              "ARGENTINA", "ARG", 1816,
              196)

# Uruguay 
post_1945_fxn(uruguay, 165, "URUGUAY", 
              "URUGUAY", "URY", 1828,
              184)

# Ireland
post_1945_fxn(ireland, 205, "IRELAND", 
              "IRELAND", "IRL", 1922,
              90)

# Iraq
post_1945_fxn(iraq, 645, "IRAQ", 
              "IRAQ", "IRQ", 1932,
              80)

# South Africa
post_1945_fxn(south_africa, 560, "SOUTH AF", 
              "SOUTH AFRICA", "ZAF", 1910,
              102)
 
# Australia
post_1945_fxn(australia, 900, "AUSTRALI", 
              "AUSTRALIA", "AUS", 1901,
              111)

# New Zealand
post_1945_fxn(new_zealand, 920, "NEW ZEAL", 
              "NEW ZEALAND", "NZL", 1907,
              105)

# Lebanon
post_1945_fxn(lebanon, 660, "LEBANON", 
              "LEBANON", "NZL", 1943,
              69)


data <- do.call("rbind", list(cuba, haiti, domiminican_rep, 
                              mexico, guatemala, honduras,
                              el_salvador, nicaragua, costa_rica,
                              panama, colombia, venezuela,
                              ecuador, peru, brazil,
                              bolivia, paraguay, chile,
                              argentina, uruguay, ireland,
                              iraq, south_africa, australia,
                              new_zealand, lebanon))

rm(cuba, haiti, domiminican_rep,
   mexico, guatemala, honduras,
   el_salvador, nicaragua, costa_rica,
   panama, colombia, venezuela,
   ecuador, peru, brazil,
   bolivia, paraguay, chile,
   argentina, uruguay, ireland,
   iraq, south_africa, australia,
   new_zealand, lebanon)


# Add Fearon and Laitin data ----------------------------------------------


setwd("~/Desktop/Thesis/Fearon_Laitin")
fearon_laitin <- read.csv("Fearon_Laitin.csv") %>% 
  mutate(cname = str_trim(cname)) %>%
  filter(year == 1990) %>% 
  mutate(country_lower = tolower(country)) %>% 
  select(ccode, country_lower, western, eeurop, 
         lamerica, ssafrica, asia, 
         nafrme, colbrit, colfra,
         mtnest, lmtnest, elevdiff, 
         Oil, ncontig, ethfrac, 
         ef,  plural, second,
         numlang, relfrac, plurrel,
         minrelpc, muslim)

pre_data_1 <- left_join(data, fearon_laitin, by = c("ccode" = "ccode"))


# Add Acemoglu, Johnson, and Robinson data --------------------------------


setwd("~/Desktop/Thesis/Acemoglu_Johnson_Robinson/maketable7")

mort_expro <- read.dta13("maketable7.dta")

pre_data_2 <- left_join(pre_data_1, mort_expro, by = c("ISO_3" = "shortnam"))


# Add polity and instability ----------------------------------------------

setwd("~/Desktop/Thesis")

polity <- read.xls("p4v2017.xls") %>% 
  select(ccode, year, polity2) 

pre_data_3 <- left_join(pre_data_2, polity, by = c("ccode" ="ccode", "year" = "year"))

pre_data_4 <- pre_data_3 %>% 
  mutate(instab = ifelse(abs(lag(polity2, 1) - (polity2)) >= 3 & ccode == lag(ccode, 1), 1, 0),
         instab = ifelse(abs(lag(polity2, 2) - (polity2)) >= 3 & ccode == lag(ccode, 2), 1, instab),
         instab = ifelse(abs(lag(polity2, 3) - (polity2)) >= 3 & ccode == lag(ccode, 3), 1, instab))


# Add UCDP Data -----------------------------------------------------------

setwd("~/Desktop/Thesis/UDCP")

ucdp <- read.csv("ucdp-prio-acd-181.csv")  %>% 
  mutate(location = as.character(location)) %>% 
  filter(type_of_conflict == 3) %>% 
  select(conflict_id, location, year, start_date, ep_end, intensity_level)

ucdp_1 <- ucdp %>% 
  mutate(location = ifelse(location == "Dominican Republic", "DOMINICAN REP.", location),
         location = toupper(location)) %>% 
  select(location, year, start_date, intensity_level) %>% 
  group_by(location, year) %>% 
  summarize(intensity_level = sum(intensity_level)) %>% 
  mutate(intensity_level = ifelse(intensity_level > 2, 2, intensity_level)) %>% 
  ungroup() %>% 
  mutate(location = as.character(location)) %>% 
  arrange(location)


pre_data_5 <- left_join(pre_data_4, ucdp_1, by = c("country" = "location", "year" = "year"))

# Add NMC Data ----------------------------------------------------------------

setwd("~/Desktop/Thesis/National_Material_Capabilities")

nmc <- read.csv("NMC_5_0.csv") %>%
  select(ccode, year, tpop, pec, upop) %>%
  mutate(nmc_pop = upop) %>%
  select(ccode, year, tpop, pec, nmc_pop)

pre_data_6 <- left_join(pre_data_5, nmc, by = c("ccode" = "ccode", "year" = "year"))


# Add onset and end codes -------------------------------------------------

start_year <- ucdp %>% 
  mutate(location = ifelse(location == "Dominican Republic", "DOMINICAN REP.", location),
         location = toupper(location)) %>% 
  mutate(start_year = year(start_date)) %>% 
  filter(year == start_year) %>% 
  select(location, year, start_year, start_date) %>% 
  arrange(location, year) %>% 
  filter(start_date != "1948-02-29") %>% 
  select(location, year, start_year)

pre_data_7 <- left_join(pre_data_6, start_year, by = c("cname" = "location" , "year" = "year")) %>% 
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

pre_data_8 <- left_join(pre_data_7, ind, by = c("ccode" = "State")) %>% 
  mutate(years_since = year - ind_year,
         forty = ifelse(year >= 1940 & year <= 1949,  1, 0),
         fifty = ifelse(year >= 1950 & year <= 1959,  1, 0),
         sixty = ifelse(year >= 1960 & year <= 1969,  1, 0),
         seventy = ifelse(year >= 1970 & year <= 1979,  1, 0),
         eighty = ifelse(year >= 1980 & year <= 1989,  1, 0),
         ninety = ifelse(year >= 1990 & year <= 1999,  1, 0))

# change intensity levels -------------------------------------------------

pre_data_9 <- pre_data_8 %>% 
  mutate(intensity_level = ifelse(cname == "SOUTH AF" & year == 1981, 2, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1982, 2, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1983, 2, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1985, 1, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1986, 2, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1987, 2, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1988, 2, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1989, 1, intensity_level),
         intensity_level = ifelse(cname == "SOUTH AF" & year == 1990, 1, intensity_level))


# add multiple ------------------------------------------------------------

pre_data_10 <- pre_data_9 %>% 
  mutate(multiple = ifelse(is.na(intensity_level), NA, 0),
         multiple = ifelse(cname == "SOUTH AF" & year == 1981, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1982, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1983, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1985, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1986, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1987, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1988, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1989, 1, multiple),
         multiple = ifelse(cname == "SOUTH AF" & year == 1990, 1, multiple))


# make no civil war 0 -----------------------------------------------------

pre_data_11 <- pre_data_10 %>% 
  mutate(intensity_level = ifelse(is.na(intensity_level), 0, intensity_level),
         minor = ifelse(intensity_level == 1, 1, 0),
         major = ifelse(intensity_level == 2, 1, 0))
  
  
# test <- pre_data_8 %>% 
#   filter(!is.na(intensity_level))
setwd("~/Desktop/Thesis")
write.csv(pre_data_11, file = "pre_1945_data.csv", row.names=FALSE)
