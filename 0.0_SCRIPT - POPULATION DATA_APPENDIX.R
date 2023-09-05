## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - POPULATION DATA

# ---
# SCRIPT: loading the necessary R Packages

library(pacman)
p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM, Cairo)

# ---
# SCRIPT: downloading (or locally loading) micro data

# DATA SOURCE: 2010 Brazilian CENSUS - obtained from IPUMS

setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/IBGE/IPUMS")

# DDI <- read_ipums_ddi("ipumsi_00003.xml")
# popEstimatesIPUMS <- read_ipums_micro(DDI); rm(DDI)
# 
# popEstimatesIPUMS <- popEstimatesIPUMS %>%
#   filter(GEO1_BR2010 == 35) %>%
#   count(YEAR, BR2010A_AGE, BR2010A_SEX, BR2010A_RACE, wt = PERWT)
# 
# write_excel_csv(popEstimatesIPUMS, "popEstimatesIPUMS.csv", na = "NA", delim = ",")

# ---
# SCRIPT: reading some previously downloaded data from a local folder and making minor adjustments

popEstimatesIPUMS <- read_csv("popEstimatesIPUMS.csv")

popEstimatesIPUMS <- popEstimatesIPUMS %>%
  rename("YEAR" = "YEAR",
         "AGE" = "BR2010A_AGE",
         "GENDER" = "BR2010A_SEX",
         "RACE_COLOR" = "BR2010A_RACE",
         "TOTAL" = "n")

skim(popEstimatesIPUMS)

popEstimatesIPUMS$GENDER <- case_match(popEstimatesIPUMS$GENDER,
                                       1 ~ "Male",
                                       2 ~ "Female")

popEstimatesIPUMS$RACE_COLOR <- case_match(popEstimatesIPUMS$RACE_COLOR,
                                           1 ~ "White",
                                           2 ~ "Mixed_Black",
                                           3 ~ "Asian",
                                           4 ~ "Mixed_Black",
                                           5 ~ "Indigenous",
                                           9 ~ "Missing")

# ---
# SCRIPT: downloading (or locally loading) micro data

# DATA SOURCE: 2019 PNADc

setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/IBGE/PNADc")

# popEstimatesPNADc <- tibble()
# 
# years_to_dowload <- 2019
# 
# for (i in 1:length(years_to_dowload)){
# 
#   temp_data <- get_pnadc(year = years_to_dowload[i], quarter = 3, design = FALSE) %>%
#     filter(UF == "São Paulo") %>%
#     count(Ano, V2009, V2007, V2010, wt = V1028)
# 
#   popEstimatesPNADc <- rbind(popEstimatesPNADc, temp_data); gc()
# 
# }
# 
# write_excel_csv(popEstimatesPNADc, "popEstimatesPNADc.csv", na = "NA", delim = ","); rm(years_to_dowload, temp_data)

# ---
# SCRIPT: reading some previously downloaded data from a local folder and making minor adjustments

popEstimatesPNADc <- read_csv("~/Documents/GitHub/MA-Thesis/IBGE/PNADc/popEstimatesPNADc.csv")

skim(popEstimatesPNADc)

popEstimatesPNADc <- popEstimatesPNADc %>%
  rename("YEAR" = "Ano",
         "AGE" = "V2009",
         "GENDER" = "V2007",
         "RACE_COLOR" = "V2010",
         "TOTAL" = "n")

popEstimatesPNADc$GENDER <- case_match(popEstimatesPNADc$GENDER,
                                       "Homem" ~ "Male",
                                       "Mulher" ~ "Female")

popEstimatesPNADc$RACE_COLOR <- case_match(popEstimatesPNADc$RACE_COLOR,
                                           "Branca" ~ "White",
                                           "Preta" ~ "Mixed_Black",
                                           "Amarela" ~ "Asian",
                                           "Parda" ~ "Mixed_Black",
                                           "Indígena" ~ "Indigenous",
                                           "Ignorado" ~ "Missing")

# ---
# SCRIPT: merging the necessary data before proceeding

popEstimates <- tibble(rbind(popEstimatesIPUMS, popEstimatesPNADc)) %>% 
  group_by(., YEAR, AGE, GENDER, RACE_COLOR) %>% 
  summarise(TOTAL = sum(TOTAL)); rm(popEstimatesIPUMS, popEstimatesPNADc)

# ---
# SCRIPT: calculating median age by gender and race/color

medianYearGenderRace <- popEstimates %>% 
  group_by(., YEAR, RACE_COLOR) %>% 
  mutate(list_occ = purrr::map(TOTAL, function(x) rep(1, x)))  %>% 
  unnest(cols=c(list_occ)) %>% 
  summarize(result = median(AGE))
