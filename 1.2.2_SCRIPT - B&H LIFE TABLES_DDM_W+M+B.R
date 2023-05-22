## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - THE LIFE TABLE

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.1_SCRIPT - GROWTH RATES.R")
    
# ---
# SCRIPT: removing unnecessary data

  rm(deathEstimates_data_TWO, deathEstimates_data_ALL,
     distRACECOLORandAGEGROUP_processdata,
     popEstimates_data_ALL_MORTALITY,
     popEstimates_data_THREE_MORTALITY, popEstimates_data_THREE_PLOT,
     popEstimates_data_TWO_MORTALITY, popEstimates_data_TWO_PLOT)

# ---
# SCRIPT: preparing the input to run the life table function

  deathEstimates_data_THREE <- deathEstimates_data_THREE %>% 
    group_by(AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(MEAN = mean(TOTAL))
  
  BHlifetable_data_THREE <- inner_join(deathEstimates_data_THREE, 
                                     GrowthRateBR_00_10,
                                   join_by(AGE_GROUP, GENDER)) %>% 
    select(AGE_GROUP, GENDER, RACE_COLOR, MEAN, GROWTH_RATE) %>% 
    separate(., AGE_GROUP, c("LOWER", "UPPER"), remove = FALSE, convert = TRUE) %>%
    mutate(N = ifelse(is.na(UPPER) == TRUE, 4, UPPER - LOWER),
           N = ifelse(LOWER >= 0, N + 1, N),
           cod = paste(ifelse(GENDER == "Female", "f", "m"), "-", RACE_COLOR))

# ---
# SCRIPT: adjusting data for incompleteness

  adjFactors_ages <- seq(15, 60, by = 5)
  
  adjFactors_data_THREE <- results_THREE %>% 
    select(cod, ggbseg, delta)
  
  BHlifetable_data_THREE <- BHlifetable_data_THREE %>%
    inner_join(., adjFactors_data_THREE, join_by(cod)) %>%
    mutate(ggbseg = ifelse(ggbseg > 1, 1, ggbseg),
           delta = ifelse(ggbseg < 1, delta, 1),
           MEAN = ifelse(LOWER %in% adjFactors_ages, MEAN / ggbseg, MEAN))

# ---
# SCRIPT: writing the function to generate the life tables

  LT = function(data, gender = c("Female", "Male"), race_color = c("White", "Mixed_Black", "Mixed", "Black")){
    
    gender = match.arg(gender)
    race_color = match.arg(race_color)
    
    temp = data %>% 
      filter(GENDER == gender,
             RACE_COLOR == race_color)
    
    LT = tibble(temp$AGE_GROUP, temp$N, temp$GENDER, temp$RACE_COLOR, temp$GROWTH_RATE, temp$MEAN) %>% 
      rename(., 
             AGE_GROUP = "temp$AGE_GROUP",
             N = "temp$N",
             GENDER = "temp$GENDER",
             RACE_COLOR = "temp$RACE_COLOR",
             GROWTH_RATE = "temp$GROWTH_RATE",
             MEAN = "temp$MEAN") %>% 
      mutate('nDy/nDx' = MEAN / lag(MEAN),
             'ndy/ndx' = `nDy/nDx` * (exp(N * (GROWTH_RATE + lag(GROWTH_RATE)) / 2)),
             'CUM_ndy/ndx' = lag(cumprod(lead(`ndy/ndx`))),
             ndx = ifelse(AGE_GROUP == "0-4", MEAN, MEAN[1] * `CUM_ndy/ndx`),
             lx = rev(cumsum(rev(ndx))),
             nqx = ndx / lx,
             Lx = ifelse(AGE_GROUP == "0-4", N * lead(lx) + 0.78 * ndx,
                         ifelse(AGE_GROUP == "85+", lx * log10(lx), N * lead(lx) + (N / 2) * ndx)),
             Tx = rev(cumsum(rev(Lx))),
             ex = Tx / lx)
    
    return(LT)
  }

# ---
# SCRIPT: Storing the results for RACE_COLOR White

  LT_Male_White <- LT(BHlifetable_data_THREE, "Male", "White")
  
  LT_Female_White <- LT(BHlifetable_data_THREE, "Female", "White")

# ---
# Storing the results for RACE_COLOR Mixed

  LT_Male_Mixed <- LT(BHlifetable_data_THREE, "Male", "Mixed")
  
  LT_Female_Mixed <- LT(BHlifetable_data_THREE, "Female", "Mixed")

# ---
# Storing the results for RACE_COLOR Black

  LT_Male_Black <- LT(BHlifetable_data_THREE, "Male", "Black")
  
  LT_Female_Black <- LT(BHlifetable_data_THREE, "Female", "Black")
