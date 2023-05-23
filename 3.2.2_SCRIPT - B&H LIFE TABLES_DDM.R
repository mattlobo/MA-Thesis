## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - THE LIFE TABLE

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.1_SCRIPT - GROWTH RATES.R")
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.2_SCRIPT - B&H MORTALITY DATA.R")
    
# ---
# SCRIPT: removing unnecessary data
  
  rm(distRACECOLORandAGEGROUP_processdata,
     deathEstimates_data_THREE, deathEstimates_data_TWO)

# ---
# SCRIPT: preparing the input to run the life table function

  deathEstimates_data_ALL <- deathEstimates_data_ALL %>% 
    group_by(AGE_GROUP, GENDER) %>% 
    summarise(MEAN = mean(TOTAL))
  
  BHlifetable_data_ALL <- inner_join(deathEstimates_data_ALL, 
                                     GrowthRateBR_00_10,
                                   join_by(AGE_GROUP, GENDER)) %>% 
    select(AGE_GROUP, GENDER, MEAN, GROWTH_RATE) %>% 
    separate(., AGE_GROUP, c("LOWER", "UPPER"), remove = FALSE, convert = TRUE) %>%
    mutate(N = ifelse(is.na(UPPER) == TRUE, 4, UPPER - LOWER),
           N = ifelse(LOWER >= 0, N + 1, N),
           cod = paste(ifelse(GENDER == "Female", "f", "m")))

# ---
# SCRIPT: adjusting data for incompleteness

  adjFactors_ages <- seq(15, 60, by = 5)
  
  adjFactors_data_ALL <- results_ALL %>% 
    select(cod, ggbseg, delta)
  
  BHlifetable_data_ALL <- BHlifetable_data_ALL %>%
    inner_join(., adjFactors_data_ALL, join_by(cod)) %>%
    mutate(ggbseg = ifelse(ggbseg > 1, 1, ggbseg),
           delta = ifelse(ggbseg < 1, delta, 1),
           MEAN = ifelse(LOWER %in% adjFactors_ages, MEAN / ggbseg, MEAN))

# ---
# SCRIPT: writing the function to generate the life tables

  LT = function(data, gender = c("Female", "Male")){
    
    gender = match.arg(gender)

    temp = data %>% 
      filter(GENDER == gender)
    
    LT = tibble(temp$AGE_GROUP, temp$N, temp$GENDER, temp$GROWTH_RATE, temp$MEAN) %>% 
      rename(., 
             AGE_GROUP = "temp$AGE_GROUP",
             N = "temp$N",
             GENDER = "temp$GENDER",
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
# SCRIPT: Storing the results

  BH_LT_Male <- LT(BHlifetable_data_ALL, "Male")
  
  BH_LT_Female <- LT(BHlifetable_data_ALL, "Female")

# ---
# SCRIPT: generating plot of the death probability used in the life table
  
  ggplot() +
    
    geom_line(data = BH_LT_Male, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = GENDER, color = GENDER),
              linewidth = 0.8) +
    
    geom_line(data = BH_LT_Female, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = GENDER, color = GENDER),
              linewidth = 0.8) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "log(nqx)", 
         title = "Probability od Death by Age and Gender: 2010-2019 (Bennet and Horiuchi 1981)",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")
  
  rm(adjFactors_data_ALL, adjFactors_ages, DDM_data_ALL, results_ALL,
     popEstimates_data_ALL_MORTALITY, deathEstimates_data_ALL,
     GrowthRateBR_00_10, GrowthRateSP_00_10)
  