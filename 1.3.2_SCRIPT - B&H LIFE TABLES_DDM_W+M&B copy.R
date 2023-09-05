## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - THE LIFE TABLE

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("1.1_SCRIPT - DDM_W+M+B.R")
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("2.1_SCRIPT - GROWTH RATES_alt.R")
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.2_SCRIPT - B&H MORTALITY DATA.R")
  
# ---
# SCRIPT: preparing the input to run the life table function

  deathEstimates_data_THREE <- deathEstimates_data_THREE %>% 
    group_by(AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(MEAN = mean(TOTAL))
  
  BHlifetable_data_THREE <- inner_join(deathEstimates_data_THREE, 
                                     GrowthRateSP_10_19,
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
  
  adjFactors_data_THREE[1, 2:3] <- adjFactors_data_THREE[2, 2:3]
  
  adjFactors_data_THREE[4, 2:3] <- adjFactors_data_THREE[5, 2:3]
  
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
             Lx = ifelse(AGE_GROUP == "0-4", N * lead(lx) + (1- 0.78) * ndx,
                         ifelse(AGE_GROUP == "85+", lx * log10(lx), N * lead(lx) + (N / 2) * ndx)),
             Tx = rev(cumsum(rev(Lx))),
             # ex = ifelse(AGE_GROUP == "85+", 7.282 * (-log(nth(nqx, length(nqx) - 1) + 0.0943))^(0.796), Tx / lx))
             ex = Tx / lx)
    
    return(LT)
  }

# ---
# SCRIPT: Storing the results for RACE_COLOR White

  BH_LT_Male_White <- LT(BHlifetable_data_THREE, "Male", "White")
  
  BH_LT_Female_White <- LT(BHlifetable_data_THREE, "Female", "White")

# ---
# Storing the results for RACE_COLOR Mixed

  BH_LT_Male_Mixed <- LT(BHlifetable_data_THREE, "Male", "Mixed")
  
  BH_LT_Female_Mixed <- LT(BHlifetable_data_THREE, "Female", "Mixed")

# ---
# Storing the results for RACE_COLOR Black

  BH_LT_Male_Black <- LT(BHlifetable_data_THREE, "Male", "Black")
  
  BH_LT_Female_Black <- LT(BHlifetable_data_THREE, "Female", "Black")

# ---
# SCRIPT: generating plot of the death probability used in the life table
  
  ggplot() +
    
    geom_line(data = BH_LT_Male_White, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = RACE_COLOR, color = RACE_COLOR),
              linewidth = 0.8) +
    
    geom_line(data = BH_LT_Male_Mixed, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = RACE_COLOR, color = RACE_COLOR),
              linewidth = 0.8) +
    
    geom_line(data = BH_LT_Male_Black, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = RACE_COLOR, color = RACE_COLOR),
              linewidth = 0.8) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Black" = "#ffd166",
                                  "Mixed" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761")) +

    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "log(nqx)", 
         title = "Probability od Death of Males by Age and Race: 2010-2019 (Bennet and Horiuchui 1981)",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")

# ---
# SCRIPT: generating plot of the death probability used in the life table
  
  ggplot() +
    
    geom_line(data = BH_LT_Female_White, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = RACE_COLOR, color = RACE_COLOR),
              linewidth = 0.8) +
    
    geom_line(data = BH_LT_Female_Mixed, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = RACE_COLOR, color = RACE_COLOR),
              linewidth = 0.8) +
    
    geom_line(data = BH_LT_Female_Black, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = RACE_COLOR, color = RACE_COLOR),
              linewidth = 0.8) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Black" = "#ffd166",
                                  "Mixed" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761")) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "log(nqx)", 
         title = "Probability od Death of Females by Age and Race: 2010-2019 (Bennet and Horiuchui 1981)",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")
# ---
# SCRIPT: generating plot to showcase the differences in Life Expectancy at Birth by Race
  
  ggplot(data = rbind(first(BH_LT_Female_White), first(BH_LT_Female_Mixed), first(BH_LT_Female_Black), 
                      first(BH_LT_Male_White), first(BH_LT_Male_Mixed), first(BH_LT_Male_Black)),
         aes(x = RACE_COLOR, fill = RACE_COLOR,
             y = ex)) + 
    
    geom_bar(stat = "identity") +
    
    geom_text(aes(label = round(ex, digits = 1)), 
              color = "white", 
              vjust = 2, size = 4, fontface = "bold") +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Black" = "#ffd166",
                                 "Mixed" = "#118ab2",
                                 "Asian" = "#06d6a0",
                                 "Indigenous" = "#7F96FF",
                                 "Missing" = "#575761")) +
    
    facet_wrap(~ GENDER, strip.position = "top") +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "", y = "Life Expectancy at Birth (in years)", 
         title = "Life Expectancy at Birth by Gender and Race/Color",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")

# ---
# Clearing out the Global Environment
  
  # rm(list=ls()[! ls() %in% c("BH_LT_Female_White","BH_LT_Female_Mixed", "BH_LT_Female_Black",
  #                            "BH_LT_Male_White","BH_LT_Male_Mixed", "BH_LT_Male_Black",
  #                            "results_THREE", "GrowthRateSP_10_19")])
  
  