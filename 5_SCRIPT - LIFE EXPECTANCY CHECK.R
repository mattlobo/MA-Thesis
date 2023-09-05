## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - CHECK LIFE EXPECTANCY

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("3.2.1_SCRIPT - LIFE TABLES_DDM.R")
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("3.2.2_SCRIPT - B&H LIFE TABLES_DDM.R")
  
# ---
# SCRIPT: creating the object to compare life expectancy at birth between the two methods
  
  Compare_e0 <- tribble(~GENDER, ~Standard, ~'Bennett & Horiuchi', ~'Official Estimates',
                        first(LT_Male$GENDER), first(LT_Male$ex), first(BH_LT_Male$ex), 73.3,
                        first(LT_Female$GENDER), first(LT_Female$ex), first(BH_LT_Female$ex), 79.4) %>% 
    pivot_longer(Standard:`Official Estimates`, names_to = "Method", values_to = "e0") %>% 
    mutate(Method = addNA(as_factor(Method)),
           Method = ordered(Method, c("Official Estimates", "Standard", "Bennett & Horiuchi"))) 
  
# ---
# SCRIPT: creating the object to compare life expectancy at birth between the two methods
  
  ggplot(data = filter(Compare_e0), 
         aes(x = Method, fill = Method,
             y = e0)) + 
    
    geom_bar(stat = "identity") +
    
    geom_text(aes(label = round(e0, digits = 1)), 
              color = "white", 
              vjust = 2, size = 3.3, fontface = "bold") +
    
    scale_fill_manual(values = c("Standard" = "#6280E4",
                                 "Bennett & Horiuchi" = "#B3C2F2",
                                 "Official Estimates" = "#304F9C")) +
    
    facet_wrap(~ GENDER, strip.position = "top") +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "", y = "Life Expectancy at Birth (in years)", 
         title = "Life Expectancy at Birth by Gender and Race/Color",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: SEADE Foundation and own calculations from IBGE and DATASUS")
  
# ---
# Clearing out the Global Environment
  
  rm(list=ls()[! ls() %in% c("LT_Female", "BH_LT_Female",
                             "LT_Male", "BH_LT_Male",
                             "Compare_e0",
                             "results_ALL",
                             "GrowthRateSP_10_19")])  
  