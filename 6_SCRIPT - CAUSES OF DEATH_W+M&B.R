## MATHEUS' MA THESIS ##

# ---
# Chapter 4 - CAUSES OF DEATH

# ---
# SCRIPT: loading the necessary R Packages

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM, patchwork)

# ---
# SCRIPT: locally loading data

  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/ICD10")
  
  deathsICD10_data_WhiteMales <- read_delim("White_Males_ICD10.csv", delim = ";") %>% 
    mutate("Cap XV" = 0,
           "Cap XIX" = 0) %>% 
    select(-c("Total"))

  deathsICD10_data_MixedMales <- read_delim("Mixed_Males_ICD10.csv", delim = ";") %>% 
    mutate("Cap XV" = 0,
           "Cap XIX" = 0) %>% 
    select(-c("Total"))
  
  deathsICD10_data_BlackMales <- read_delim("Black_Males_ICD10.csv", delim = ";") %>% 
    mutate("Cap XV" = 0,
           "Cap XIX" = 0) %>% 
    select(-c("Total"))
  
  deathsICD10_data_WhiteFemales <- read_delim("White_Females_ICD10.csv", delim = ";") %>% 
    mutate("Cap XIX" = 0) %>% 
    select(-c("Total"))
  
  deathsICD10_data_MixedFemales <- read_delim("Mixed_Females_ICD10.csv", delim = ";") %>% 
    mutate("Cap XIX" = 0) %>% 
    select(-c("Total"))
  
  deathsICD10_data_BlackFemales <- read_delim("Black_Females_ICD10.csv", delim = ";") %>% 
    mutate("Cap XIX" = 0) %>% 
    select(-c("Total"))
  
# ---
# SCRIPT: making (not so) small adjustments to the data
  
  deathsICD10_data_WhiteMales <- deathsICD10_data_WhiteMales %>% 
    rename("AGE" = "Idade") %>% 
    rename_with(~str_remove(., 'Cap ')) %>% 
    pivot_longer(I:XIX, names_to = "CHAPTER", values_to = "TOTAL") %>% 
    mutate(CHAPTER = ordered(CHAPTER, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 
                                        "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", 
                                        "XVIII", "XIX", "XX")),
           RACE_COLOR = "White") %>% 
    group_by(., AGE) %>% 
    mutate(SHARE = TOTAL / sum(TOTAL))
  
  deathsICD10_data_MixedMales <- deathsICD10_data_MixedMales %>% 
    rename("AGE" = "Idade") %>% 
    rename_with(~str_remove(., 'Cap ')) %>% 
    pivot_longer(I:XIX, names_to = "CHAPTER", values_to = "TOTAL") %>% 
    mutate(CHAPTER = ordered(CHAPTER, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 
                                        "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", 
                                        "XVIII", "XIX", "XX")),
           RACE_COLOR = "Mixed")
  
  deathsICD10_data_BlackMales <- deathsICD10_data_BlackMales %>% 
    rename("AGE" = "Idade") %>% 
    rename_with(~str_remove(., 'Cap ')) %>% 
    pivot_longer(I:XIX, names_to = "CHAPTER", values_to = "TOTAL") %>% 
    mutate(CHAPTER = ordered(CHAPTER, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 
                                        "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", 
                                        "XVIII", "XIX", "XX")),
           RACE_COLOR = "Black")

  deathsICD10_data_MixedBlackMales <- rbind(deathsICD10_data_MixedMales, deathsICD10_data_BlackMales) %>% 
    group_by(., AGE) %>% 
    mutate(SHARE = TOTAL / sum(TOTAL))
  
  deathsICD10_data_MixedBlackMales$RACE_COLOR <- case_match(deathsICD10_data_MixedBlackMales$RACE_COLOR,
                                                            "Black" ~ "Mixed_Black",
                                                            "Mixed" ~ "Mixed_Black")
  
# ---
# SCRIPT: making (not so) small adjustments to the data
  
  deathsICD10_data_WhiteFemales <- deathsICD10_data_WhiteFemales %>% 
    rename("AGE" = "Idade") %>% 
    rename_with(~str_remove(., 'Cap ')) %>% 
    pivot_longer(I:XIX, names_to = "CHAPTER", values_to = "TOTAL") %>% 
    mutate(CHAPTER = ordered(CHAPTER, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 
                                        "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", 
                                        "XVIII", "XIX", "XX")),
           RACE_COLOR = "White") %>% 
    group_by(., AGE) %>% 
    mutate(SHARE = TOTAL / sum(TOTAL))
  
  deathsICD10_data_MixedFemales <- deathsICD10_data_MixedFemales %>% 
    rename("AGE" = "Idade") %>% 
    rename_with(~str_remove(., 'Cap ')) %>% 
    pivot_longer(I:XIX, names_to = "CHAPTER", values_to = "TOTAL") %>% 
    mutate(CHAPTER = ordered(CHAPTER, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 
                                        "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", 
                                        "XVIII", "XIX", "XX")),
           RACE_COLOR = "Mixed")
  
  deathsICD10_data_BlackFemales <- deathsICD10_data_BlackFemales %>% 
    rename("AGE" = "Idade") %>% 
    rename_with(~str_remove(., 'Cap ')) %>% 
    pivot_longer(I:XIX, names_to = "CHAPTER", values_to = "TOTAL") %>% 
    mutate(CHAPTER = ordered(CHAPTER, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 
                                        "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", 
                                        "XVIII", "XIX", "XX")),
           RACE_COLOR = "Black")
  
  deathsICD10_data_MixedBlackFemales <- rbind(deathsICD10_data_MixedFemales, deathsICD10_data_BlackFemales) %>% 
    group_by(., AGE) %>% 
    mutate(SHARE = TOTAL / sum(TOTAL))
  
  deathsICD10_data_MixedBlackFemales$RACE_COLOR <- case_match(deathsICD10_data_MixedBlackFemales$RACE_COLOR,
                                                              "Black" ~ "Mixed_Black",
                                                              "Mixed" ~ "Mixed_Black")
  
# ---
# SCRIPT: plotting everything
  
  p1 <- ggplot(rbind(filter(deathsICD10_data_WhiteMales, AGE == "Total"),
                     filter(deathsICD10_data_MixedBlackMales, AGE == "Total")) %>% 
                 mutate(RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black")))) +
    
    geom_bar(position = "fill",stat = "Identity", 
             mapping = aes(x = CHAPTER, y = SHARE, fill = CHAPTER, color = CHAPTER)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100), limits = c(0,0.4)) +
    
    facet_wrap(~ RACE_COLOR) +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "none") +
    
    guides(x = guide_axis(angle = 45, n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "ICD 10 Chapter", y = "Share (in %)", 
         title = "Causes of Death by Race: Males, 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from DATASUS")
  
  p2 <- ggplot(rbind(filter(deathsICD10_data_WhiteFemales, AGE == "Total"),
                     filter(deathsICD10_data_MixedBlackFemales, AGE == "Total")) %>% 
                 mutate(RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black")))) +
    
    geom_bar(stat = "Identity", 
             mapping = aes(x = CHAPTER, y = SHARE, fill = CHAPTER, color = CHAPTER)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100), limits = c(0,0.4)) +
    
    facet_wrap(~ RACE_COLOR) +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "none") +
    
    guides(x = guide_axis(angle = 45, n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "ICD 10 Chapter", y = "Share (in %)", 
         title = "Causes of Death by Race: Females, 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from DATASUS")
  
  p1
  p2
  
  p3 <- ggplot(rbind(filter(deathsICD10_data_WhiteMales, AGE == "Total"),
                     filter(deathsICD10_data_MixedBlackMales, AGE == "Total")) %>% 
                 mutate(RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black")))) +
    
    geom_bar(stat = "Identity", 
             mapping = aes(x = CHAPTER, y = SHARE, fill = CHAPTER, color = CHAPTER)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100), limits = c(0,0.4)) +
    
    facet_wrap(~ RACE_COLOR, labeller = labeller(RACE_COLOR = c("White" = "White Males", "Mixed_Black" = "Mixed_Black Males"))) +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "none") +
    
    guides(x = guide_axis(angle = 45, n.dodge = 1, check.overlap = TRUE)) +
    
    theme(axis.text.x = element_blank(),
          axis.ticks.x=element_blank()) +
    
    labs(x = "", y = "Share (in %)", 
         title = "Causes of Death by Gender and Race: 2010-2019",
         subtitle = "São Paulo, Brazil")
  
  p4 <- ggplot(rbind(filter(deathsICD10_data_WhiteFemales, AGE == "Total"),
               filter(deathsICD10_data_MixedBlackFemales, AGE == "Total")) %>% 
           mutate(RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black")))) +
    
    geom_bar(stat = "Identity", 
             mapping = aes(x = CHAPTER, y = SHARE, fill = CHAPTER, color = CHAPTER)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100), limits = c(0,0.4)) +
    
    facet_wrap(~ RACE_COLOR, labeller = labeller(RACE_COLOR = c("White" = "White Females", "Mixed_Black" = "Mixed_Black Females"))) +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "none") +
    
    guides(x = guide_axis(angle = 45, n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "ICD 10 Chapter", y = "Share (in %)",
         caption = "\n Source: Own calculations from DATASUS")

  p3/p4
  