## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - POPULATION DATA

# ---
# SCRIPT: loading the necessary R Packages

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("2.2.2_SCRIPT - B&H LIFE TABLES_DDM_W+MB.R")
  
# ---
# SCRIPT: writing the decomposition function
  
  decomposition <- function(LT_1, LT_2){
    
    temp <- tibble(LT_1$AGE_GROUP, LT_1$lx, LT_1$Lx, LT_1$Tx, LT_2$lx, LT_2$Lx, LT_2$Tx) %>% 
      rename("AGE_GROUP" = "LT_1$AGE_GROUP", 
             "lx_1"= "LT_1$lx",
             "Lx_1"= 'LT_1$Lx',
             "Tx_1"= "LT_1$Tx",
             "lx_2"= "LT_2$lx",
             "Lx_2"= "LT_2$Lx",
             "Tx_2"= "LT_2$Tx")
    
    Direct <- temp$lx_1 / first(temp$lx_1) * (temp$Lx_2 / temp$lx_2 - temp$Lx_1 / temp$lx_1)
  
    Indirect <- lead(temp$Tx_1) / first(temp$lx_1) * (((temp$lx_1 * lead(temp$lx_2)) / (lead(temp$lx_1) * temp$lx_2)) - 1)
    
    Indirect <- ifelse(is.na(Indirect), 0, Indirect)
    
    OE <- lead(temp$Tx_2) / first(temp$lx_1) * (temp$lx_1 / temp$lx_2 - lead(temp$lx_1) / lead(temp$lx_2))
    
    OE <- ifelse(is.na(OE), 0, OE)
    
    Interaction <- OE - Indirect
    
    output <- tibble(temp$AGE_GROUP, Direct, Indirect, OE, Interaction) %>%
      rename("AGE_GROUP" = "temp$AGE_GROUP") %>% 
      # mutate(Total = Direct + Indirect + Interaction,
      #        Check = OE - (Indirect + Interaction)) %>%
      select(., c("AGE_GROUP", "Direct", "OE")) %>%
      rename("Indirect" = "OE") %>%
      pivot_longer(Direct:Indirect, names_to = "EFFECT", values_to = "Cx") %>%
      mutate(Share = Cx / sum(Cx))

    return(output)
    
  }

# ---
# SCRIPT: running the decomposition for Males
    
  DecompMale_MixedBlack_White <- decomposition(BH_LT_Male_Mixed_Black, BH_LT_Male_White)

# ---
# SCRIPT: generating plot of the decomposition for Males

ggplot() +
  
  geom_line(data = DecompMale_MixedBlack_White, 
            mapping = aes(x = AGE_GROUP, y = Share, group = EFFECT, color = EFFECT),
            linewidth = 0.8) +
  
  geom_line(data = DecompMale_MixedBlack_White %>% group_by(AGE_GROUP) %>% summarise(Total = sum(Share)) %>% mutate(EFFECT = "Total"), 
            mapping = aes(x = AGE_GROUP, y = Total, group = EFFECT, color = EFFECT),
            linewidth = 0.8) +
  
  scale_color_manual(values = c("Total" = "#00664D",
                                "Indirect" = "#00A37A",
                                "Direct" = "#00E0A8")) +
  
  scale_y_continuous(limits = c(-0.01, 0.15),
                     labels = function(x) x * 100) +
  
  theme(axis.title = element_text(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  
  guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
  
  labs(x = "Age Groups", y = "Share (in %)", 
       title = "Decomposition by Age and Race for Males: 2010-2019",
       subtitle = "São Paulo, Brazil",
       caption = "\n Source: Own calculations from IBGE and DATASUS")

# ---
# SCRIPT: running the decomposition for Females

  DecompFemale_MixedBlack_White <- decomposition(BH_LT_Female_Mixed_Black, BH_LT_Female_White)
  
# ---
# SCRIPT: generating plot of the decomposition for Females
  
  ggplot() +
    
    geom_line(data = DecompFemale_MixedBlack_White, 
              mapping = aes(x = AGE_GROUP, y = Share, group = EFFECT, color = EFFECT),
              linewidth = 0.8) +
    
    geom_line(data = DecompFemale_MixedBlack_White %>% group_by(AGE_GROUP) %>% summarise(Total = sum(Share)) %>% mutate(EFFECT = "Total"), 
              mapping = aes(x = AGE_GROUP, y = Total, group = EFFECT, color = EFFECT),
              linewidth = 0.8) +
    
    scale_color_manual(values = c("Total" = "#785889",
                                  "Indirect" = "#A083AF",
                                  "Direct" = "#C6B4CF")) +
    
    scale_y_continuous(limits = c(-0.01, 0.15),
                       labels = function(x) x * 100) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "Share (in %)", 
         title = "Decomposition by Age and Race for Females: 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")

# ---
# SCRIPT: running the decomposition for Whites
  
  Decomp_White <- decomposition(BH_LT_Male_White, BH_LT_Female_White)
  
# ---
# SCRIPT: generating plot of the decomposition for Whites
  
  ggplot() +
    
    geom_line(data = Decomp_White, 
              mapping = aes(x = AGE_GROUP, y = Share, group = EFFECT, color = EFFECT),
              linewidth = 0.8) +
    
    geom_line(data = Decomp_White %>% group_by(AGE_GROUP) %>% summarise(Total = sum(Share)) %>% mutate(EFFECT = "Total"), 
              mapping = aes(x = AGE_GROUP, y = Total, group = EFFECT, color = EFFECT),
              linewidth = 0.8) +
    
    scale_color_manual(values = c("Total" = "#ef476f",
                                  "Indirect" = "#F47C98",
                                  "Direct" = "#FAC7D3")) +
    
    scale_y_continuous(limits = c(-0.01, 0.15),
                       labels = function(x) x * 100) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "Share (in %)", 
         title = "Decomposition by Age and Gender for Whites: 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")  

# ---
# SCRIPT: running the decomposition for Mixed_Blacks
  
  Decomp_Mixed_Black <- decomposition(BH_LT_Male_Mixed_Black, BH_LT_Female_Mixed_Black)
  
# ---
# SCRIPT: generating plot of the decomposition for Mixed_Blacks
  
  ggplot() +
    
    geom_line(data = Decomp_Mixed_Black, 
              mapping = aes(x = AGE_GROUP, y = Share, group = EFFECT, color = EFFECT),
              linewidth = 0.8) +
    
    geom_line(data = Decomp_Mixed_Black %>% group_by(AGE_GROUP) %>% summarise(Total = sum(Share)) %>% mutate(EFFECT = "Total"), 
              mapping = aes(x = AGE_GROUP, y = Total, group = EFFECT, color = EFFECT),
              linewidth = 0.8) +
    
    scale_color_manual(values = c("Total" = "#0C6583",
                                  "Indirect" = "#139FCD",
                                  "Direct" = "#6BD1EF")) +
    
    scale_y_continuous(limits = c(-0.01, 0.15),
                       labels = function(x) x * 100) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "Share (in %)", 
         title = "Decomposition by Age and Gender for Mixed/Blacks: 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")

# ---
# Clearing out the Global Environment
  
  rm(list=ls()[! ls() %in% c("BH_LT_Female_White","BH_LT_Female_Mixed_Black",
                             "BH_LT_Male_White","BH_LT_Male_Mixed_Black",
                             "DecompFemale_MixedBlack_White",
                             "DecompMale_MixedBlack_White")])
  