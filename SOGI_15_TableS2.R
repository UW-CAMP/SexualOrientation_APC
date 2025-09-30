## Get sample sizes for Table S2?
## Jessica Godwin

# Setup ####
rm(list = ls())

## Libraries ####
library(tidyverse)


## Functions ####
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}

# Load Data ####

combo <- readRDS(file = "data - clean/combined.rds")

str(combo)
summary(combo)
head(combo)

tableNA(combo$so)
tableNA(combo$so_new)


table_sampsize <- combo %>%
  filter(!(source == "YRBS" & age == 18)) %>% 
  group_by(year, so_new) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(Total = sum(n, na.rm = TRUE),
         sm_identity = Total - n[1]) %>% 
  pivot_wider(id_cols = c("year", "Total", "sm_identity"),
              names_from = "so_new",
              values_from = "n") %>% 
  select(-`1_straight`) %>% 
  rename("Year" = "year", "N" = "Total",
         "Sexual Minority" = "sm_identity",
         "Lesbian/Gay" = "2_lesgay",
         "Bisexual" = "3_bi",
         "Other response" = "4_dko",
         "Declined to Answer" = "5_ref") %>% 
  mutate(Straight = N - `Sexual Minority`)

table_sampsize %>% 
  View()
