## Get Horvitz-Thompson/Hajek estimators for
## Prevalence by age x period (survey) x cohort
## Jessica Godwin

# Setup ####
rm(list = ls())

## Libraries ####
library(survey)
library(tidyverse)


## Functions ####
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}

logit <- function(prob){
  log(prob/(1-prob))
}

expit <- function(logodds){
  exp(logodds)/(1 + exp(logodds))
}

# Load Data ####

combo <- readRDS(file = "data - clean/combined.rds")

str(combo)
summary(combo)
head(combo)

tableNA(combo$so)
tableNA(combo$so_new)
  

# Prep for estimation ####


## Filter to analysis ages ####

data_1x1 <- combo %>% 
  filter(source != "YRBS") %>% 
  select(year, age, cohort,
         sex, gender, so, so_new,
         contains("cohort_"), contains("pd_"),
         index, weight, stratum) %>% 
  mutate(lesgay_bin = ifelse(so_new == "2_lesgay", 1, 0),
         bi_bin = ifelse(so_new == "3_bi", 1, 0),
         dko_bin = ifelse(so_new == "4_dko", 1, 0),
         ref_bin = ifelse(so_new == "5_ref", 1, 0)) %>%
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  rename("period" = "year")
  
## Check out pbject
summary(data_1x1)
tableNA(data_1x1$age, data_1x1$cohort)
tableNA(data_1x1$period, data_1x1$cohort)
tableNA(data_1x1$age, data_1x1$period)


### YRBS ####

data_1x1_YRBS <- combo %>% 
  filter(source == "YRBS" & age != 18) %>% 
  select(year, age, cohort,
         sex, gender, so, so_new, so_21,
         contains("cohort_"), contains("pd_"),
         index, weight, stratum, psu) %>% 
  mutate(lesgay_bin = ifelse(so_new == "2_lesgay", 1, 0),
         bi_bin = ifelse(so_new == "3_bi", 1, 0),
         dko_bin = ifelse(so_new == "4_dko", 1, 0),
         ref_bin = ifelse(so_new == "5_ref", 1, 0),
         dkns_bin = ifelse(so_21 == "4_dkns", 1, 0),
         pnta_bin = ifelse(so_21 == "5_pnta", 1, 0),
         dkwtm_bin = ifelse(so_21 == "6_dkwtm", 1, 0),
         somethingelse_bin = ifelse(so_21 == "7_somethingelse", 1, 0)) %>%
  mutate(across(where(is.factor), ~as.character(.x))) %>% 
  rename("period" = "year")


## Specify design object ####

brfss_des <- svydesign(ids = ~1, strata = ~period + stratum,
                       weights = ~weight, data = data_1x1)

brfss_des

### YRBS ####
yrbs_des <- svydesign(ids = ~psu, strata = ~period + stratum,
                      weights = ~weight, nest = TRUE, data = data_1x1_YRBS)

yrbs_des

# Estimate prevalence ####
## BRFSS ####

prevs <- svyby(~lesgay_bin + bi_bin + dko_bin + ref_bin,
               by = ~sex + period + age + cohort,
               design = brfss_des,
               svymean)

## YRBS ####

prevs_yrbs <- svyby(~lesgay_bin + bi_bin + dko_bin + ref_bin,
                    by = ~sex + period + age + cohort,
                    design = yrbs_des,
                    svymean)

prevs_age_per_yrbs <- svyby(~lesgay_bin + bi_bin + dko_bin + ref_bin,
                            by = ~sex + period + age,
                            design = yrbs_des,
                            svymean)

## YRBS 2021 ####

prevs_yrbs_2021 <- svyby(~lesgay_bin + bi_bin + dkns_bin +
                           pnta_bin + dkwtm_bin + somethingelse_bin,
                         by = ~sex + period + age + cohort,
                         design = subset(yrbs_des, period == 2021),
                         svymean)

prevs_age_per_yrbs_2021 <- svyby(~lesgay_bin + bi_bin + dkns_bin +
                           pnta_bin + dkwtm_bin + somethingelse_bin,
                         by = ~sex + period + age,
                         design = subset(yrbs_des, period == 2021),
                         svymean)

# Clean up output & Save ####

## Fix names ####
### BRFSSS ####
names(prevs)[names(prevs) %in% paste(c("lesgay","bi", "dko", "ref"),
                                    "bin", sep = "_")] <- 
  paste0("mean.", c("lesgay","bi", "dko", "ref"), "_bin")

names(prevs)

### YRBS ####
names(prevs_yrbs)[names(prevs_yrbs) %in% paste(c("lesgay","bi", "dko", "ref"),
                                               "bin", sep = "_")] <- 
  paste0("mean.", c("lesgay","bi", "dko", "ref"), "_bin")

names(prevs_yrbs)


names(prevs_age_per_yrbs)[names(prevs_age_per_yrbs) %in% 
                           paste(c("lesgay","bi", "dko", "ref"), "bin",
                                 sep = "_")] <- 
  paste0("mean.", c("lesgay","bi", "dko", "ref"), "_bin")

names(prevs_age_per_yrbs)

### YRBS 2021 ####
names(prevs_yrbs_2021)[names(prevs_yrbs_2021) %in%
                         paste(c("lesgay","bi", "dkns", "pnta", "dkwtm",
                                 "somethingelse"), "bin", sep = "_")] <- 
  paste0("mean.", c("lesgay","bi", "dkns", "pnta", "dkwtm",
                    "somethingelse"), "_bin") 
names(prevs_yrbs_2021)

names(prevs_age_per_yrbs_2021)[names(prevs_age_per_yrbs_2021) %in%
                         paste(c("lesgay","bi", "dkns", "pnta", "dkwtm",
                                 "somethingelse"), "bin", sep = "_")] <- 
  paste0("mean.", c("lesgay","bi", "dkns", "pnta", "dkwtm",
                    "somethingelse"), "_bin") 
names(prevs_age_per_yrbs_2021)

## pivot_longer ####
### BRFSS ####
prevs_long <- prevs %>% 
  pivot_longer(cols = contains("_bin"),
               names_to = c(".value", "so"),
               names_pattern = "(.*)\\.(.*)_bin") %>% 
  ## Calculate logit(phat)
  ## And var(logit(phat)) = var(phat)/[phat^2*(1-phat)^2]
  ## via delta method
  mutate(logit_pijk = logit(mean),
         var = se^2,
         var_logit_pijk = var/(mean^2*(1-mean)^2))

write.csv(prevs_long, row.names = FALSE,
          file = paste0("data - clean/", "BRFSS_HT_prevs.csv"))

### YRBS ####
prevs_yrbs_long <- prevs_yrbs %>% 
  pivot_longer(cols = contains("_bin"),
               names_to = c(".value", "so"),
               names_pattern = "(.*)\\.(.*)_bin") %>% 
  ## Calculate logit(phat)
  ## And var(logit(phat)) = var(phat)/[phat^2*(1-phat)^2]
  ## via delta method
  mutate(logit_pijk = logit(mean),
         var = se^2,
         var_logit_pijk = var/(mean^2*(1-mean)^2))

write.csv(prevs_yrbs_long, row.names = FALSE,
          file = paste0("data - clean/", "YRBS_HT_prevs.csv"))


prevs_age_per_yrbs_long <- prevs_age_per_yrbs %>% 
  pivot_longer(cols = contains("_bin"),
               names_to = c(".value", "so"),
               names_pattern = "(.*)\\.(.*)_bin") %>% 
  ## Calculate logit(phat)
  ## And var(logit(phat)) = var(phat)/[phat^2*(1-phat)^2]
  ## via delta method
  mutate(logit_pijk = logit(mean),
         var = se^2,
         var_logit_pijk = var/(mean^2*(1-mean)^2))

write.csv(prevs_age_per_yrbs_long, row.names = FALSE,
          file = paste0("data - clean/", "YRBS_HT_AP_prevs.csv"))



### YRBS 2021 ####
prevs_yrbs_2021_long <- prevs_yrbs_2021 %>% 
  pivot_longer(cols = contains("_bin"),
               names_to = c(".value", "so"),
               names_pattern = "(.*)\\.(.*)_bin") %>% 
  ## Calculate logit(phat)
  ## And var(logit(phat)) = var(phat)/[phat^2*(1-phat)^2]
  ## via delta method
  mutate(logit_pijk = logit(mean),
         var = se^2,
         var_logit_pijk = var/(mean^2*(1-mean)^2))

write.csv(prevs_yrbs_2021_long, row.names = FALSE,
          file = paste0("data - clean/", "YRBS2021_HT_prevs.csv"))

prevs_age_per_yrbs_2021_long <- prevs_age_per_yrbs_2021 %>% 
  pivot_longer(cols = contains("_bin"),
               names_to = c(".value", "so"),
               names_pattern = "(.*)\\.(.*)_bin") %>% 
  ## Calculate logit(phat)
  ## And var(logit(phat)) = var(phat)/[phat^2*(1-phat)^2]
  ## via delta method
  mutate(logit_pijk = logit(mean),
         var = se^2,
         var_logit_pijk = var/(mean^2*(1-mean)^2))

write.csv(prevs_age_per_yrbs_2021_long, row.names = FALSE,
          file = paste0("data - clean/", "YRBS2021_HT_AP_prevs.csv"))
