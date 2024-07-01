# YRBS & BRFSS variable investigation & data set generation script
# Mike Barry, MPH | mpbarry@uw.edu | 2023 March

# !!! Before running this script, please review this repository's ReadMe file to ensure that "Step 1: Prepare Data Sets" is complete.

# This script previously preapred .rds files for BRFSS and YRBS and prepares them for a combined analysis.

# Key products:
# A single .rds for the SO analyses in Script

### Prepare workspace ----- 
# clear environment
rm(list = ls())

# * call in packages ----
source("SOGI_00_packages.R")

# * define functions -----
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}
# the `tableNA` function tabulates data as the `table` function, but includes "NA"s by default.

# * Call in data -------------------
brfss <- readRDS("data - clean/brfss_final.rds")
yrbs <- readRDS("data - clean/yrbs_final.rds")

# Prepare datasets for combination ------
# * calibrate weight -------
# wt_factor <-mean(yrbs$weight)/mean(brfss$weight)
# brfss$weight <- brfss$weight*wt_factor

# * rename strata variable ----
brfss <- brfss %>% 
  rename("stratum" = "strata")

# * reduced period variable -----------
yrbs$pd_2 <- factor(yrbs$year,
                   levels = c(2015, 2017, 2019, 2021),
                   labels = c(
                      "01_2015",
                      "02_2017",
                      "03_2019",
                      "04_2021"
                   ))

brfss$pd_2 <- factor(brfss$year,
                     levels = c(2014:2021),
                     labels = c(
                        "01_2015",
                        "01_2015",
                        "02_2017",
                        "02_2017",
                        "03_2019",
                        "03_2019",
                        "04_2021",
                        "04_2021"
                     ))

# combine to generate common variables ------
brfss$year <- as.numeric(brfss$year)
brfss <-  remove_var_label(brfss)
combined <- bind_rows(brfss, yrbs)

# * cohort variable ---------
combined$cohort <- combined$year - combined$age

# * cohort reduced variable -------

combined$cohort_2 <- factor(
   combined$cohort,
   levels = c(1935:2007),
   labels = c(
      "01_1936",
      "01_1936",
      "02_1938",
      "02_1938",
      "03_1940",
      "03_1940",
      "04_1942",
      "04_1942",
      "05_1944",
      "05_1944",
      "06_1946",
      "06_1946",
      "07_1948",
      "07_1948",
      "08_1950",
      "08_1950",
      "09_1952",
      "09_1952",
      "10_1954",
      "10_1954",
      "11_1956",
      "11_1956",
      "12_1958",
      "12_1958",
      "13_1960",
      "13_1960",
      "14_1962",
      "14_1962",
      "15_1964",
      "15_1964",
      "16_1966",
      "16_1966",
      "17_1968",
      "17_1968",
      "18_1970",
      "18_1970",
      "19_1972",
      "19_1972",
      "20_1974",
      "20_1974",
      "21_1976",
      "21_1976",
      "22_1978",
      "22_1978",
      "23_1980",
      "23_1980",
      "24_1982",
      "24_1982",
      "25_1984",
      "25_1984",
      "26_1986",
      "26_1986",
      "27_1988",
      "27_1988",
      "28_1990",
      "28_1990",
      "29_1992",
      "29_1992",
      "30_1994",
      "30_1994",
      "31_1996",
      "31_1996",
      "32_1998",
      "32_1998",
      "33_2000",
      "33_2000",
      "34_2002",
      "34_2002",
      "35_2004",
      "35_2004",
      "36_2006",
      "36_2006",
      "36_2006"   ## Note: those born in 2007 are grouped with the 2006 cohort
   )
)

# write new combined dataset as .rds ---------
write_rds(combined, "data - clean/combined.rds")
