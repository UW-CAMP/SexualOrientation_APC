# YRBS data preparation script
# Mike Barry, MPH PhC | mpbarry@uw.edu | 2023 September

# !!! Before running this script, please review this repository's ReadMe file to ensure that "Step 1: Prepare Data Sets" is complete.

# This script merges YRBS 2021 data with YRBS 2015:2019 data for analysis in the SOGI project.

### Prepare workspace ----- 
# clear environment
rm(list = ls())

# * call in packages ----
source("SOGI_00_packages.R")

# Note: You if you haven't already run these two scripts, be sure to uncomment and run scripts 02.1 and 02.2. 
# source("SOGI_02.1_Prepare_YRBS2021.R")
# source("SOGI_02.2_Prepare_YRBS2015-2019.R")

# * call in data ------------
yrbs_21 <- readRDS("data - raw/yrbs21_tomerge.RDS")
yrbs_1519 <- readRDS("data - raw/yrbs1519_tomerge.RDS")

yrbs_1519 <- yrbs_1519 %>% 
  rename("psu" = "PSU")

# merge datasets -------
yrbs_merge <- rbind(
   yrbs_21,
   yrbs_1519
)

# Write .rds file ------
write_rds(yrbs_merge,
          "data - clean/yrbs_final.RDS")
