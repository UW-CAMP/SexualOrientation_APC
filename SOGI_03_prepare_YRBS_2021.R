# YRBS data preparation script
# Mike Barry, MPH PhC | mpbarry@uw.edu | 2023 September

# !!! Before running this script, please review this repository's ReadMe file to ensure that "Step 1: Prepare Data Sets" is complete.

# This script takes YRBS 2021 dataset, which must have been previously obtained and prepared for use in the R environment, 
# and prepares it for merging with YRBS 2015:2019.

# Key product: A single, YRBS 2021 .rds file.

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

# * call in data ------------
# Note: before calling in data, ensure that the directory location is correct; you may need to change that used here.
yrbs21 <- read.csv("data - raw/yrbs2021.csv")

# * prepare data for cleanup ------
names(yrbs21) <- tolower(names(yrbs21)) # make variable names lowercase
yrbs21 <- yrbs21 %>% mutate(id = row_number()) # generate an index variable (id)
yrbs21[yrbs21 == "Missing"] <- NA # make "missing" NA

### Remove observations with missing for sex, age -------
yrbs21 <- subset(yrbs21, !is.na(q1) & !is.na(q2))

### Define new variables / apply informative missingness as needed -----------
# * year ------
yrbs21$year <- 2021

# * source ------
yrbs21$source <- "YRBS"

# * age ------
yrbs21$age <- as.numeric(substr(yrbs21$q1, 1, 2))

# REMOVE those under 14 (N=94) ----------
yrbs21 <- yrbs21[yrbs21$age >= 14,] 

# * sex --------
yrbs21$sex <- yrbs21$q2

# *'so_21' -----------
yrbs21$so_21 <- NA

yrbs21$so_21[yrbs21$q65 == "Heterosexual (straight)"] <- "1_straight"
yrbs21$so_21[yrbs21$q65 == "Gay or lesbian"] <- "2_lesgay"
yrbs21$so_21[yrbs21$q65 == "Bisexual"] <- "3_bi"
yrbs21$so_21[yrbs21$q65 == "Not sure"] <- "4_dkns"
yrbs21$so_21[is.na(yrbs21$q65)] <- "5_pnta"
yrbs21$so_21[yrbs21$q65 == "Don't know what this means"] <- "6_dkwtm" 
yrbs21$so_21[yrbs21$q65 == "Some other way"] <- "7_somethingelse" 

# *'so' ------------
yrbs21$so[yrbs21$so_21 == "1_straight"] <- "1_straight"
yrbs21$so[yrbs21$so_21 == "2_lesgay"] <- "2_lesgay"
yrbs21$so[yrbs21$so_21 == "3_bi"] <- "3_bi"
yrbs21$so[yrbs21$so_21 == "4_dkns" |
             yrbs21$so_21 == "6_dkwtm"] <- "4_dkns"
yrbs21$so[yrbs21$so_21 == "5_pnta" |
             yrbs21$so_21 == "7_somethingelse"] <- "5_pnta"

# * so_new -------
yrbs21$so_new[yrbs21$so_21 == "1_straight"] <- "1_straight"
yrbs21$so_new[yrbs21$so_21 == "2_lesgay"] <- "2_lesgay"
yrbs21$so_new[yrbs21$so_21 == "3_bi"] <- "3_bi"
yrbs21$so_new[yrbs21$so_21 == "4_dkns" |
                 yrbs21$so_21 == "6_dkwtm" |
                yrbs21$so_21 == "7_somethingelse"] <- "4_dko"
yrbs21$so_new[yrbs21$so_21 == "5_pnta"] <- "5_ref"

# *'cohort' --------------
yrbs21$cohort <- 2021-yrbs21$age

# * ever_sex -----
yrbs21$ever_sex <- NA

# informative missingness
yrbs21$ever_sex[yrbs21$q58 == "Never had sex" | 
                   yrbs21$q59 == "Never had sex" |
                   yrbs21$q60 == "Never had sex" |
                   yrbs21$q61 == "Never had sex" |     
                   yrbs21$q62 == "Never had sex" |  
                   yrbs21$q64 == "Never had sexual contact"
] <- 0 #any time R indicates never having had sex in a later question, 

yrbs21$ever_sex[yrbs21$q58 != "Never had sex" & !is.na(yrbs21$q58)] <- 1
yrbs21$ever_sex[yrbs21$q59 != "Never had sex" & !is.na(yrbs21$q59)] <- 1
yrbs21$ever_sex[yrbs21$q60 != "Never had sex" & !is.na(yrbs21$q60)] <- 1
yrbs21$ever_sex[yrbs21$q61 != "Never had sex" & !is.na(yrbs21$q61)] <- 1
yrbs21$ever_sex[yrbs21$q62 != "Never had sex" & !is.na(yrbs21$q62)] <- 1
yrbs21$ever_sex[yrbs21$q64 != "Never had sexual contact" & !is.na(yrbs21$q64)] <- 1

yrbs21$ever_sex[yrbs21$q57 == "No"] <- 0
yrbs21$ever_sex[yrbs21$q57 == "Yes"] <- 1

# * sex_of_sps ------
yrbs21$sex_of_sps <- NA

yrbs21$sex_of_sps[yrbs21$q64 == "Never had sexual contact"] <- "1_never"
yrbs21$sex_of_sps[yrbs21$q64 == "Females"] <- "2_female"
yrbs21$sex_of_sps[yrbs21$q64 == "Males"] <- "3_male"
yrbs21$sex_of_sps[yrbs21$q64 == "Females and males"] <- "4_fem+mal"

# yrbs21 <- subset(yrbs21, !is.na(q8)) #remove observations based on missingness investigation


### Remove the "Survey Stoppers" ---------
yrbs21$drop <- ifelse((is.na(yrbs21$q60) &
                               is.na(yrbs21$q61) &
                               is.na(yrbs21$q62) &
                               is.na(yrbs21$q63) &
                               is.na(yrbs21$q64) &
                               is.na(yrbs21$q65) &
                               is.na(yrbs21$q66) &
                               is.na(yrbs21$q67) &
                               is.na(yrbs21$q68) &
                               is.na(yrbs21$q69) &
                               is.na(yrbs21$q70) &
                               is.na(yrbs21$q71) &
                               is.na(yrbs21$q72) &
                               is.na(yrbs21$q73) &
                               is.na(yrbs21$q74) &
                               is.na(yrbs21$q75) &
                               is.na(yrbs21$q76) &
                               is.na(yrbs21$q77) &
                               is.na(yrbs21$q78) &
                               is.na(yrbs21$q71) &
                               is.na(yrbs21$q72) &
                               is.na(yrbs21$q73) &
                               is.na(yrbs21$q74) &
                               is.na(yrbs21$q75) &
                               is.na(yrbs21$q76) &
                               is.na(yrbs21$q77) &
                               is.na(yrbs21$q78) &
                               is.na(yrbs21$q79) &
                               is.na(yrbs21$q80) &
                               is.na(yrbs21$q81) &
                               is.na(yrbs21$q82) &
                               is.na(yrbs21$q83) &
                               is.na(yrbs21$q84) &
                               is.na(yrbs21$q85) &
                               is.na(yrbs21$q86) &
                               is.na(yrbs21$q87) &
                               is.na(yrbs21$q88) &
                               is.na(yrbs21$q89) &
                               is.na(yrbs21$q90) &
                               is.na(yrbs21$q91) &
                               is.na(yrbs21$q92) &
                               is.na(yrbs21$q93) &
                               is.na(yrbs21$q94) &
                               is.na(yrbs21$q95) &
                               is.na(yrbs21$q96) &
                               is.na(yrbs21$q97) &
                               is.na(yrbs21$q98) &
                               is.na(yrbs21$q99)), 1, 0)

yrbs21 <- subset(yrbs21, drop == 0)

### Remove the people who answered none of the sexual behavior questions --------
yrbs21$drop2 <- ifelse((is.na(yrbs21$q57) &
                           is.na(yrbs21$q58) &
                           is.na(yrbs21$q59) &
                           is.na(yrbs21$q60) &
                           is.na(yrbs21$q61) &
                           is.na(yrbs21$q62) &
                           is.na(yrbs21$q63) &
                           is.na(yrbs21$q64)), 1, 0)

yrbs21 <- subset(yrbs21, drop2 == 0)

### Examine missingness for `so_21`, `sex_of_sps` and `ever_sex` -----
round(prop.table(tableNA(yrbs21$so_21))*100,1) #1.5% PNTA
round(prop.table(tableNA(yrbs21$sex_of_sps))*100,1) # 3.0% NA
round(prop.table(tableNA(yrbs21$ever_sex))*100,1) # 1.9% NA

### REMOVE Rs with NA for `sex_of_sps` and `ever_sex` --------
yrbs21 <- subset(yrbs21, !is.na(sex_of_sps))
yrbs21 <- subset(yrbs21, !is.na(ever_sex))

### Examine missingness after removing NAs for `sex_of_sps` and `ever_sex` --------
round(prop.table(tableNA(yrbs21$so_21))*100,1) #1.0% PNTA
round(prop.table(tableNA(yrbs21$sex_of_sps))*100,1) # 0 NA
round(prop.table(tableNA(yrbs21$ever_sex))*100,1) # 0 NA

# * restrict to only needed variables and merge with prior years ------
varnames <- c("year", "weight", "psu", "stratum", "age", "sex",
              "so", "so_new", "so_21", "ever_sex", "sex_of_sps",
              "cohort", "source")
yrbs21 <- yrbs21[,varnames]


# Write .rds file ------
write_rds(yrbs21,
          "data - raw/yrbs21_tomerge.RDS")
