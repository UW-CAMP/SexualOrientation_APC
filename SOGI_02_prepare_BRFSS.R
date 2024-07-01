# BRFSS data preparation script
# Mike Barry, MPH PhC | mpbarry@uw.edu | 2023 September

# !!! Before running this script, please review this repository's ReadMe file to ensure that "Step 1: Prepare Data Sets" is complete.

# This script takes BRFSS data sets (2014:2021), which must have been previously obtained and prepared for use in the R environment, 
# and prepares them data for an analysis concerned with sexual orientation (SO) and gender identity (GI).

# Key product: A single, combined BRFSS .rds data set for 2014:2021 with variables needed for SO & GI analyses.

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

# * call in data -------------------
# Note: before calling in data, ensure that the directory location is correct; you may need to change that used here.

# the `brfss_years` object is designed to facilitate the addition of future years of BRFSS
brfss_years <- 14:21

brfss<- list()
for (year in brfss_years) {
   cat("Loading BRFSS 20", year, "\n", sep="")
   filename <- paste0("data - clean/brfss", year, ".rds")
   brfss[[year]] <-
   readRDS(filename)
}


# Data management --------
# * 6-level S.O. variable -------------
for(year in 14:17) {
   brfss[[year]]$so <- factor(brfss[[year]]$SXORIENT,
                              levels = c(1,2,3,4,7,9),
                              labels = c("1_straight",
                                         "2_lesgay",
                                         "3_bi",
                                         "4_other",
                                         "5_dkns",
                                         "6_ref"))
}

for(year in 18:max(brfss_years)) {
   brfss[[year]]$so <- NA
   brfss[[year]]$so[brfss[[year]]$SOMALE == 1 | brfss[[year]]$SOFEMALE == 1] <- "2_lesgay"
   brfss[[year]]$so[brfss[[year]]$SOMALE == 2 | brfss[[year]]$SOFEMALE == 2] <- "1_straight"
   brfss[[year]]$so[brfss[[year]]$SOMALE == 3 | brfss[[year]]$SOFEMALE == 3] <- "3_bi"
   brfss[[year]]$so[brfss[[year]]$SOMALE == 4 | brfss[[year]]$SOFEMALE == 4] <- "4_other"
   brfss[[year]]$so[brfss[[year]]$SOMALE == 7 | brfss[[year]]$SOFEMALE == 7] <- "5_dkns"
   brfss[[year]]$so[brfss[[year]]$SOMALE == 9 | brfss[[year]]$SOFEMALE == 9] <- "6_ref"
}

# * so_new (to match up with YRBS categories) ------
for(year in brfss_years) {
   brfss[[year]]$so_new <- NA
   brfss[[year]]$so_new[brfss[[year]]$so == "1_straight"] <- "1_straight"
   brfss[[year]]$so_new[brfss[[year]]$so == "2_lesgay"] <- "2_lesgay"
   brfss[[year]]$so_new[brfss[[year]]$so == "3_bi"] <- "3_bi"
   brfss[[year]]$so_new[brfss[[year]]$so == "4_other"] <- "4_dko"
   brfss[[year]]$so_new[brfss[[year]]$so == "5_dkns"] <- "4_dko"
   brfss[[year]]$so_new[brfss[[year]]$so == "6_ref"] <- "5_ref"
}

# * G.I. variable --------
for(year in brfss_years) {
   brfss[[year]]$gender <- factor(brfss[[year]]$TRNSGNDR,
                                  levels = c(1,2,3,4,7,9),
                                  labels = c(
                                     "1_transwoman",
                                     "2_transman",
                                     "3_nbgnc",
                                     "4_cisgender",
                                     "5_DKNS",
                                     "6_ref"
                                  ))
}

# * age variable ------
for (year in brfss_years) {
   brfss[[year]]$age <- brfss[[year]]$`_AGE80`
}

# * restrict to those under 80 -----
for (year in brfss_years) {
   brfss[[year]] <- subset(brfss[[year]], age < 80)
}

# * year variable -----
for (yr in brfss_years) {
   brfss[[yr]]$year <- paste0("20", yr)
}

# * strata variable ----
for (year in brfss_years) {
  brfss[[year]]$strata <- brfss[[year]]$`_STSTR`
}


# * weight variable -----
for (year in brfss_years) {
   brfss[[year]]$weight <- brfss[[year]]$`_LLCPWT`
}

# * sex variable -------
for (year in 14:17) {
   brfss[[year]]$sex <- factor(brfss[[year]]$SEX, 
                               levels = 1:2,
                               labels = c("Male", "Female"))
}

brfss[[18]]$sex <- factor(brfss[[18]]$SEX1, 
                          levels = 1:2,
                          labels = c("Male", "Female"))

for (year in 19:max(brfss_years)) {
   brfss[[year]]$sex <- factor(brfss[[year]]$SEXVAR, 
                               levels = 1:2,
                               labels = c("Male", "Female"))
}

# * sex_bin -------
for (yr in brfss_years) {
   brfss[[yr]]$sex_bin <- NA
   brfss[[yr]]$sex_bin[brfss[[yr]]$sex == "Male"] <- "male"
   brfss[[yr]]$sex_bin[brfss[[yr]]$sex == "Female"] <- "female"
}

# * birthsex_state -------
for (year in 14:18) {
   brfss[[year]]$birthsex_state <- 0
}

brfss[[19]]$birthsex_state <- ifelse(brfss[[19]]$`_STATE` %in% c(15, 22, 27, 36, 42, 49, 50), 1, 0)
brfss[[20]]$birthsex_state <- ifelse(brfss[[20]]$`_STATE` %in% c(6, 13, 15, 19, 22, 27, 35, 36, 39, 49, 50), 1, 0)
brfss[[21]]$birthsex_state <- ifelse(brfss[[21]]$`_STATE` %in% c(13, 15, 19, 20, 22, 27, 35, 39, 49, 50), 1, 0)

# * sab ---------
for (year in 14:18) {
   brfss[[year]]$sab <- NA
}

for (year in 19:21) {
   brfss[[year]]$sab <- ifelse(brfss[[year]]$birthsex_state == 1, brfss[[year]]$BIRTHSEX, NA)
   
   brfss[[year]]$sab <- factor(brfss[[year]]$sab, 
                               levels = c(1,2,7,9),
                               labels = c("1_male", "2_female", "3_dnks", "4_ref"))
}

# * sab_bin -------
for (yr in brfss_years) {
   brfss[[yr]]$sab_bin <- NA
}
   
for (yr in 19:21) {
   brfss[[yr]]$sab_bin <- NA
   brfss[[yr]]$sab_bin[brfss[[yr]]$sab == "1_male"] <- "male"
   brfss[[yr]]$sab_bin[brfss[[yr]]$sab == "2_female"] <- "female"
}

# Filter to SOGI States ####
# Note: here, we limit to observations from states where SOGI module was used
for (year in brfss_years) {
   brfss[[year]]$state <- brfss[[year]]$`_STATE`
}

brfss_states <- list()
brfss_states[[14]] <- c(10, 15, 16, 18:22, 24, 27, 30, 32, 36, 39, 42, 50, 51, 55, 56)
brfss_states[[15]] <- c(8, 9, 10, 13, 15:18, 20, 24, 25, 27, 29, 32, 36, 39, 42, 48, 51, 54, 55)
brfss_states[[16]] <- c(6, 9, 10, 13, 15, 16, 17, 18, 19, 21, 22, 25, 27, 28, 29, 32, 36, 39, 42, 44, 48, 50, 51, 53, 55)
brfss_states[[17]] <- c(6, 9, 10, 12, 13, 15, 17, 18, 19, 22, 25, 27, 28, 30, 32, 36, 37, 39, 40, 42, 44, 45, 48, 50, 51, 53, 55)
brfss_states[[18]] <- c(9, 10, 12, 15, 16, 17, 20, 22, 24, 27, 28, 29, 30, 32, 36, 37, 39, 40, 42, 44, 45, 47, 48, 50, 51, 53, 54, 55)
brfss_states[[19]] <- c(2, 4, 8, 9, 10, 12, 13, 15, 16, 19, 20, 22, 24, 27, 28, 30, 36, 37, 39, 40, 44, 45, 47, 48, 49, 50, 51, 53, 54, 55)
brfss_states[[20]] <- c(2, 5, 6, 8, 9, 13, 15, 16, 17, 18, 19, 20, 22, 25, 26, 27, 30, 34, 35, 36, 37, 39, 40, 44, 45, 48, 49, 50, 51, 53, 54, 55)
brfss_states[[21]] <- c(2, 5, 8, 9, 13, 15, 16, 17, 18, 19, 20, 21, 22, 25, 27, 28, 29, 30, 32, 34, 35, 37, 39, 40, 42, 44, 48, 49, 50, 51, 53, 54, 55)

for (year in brfss_years) {
   brfss[[year]]$sogi_state <- ifelse(brfss[[year]]$state %in% brfss_states[[year]], 1, 0)
}

# remove people who were not asked SOGI questions
for (year in brfss_years) {
   brfss[[year]] <- brfss[[year]][brfss[[year]]$sogi_state == 1,]
}

# Remove NA SO & sex ####
# select only those with data for `so`
for (year in brfss_years) {
   brfss[[year]] <- brfss[[year]][!is.na(brfss[[year]]$so),]
}

# drop those with NA for `sex`
for (year in brfss_years) {
   brfss[[year]] <- brfss[[year]][!is.na(brfss[[year]]$sex),]
}

# Select variables ####
# select only necessary variables --
varnames <- c("age", "year", "so", "so_new", "sab", "sab_bin", "weight", 
              "strata", "sex", "sex_bin", "sogi_state", "state", "gender",
              "index")
for (year in brfss_years) {
   brfss[[year]] <- brfss[[year]][, varnames]
}

# Remove NA gender ####
# remove people with NA for gender
for (year in brfss_years) {
   brfss[[year]] <- brfss[[year]][!is.na(brfss[[year]]$gender),]
}

# Make year numeric ####
# make `year` variable a numerica
for (yr in brfss_years) {
   brfss[[yr]]$year <- as.numeric(brfss[[yr]]$year)
}

# Create cohort variable ####
# cohort variable
for (yr in brfss_years) {
   brfss[[yr]]$cohort <- brfss[[yr]]$year - brfss[[yr]]$age
}

# Remove NA so_new ####
# drop NAs for `so_new`
for (year in brfss_years) {
   brfss[[year]] <- brfss[[year]][!is.na(brfss[[year]]$so_new),]
}

# Add last variables ####
# add `source` variable
for (year in brfss_years) {
   brfss[[year]]$source <- "BRFSS"
}

# add `cohort_5` and `age_5` variable
for (yr in brfss_years) {
   
   brfss[[yr]]$cohort_5 <- NA
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort <= 1939] <- "A_1939"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1940 & brfss[[yr]]$cohort <= 1944] <- "B_1944"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1945 & brfss[[yr]]$cohort <= 1949] <- "C_1949"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1950 & brfss[[yr]]$cohort <= 1954] <- "D_1954"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1955 & brfss[[yr]]$cohort <= 1959] <- "E_1959"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1960 & brfss[[yr]]$cohort <= 1964] <- "F_1964"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1965 & brfss[[yr]]$cohort <= 1969] <- "G_1969"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1970 & brfss[[yr]]$cohort <= 1974] <- "H_1974"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1975 & brfss[[yr]]$cohort <= 1979] <- "I_1979"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1980 & brfss[[yr]]$cohort <= 1984] <- "J_1984"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1985 & brfss[[yr]]$cohort <= 1989] <- "K_1989"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1990 & brfss[[yr]]$cohort <= 1994] <- "L_1994"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 1995 & brfss[[yr]]$cohort <= 1999] <- "M_1999"
   brfss[[yr]]$cohort_5[brfss[[yr]]$cohort >= 2000] <- "N_2003"

   brfss[[yr]]$age_5 <- NA
   brfss[[yr]]$age_5[brfss[[yr]]$age <= 19] <- "A_19"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 20 & brfss[[yr]]$age <= 24] <- "B_24"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 25 & brfss[[yr]]$age <= 29] <- "C_29"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 30 & brfss[[yr]]$age <= 34] <- "D_34"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 35 & brfss[[yr]]$age <= 39] <- "E_39"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 40 & brfss[[yr]]$age <= 44] <- "F_44"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 45 & brfss[[yr]]$age <= 49] <- "G_49"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 50 & brfss[[yr]]$age <= 54] <- "H_54"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 55 & brfss[[yr]]$age <= 59] <- "I_59"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 60 & brfss[[yr]]$age <= 64] <- "J_64"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 65 & brfss[[yr]]$age <= 69] <- "K_69"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 70 & brfss[[yr]]$age <= 74] <- "L_74"
   brfss[[yr]]$age_5[brfss[[yr]]$age >= 75 & brfss[[yr]]$age <= 79] <- "M_79"
}

# write BRFSS RDS file ------
brfss_data <- rbind(brfss[[14]],
                    brfss[[15]],
                    brfss[[16]],
                    brfss[[17]],
                    brfss[[18]],
                    brfss[[19]],
                    brfss[[20]],
                    brfss[[21]])

write_rds(brfss_data,
          "data - clean/brfss_final.rds")

# prepare and write sex-at-birth RDS file ------
brfss_sab <- rbind(brfss[[19]],
                   brfss[[20]],
                   brfss[[21]])

write_rds(brfss_sab[!is.na(brfss_sab$sab),],
          "data - clean/brfss_sab.rds")

write_rds(brfss_sab[is.na(brfss_sab$sab),],
          "data - clean/brfss_sab_compare.rds")
