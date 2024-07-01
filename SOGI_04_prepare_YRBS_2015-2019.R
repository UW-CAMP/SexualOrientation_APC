# YRBS data preparation script
# Mike Barry, MPH PhC | mpbarry@uw.edu | 2023 September

# !!! Before running this script, please review this repository's ReadMe file to ensure that "Step 1: Prepare Data Sets" is complete.

# This script takes YRBS data sets (2015:2019), which must have been previously obtained and prepared for use in the R environment, 
# and prepares them data for mergine with YRBS 2021 data/

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

# `yrbs_years` object 
yrbs_years <- seq(15, 19, 2)
yrbs_data_in <- read.csv("data - raw/yrbs_clean.csv")

yrbs <- list()
for (yr in yrbs_years) {
   yrbs[[yr]] <- subset(yrbs_data_in, year == 2000+yr)
}

# data management --------
# * age ----------
for (yr in yrbs_years) {
   yrbs[[yr]]$age_cat <- factor(yrbs[[yr]]$age,
                          levels = 1:7,
                          labels = c(
                             "<14",
                             "<14",
                             "14",
                             "15",
                             "16",
                             "17",
                             ">=18"))
}

for (yr in yrbs_years) {
   yrbs[[yr]]$age <- yrbs[[yr]]$age + 11 # make age variable numeric with actual age
}

for (yr in yrbs_years) {
   yrbs[[yr]] <- subset(yrbs[[yr]], (age >= 14 & !is.na(age))) # drop those under 14 or with no age data
}

# * sex --------
for (yr in yrbs_years) {
   yrbs[[yr]]$sex <- factor(yrbs[[yr]]$sex,
                      levels = 1:2,
                      labels = c("Female", "Male"))
   
   yrbs[[yr]] <- subset(yrbs[[yr]], !is.na(sex)) # drop those with no sex reported
}

# * remove observations with 100% missingness ------
for (yr in yrbs_years) {
   yrbs[[yr]] <- yrbs[[yr]][,colSums(is.na(yrbs[[yr]]))<nrow(yrbs[[yr]])]
}

# * keep only variables needed for restriction process (based on missingness investigation) ----
yrbs[[15]] <- yrbs[[15]] %>% select(c("year",		
                                               "survyear",		
                                               "weight",		
                                               "stratum",		
                                               "PSU",		
                                               "record",		
                                               "age",	#	1
                                               "sex",	#	2
                                               "grade",	#	3
                                               # hispanic ID	4
                                               # race ID	5
                                               "stheight",	#	6
                                               "stweight",	#	7
                                               "qbikehelmet",	#	8
                                               "q8",	# seatbelt use	9
                                               "q9",	# riding in car with driver who drank	10
                                               "q10",	# drove after drinking	11
                                               "q11",	# text while driving	12
                                               "q12",	# carry weapon	13
                                               "q13",	# weapon at school	15
                                               "q15",	# unsafe at school	16
                                               "q16",	# threatened at school	17
                                               "q17",	# physical fight	18
                                               "q18",	# fight at school	20
                                               "q19",	# sexual coercion	21
                                               "q22",	# dating violence	22
                                               "q21",	# dating pressure for sex	23
                                               "q23",	# bullied at school	24
                                               "q24",	# cyberbullying	25
                                               "q25",	# depressive sx	26
                                               "q26",	# S.I.	27
                                               "q27",	# suicide plan	28
                                               "q28",	# suicide attempt	29
                                               "q29",	# treated for S.A.	30
                                               "q30",	# tried smoking	31
                                               "q32",	# smoking days	33
                                               "q33",	# number of cigs per smoking day	34
                                               "q38",	# how many days smoked cigars	38
                                               "q34",	# ever vaped	39
                                               "q35",	# vaping days	40
                                               "q40",	# age at first alc drink	42
                                               "q41",	# drinking days	43
                                               "q43",	# number of drinks, max	45
                                               "q44",	# how get alc	46
                                               "q45",	# N times using cannabis	47
                                               "q46",	# age at first cannabis use	48
                                               "q47",	# cannabis days	49
                                               "q50",	# cocaine	50
                                               "q51",	# inhalants	51
                                               "q52",	# opioids	52
                                               "q53",	# meth	53
                                               "q54",	# MDMA	54
                                               "q48",	# synthetic cannabis	55
                                               "q55",	# steroids	56
                                               "q56",	# injection	58
                                               "q57",	# drugs sold school	59
                                               "q58",	# ever sex	60
                                               "q59",	# age at sexual debut	61
                                               "q60",	# N sex partners ever	62
                                               "q61",	# N SPs p3 mo	63
                                               "q62",	# drugs or drink before sex	64
                                               "q63",	# condom last sex	65
                                               "q64",	# contraception	66
                                               "q65",	# sex of sex partners	67
                                               "q66",	# ************* S.O.	68
                                               "sexid",
                                               "q67",	# weight self-describe	69
                                               "q68",	# weight change desire	70
                                               "q69",	# juice	71
                                               "q70",	# fruit	72
                                               "q71",	# salad	73
                                               "q72",	# potatoes	74
                                               "q73",	# carrots	75
                                               "q74",	# other veg	76
                                               "q75",	# soda	77
                                               "q76",	# milk	78
                                               "q77",	# breakfast	79
                                               "q78",	# physically active	80
                                               "q79",	# TV on school days	81
                                               "q80",	# video games on school days	82
                                               "q81",	# PE classes	83
                                               "q82",	# sports teams	84
                                               "q84",	# HIV test ever	85
                                               "q86",	# dentist	86
                                               "q87",	# asthma	87
                                               "q88",	# hours of sleep	88
                                               "q89"	# grades	89
))

yrbs[[17]] <- yrbs[[17]] %>% select(c("year",		
                                               "survyear",		
                                               "weight",		
                                               "stratum",		
                                               "PSU",		
                                               "record",	
                                               "age",	# 	1
                                               "sex",	#	2
                                               "grade",	#	3
                                               #hispanic/latino	4
                                               #race	5
                                               "stheight",	#	6
                                               "stweight",	#	7
                                               "q8",	# seatbelt use	8
                                               "q9",	# riding in car with driver who drank	9
                                               "q10",	# drove after drinking	10
                                               "q11",	# text while driving	11
                                               "q12",	# carry weapon	12
                                               "q13",	# weapon at school	13
                                               "q14",	# gun at school	14
                                               "q15",	# unsafe at school	15
                                               "q16",	# threatened or injured at school	16
                                               "q17",	# physical fight	17
                                               "q18",	# fight at school	18
                                               "q19",	# sexual coercion ever	19
                                               "q20",	# sexual coercion last year	20
                                               "q21",	# dating sexual coercion	21
                                               "q22",	# dating violence	22
                                               "q23",	# builled on school property	23
                                               "q24",	# e-bullied	24
                                               "q25",	# depression screen	25
                                               "q26",	# considered suicide	26
                                               "q27",	# suicide plan	27
                                               "q28",	# suicide attempt	28
                                               "q29",	# suicide treatment 	29
                                               "q30",	# ever tried smoking	30
                                               "q31",	# age tried smoking	31
                                               "q32",	# number of days smoking	32
                                               "q33",	# number of cigs per day	33
                                               "q34",	# vaping ever	34
                                               "q35",	# vaping last month	35
                                               "q36",	# how get vapes	36
                                               "q37",	# tobacco products	37
                                               "q38",	# cigars	38
                                               "q39",	# try quit tobacco	39
                                               # 	40
                                               "q40",	# how old first alcohol	41
                                               "q41",	# how many drinking days last month	42
                                               "q44",	# how get alc last month	43
                                               "q42",	# binge drinking	44
                                               "q43",	# most drinks in a row	45
                                               "q45",	# how many times used cannabid	46
                                               "q46",	# age first cannabis	47
                                               "q47",	# cannabis, last month	48
                                               "q50",	# cocaine	49
                                               "q51",	# inhalants	50
                                               "q52",	# heroin	51
                                               "q53",	# meth	52
                                               "q54",	# mdma	53
                                               "q48",	# synthetic cannabis"	54
                                               "q55",	# steroids	55
                                               "q49",	# rx pain meds	56
                                               "q56",	# injection drugs	57
                                               "q57",	# bought drugs at school	58
                                               "q58",	# ever had sex	59
                                               "q59",	# age at sexual debuts	60
                                               "q60",	# how many sex partners ever	61
                                               "q61",	# how many sex partners last 3 months	62
                                               "q62",	# alc or drugs before last sex	63
                                               "q63",	# used condom?	64
                                               "q64",	# one form of contraception	65
                                               "q65",	# sex of sex partners	66
                                               "q66",	# ********** S.O.	67
                                               "sexid",
                                               "q67",	# over/under weight	68
                                               "q68",	# lose/gain weight	69
                                               "q69",	# drank juice	70
                                               "q70",	# ate fruit	71
                                               "q71",	# ate salad	72
                                               "q72",	# ate potatoes	73
                                               "q73",	# carrots	74
                                               "q74",	# other veggies	75
                                               "q75",	# soda	76
                                               "q76",	# milk	77
                                               "q77",	# breakfast	78
                                               "q78",	# physically active	79
                                               "q79",	# TV hours school day	80
                                               "q80",	# video games school day	81
                                               "q81",	# phys ed	82
                                               "q82",	# sports teams	83
                                               "q83",	# concussion	84
                                               "q84",	# HIV test	85
                                               "q86",	# dentist	86
                                               "q87",	# asthma	87
                                               "q88",	# hours of sleep	88
                                               "q89"	# grades	89
))

# Note: in the absence of a data dictionary with answer breakdowns, we assume that 2019 questions were asked in numeric order.
yrbs[[19]] <- yrbs[[19]] %>% select(-(starts_with("qn")))
yrbs[[19]] <- yrbs[[19]][,1:105]
yrbs[[19]] <- yrbs[[19]] %>% select(c("year",
                                                "survyear",
                                                "weight",
                                                "stratum",
                                                "PSU",
                                                "record",
                                                "age",
                                                "sex",
                                                "grade",
                                                "q8",
                                                "q9",
                                                "q10",
                                                "q11",
                                                "q12",
                                                "q13",
                                                "q14",
                                                "q15",
                                                "q16",
                                                "q17",
                                                "q18",
                                                "q19",
                                                "q20",
                                                "q21",
                                                "q22",
                                                "q23",
                                                "q24",
                                                "q25",
                                                "q26",
                                                "q27",
                                                "q28",
                                                "q29",
                                                "q30",
                                                "q31",
                                                "q32",
                                                "q33",
                                                "q34",
                                                "q35",
                                                "q36",
                                                "q37",
                                                "q38",
                                                "q39",
                                                "q40",
                                                "q41",
                                                "q42",
                                                "q43",
                                                "q44",
                                                "q45",
                                                "q46",
                                                "q47",
                                                "q48",
                                                "q49",
                                                "q50",
                                                "q51",
                                                "q52",
                                                "q53",
                                                "q54",
                                                "q55",
                                                "q56",
                                                "q57",
                                                "q58",
                                                "q59",
                                                "q60",
                                                "q61",
                                                "q62",
                                                "q63",
                                                "q64",
                                                "q65",
                                                "q66",
                                                "sexid",
                                                "q67",
                                                "q68",
                                                "q69",
                                                "q70",
                                                "q71",
                                                "q72",
                                                "q73",
                                                "q74",
                                                "q75",
                                                "q76",
                                                "q77",
                                                "q78",
                                                "q79",
                                                "q80",
                                                "q81",
                                                "q82",
                                                "q83",
                                                "q84",
                                                "q85",
                                                "q86",
                                                "q87",
                                                "q88",
                                                "q89"))



# * remove those who apparently stopped the survey (based on missingness investigation
for (year in yrbs_years) {
   yrbs[[year]]$drop1 <- ifelse((is.na(yrbs[[year]]$q61) &
                                   is.na(yrbs[[year]]$q62) &
                                   is.na(yrbs[[year]]$q63) &
                                   is.na(yrbs[[year]]$q64) &
                                   is.na(yrbs[[year]]$q65) &
                                   is.na(yrbs[[year]]$q66) &        
                                   is.na(yrbs[[year]]$q67) &        
                                   is.na(yrbs[[year]]$q68) &         
                                   is.na(yrbs[[year]]$q69) &        
                                   is.na(yrbs[[year]]$q70) &        
                                   is.na(yrbs[[year]]$q71) &       
                                   is.na(yrbs[[year]]$q72) &      
                                   is.na(yrbs[[year]]$q73) &     
                                   is.na(yrbs[[year]]$q74) &    
                                   is.na(yrbs[[year]]$q75) &  
                                   is.na(yrbs[[year]]$q76) &  
                                   is.na(yrbs[[year]]$q77) & 
                                   is.na(yrbs[[year]]$q78) &
                                   is.na(yrbs[[year]]$q79) &
                                   is.na(yrbs[[year]]$q80) &
                                   is.na(yrbs[[year]]$q81) &
                                   is.na(yrbs[[year]]$q82) &
                                   is.na(yrbs[[year]]$q84) &
                                   is.na(yrbs[[year]]$q86) &
                                   is.na(yrbs[[year]]$q87) &
                                   is.na(yrbs[[year]]$q88) &         
                                   is.na(yrbs[[year]]$q89)),1,0)
}

for (year in yrbs_years) {
   yrbs[[year]] <- subset(yrbs[[year]], drop1 != 1)
}


# * remove those who systematically were not asked sex questions
for (year in yrbs_years) {
   yrbs[[year]]$drop2 <- ifelse((is.na(yrbs[[year]]$q58) &
                                    is.na(yrbs[[year]]$q59) &
                                    is.na(yrbs[[year]]$q60) &
                                    is.na(yrbs[[year]]$q61) &
                                    is.na(yrbs[[year]]$q62) &
                                    is.na(yrbs[[year]]$q63) &
                                    is.na(yrbs[[year]]$q64) &
                                    is.na(yrbs[[year]]$q65) &
                                    is.na(yrbs[[year]]$q66)),1,0)
}

for (year in yrbs_years) {
   yrbs[[year]] <- subset(yrbs[[year]], drop2 != 1)
}

# * so variable ------
for (year in yrbs_years) {
   yrbs[[year]]$sexid[is.na(yrbs[[year]]$sexid)] <- 5
   
   yrbs[[year]]$so <- factor(yrbs[[year]]$sexid,
                             levels = c(1:5),
                             labels = c(
                                "1_straight",
                                "2_lesgay",
                                "3_bi",
                                "4_dkns",
                                "5_pnta"
                                )
                             )
   yrbs[[year]]$so_new <- factor(yrbs[[year]]$sexid,
                                 levels = c(1:5),
                                 labels = c(
                                    "1_straight",
                                    "2_lesgay",
                                    "3_bi",
                                    "4_dko",
                                    "5_ref"
                                 )
   )
   
}

# * ever_sex ------
for (year in yrbs_years) {
   yrbs[[year]]$ever_sex <- 2-yrbs[[year]]$q58

   # * apply informative missingness to `ever_sex'
   yrbs[[year]]$ever_sex[yrbs[[year]]$q58 == 2 | #"Never had sex"
                          yrbs[[year]]$q59 == 1 | #"Never had sex"
                          yrbs[[year]]$q60 == 1 | #"Never had sex"
                          yrbs[[year]]$q61 == 1 | #"Never had sex"
                          yrbs[[year]]$q62 == 1 | #"Never had sex"
                          yrbs[[year]]$q63 == 1 | #"Never had sex"
                          yrbs[[year]]$q64 == 1 | #"Never had sex"
                          yrbs[[year]]$q65 == 1  #"Never had sex"
   ] <- 0 #any time R indicates never having had sex in a later question, ] 
   
   yrbs[[year]]$ever_sex[yrbs[[year]]$q58 != 2 & !is.na(yrbs[[year]]$q58)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q59 != 1 & !is.na(yrbs[[year]]$q59)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q60 != 1 & !is.na(yrbs[[year]]$q60)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q61 != 1 & !is.na(yrbs[[year]]$q61)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q62 != 1 & !is.na(yrbs[[year]]$q62)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q63 != 1 & !is.na(yrbs[[year]]$q63)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q64 != 1 & !is.na(yrbs[[year]]$q64)] <- 1
   yrbs[[year]]$ever_sex[yrbs[[year]]$q65 != 1 & !is.na(yrbs[[year]]$q65)] <- 1
   
}
   
# * sex_of_sps -----
for (year in yrbs_years) {
   yrbs[[year]]$sex_of_sps <- factor(yrbs[[year]]$q65,
                                     levels = c(1:4),
                                     labels = c(
                                        "1_never",
                                        "2_female",
                                        "3_male",
                                        "4_fem+mal"
                                     ))
   
   # * apply informative missingness to `sex_of_sps'
   yrbs[[year]]$sex_of_sps[yrbs[[year]]$q58 == 2 | #"Never had sex"
                            yrbs[[year]]$q59 == 1 | #"Never had sex"
                            yrbs[[year]]$q60 == 1 | #"Never had sex"
                            yrbs[[year]]$q61 == 1 | #"Never had sex"
                            yrbs[[year]]$q62 == 1 | #"Never had sex"
                            yrbs[[year]]$q63 == 1 | #"Never had sex"
                            yrbs[[year]]$q64 == 1 | #"Never had sex"
                            yrbs[[year]]$ever_sex == 0
   ] <- "1_never" #any time R indicates never having had sex in a later question, ]

}

### REMOVE observations with NA for `sex_of_sps` or `ever_sex` --------
for (year in yrbs_years) {
   yrbs[[year]] <- subset(yrbs[[year]], !is.na(sex_of_sps))
   yrbs[[year]] <- subset(yrbs[[year]], !is.na(ever_sex))
}

# * cohort --------
for (year in yrbs_years) {
   yrbs[[year]]$cohort <- yrbs[[year]]$year - yrbs[[year]]$age
}

# * so_21 --------
for (year in yrbs_years) {
   yrbs[[year]]$so_21 <- NA
}

# * source ----
for (year in yrbs_years) {
   yrbs[[year]]$source <- "YRBS"
}

# * restrict to on variables needed for analysis -----
varnames <- c("year", "weight", "PSU", "stratum", "age", "sex", "so", "so_new",
              "ever_sex", "sex_of_sps", "cohort", "so_21", "source")

for (year in yrbs_years) {
   yrbs[[year]] <- yrbs[[year]][,varnames]
}

# * generate a YRBS 2015:2021 dataset to merge with 2021 ------
yrbs1519_merge <- rbind(yrbs[[15]],
                  yrbs[[17]],
                  yrbs[[19]])

# Write .rds file ------
write_rds(yrbs1519_merge,
          "data - raw/yrbs1519_tomerge.RDS")
