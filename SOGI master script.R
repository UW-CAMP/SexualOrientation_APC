### This code runs the entire YRBS/BRFSS sexual orientation, gender identity, and sex-of-sex-partners analysis (CAMP 60).
# Goodreau SM, Barry MP
# University of Washington

# Abbreviations:
# SO: sexual orientation
# GI: gender identity
# SSP: sex(es) of sex partners

# SAS analyses, address in the corresponding ReadMe file must precede this program steps
rm(list=ls())
set.seed(1)

# Prepare project files
source("SOGI_00_packages.R") # call in packages; you may have to open this file and install new packages 
source("SOGI_01_generate_BRFSS_index_variable.R") # !! SLOW !! creates manageable versions of BRFSS data (.rds rather than .XPT) - only run once to generate these files
source("SOGI_02_prepare_BRFSS.R") # run this file to prepare a single file `brfss_final.rds`
source("SOGI_03_prepare_YRBS_2021.R") 
source("SOGI_04_prepare_YRBS_2015-2019.R") 
source("SOGI_05_merge_YRBS.R") # run this file to prepare a single file `yrbs_final.rds`
source("SOGI_06_combine_YRBS_BRFSS.R") # run this file to prepare a single file `combined.rds`, which contains both BRFSS and YRBS datasets

# SO Analyses
source("SOGI_11_SO_prepare.R") # prepare objects for SO analyses
source("SOGI_12_SO_plots.R") # SO-specific analyses
source("SOGI_13_SO_compare_18yo.R") # visual and statistical comparisons between 18YO YRBS and BRFSS respondents

source("SOGI_14_SO_design_based.R") # Create design-based estimators of prev
source("SOGI_14_SO_Q1.R") # Fit A, P, C, AP, AC, PC models
source("SOGI_14_SO_Q2_mAPC.R") # Fit multivariate APC models & plot curvatures
source("SOGI_14_SO_Q3.R") # Carry out chi-sq tests across period
source("SOGI_14_SO_Q4.R") # Carry out pairwise t-tests across cohorts
source("SOGI_14_SO_Q5.R") # fit A/P/C/AP/AC/PC models for dko/ref & descriptives for yrbs 2021

# GI Analyses
source("SOGI_21_GI_prepare.R") # prepare objects for GI analyses
source("SOGI_22_GI_analyses.R") # GI analyses

# SSP Analyses
source("SOGI_31_SSP_prepare") # prepare objects for SSP analyses
source("SOGI_32_SSP_analyses") # SSP analyses 

