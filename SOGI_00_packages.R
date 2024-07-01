##### This script contains all packages used by SOGI-SSP Analyses -----
library(visdat) #used to visualize missing data patterns in script 01
library(tidyverse) #used for data management
library(haven) #used for data management
library(dplyr) #used for data management
library(tidyr) #used for data management
library(tidyselect) #used for data management
# library(ggplot2)
# library(trend)
library(DescTools) # for obtaining 95% CI for multinomial proportions (comparing 18 YO in BRFSS, YRBS)
# library(msm)
# library(EnvStats)
library(magrittr) # used for piping
# library(exact2x2)
library(questionr) # used for applying weights to tables in BRFSS and YRBS
# library(VIM)
# library(lme4)
# library(mice)
# library(geepack)
# library(broom.mixed)
library(colorspace) # used for colors in graphs
library(dichromat) # used for colors in graphs
# library(GDAtools) 
library(labelled) # used for data management
library(weights) # for using chi squared test with weighted data 
library(survey) # for weighted prev estimates and 95% CI (comparing 18 YO)
library(ggplot2)
