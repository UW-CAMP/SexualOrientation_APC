### Prepare for BRFSS & YRBS SO Analyses

# This script's code uses the combined BRFSS and YRBS dataset, previously prepared in script "SOGI_03_Prepare Combined Dataset"
### and prepares objects for the sexual orientation analyses in script "SOGI_05_SO Analyze"

### Prepare workspace ----- 
# clear environment
rm(list = ls())

# packages
source("SOGI_00_packages.R")

# define functions
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

# call in data
combined <- readRDS("data - clean/combined.rds")

### Generate analytic objects -----
# * Multinomial SO arrays --------
# prepare objects for proportion arrays
so_values <- levels(as.factor(combined$so_new))

comb_cohort_n <- length(min(combined$cohort):max(combined$cohort))
comb_period_n <- length(min(combined$year):max(combined$year))
comb_cohort_v <- min(combined$cohort):max(combined$cohort) 
comb_period_v <- min(combined$year):max(combined$year)

comb_cohort <- list()
for (yr in comb_cohort_v) {
   comb_cohort[[yr]] <- subset(combined, cohort == yr)  # if no rows, nothing to add
}

# create proportion array objects (A = all, F = females, M = males)
so_props_A <- so_props_F <- so_props_M <- array(NA, dim=c(comb_cohort_n, comb_period_n, length(unique(combined$so_new))))

# fill the empty arrays with population-level estimates
for(comb_cohort_xi in 1:comb_cohort_n) {
   comb_cohort_vi <- comb_cohort_v[comb_cohort_xi]
   comb_period_x <- which(comb_period_v %in% comb_cohort[[comb_cohort_vi]]$year)
   for (comb_period_xi in comb_period_x) {
      temp1 <- comb_cohort[[comb_cohort_vi]] %>% filter(year == comb_period_v[comb_period_xi]) 
      temp2 <- round(prop.table(questionr::wtd.table(x=temp1$age,
                                          y=temp1$so_new,
                                          weights=temp1$weight,
                                          digits=1),
                                margin=1)*100, 3)
      so_props_A[comb_cohort_xi, comb_period_xi,] <- temp2
      
      temp3 <- temp1 %>% filter(sex == "Female")
      temp4 <- round(prop.table(questionr::wtd.table(x=temp3$age,
                                          y=factor(temp3$so_new, levels = so_values),
                                          weights=temp3$weight,
                                          digits=1),
                                margin=1)*100, 3)
      so_props_F[comb_cohort_xi, comb_period_xi,] <- temp4

      temp5 <- temp1 %>% filter(sex == "Male")
      temp6 <- round(prop.table(questionr::wtd.table(x=temp5$age,
                                          y=factor(temp5$so_new, levels = so_values),
                                          weights=temp5$weight,
                                          digits=1),
                                margin=1)*100, 3)
      so_props_M[comb_cohort_xi, comb_period_xi,] <- temp6
   }
}

### Save analytic objects for analysis as .rds files --------
write_rds(so_props_A, "data - clean/so_props_A.rds")
write_rds(so_props_F, "data - clean/so_props_F.rds")
write_rds(so_props_M, "data - clean/so_props_M.rds")
