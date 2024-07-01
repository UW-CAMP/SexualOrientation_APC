## Fit Multivariate APC 
## Prevalence by age x period (survey) x cohort
## Jessica Godwin

# Setup ####
rm(list = ls())

if(!dir.exists("tables/SO_Q2")){
  if(!dir.exists("tables/")){
    dir.create("tables/")
  }
  dir.create("tables/SO_Q2")
}

if(!dir.exists("plots/SO_Q2")){
  if(!dir.exists("plots/")){
    dir.create("plots/")
  }
  dir.create("plots/SO_Q2")
}

# ## Libraries ####
# install.packages("INLA",
#                  repos=c(getOption("repos"),
#                          INLA="https://inla.r-inla-download.org/R/stable"))
library(INLA) 
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

prev_data <- read.csv("data - clean/BRFSS_HT_prevs.csv")
prev_data_yrbs <- read.csv("data - clean/YRBS_HT_prevs.csv")

str(prev_data)
summary(prev_data)
head(prev_data)


# Prep for estimation ####

mod_data <- prev_data %>% 
  bind_rows(prev_data_yrbs) %>% 
  arrange(age, period, cohort) %>% 
  mutate(period_fac = factor(period, levels = 2014:2021),
         age_fac = factor(age, levels = 14:79),
         cohort_fac = factor(cohort, levels = 1935:2007),
         sex_fac = factor(sex),
         period_idx = as.numeric(period_fac),
         age_idx = as.numeric(age_fac),
         cohort_idx = as.numeric(cohort_fac),
         sex_idx = as.numeric(sex_fac),
         prec_logit_pijk = 1/var_logit_pijk)

coh_summary <- mod_data %>% 
  group_by(cohort) %>% tally()
age_summary <- mod_data %>% 
  group_by(age) %>% tally()
per_summary <- mod_data %>% 
  group_by(period) %>% tally()

png(filename = "plots/SO_Q2/Samplesize_bi_apc.png")
par(mfrow = c(1,3))
hist(mod_data$cohort[mod_data$so == "bi"], xlab = "", main = "",
     breaks = (min(mod_data$cohort)-1):(max(mod_data$cohort) + 1))
hist(mod_data$age[mod_data$so == "bi"], xlab = "", main = "",
     breaks = (min(mod_data$age)-1):(max(mod_data$age) + 1))
hist(mod_data$period[mod_data$so == "bi"], xlab = "", main = "",
     breaks = (min(mod_data$period)-1):(max(mod_data$period) + 1))
dev.off()

png(filename = "plots/SO_Q2/Samplesize_lg_apc.png")
par(mfrow = c(1,3))
hist(mod_data$cohort[mod_data$so == "lesgay"], xlab = "", main = "",
     breaks = (min(mod_data$cohort)-1):(max(mod_data$cohort) + 1))
hist(mod_data$age[mod_data$so == "lesgay"], xlab = "", main = "",
     breaks = (min(mod_data$age)-1):(max(mod_data$age) + 1))
hist(mod_data$period[mod_data$so == "lesgay"], xlab = "", main = "",
     breaks = (min(mod_data$period)-1):(max(mod_data$period) + 1))
dev.off()

# Fit models ####

## Define formula ####
# logit_pijks =μ_strata +θ_age +φ_period,strata +ψ_cohort,strata.

pc_prec <- list(theta = list(prior = 'pc.prec', param=c(1, .5)))
mod_formula_shareco <- logit_pijk ~ (-1) + sex + 
  f(age_idx, model = "rw2", replicate = sex_idx,
    scale.model = TRUE, constr = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(period_idx, model = "rw2", replicate = sex_idx,
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(cohort_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec)

mod_formula_shareper <- logit_pijk ~ (-1) + sex + 
  f(age_idx, model = "rw2", replicate = sex_idx,
    scale.model = TRUE, constr = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(period_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(cohort_idx, model = "rw2", replicate = sex_idx,
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec)

mod_formula_shareage <- logit_pijk ~ (-1) + sex + 
  f(age_idx, model = "rw2", 
    scale.model = TRUE, constr = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(period_idx, model = "rw2", replicate = sex_idx,
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(cohort_idx, model = "rw2", replicate = sex_idx,
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec)


## Lincombs ####

### Shared Cohort ####

lincombs_info_age_shareco <-
  data.frame(Index = 1:(max(mod_data$age_idx)*2),
             Sex = rep(c(1,2), each = max(mod_data$age_idx)),
             Term = rep(1:max(mod_data$age_idx), 2),
             TermName = "Age",
             SharedName = "Cohort",
             LC_Index = NA)
row_idx <- 0
for(row in 1:(nrow(lincombs_info_age_shareco))){
  if(lincombs_info_age_shareco$Term[row] %in%
     2:(max(lincombs_info_age_shareco$Term) -1)){
    row_idx <- row_idx + 1
    lincombs_info_age_shareco$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    age <- rep(NA, max(mod_data$age_idx)*2)
    age_element <- lincombs_info_age_shareco$Index[row]
    age[age_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(age_idx = age))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareco <- get(object.name)
      names(lincombs_shareco)[row_idx] <- object.name
    }else{
      lincombs_shareco <- c(lincombs_shareco, get(object.name))
      names(lincombs_shareco)[row_idx] <- object.name
    }
    
    rm(object.name)
  }
}

lincombs_info_per_shareco <-
  data.frame(Index = 1:(max(mod_data$period_idx)*2),
             Sex = rep(c(1,2), each = max(mod_data$period_idx)),
             Term = rep(1:max(mod_data$period_idx), 2),
             TermName = "Period",
             SharedName = "Cohort",
             LC_Index = NA)

for(row in 1:(nrow(lincombs_info_per_shareco))){
  if(lincombs_info_per_shareco$Term[row] %in%
     2:(max(lincombs_info_per_shareco$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_per_shareco$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    period <- rep(NA, max(mod_data$period_idx)*2)
    period_element <- lincombs_info_per_shareco$Index[row]
    period[period_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(period_idx = period))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareco <- get(object.name)
      names(lincombs_shareco)[row_idx] <- object.name
    }else{
      lincombs_shareco <- c(lincombs_shareco, get(object.name))
      names(lincombs_shareco)[row_idx] <- object.name
    }
    
    rm(object.name)
  }
}

lincombs_info_coh_shareco <-
  data.frame(Index = 1:max(mod_data$cohort_idx),
             Sex = rep(NA, each = max(mod_data$cohort_idx)),
             Term = rep(1:max(mod_data$cohort_idx), 1),
             TermName = "Cohort",
             SharedName = "Cohort",
             LC_Index = NA)

for(row in 1:(nrow(lincombs_info_coh_shareco))){
  if(lincombs_info_coh_shareco$Term[row] %in% 
     2:(max(lincombs_info_coh_shareco$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_coh_shareco$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    cohort <- rep(NA, max(mod_data$cohort_idx)*1)
    cohort_element <- lincombs_info_coh_shareco$Index[row]
    cohort[cohort_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(cohort_idx = cohort))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareco <- get(object.name)
      names(lincombs_shareco)[row_idx] <- object.name
    }else{
      lincombs_shareco <- c(lincombs_shareco, get(object.name))
      names(lincombs_shareco)[row_idx] <- object.name
    }
    
    rm(object.name)
  }
}

lincombs_info_shareco <- bind_rows(lincombs_info_age_shareco,
                                   lincombs_info_per_shareco,
                                   lincombs_info_coh_shareco)

### Shared Age ####
lincombs_info_coh_shareage <-
  data.frame(Index = 1:(max(mod_data$cohort_idx)*2),
             Sex = rep(c(1,2), each = max(mod_data$cohort_idx)),
             Term = rep(1:max(mod_data$cohort_idx), 2),
             TermName = "Cohort",
             SharedName = "Age",
             LC_Index = NA)

row_idx <- 0
for(row in 1:(nrow(lincombs_info_coh_shareage))){
  if(lincombs_info_coh_shareage$Term[row] %in%
     2:(max(lincombs_info_coh_shareage$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_coh_shareage$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    cohort <- rep(NA, max(mod_data$cohort_idx)*2)
    coh_element <- lincombs_info_coh_shareage$Index[row]
    cohort[coh_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(cohort_idx = cohort))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareage <- get(object.name)
      names(lincombs_shareage)[row_idx] <- object.name
    }else{
      lincombs_shareage <- c(lincombs_shareage, get(object.name))
      names(lincombs_shareage)[row_idx] <- object.name
    }
  }
}  
  
lincombs_info_per_shareage <-
  data.frame(Index = 1:(max(mod_data$period_idx)*2),
             Sex = rep(c(1,2), each = max(mod_data$period_idx)),
             Term = rep(1:max(mod_data$period_idx), 2),
             TermName = "Period",
             SharedName = "Age",
             LC_Index = NA)

for(row in 1:(nrow(lincombs_info_per_shareage))){
  if(lincombs_info_per_shareage$Term[row] %in%
     2:(max(lincombs_info_per_shareage$Term) -1)){
    row_idx <- row_idx + 1
    lincombs_info_per_shareage$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    period <- rep(NA, max(mod_data$period_idx)*2)
    per_element <- lincombs_info_per_shareage$Index[row]
    period[per_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(period_idx = period))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareage <- get(object.name)
      names(lincombs_shareage)[row_idx] <- object.name
    }else{
      lincombs_shareage <- c(lincombs_shareage, get(object.name))
      names(lincombs_shareage)[row_idx] <- object.name
    }
  }
}  


lincombs_info_age_shareage <-
  data.frame(Index = 1:(max(mod_data$age_idx)*1),
             Sex = rep(NA, each = max(mod_data$age_idx)),
             Term = rep(1:max(mod_data$age_idx), 1),
             TermName = "Age",
             SharedName = "Age",
             LC_Index = NA)

for(row in 1:(nrow(lincombs_info_age_shareage))){
  if(lincombs_info_age_shareage$Term[row] %in%
     2:(max(lincombs_info_age_shareage$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_age_shareage$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    age <- rep(NA, max(mod_data$age_idx)*1)
    age_element <- lincombs_info_age_shareage$Index[row]
    age[age_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(age_idx = age))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareage <- get(object.name)
      names(lincombs_shareage)[row_idx] <- object.name
    }else{
      lincombs_shareage <- c(lincombs_shareage, get(object.name))
      names(lincombs_shareage)[row_idx] <- object.name
    }
  }
}  

lincombs_info_shareage <- bind_rows(lincombs_info_age_shareage,
                                    lincombs_info_per_shareage,
                                    lincombs_info_coh_shareage)

### Shared Period ####
lincombs_info_coh_shareper<-
  data.frame(Index = 1:(max(mod_data$cohort_idx)*2),
             Sex = rep(c(1,2), each = max(mod_data$cohort_idx)),
             Term = rep(1:max(mod_data$cohort_idx), 2),
             TermName = "Cohort",
             SharedName = "Period",
             LC_Index = NA)

row_idx <- 0
for(row in 1:(nrow(lincombs_info_coh_shareper))){
  if(lincombs_info_coh_shareper$Term[row] %in%
     2:(max(lincombs_info_coh_shareper$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_coh_shareper$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    cohort <- rep(NA, max(mod_data$cohort_idx)*2)
    coh_element <- lincombs_info_coh_shareper$Index[row]
    cohort[coh_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(cohort_idx = cohort))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareper <- get(object.name)
      names(lincombs_shareper)[row_idx] <- object.name
    }else{
      lincombs_shareper <- c(lincombs_shareper, get(object.name))
      names(lincombs_shareper)[row_idx] <- object.name
    }
  }
}  

lincombs_info_age_shareper <-
  data.frame(Index = 1:(max(mod_data$age_idx)*2),
             Sex = rep(c(1,2), each = max(mod_data$age_idx)),
             Term = rep(1:max(mod_data$age_idx), 2),
             TermName = "Age",
             SharedName = "Period",
             LC_Index = NA)

for(row in 1:(nrow(lincombs_info_age_shareper))){
  if(lincombs_info_age_shareper$Term[row] %in%
     2:(max(lincombs_info_age_shareper$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_age_shareper$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    age <- rep(NA, max(mod_data$age_idx)*2)
    age_element <- lincombs_info_age_shareper$Index[row]
    age[age_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(age_idx = age))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareper <- get(object.name)
      names(lincombs_shareper)[row_idx] <- object.name
    }else{
      lincombs_shareper <- c(lincombs_shareper, get(object.name))
      names(lincombs_shareper)[row_idx] <- object.name
    }
  }
}  


lincombs_info_per_shareper <-
  data.frame(Index = 1:(max(mod_data$period_idx)*1),
             Sex = rep(NA, each = max(mod_data$period_idx)),
             Term = rep(1:max(mod_data$period_idx), 1),
             TermName = "Period",
             SharedName = "Period",
             LC_Index = NA)

for(row in 1:(nrow(lincombs_info_per_shareper))){
  if(lincombs_info_per_shareper$Term[row] %in%
     2:(max(lincombs_info_per_shareper$Term) - 1)){
    row_idx <- row_idx + 1
    lincombs_info_per_shareper$LC_Index[row] <- row_idx
    ## Put linear combination weights 
    ## in for N/As
    period <- rep(NA, max(mod_data$period_idx)*1)
    per_element <- lincombs_info_per_shareper$Index[row]
    period[per_element + c(-1, 0, 1)] <- c(1, -2, 1)
    
    ## Make lincomb in INLA and store
    object.name <- paste("lc", row_idx, sep = "")
    assign(object.name, inla.make.lincomb(period_idx = period))
    
    ## Add to lincombs list
    if(row_idx == 1){
      lincombs_shareper <- get(object.name)
      names(lincombs_shareper)[row_idx] <- object.name
    }else{
      lincombs_shareper <- c(lincombs_shareper, get(object.name))
      names(lincombs_shareper)[row_idx] <- object.name
    }
  }
}  


lincombs_info_shareper <- bind_rows(lincombs_info_age_shareper,
                                    lincombs_info_per_shareper,
                                    lincombs_info_coh_shareper)

## Bisexual ####

### Shared: Age ####
mod_bi_shareage <- inla(mod_formula_shareage,
                       data = mod_data %>% filter(so == "bi"), 
                       family = "gaussian",
                       control.family = list(hyper = list(
                         prec = list(initial = log(1),
                                     fixed=TRUE))), 
                       control.predictor = list(compute=TRUE),
                       control.compute = list(config = TRUE),
                       lincomb = lincombs_shareage,
                       scale = prec_logit_pijk,
                       verbose = TRUE)

age_info <- unique(mod_data[, c("age_idx", "age")])
per_info <- unique(mod_data[, c("period_idx", "period")])
coh_info <- unique(mod_data[, c("cohort_idx", "cohort")])

#### Plots ####
bi_age_shareage <- mod_bi_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age) %>% 
  ggplot(aes(x = Age, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Bisexual: Age Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_age_curvatures_shareage.png", bi_age_shareage)

bi_age_shareage_std <- bi_age_shareage + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_age_curvatures_shareage_std.png",
       bi_age_shareage_std)

bi_per_shareage <- mod_bi_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period) %>% 
  ggplot(aes(x = Period, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Bisexual: Period Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_per_curvatures_shareage.png", bi_per_shareage)

bi_per_shareage_std <- bi_per_shareage + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_per_curvatures_shareage_std.png", 
       bi_per_shareage_std)

bi_coh_shareage <- mod_bi_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort) %>% 
  ggplot(aes(x = Cohort, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  scale_x_reverse() +
  ggtitle("Bisexual: Cohort Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_coh_curvatures_shareage.png", bi_coh_shareage)

bi_coh_shareage_std <- bi_coh_shareage + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_coh_curvatures_shareage_std.png",
       bi_coh_shareage_std)

### Shared: Cohort ####

mod_bi_shareco <- inla(mod_formula_shareco,
                       data = mod_data %>% filter(so == "bi"), 
                       family = "gaussian",
                       control.family = list(hyper = list(
                         prec = list(initial = log(1),
                                     fixed=TRUE))), 
                       control.predictor = list(compute=TRUE),
                       control.compute = list(config = TRUE),
                       lincomb = lincombs_shareco,
                       scale = prec_logit_pijk,
                       verbose = TRUE)

#### Plots ####
bi_age_shareco <- mod_bi_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age) %>% 
  ggplot(aes(x = Age, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Bisexual: Age Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_age_curvatures_shareco.png", bi_age_shareco)

bi_age_shareco_std <- bi_age_shareco + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_age_curvatures_shareco_std.png",
       bi_age_shareco_std)

bi_per_shareco <- mod_bi_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period) %>% 
  ggplot(aes(x = Period, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Bisexual: Period Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_per_curvatures_shareco.png", bi_per_shareco)

bi_per_shareco_std <- bi_per_shareco + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_per_curvatures_shareco_std.png",
       bi_per_shareco_std)

bi_coh_shareco <- mod_bi_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort) %>% 
  ggplot(aes(x = Cohort, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  scale_x_reverse() +
  ggtitle("Bisexual: Cohort Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_coh_curvatures_shareco.png", bi_coh_shareco)

bi_coh_shareco_std <- bi_coh_shareco + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_coh_curvatures_shareco_std.png",
       bi_coh_shareco_std)

### Shared: Period ####
mod_bi_shareper <- inla(mod_formula_shareper,
                        data = mod_data %>% filter(so == "bi"), 
                        family = "gaussian",
                        control.family = list(hyper = list(
                          prec = list(initial = log(1),
                                      fixed=TRUE))), 
                        control.predictor = list(compute=TRUE),
                        control.compute = list(config = TRUE),
                        lincomb = lincombs_shareper,
                        scale = prec_logit_pijk,
                        verbose = TRUE)

#### Plots ####
bi_age_shareper<- mod_bi_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age) %>% 
  ggplot(aes(x = Age, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Bisexual: Age Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_age_curvatures_shareper.png", bi_age_shareper)

bi_age_shareper_std <- bi_age_shareper + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_age_curvatures_shareper_std.png",
       bi_age_shareper_std)

bi_per_shareper <- mod_bi_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period) %>% 
  ggplot(aes(x = Period, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Bisexual: Period Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_per_curvatures_shareper.png", bi_per_shareper)

bi_per_shareper_std <- bi_per_shareper + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_per_curvatures_shareper_std.png",
       bi_per_shareper_std)

bi_coh_shareper <- mod_bi_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort) %>% 
  ggplot(aes(x = Cohort, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  scale_x_reverse() +
  ggtitle("Bisexual: Cohort Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/Bi_coh_curvatures_shareper.png", bi_coh_shareper)

bi_coh_shareper_std <- bi_coh_shareper + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/Bi_coh_curvatures_shareper_std.png",
       bi_coh_shareper_std)

### Save Model Outputs ####
sink(file = "tables/SO_Q2/mod_bi_shareper.txt")
summary(mod_bi_shareper)
head(mod_bi_shareper$summary.random$age_idx)
head(mod_bi_shareper$summary.linear.predictor)
sink(file = NULL)

sink(file = "tables/SO_Q2/mod_bi_shareco.txt")
summary(mod_bi_shareco)
head(mod_bi_shareco$summary.random$age_idx)
head(mod_bi_shareco$summary.linear.predictor)
sink(file = NULL)
# 
# ### inla.posterior.sample() 
# bi_posterior_per <- inla.posterior.sample(n = 1000, mod_bi_shareper)
# bi_posterior_co <- inla.posterior.sample(n = 1000, mod_bi_shareco)
# 
# #### Get draws
# bi_posterior_per_draws <- lapply(bi_posterior_per, function(x){
#   x$latent
# })
# 
# bi_posterior_co_draws <- lapply(bi_posterior_co, function(x){
#   x$latent
# })
# 
# #### Reorganize
# bi_posterior_per_df <- do.call(cbind, bi_posterior_per_draws) %>% 
#   as.data.frame() %>% 
#   mutate(term = rownames(.)) %>% 
#   pivot_longer(cols = contains("V"),
#                names_to = "draw_no",
#                values_to = "draw",
#                names_prefix = "V")
# 
# bi_posterior_co_df <- do.call(cbind, bi_posterior_co_draws) %>% 
#   as.data.frame() %>% 
#   mutate(term = rownames(.)) %>% 
#   pivot_longer(cols = contains("V"),
#                names_to = "draw_no",
#                values_to = "draw",
#                names_prefix = "V")
# 
# 
# ## Get term specific draws
# bi_sex_per_draws <- bi_posterior_per_df %>% 
#   filter(grepl("sex", term)) %>% 
#   mutate(sex_idx = ifelse(grepl("sexFemale", term), "1", "2"))
# 
# bi_sex_co_draws <- bi_posterior_co_df %>% 
#   filter(grepl("sex", term)) %>% 
#   mutate(sex_idx = ifelse(grepl("sexFemale", term), "1", "2"))
# 
# 
# bi_age_per_draws <- bi_posterior_per_df %>% 
#   filter(grepl("age_idx", term)) %>% 
#   mutate(term_str = str_split_i(term, ":", 1),
#          term_idx = str_split_i(term, ":", 2),
#          sex_idx = ifelse(as.numeric(term_idx) <=
#                             max(mod_data$age_idx), "1", "2"))
# bi_age_co_draws <- bi_posterior_co_df %>% 
#   filter(grepl("age_idx", term)) %>% 
#   mutate(term_str = str_split_i(term, ":", 1),
#          term_idx = str_split_i(term, ":", 2),
#          sex_idx = ifelse(as.numeric(term_idx) <=
#                             max(mod_data$age_idx), "1", "2"))
# 
# bi_period_per_draws <- bi_posterior_per_df %>% 
#   filter(grepl("period_idx", term)) %>% 
#   mutate(term_str = str_split_i(term, ":", 1),
#          term_idx = str_split_i(term, ":", 2),
#          sex_idx = ifelse(as.numeric(term_idx) <=
#                             max(mod_data$period_idx), "1", "2"))
# bi_period_co_draws <- bi_posterior_co_df %>% 
#   filter(grepl("period_idx", term)) %>% 
#   mutate(term_str = str_split_i(term, ":", 1),
#          term_idx = str_split_i(term, ":", 2),
#          sex_idx = ifelse(as.numeric(term_idx) <=
#                             max(mod_data$period_idx), "1", "2"))
# 
# bi_cohort_per_draws <- bi_posterior_per_df %>% 
#   filter(grepl("cohort_idx", term)) %>% 
#   mutate(term_str = str_split_i(term, ":", 1),
#          term_idx = str_split_i(term, ":", 2),
#          sex_idx = ifelse(as.numeric(term_idx) <=
#                             max(mod_data$cohort_idx), "1", "2"))
# 
# 
# bi_cohort_co_draws <- bi_posterior_co_df %>% 
#   filter(grepl("cohort_idx", term)) %>% 
#   mutate(term_str = str_split_i(term, ":", 1),
#          term_idx = str_split_i(term, ":", 2),
#          sex_idx = ifelse(as.numeric(term_idx) <=
#                             max(mod_data$cohort_idx), "1", "2"))
# 
# 
# ## Get summarys
# 
# bi_age_per_summary <- bi_age_per_draws %>%
#   left_join(bi_sex_per_draws %>% 
#               select(-contains("term")),
#             by = c("sex_idx", "draw_no"),
#             suffix = c("_age", "_sex")) %>% 
#   mutate(draw = draw_sex + draw_age,
#          sex_idx = as.numeric(sex_idx),
#          term_idx = as.numeric(term_idx)) %>% 
#   group_by(term_str, term_idx, sex_idx) %>% 
#   dplyr::summarize(median = median(draw),
#             q025 = quantile(draw, .025),
#             q05 = quantile(draw, .05),
#             q10 = quantile(draw, .10),
#             q90 = quantile(draw, .90),
#             q95 = quantile(draw, .95),
#             q975 = quantile(draw, .975)) %>% 
#   ungroup()
# 
# 
# bi_age_co_summary <- bi_age_co_draws %>%
#   left_join(bi_sex_co_draws %>% 
#               select(-contains("term")),
#             by = c("sex_idx", "draw_no"),
#             suffix = c("_age", "_sex")) %>% 
#   mutate(draw = draw_sex + draw_age,
#          sex_idx = as.numeric(sex_idx),
#          term_idx = as.numeric(term_idx)) %>% 
#   group_by(term_str, term_idx, sex_idx) %>% 
#   dplyr::summarize(median = median(draw),
#             q025 = quantile(draw, .025),
#             q05 = quantile(draw, .05),
#             q10 = quantile(draw, .10),
#             q90 = quantile(draw, .90),
#             q95 = quantile(draw, .95),
#             q975 = quantile(draw, .975)) %>% 
#   ungroup()
# 
# bi_period_per_summary <- bi_period_per_draws %>%
#   left_join(bi_sex_per_draws %>% 
#               select(-contains("term")),
#             by = c("sex_idx", "draw_no"),
#             suffix = c("_period", "_sex")) %>% 
#   mutate(draw = draw_sex + draw_period,
#          sex_idx = as.numeric(sex_idx),
#          term_idx = as.numeric(term_idx)) %>% 
#   group_by(term_str, term_idx, sex_idx) %>% 
#   dplyr::summarize(median = median(draw),
#             q025 = quantile(draw, .025),
#             q05 = quantile(draw, .05),
#             q10 = quantile(draw, .10),
#             q90 = quantile(draw, .90),
#             q95 = quantile(draw, .95),
#             q975 = quantile(draw, .975)) %>% 
#   ungroup()
# 
# bi_period_co_summary <- bi_period_co_draws %>%
#   left_join(bi_sex_co_draws %>% 
#               select(-contains("term")),
#             by = c("sex_idx", "draw_no"),
#             suffix = c("_period", "_sex")) %>% 
#   mutate(draw = draw_sex + draw_period,
#          sex_idx = as.numeric(sex_idx),
#          term_idx = as.numeric(term_idx)) %>% 
#   group_by(term_str, term_idx, sex_idx) %>% 
#   dplyr::summarize(median = median(draw),
#             q025 = quantile(draw, .025),
#             q05 = quantile(draw, .05),
#             q10 = quantile(draw, .10),
#             q90 = quantile(draw, .90),
#             q95 = quantile(draw, .95),
#             q975 = quantile(draw, .975)) %>% 
#   ungroup()
# 
# 
# bi_cohort_per_summary <- bi_cohort_per_draws %>% 
#   left_join(bi_sex_per_draws %>% 
#               select(-contains("term")),
#             by = c("sex_idx", "draw_no"),
#             suffix = c("_cohort", "_sex")) %>% 
#   mutate(draw = draw_sex + draw_cohort,
#          sex_idx = as.numeric(sex_idx),
#          term_idx = as.numeric(term_idx)) %>% 
#   group_by(term_str, term_idx, sex_idx) %>% 
#   dplyr::summarize(median = median(draw),
#             q025 = quantile(draw, .025),
#             q05 = quantile(draw, .05),
#             q10 = quantile(draw, .10),
#             q90 = quantile(draw, .90),
#             q95 = quantile(draw, .95),
#             q975 = quantile(draw, .975)) %>% 
#   ungroup()
# 
# bi_cohort_co_summary <- bi_cohort_co_draws %>% 
#   left_join(bi_sex_co_draws %>% 
#               select(-contains("term")),
#             by = c("sex_idx", "draw_no"),
#             suffix = c("_cohort", "_sex")) %>% 
#   mutate(draw = draw_sex + draw_cohort,
#          sex_idx = as.numeric(sex_idx),
#          term_idx = as.numeric(term_idx)) %>% 
#   group_by(term_str, term_idx, sex_idx) %>% 
#   dplyr::summarize(median = median(draw),
#             q025 = quantile(draw, .025),
#             q05 = quantile(draw, .05),
#             q10 = quantile(draw, .10),
#             q90 = quantile(draw, .90),
#             q95 = quantile(draw, .95),
#             q975 = quantile(draw, .975)) %>% 
#   ungroup()
# 
# 
# 
# ### Plots 
# #### Age 
# 
# bi_plot_age_per <- bi_age_per_summary %>% 
#   mutate(age_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                             max(mod_data$age_idx))) %>% 
#   left_join(mod_data %>% 
#               select(age_idx, age_fac, age,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "age_idx" = "age_idx")) %>% 
#   mutate(Sex = sex) %>% 
#   ggplot(aes(x = age, y = median,  color = Sex)) + 
#   geom_line() +
#   xlab("Age") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_age_per, file = "plots/SO_Q2/mAPC_bi_age_shareper.png")
# 
# bi_plot_age_per_ci <- bi_age_per_summary %>% 
#   mutate(age_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                             max(mod_data$age_idx))) %>% 
#   left_join(mod_data %>% 
#               select(age_idx, age_fac, age,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "age_idx" = "age_idx")) %>% 
#   mutate(Sex = sex) %>% 
#   ggplot(aes(x = age, y = median, group = Sex, color = Sex)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = q10, ymax = q90, fill= Sex), alpha = 0.3) +
#   xlab("Age") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_age_per_ci, file = "plots/SO_Q2/mAPC_bi_age_shareper_ci.png")
# 
# bi_plot_age_co <- bi_age_co_summary %>% 
#   mutate(age_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                             max(mod_data$age_idx))) %>% 
#   left_join(mod_data %>% 
#               select(age_idx, age_fac, age,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "age_idx" = "age_idx")) %>% 
#   mutate(Sex = sex) %>% 
#   ggplot(aes(x = age, y = median,  color = Sex)) + 
#   geom_line() +
#   xlab("Age") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_age_co, file = "plots/SO_Q2/mAPC_bi_age_shareco.png")
# 
# bi_plot_age_co_ci <- bi_age_co_summary %>% 
#   mutate(age_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                             max(mod_data$age_idx))) %>% 
#   left_join(mod_data %>% 
#               select(age_idx, age_fac, age,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "age_idx" = "age_idx")) %>% 
#   mutate(Sex = sex) %>% 
#   ggplot(aes(x = age, y = median, group = Sex, color = Sex)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = q10, ymax = q90, fill= Sex), alpha = 0.3) +
#   xlab("Age") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_age_co_ci, file = "plots/SO_Q2/mAPC_bi_age_shareco_ci.png")
# 
# 
# #### Period 
# 
# bi_plot_period_per <- bi_period_per_summary %>% 
#   mutate(period_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                                max(mod_data$period_idx))) %>% 
#   left_join(mod_data %>% 
#               select(period_idx, period_fac, period,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "period_idx" = "period_idx")) %>%
#   ggplot(aes(x = period, y = median)) + 
#   geom_line() +
#   xlab("Period") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_period_per, file = "plots/SO_Q2/mAPC_bi_per_shareper.png")
# 
# bi_plot_period_per_ci <- bi_period_per_summary %>% 
#   mutate(period_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                                max(mod_data$period_idx))) %>% 
#   left_join(mod_data %>% 
#               select(period_idx, period_fac, period,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "period_idx" = "period_idx")) %>%
#   ggplot(aes(x = period, y = median)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.3) +
#   xlab("Period") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_period_per_ci, file = "plots/SO_Q2/mAPC_bi_per_shareper_ci.png")
# 
# 
# bi_plot_period_co <- bi_period_co_summary %>% 
#   mutate(period_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                                max(mod_data$period_idx))) %>% 
#   left_join(mod_data %>% 
#               select(period_idx, period_fac, period,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "period_idx" = "period_idx")) %>%
#   mutate(Sex = sex)%>% 
#   ggplot(aes(x = period, y = median, color = Sex)) + 
#   geom_line() +
#   xlab("Period") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_period_co, file = "plots/SO_Q2/mAPC_bi_per_shareco.png")
# 
# 
# 
# bi_plot_period_co_ci <- bi_period_co_summary %>% 
#   mutate(period_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                                max(mod_data$period_idx))) %>% 
#   left_join(mod_data %>% 
#               select(period_idx, period_fac, period,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "period_idx" = "period_idx")) %>%
#   mutate(Sex = sex)%>% 
#   ggplot(aes(x = period, y = median, color = Sex)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = q10, ymax = q90, fill = Sex), alpha = 0.3) +
#   xlab("Period") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_period_co_ci, file = "plots/SO_Q2/mAPC_bi_per_shareco_ci.png")
# 
# 
# #### Cohort 
# 
# bi_plot_cohort_per <- bi_cohort_per_summary %>% 
#   mutate(cohort_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                                max(mod_data$cohort_idx))) %>% 
#   left_join(mod_data %>% 
#               select(cohort_idx, cohort_fac, cohort,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "cohort_idx" = "cohort_idx")) %>% 
#   mutate(Sex = sex) %>% 
#   ggplot(aes(x = cohort, y = median, group = Sex, color =Sex)) + 
#   geom_line() +
#   xlab("Cohort") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_cohort_per, filename =  "plots/SO_Q2/mAPC_bi_co_shareper.png") 
# 
# 
# bi_plot_cohort_per_ci <- bi_cohort_per_summary %>% 
#   mutate(cohort_idx = ifelse(sex_idx == 1, term_idx, term_idx - 
#                                max(mod_data$cohort_idx))) %>% 
#   left_join(mod_data %>% 
#               select(cohort_idx, cohort_fac, cohort,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "cohort_idx" = "cohort_idx")) %>% 
#   mutate(Sex = sex) %>% 
#   ggplot(aes(x = cohort, y = median, group = Sex, color =Sex)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = q10, ymax = q90, fill = Sex), alpha = 0.3) +
#   xlab("Cohort") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_cohort_per_ci, filename =  "plots/SO_Q2/mAPC_bi_co_shareper_ci.png") 
# 
# 
# 
# bi_plot_cohort_co <- bi_cohort_co_summary %>% 
#   mutate(cohort_idx = as.numeric(term_idx)) %>% 
#   left_join(mod_data %>% 
#               select(cohort_idx, cohort_fac, cohort,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "cohort_idx" = "cohort_idx")) %>% 
#   # mutate(Sex = sex) %>% 
#   ggplot(aes(x = cohort, y = median)) + 
#   geom_line() +
#   xlab("Cohort") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_cohort_co, filename =  "plots/SO_Q2/mAPC_bi_co_shareco.png") 
# 
# 
# bi_plot_cohort_co_ci <- bi_cohort_co_summary %>% 
#   mutate(cohort_idx = as.numeric(term_idx)) %>% 
#   left_join(mod_data %>% 
#               select(cohort_idx, cohort_fac, cohort,
#                      sex_idx, sex_fac, sex) %>% 
#               unique(),
#             by = c("sex_idx" = "sex_idx", "cohort_idx" = "cohort_idx")) %>% 
#   ggplot(aes(x = cohort, y = median)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.3) +
#   xlab("Cohort") +
#   ylab("Log odds") +
#   ggtitle("Bisexual: 18-74 (BRFSS)")
# 
# ggsave(bi_plot_cohort_co_ci, filename =  "plots/SO_Q2/mAPC_bi_co_shareco_ci.png") 
# 

## Lesbian/gay ####

### Shared: Age ####
mod_lg_shareage <- inla(mod_formula_shareage,
                        data = mod_data %>% filter(so == "lesgay"), 
                        family = "gaussian",
                        control.family = list(hyper = list(
                          prec = list(initial = log(1),
                                      fixed=TRUE))), 
                        control.predictor = list(compute=TRUE),
                        control.compute = list(config = TRUE),
                        lincomb = lincombs_shareage,
                        scale = prec_logit_pijk,
                        verbose = TRUE)


#### Plots ####
lg_age_shareage <- mod_lg_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age) %>% 
  ggplot(aes(x = Age, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Lesbian/gay: Age Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_age_curvatures_shareage.png", lg_age_shareage)

lg_age_shareage_std <- lg_age_shareage + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_age_curvatures_shareage_std.png",
       lg_age_shareage_std)

lg_per_shareage <- mod_lg_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period) %>% 
  ggplot(aes(x = Period, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Lesbian/gay: Period Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_per_curvatures_shareage.png", lg_per_shareage)

lg_per_shareage_std <- lg_per_shareage + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_per_curvatures_shareage_std.png",
       lg_per_shareage_std)

lg_coh_shareage <- mod_lg_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort) %>% 
  ggplot(aes(x = Cohort, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  scale_x_reverse() +
  ggtitle("Lesbian/gay: Cohort Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_coh_curvatures_shareage.png", lg_coh_shareage)

lg_coh_shareage_std <- lg_coh_shareage + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_coh_curvatures_shareage_std.png",
       lg_coh_shareage_std)

### Shared: Cohort ####
mod_lg_shareco <- inla(mod_formula_shareco,
                       data = mod_data %>% filter(so == "lesgay"), 
                       family = "gaussian",
                       control.family = list(hyper = list(
                         prec = list(initial = log(1),
                                     fixed=TRUE))), 
                       control.predictor = list(compute=TRUE),
                       control.compute = list(config = TRUE),
                       lincomb = lincombs_shareco,
                       scale = prec_logit_pijk,
                       verbose = TRUE)

#### Plots ####
lg_age_shareco <- mod_lg_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age) %>% 
  ggplot(aes(x = Age, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Lesbian/gay: Age Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_age_curvatures_shareco.png", lg_age_shareco)

lg_age_shareco_std <- lg_age_shareco + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_age_curvatures_shareco_std.png",
       lg_age_shareco_std)

lg_per_shareco <- mod_lg_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period) %>% 
  ggplot(aes(x = Period, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Lesbian/gay: Period Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_per_curvatures_shareco.png", lg_per_shareco)

lg_per_shareco_std <- lg_per_shareco + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_per_curvatures_shareco_std.png",
       lg_per_shareco_std)

lg_coh_shareco <- mod_lg_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort) %>% 
  ggplot(aes(x = Cohort, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  scale_x_reverse() +
  ggtitle("Lesbian/gay: Cohort Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_coh_curvatures_shareco.png", lg_coh_shareco)

lg_coh_shareco_std <- lg_coh_shareco + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_coh_curvatures_shareco_std.png",
       lg_coh_shareco_std)

### Shared: Period ####
mod_lg_shareper <- inla(mod_formula_shareper,
                        data = mod_data %>% filter(so == "lesgay"), 
                        family = "gaussian",
                        control.family = list(hyper = list(
                          prec = list(initial = log(1),
                                      fixed=TRUE))), 
                        control.predictor = list(compute=TRUE),
                        control.compute = list(config = TRUE),
                        lincomb = lincombs_shareper,
                        scale = prec_logit_pijk,
                        verbose = TRUE)

#### Plots ####
lg_age_shareper<- mod_lg_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age) %>% 
  ggplot(aes(x = Age, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Lesbian/gay: Age Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_age_curvatures_shareper.png", lg_age_shareper)

lg_age_shareper_std <- lg_age_shareper + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_age_curvatures_shareper_std.png",
       lg_age_shareper_std)

lg_per_shareper <- mod_lg_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period) %>% 
  ggplot(aes(x = Period, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  ggtitle("Lesbian/gay: Period Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_per_curvatures_shareper.png", lg_per_shareper)

lg_per_shareper_std <- lg_per_shareper + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_per_curvatures_shareper_std.png",
       lg_per_shareper_std)

lg_coh_shareper <- mod_lg_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort) %>% 
  ggplot(aes(x = Cohort, y = `0.5quant`, group = Sex,
             color = Sex, fill = Sex)) +
  geom_abline(slope = 0, intercept = 0) + 
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax =  `0.975quant`),
              alpha = 0.5, color = NA) +
  scale_color_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                               "Male" = "forestgreen")) +
  scale_fill_manual(values =c("Shared" = "goldenrod", "Female" = "navy",
                              "Male" = "forestgreen")) +
  theme_classic() +
  facet_wrap(~Sex) +
  scale_x_reverse() +
  ggtitle("Lesbian/gay: Cohort Curvature") +
  ylab("Curvature") 

ggsave(filename = "plots/SO_Q2/LG_coh_curvatures_shareper.png", lg_coh_shareper)

lg_coh_shareper_std <- lg_coh_shareper + ylim(c(-0.4, 0.4))
ggsave(filename = "plots/SO_Q2/LG_coh_curvatures_shareper_std.png",
       lg_coh_shareper_std)

### Save Model Outputs ####

sink(file = "tables/SO_Q2/mod_lg_shareper.txt")
summary(mod_lg_shareper)
head(mod_lg_shareper$summary.random$age_idx)
head(mod_lg_shareper$summary.linear.predictor)
sink(file = NULL)

sink(file = "tables/SO_Q2/mod_lg_shareco.txt")
summary(mod_lg_shareco)
head(mod_lg_shareco$summary.random$age_idx)
head(mod_lg_shareco$summary.linear.predictor)
sink(file = NULL)

# Manuscript plots ####

## Shared Age ####

### Prep Age ####

## BI
bi_age_shareage <- mod_bi_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age,
         SO = "Bisexual") 

## LG
lg_age_shareage <- mod_lg_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age,
         SO = "Lesbian/gay")

## Combine
age_shareage <- bind_rows(bi_age_shareage, lg_age_shareage) %>% 
  arrange(SO, Term)

### Prep Period ####

## BI
bi_per_shareage <- mod_bi_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period,
         SO = "Bisexual")

## LG
lg_per_shareage <- mod_lg_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period,
         SO = "Lesbian/gay")

## Combine
per_shareage <- bind_rows(bi_per_shareage, lg_per_shareage) %>% 
  arrange(SO, Term)

### Prep Cohort ####

## BI
bi_coh_shareage <- mod_bi_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort,
         SO = "Bisexual")

## LG
lg_coh_shareage <- mod_lg_shareage$summary.lincomb.derived %>%
  left_join(lincombs_info_shareage,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort,
         SO = "Lesbian/gay")

coh_shareage <- bind_rows(bi_coh_shareage, lg_coh_shareage) %>% 
  arrange(SO, Term)

## Get limits
age_shareage %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))
per_shareage %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))
coh_shareage %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))

A_xlims <- c(14, 79)
P_xlims <- c(2013.75, 2021.25) #c(2014, 2021)
C_xlims <- c(1935, 2007)

shareage_ylims <- c(-0.45, 0.45)


### Plot ####
png("plots/SO_Q2/SharedAge_10panel_curvature.png", 
    width = 300*10, height = 300*5, res=300)
{
  par(mfrow = c(2,5), lend = 1)
  par(mar=c(5,0,2,0))
  par(oma=c(0,4,2,1))
  
  ## Bisexual 
  {
    { 
      ## Age
      plot(NA, xlim = A_xlims, ylim = shareage_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)

      mtext("Bisexual", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
      ## title
      text("a. Age", x= 15, y= 0.43, cex=.9, adj=0)
      # title("Age", adj = 0, line = 4)
      
      #mtext("Curvature", adj = 0.275, side = 2,
      #      line = 2.5, cex = 0.8, outer = TRUE)
      #mtext("Curvature", adj = 0.85, side = 2,
      #      line = 2.5, cex = 0.8, outer = TRUE)
      ## Age axis
      #axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = .9)
      mtext("Age", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      axis(2, at = seq(-.45, .45, .15), cex=0.5, labels = FALSE)
      mtext(side = 2, round(seq(-.45, .45, .15), 2), at = seq(-.45, .45, .15), 
            cex=0.5, line=0.5)
      mtext("Second Differences", side = 2, line = 2, cex = 0.6)

      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(age_shareage$Age[age_shareage$SO == "Bisexual"],
            age_shareage$`0.5quant`[age_shareage$SO == "Bisexual"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(age_shareage$Age[age_shareage$SO == "Bisexual"],
                    rev(age_shareage$Age[age_shareage$SO == "Bisexual"])),
              y = c(age_shareage$`0.025quant`[age_shareage$SO == "Bisexual"],
                    rev(age_shareage$`0.975quant`[age_shareage$SO == 
                                                    "Bisexual"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Period
      plot(NA, xlim = P_xlims, ylim = shareage_ylims,
           xlab = "", ylab = "", 
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("b. Period - female", x= 2014, y= 0.43, cex=.9, adj=0)

      ## Period axis
      mtext("Period", side = 1, line = 2, cex=0.6)
      axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
      mtext(side = 1, 2014:2021, at = 2014:2021, 
            cex=0.5, line=0.5)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(per_shareage$Period[per_shareage$SO == "Bisexual" &
                                  per_shareage$Sex == "Female"],
            per_shareage$`0.5quant`[per_shareage$SO == "Bisexual" &
                                      per_shareage$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(per_shareage$Period[per_shareage$SO == "Bisexual" &
                                          per_shareage$Sex == "Female"],
                    rev(per_shareage$Period[per_shareage$SO == "Bisexual" &
                                              per_shareage$Sex == "Female"])),
              y = c(per_shareage$`0.025quant`[per_shareage$SO == "Bisexual" &
                                                per_shareage$Sex == "Female"],
                    rev(per_shareage$`0.975quant`[per_shareage$SO == "Bisexual" &
                                                    per_shareage$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Period
      plot(NA, xlim = P_xlims, ylim = shareage_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("c. Period - male", x= 2014, y= 0.43, cex=.9, adj=0)
       
      ## Period axis
      mtext("Period", side = 1, line = 2, cex=0.6)
      axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
      mtext(side = 1, 2014:2021, at = 2014:2021, 
            cex=0.5, line=0.5)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(per_shareage$Period[per_shareage$SO == "Bisexual" &
                                  per_shareage$Sex == "Male"],
            per_shareage$`0.5quant`[per_shareage$SO == "Bisexual" &
                                      per_shareage$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(per_shareage$Period[per_shareage$SO == "Bisexual" &
                                          per_shareage$Sex == "Male"],
                    rev(per_shareage$Period[per_shareage$SO == "Bisexual" &
                                              per_shareage$Sex == "Male"])),
              y = c(per_shareage$`0.025quant`[per_shareage$SO == "Bisexual" &
                                                per_shareage$Sex == "Male"],
                    rev(per_shareage$`0.975quant`[per_shareage$SO == "Bisexual" &
                                                    per_shareage$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Cohort
      plot(NA, xlim = C_xlims, ylim = shareage_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("d. Cohort - female", x= 1938, y= 0.43, cex=.9, adj=0)
       
      ## Cohort axis
      mtext("Cohort", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
            cex=0.5, line=0.5)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(coh_shareage$Cohort[coh_shareage$SO == "Bisexual" &
                                  coh_shareage$Sex == "Female"],
            coh_shareage$`0.5quant`[coh_shareage$SO == "Bisexual" &
                                      coh_shareage$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(coh_shareage$Cohort[coh_shareage$SO == "Bisexual" &
                                          coh_shareage$Sex == "Female"],
                    rev(coh_shareage$Cohort[coh_shareage$SO == "Bisexual" &
                                              coh_shareage$Sex == "Female"])),
              y = c(coh_shareage$`0.025quant`[coh_shareage$SO == "Bisexual" &
                                                coh_shareage$Sex == "Female"],
                    rev(coh_shareage$`0.975quant`[coh_shareage$SO == "Bisexual" &
                                                    coh_shareage$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Cohort
      plot(NA, xlim = C_xlims, ylim = shareage_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("e. Cohort - male", x= 1938, y= 0.43, cex=.9, adj=0)
       
      ## Cohort axis
      mtext("Cohort", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
            cex=0.5, line=0.5)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(coh_shareage$Cohort[coh_shareage$SO == "Bisexual" &
                                  coh_shareage$Sex == "Male"],
            coh_shareage$`0.5quant`[coh_shareage$SO == "Bisexual" &
                                      coh_shareage$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(coh_shareage$Cohort[coh_shareage$SO == "Bisexual" &
                                          coh_shareage$Sex == "Male"],
                    rev(coh_shareage$Cohort[coh_shareage$SO == "Bisexual" &
                                              coh_shareage$Sex == "Male"])),
              y = c(coh_shareage$`0.025quant`[coh_shareage$SO == "Bisexual" &
                                                coh_shareage$Sex == "Male"],
                    rev(coh_shareage$`0.975quant`[coh_shareage$SO == "Bisexual" &
                                                    coh_shareage$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
  
  ## Lesbian/gay
  {
    { 
      ## Age
      plot(NA, xlim = A_xlims, ylim = shareage_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)

      ## title
      text("f. Age", x= 15, y= 0.43, cex=.9, adj=0)
      mtext("Lesbian/gay", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
      
      mtext("Age", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      axis(2, at = seq(-.45, .45, .15), cex=0.5, labels = FALSE)
      mtext(side = 2, round(seq(-.45, .45, .15), 2), at = seq(-.45, .45, .15), 
            cex=0.5, line=0.5)
      mtext("Second Differences", side = 2, line = 2, cex = 0.6)
      
      ## y = 0
      abline(h = 0)

      ## Ests
      lines(age_shareage$Age[age_shareage$SO == "Lesbian/gay"],
            age_shareage$`0.5quant`[age_shareage$SO == "Lesbian/gay"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(age_shareage$Age[age_shareage$SO == "Lesbian/gay"],
                    rev(age_shareage$Age[age_shareage$SO == "Lesbian/gay"])),
              y = c(age_shareage$`0.025quant`[age_shareage$SO == "Lesbian/gay"],
                    rev(age_shareage$`0.975quant`[age_shareage$SO == 
                                                    "Lesbian/gay"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("g. Period - female", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(per_shareage$Period[per_shareage$SO == "Lesbian/gay" &
                                  per_shareage$Sex == "Female"],
            per_shareage$`0.5quant`[per_shareage$SO == "Lesbian/gay" &
                                      per_shareage$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(per_shareage$Period[per_shareage$SO == "Lesbian/gay" &
                                          per_shareage$Sex == "Female"],
                    rev(per_shareage$Period[per_shareage$SO == "Lesbian/gay" &
                                              per_shareage$Sex == "Female"])),
              y = c(per_shareage$`0.025quant`[per_shareage$SO == "Lesbian/gay" &
                                                per_shareage$Sex == "Female"],
                    rev(per_shareage$`0.975quant`[per_shareage$SO == "Lesbian/gay" &
                                                    per_shareage$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("h. Period - male", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(per_shareage$Period[per_shareage$SO == "Lesbian/gay" &
                                  per_shareage$Sex == "Male"],
            per_shareage$`0.5quant`[per_shareage$SO == "Lesbian/gay" &
                                      per_shareage$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(per_shareage$Period[per_shareage$SO == "Lesbian/gay" &
                                          per_shareage$Sex == "Male"],
                    rev(per_shareage$Period[per_shareage$SO == "Lesbian/gay" &
                                              per_shareage$Sex == "Male"])),
              y = c(per_shareage$`0.025quant`[per_shareage$SO == "Lesbian/gay" &
                                                per_shareage$Sex == "Male"],
                    rev(per_shareage$`0.975quant`[per_shareage$SO == "Lesbian/gay" &
                                                    per_shareage$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("i. Cohort - female", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       ## Ests
      lines(coh_shareage$Cohort[coh_shareage$SO == "Lesbian/gay" &
                                  coh_shareage$Sex == "Female"],
            coh_shareage$`0.5quant`[coh_shareage$SO == "Lesbian/gay" &
                                      coh_shareage$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(coh_shareage$Cohort[coh_shareage$SO == "Lesbian/gay" &
                                          coh_shareage$Sex == "Female"],
                    rev(coh_shareage$Cohort[coh_shareage$SO == "Lesbian/gay" &
                                              coh_shareage$Sex == "Female"])),
              y = c(coh_shareage$`0.025quant`[coh_shareage$SO == "Lesbian/gay" &
                                                coh_shareage$Sex == "Female"],
                    rev(coh_shareage$`0.975quant`[coh_shareage$SO == "Lesbian/gay" &
                                                    coh_shareage$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("j. Cohort - male", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(coh_shareage$Cohort[coh_shareage$SO == "Lesbian/gay" &
                                  coh_shareage$Sex == "Male"],
            coh_shareage$`0.5quant`[coh_shareage$SO == "Lesbian/gay" &
                                      coh_shareage$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(coh_shareage$Cohort[coh_shareage$SO == "Lesbian/gay" &
                                          coh_shareage$Sex == "Male"],
                    rev(coh_shareage$Cohort[coh_shareage$SO == "Lesbian/gay" &
                                              coh_shareage$Sex == "Male"])),
              y = c(coh_shareage$`0.025quant`[coh_shareage$SO == "Lesbian/gay" &
                                                coh_shareage$Sex == "Male"],
                    rev(coh_shareage$`0.975quant`[coh_shareage$SO == "Lesbian/gay" &
                                                    coh_shareage$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

## Shared Period ####

### Prep Age ####

## BI
bi_age_shareper <- mod_bi_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age,
         SO = "Bisexual") 

## LG
lg_age_shareper <- mod_lg_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age,
         SO = "Lesbian/gay")

## Combine
age_shareper <- bind_rows(bi_age_shareper, lg_age_shareper) %>% 
  arrange(SO, Term)

### Prep Period ####

## BI
bi_per_shareper <- mod_bi_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period,
         SO = "Bisexual")

## LG
lg_per_shareper <- mod_lg_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period,
         SO = "Lesbian/gay")

## Combine
per_shareper <- bind_rows(bi_per_shareper, lg_per_shareper) %>% 
  arrange(SO, Term)

### Prep Cohort ####

## BI
bi_coh_shareper <- mod_bi_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort,
         SO = "Bisexual")

## LG
lg_coh_shareper <- mod_lg_shareper$summary.lincomb.derived %>%
  left_join(lincombs_info_shareper,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort,
         SO = "Lesbian/gay")

coh_shareper <- bind_rows(bi_coh_shareper, lg_coh_shareper) %>% 
  arrange(SO, Term)

## Get limits
age_shareper %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))
per_shareper %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))
coh_shareper %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))

A_xlims <- c(14, 79)
P_xlims <- c(2013.75, 2021.25) #c(2014, 2021)
C_xlims <- c(1935, 2007)

shareper_ylims <- c(-0.45, 0.45)

### Plot ####
png("plots/SO_Q2/SharedPer_10panel_curvature.png", 
    width = 300*10, height = 300*5, res=300)
{
  par(mfrow = c(2,5), lend = 1)
  par(mar=c(5,0,2,0))
  par(oma=c(0,4,2,1))
  
  ## Bisexual 
  {
    { 
      ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       mtext("Bisexual", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## title
       text("a. Age - female", x= 15, y= 0.43, cex=.9, adj=0)

       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       axis(2, at = seq(-.45, .45, .15), cex=0.5, labels = FALSE)
       mtext(side = 2, round(seq(-.45, .45, .15), 2), at = seq(-.45, .45, .15), 
             cex=0.5, line=0.5)
       mtext("Second Differences", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(age_shareper$Age[age_shareper$SO == "Bisexual" &
                               age_shareper$Sex == "Female"],
            age_shareper$`0.5quant`[age_shareper$SO == "Bisexual" &
                                      age_shareper$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(age_shareper$Age[age_shareper$SO == "Bisexual" &
                                       age_shareper$Sex == "Female"],
                    rev(age_shareper$Age[age_shareper$SO == "Bisexual" &
                                           age_shareper$Sex == "Female"])),
              y = c(age_shareper$`0.025quant`[age_shareper$SO == "Bisexual" &
                                                age_shareper$Sex == "Female"],
                    rev(age_shareper$`0.975quant`[age_shareper$SO == 
                                                    "Bisexual" &
                                                    age_shareper$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("b. Age - male", x= 15, y= 0.43, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(age_shareper$Age[age_shareper$SO == "Bisexual" &
                               age_shareper$Sex == "Male"],
            age_shareper$`0.5quant`[age_shareper$SO == "Bisexual" &
                                      age_shareper$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(age_shareper$Age[age_shareper$SO == "Bisexual" &
                                       age_shareper$Sex == "Male"],
                    rev(age_shareper$Age[age_shareper$SO == "Bisexual" &
                                           age_shareper$Sex == "Male"])),
              y = c(age_shareper$`0.025quant`[age_shareper$SO == "Bisexual" &
                                                age_shareper$Sex == "Male"],
                    rev(age_shareper$`0.975quant`[age_shareper$SO == 
                                                    "Bisexual" &
                                                    age_shareper$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("c. Period", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(per_shareper$Period[per_shareper$SO == "Bisexual"],
            per_shareper$`0.5quant`[per_shareper$SO == "Bisexual"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(per_shareper$Period[per_shareper$SO == "Bisexual" ],
                    rev(per_shareper$Period[per_shareper$SO == "Bisexual"])),
              y = c(per_shareper$`0.025quant`[per_shareper$SO == "Bisexual"],
                    rev(per_shareper$`0.975quant`[per_shareper$SO == "Bisexual"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
       ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("d. Cohort - female", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(coh_shareper$Cohort[coh_shareper$SO == "Bisexual" &
                                  coh_shareper$Sex == "Female"],
            coh_shareper$`0.5quant`[coh_shareper$SO == "Bisexual" &
                                      coh_shareper$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(coh_shareper$Cohort[coh_shareper$SO == "Bisexual" &
                                          coh_shareper$Sex == "Female"],
                    rev(coh_shareper$Cohort[coh_shareper$SO == "Bisexual" &
                                              coh_shareper$Sex == "Female"])),
              y = c(coh_shareper$`0.025quant`[coh_shareper$SO == "Bisexual" &
                                                coh_shareper$Sex == "Female"],
                    rev(coh_shareper$`0.975quant`[coh_shareper$SO == "Bisexual" &
                                                    coh_shareper$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
       ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("e. Cohort - male", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(coh_shareper$Cohort[coh_shareper$SO == "Bisexual" &
                                  coh_shareper$Sex == "Male"],
            coh_shareper$`0.5quant`[coh_shareper$SO == "Bisexual" &
                                      coh_shareper$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(coh_shareper$Cohort[coh_shareper$SO == "Bisexual" &
                                          coh_shareper$Sex == "Male"],
                    rev(coh_shareper$Cohort[coh_shareper$SO == "Bisexual" &
                                              coh_shareper$Sex == "Male"])),
              y = c(coh_shareper$`0.025quant`[coh_shareper$SO == "Bisexual" &
                                                coh_shareper$Sex == "Male"],
                    rev(coh_shareper$`0.975quant`[coh_shareper$SO == "Bisexual" &
                                                    coh_shareper$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
  
  ## Lesbian/gay
  {
    { 
      ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       mtext("Lesbian/gay", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## title
       text("f. Age - female", x= 15, y= 0.43, cex=.9, adj=0)

       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       axis(2, at = seq(-.45, .45, .15), cex=0.5, labels = FALSE)
       mtext(side = 2, round(seq(-.45, .45, .15), 2), at = seq(-.45, .45, .15), 
             cex=0.5, line=0.5)
       mtext("Second Differences", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(age_shareper$Age[age_shareper$SO == "Lesbian/gay" &
                               age_shareper$Sex == "Female"],
            age_shareper$`0.5quant`[age_shareper$SO == "Lesbian/gay" &
                                      age_shareper$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(age_shareper$Age[age_shareper$SO == "Lesbian/gay" &
                                       age_shareper$Sex == "Female"],
                    rev(age_shareper$Age[age_shareper$SO == "Lesbian/gay" &
                                           age_shareper$Sex == "Female"])),
              y = c(age_shareper$`0.025quant`[age_shareper$SO == "Lesbian/gay" &
                                                age_shareper$Sex == "Female"],
                    rev(age_shareper$`0.975quant`[age_shareper$SO == 
                                                    "Lesbian/gay" &
                                                    age_shareper$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("g. Age - male", x= 15, y= 0.43, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(age_shareper$Age[age_shareper$SO == "Lesbian/gay" &
                               age_shareper$Sex == "Male"],
            age_shareper$`0.5quant`[age_shareper$SO == "Lesbian/gay" &
                                      age_shareper$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(age_shareper$Age[age_shareper$SO == "Lesbian/gay" &
                                       age_shareper$Sex == "Male"],
                    rev(age_shareper$Age[age_shareper$SO == "Lesbian/gay" &
                                           age_shareper$Sex == "Male"])),
              y = c(age_shareper$`0.025quant`[age_shareper$SO == "Lesbian/gay" &
                                                age_shareper$Sex == "Male"],
                    rev(age_shareper$`0.975quant`[age_shareper$SO == 
                                                    "Lesbian/gay" &
                                                    age_shareper$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("h. Period", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(per_shareper$Period[per_shareper$SO == "Lesbian/gay"],
            per_shareper$`0.5quant`[per_shareper$SO == "Lesbian/gay"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(per_shareper$Period[per_shareper$SO == "Lesbian/gay" ],
                    rev(per_shareper$Period[per_shareper$SO == "Lesbian/gay"])),
              y = c(per_shareper$`0.025quant`[per_shareper$SO == "Lesbian/gay"],
                    rev(per_shareper$`0.975quant`[per_shareper$SO == "Lesbian/gay"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
       ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("i. Cohort - female", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(coh_shareper$Cohort[coh_shareper$SO == "Lesbian/gay" &
                                  coh_shareper$Sex == "Female"],
            coh_shareper$`0.5quant`[coh_shareper$SO == "Lesbian/gay" &
                                      coh_shareper$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(coh_shareper$Cohort[coh_shareper$SO == "Lesbian/gay" &
                                          coh_shareper$Sex == "Female"],
                    rev(coh_shareper$Cohort[coh_shareper$SO == "Lesbian/gay" &
                                              coh_shareper$Sex == "Female"])),
              y = c(coh_shareper$`0.025quant`[coh_shareper$SO == "Lesbian/gay" &
                                                coh_shareper$Sex == "Female"],
                    rev(coh_shareper$`0.975quant`[coh_shareper$SO == "Lesbian/gay" &
                                                    coh_shareper$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("j. Cohort - male", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(coh_shareper$Cohort[coh_shareper$SO == "Lesbian/gay" &
                                  coh_shareper$Sex == "Male"],
            coh_shareper$`0.5quant`[coh_shareper$SO == "Lesbian/gay" &
                                      coh_shareper$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(coh_shareper$Cohort[coh_shareper$SO == "Lesbian/gay" &
                                          coh_shareper$Sex == "Male"],
                    rev(coh_shareper$Cohort[coh_shareper$SO == "Lesbian/gay" &
                                              coh_shareper$Sex == "Male"])),
              y = c(coh_shareper$`0.025quant`[coh_shareper$SO == "Lesbian/gay" &
                                                coh_shareper$Sex == "Male"],
                    rev(coh_shareper$`0.975quant`[coh_shareper$SO == "Lesbian/gay" &
                                                    coh_shareper$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

## Shared Cohort####

### Prep Age ####

## BI
bi_age_shareco <- mod_bi_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age,
         SO = "Bisexual") 

## LG
lg_age_shareco <- mod_lg_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Age") %>% 
  left_join(age_info,
            by = c("Term" = "age_idx")) %>% 
  mutate(Age = age,
         SO = "Lesbian/gay")

## Combine
age_shareco <- bind_rows(bi_age_shareco, lg_age_shareco) %>% 
  arrange(SO, Term)

### Prep Period ####

## BI
bi_per_shareco <- mod_bi_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period,
         SO = "Bisexual")

## LG
lg_per_shareco <- mod_lg_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Period") %>% 
  left_join(per_info,
            by = c("Term" = "period_idx")) %>% 
  mutate(Period = period,
         SO = "Lesbian/gay")

## Combine
per_shareco <- bind_rows(bi_per_shareco, lg_per_shareco) %>% 
  arrange(SO, Term)

### Prep Cohort ####

## BI
bi_coh_shareco <- mod_bi_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort,
         SO = "Bisexual")

## LG
lg_coh_shareco <- mod_lg_shareco$summary.lincomb.derived %>%
  left_join(lincombs_info_shareco,
            by = c("ID" = "LC_Index")) %>% 
  drop_na(Term) %>%  
  mutate(Sex = case_when(Sex == 1 ~ "Female",
                         Sex == 2 ~ "Male",
                         TRUE ~ "Shared")) %>%
  filter(TermName == "Cohort") %>% 
  left_join(coh_info,
            by = c("Term" = "cohort_idx")) %>% 
  mutate(Cohort = cohort,
         SO = "Lesbian/gay")

coh_shareco <- bind_rows(bi_coh_shareco, lg_coh_shareco) %>% 
  arrange(SO, Term)

## Get limits
age_shareco %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))
per_shareco %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))
coh_shareco %>% 
  dplyr::summarize(lower = min(`0.025quant`),
            upper = max(`0.975quant`))

A_xlims <- c(14, 79)
P_xlims <- c(2013.75, 2021.25) #c(2014, 2021)
C_xlims <- c(1935, 2007)

shareco_ylims <- c(-0.45, 0.45)

### Plot ####
png("plots/SO_Q2/SharedCoh_10panel_curvature.png", 
    width = 300*10, height = 300*5, res=300)
{
   par(mfrow = c(2,5), lend = 1)
   par(mar=c(5,0,2,0))
   par(oma=c(0,4,2,1))
   
   
  ## Bisexual 
  {
    { 
       ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       mtext("Bisexual", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## title
       text("a. Age - female", x= 15, y= 0.43, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       axis(2, at = seq(-.45, .45, .15), cex=0.5, labels = FALSE)
       mtext(side = 2, round(seq(-.45, .45, .15), 2), at = seq(-.45, .45, .15), 
             cex=0.5, line=0.5)
       mtext("Second Differences", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(age_shareco$Age[age_shareco$SO == "Bisexual" &
                               age_shareco$Sex == "Female"],
            age_shareco$`0.5quant`[age_shareco$SO == "Bisexual" &
                                      age_shareco$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(age_shareco$Age[age_shareco$SO == "Bisexual" &
                                       age_shareco$Sex == "Female"],
                    rev(age_shareco$Age[age_shareco$SO == "Bisexual" &
                                           age_shareco$Sex == "Female"])),
              y = c(age_shareco$`0.025quant`[age_shareco$SO == "Bisexual" &
                                                age_shareco$Sex == "Female"],
                    rev(age_shareco$`0.975quant`[age_shareco$SO == 
                                                    "Bisexual" &
                                                    age_shareco$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("b. Age - male", x= 15, y= 0.43, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(age_shareco$Age[age_shareco$SO == "Bisexual" &
                               age_shareco$Sex == "Male"],
            age_shareco$`0.5quant`[age_shareco$SO == "Bisexual" &
                                      age_shareco$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(age_shareco$Age[age_shareco$SO == "Bisexual" &
                                       age_shareco$Sex == "Male"],
                    rev(age_shareco$Age[age_shareco$SO == "Bisexual" &
                                           age_shareco$Sex == "Male"])),
              y = c(age_shareco$`0.025quant`[age_shareco$SO == "Bisexual" &
                                                age_shareco$Sex == "Male"],
                    rev(age_shareco$`0.975quant`[age_shareco$SO == 
                                                    "Bisexual" &
                                                    age_shareco$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("c. Period - female", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(per_shareco$Period[per_shareco$SO == "Bisexual" &
                                 per_shareco$Sex == "Female"],
            per_shareco$`0.5quant`[per_shareco$SO == "Bisexual" &
                                     per_shareco$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(per_shareco$Period[per_shareco$SO == "Bisexual" &
                                         per_shareco$Sex == "Female"],
                    rev(per_shareco$Period[per_shareco$SO == "Bisexual" &
                                             per_shareco$Sex == "Female"])),
              y = c(per_shareco$`0.025quant`[per_shareco$SO == "Bisexual" &
                                               per_shareco$Sex == "Female"],
                    rev(per_shareco$`0.975quant`[per_shareco$SO == "Bisexual" &
                                                   per_shareco$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
       ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("d. Period - male", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(per_shareco$Period[per_shareco$SO == "Bisexual" &
                                 per_shareco$Sex == "Male"],
            per_shareco$`0.5quant`[per_shareco$SO == "Bisexual" &
                                     per_shareco$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(per_shareco$Period[per_shareco$SO == "Bisexual" &
                                         per_shareco$Sex == "Male"],
                    rev(per_shareco$Period[per_shareco$SO == "Bisexual" &
                                             per_shareco$Sex == "Male"])),
              y = c(per_shareco$`0.025quant`[per_shareco$SO == "Bisexual" &
                                               per_shareco$Sex == "Male"],
                    rev(per_shareco$`0.975quant`[per_shareco$SO == "Bisexual" &
                                                   per_shareco$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
       ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("e. Cohort", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(coh_shareco$Cohort[coh_shareco$SO == "Bisexual"],
            coh_shareco$`0.5quant`[coh_shareco$SO == "Bisexual"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(coh_shareco$Cohort[coh_shareco$SO == "Bisexual"],
                    rev(coh_shareco$Cohort[coh_shareco$SO == "Bisexual"])),
              y = c(coh_shareco$`0.025quant`[coh_shareco$SO == "Bisexual"],
                    rev(coh_shareco$`0.975quant`[coh_shareco$SO == "Bisexual"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
  }
  
  ## Lesbian/gay
  {
    { 
       ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       mtext("Lesbian/gay", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## title
       text("f. Age - female", x= 15, y= 0.43, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       axis(2, at = seq(-.45, .45, .15), cex=0.5, labels = FALSE)
       mtext(side = 2, round(seq(-.45, .45, .15), 2), at = seq(-.45, .45, .15), 
             cex=0.5, line=0.5)
       mtext("Second Differences", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(age_shareco$Age[age_shareco$SO == "Lesbian/gay" &
                              age_shareco$Sex == "Female"],
            age_shareco$`0.5quant`[age_shareco$SO == "Lesbian/gay" &
                                     age_shareco$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(age_shareco$Age[age_shareco$SO == "Lesbian/gay" &
                                      age_shareco$Sex == "Female"],
                    rev(age_shareco$Age[age_shareco$SO == "Lesbian/gay" &
                                          age_shareco$Sex == "Female"])),
              y = c(age_shareco$`0.025quant`[age_shareco$SO == "Lesbian/gay" &
                                               age_shareco$Sex == "Female"],
                    rev(age_shareco$`0.975quant`[age_shareco$SO == 
                                                   "Lesbian/gay" &
                                                   age_shareco$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
       ## Age
       plot(NA, xlim = A_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("g. Age - male", x= 15, y= 0.43, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(age_shareco$Age[age_shareco$SO == "Lesbian/gay" &
                              age_shareco$Sex == "Male"],
            age_shareco$`0.5quant`[age_shareco$SO == "Lesbian/gay" &
                                     age_shareco$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(age_shareco$Age[age_shareco$SO == "Lesbian/gay" &
                                      age_shareco$Sex == "Male"],
                    rev(age_shareco$Age[age_shareco$SO == "Lesbian/gay" &
                                          age_shareco$Sex == "Male"])),
              y = c(age_shareco$`0.025quant`[age_shareco$SO == "Lesbian/gay" &
                                               age_shareco$Sex == "Male"],
                    rev(age_shareco$`0.975quant`[age_shareco$SO == 
                                                   "Lesbian/gay" &
                                                   age_shareco$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
       ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("h. Period - female", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(per_shareco$Period[per_shareco$SO == "Lesbian/gay" &
                                 per_shareco$Sex == "Female"],
            per_shareco$`0.5quant`[per_shareco$SO == "Lesbian/gay" &
                                     per_shareco$Sex == "Female"],
            lwd = 2, col = "navy")
      polygon(x = c(per_shareco$Period[per_shareco$SO == "Lesbian/gay" &
                                         per_shareco$Sex == "Female"],
                    rev(per_shareco$Period[per_shareco$SO == "Lesbian/gay" &
                                             per_shareco$Sex == "Female"])),
              y = c(per_shareco$`0.025quant`[per_shareco$SO == "Lesbian/gay" &
                                               per_shareco$Sex == "Female"],
                    rev(per_shareco$`0.975quant`[per_shareco$SO == "Lesbian/gay" &
                                                   per_shareco$Sex == "Female"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
       ## Period
       plot(NA, xlim = P_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "", 
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("i. Period - male", x= 2014, y= 0.43, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, 2014:2021, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(per_shareco$Period[per_shareco$SO == "Lesbian/gay" &
                                 per_shareco$Sex == "Male"],
            per_shareco$`0.5quant`[per_shareco$SO == "Lesbian/gay" &
                                     per_shareco$Sex == "Male"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(per_shareco$Period[per_shareco$SO == "Lesbian/gay" &
                                         per_shareco$Sex == "Male"],
                    rev(per_shareco$Period[per_shareco$SO == "Lesbian/gay" &
                                             per_shareco$Sex == "Male"])),
              y = c(per_shareco$`0.025quant`[per_shareco$SO == "Lesbian/gay" &
                                               per_shareco$Sex == "Male"],
                    rev(per_shareco$`0.975quant`[per_shareco$SO == "Lesbian/gay" &
                                                   per_shareco$Sex == "Male"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
       ## Cohort
       plot(NA, xlim = C_xlims, ylim = shareage_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("j. Cohort", x= 1938, y= 0.43, cex=.9, adj=0)
       
       ## Cohort axis
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
       
      ## Ests
      lines(coh_shareco$Cohort[coh_shareco$SO == "Lesbian/gay"],
            coh_shareco$`0.5quant`[coh_shareco$SO == "Lesbian/gay"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(coh_shareco$Cohort[coh_shareco$SO == "Lesbian/gay"],
                    rev(coh_shareco$Cohort[coh_shareco$SO == "Lesbian/gay"])),
              y = c(coh_shareco$`0.025quant`[coh_shareco$SO == "Lesbian/gay"],
                    rev(coh_shareco$`0.975quant`[coh_shareco$SO == "Lesbian/gay"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

