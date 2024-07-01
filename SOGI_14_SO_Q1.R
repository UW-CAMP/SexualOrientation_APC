## Fit null, A, P, C, AP, AC, PC models
## Prevalence by age x period (survey) x cohort
## Jessica Godwin

# Setup ####
rm(list = ls())

if(!dir.exists("tables/SO_Q1")){
  if(!dir.exists("tables/")){
    dir.create("tables/")
  }
  dir.create("tables/SO_Q1")
}

if(!dir.exists("plots/SO_Q1")){
  if(!dir.exists("plots/")){
    dir.create("plots/")
  }
  dir.create("plots/SO_Q1")
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

# Fit models ####

## Define formulas ####
pc_prec <- list(theta = list(prior = 'pc.prec', param=c(1, .5)))
mod_null <- logit_pijk ~ 1
mod_A <- logit_pijk ~  1 +  
  f(age_idx, model = "rw2", 
    scale.model = TRUE, constr = TRUE, extraconstr = NULL,
    hyper = pc_prec)
mod_P <- logit_pijk ~ 1 + 
  f(period_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec) 

mod_C <- logit_pijk ~ 1 + 
  f(cohort_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec)

mod_AP <- logit_pijk ~ 1 +
  f(age_idx, model = "rw2", 
    scale.model = TRUE, constr = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(period_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec) 

mod_AC <- logit_pijk ~ 1 + 
  f(age_idx, model = "rw2", 
    scale.model = TRUE, constr = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(cohort_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec)

mod_PC <- logit_pijk ~ 1 + 
  f(period_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec) +
  f(cohort_idx, model = "rw2", 
    constr = TRUE, scale.model = TRUE, extraconstr = NULL,
    hyper = pc_prec)



## Bisexual: Female ####

mod_bi_f_null <- inla(mod_null,
                      data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_bi_f_P <- inla(mod_P,
                   data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_bi_f_C <- inla(mod_C,
                   data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_bi_f_A <- inla(mod_A,
                   data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_bi_f_AP <- inla(mod_AP,
                    data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_bi_f_AC <- inla(mod_AC,
                    data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_bi_f_PC <- inla(mod_PC,
                    data = mod_data %>% filter(so == "bi" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

#### Tables ####
bi_f_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_bi_f_null$mlik[2],
                                       mod_bi_f_A$mlik[2],
                                       mod_bi_f_P$mlik[2],
                                       mod_bi_f_C$mlik[2],
                                       mod_bi_f_AP$mlik[2],
                                       mod_bi_f_AC$mlik[2],
                                       mod_bi_f_PC$mlik[2]),
                            WAIC = c(mod_bi_f_null$waic$waic,
                                     mod_bi_f_A$waic$waic,
                                     mod_bi_f_P$waic$waic,
                                     mod_bi_f_C$waic$waic,
                                     mod_bi_f_AP$waic$waic,
                                     mod_bi_f_AC$waic$waic,
                                     mod_bi_f_PC$waic$waic),
                            DIC = c(mod_bi_f_null$dic$dic,
                                    mod_bi_f_A$dic$dic,
                                    mod_bi_f_P$dic$dic,
                                    mod_bi_f_C$dic$dic,
                                    mod_bi_f_AP$dic$dic,
                                    mod_bi_f_AC$dic$dic,
                                    mod_bi_f_PC$dic$dic))

write.csv(bi_f_criteria, file = paste0("tables/SO_Q1/",
                                       "bi_f_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q1/mod_bi_f_summaries.txt")
summary(mod_bi_f_null)
summary(mod_bi_f_A)
summary(mod_bi_f_P)
summary(mod_bi_f_C)
summary(mod_bi_f_AP)
summary(mod_bi_f_AC)
summary(mod_bi_f_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_bi_f_AC <- inla.posterior.sample(n = 1000, mod_bi_f_AC)

age_idx <- mod_data %>% 
  select(age, age_idx) %>% 
  unique()
cohort_idx <- mod_data %>% 
  select(cohort, cohort_idx) %>% 
  unique() %>% 
  arrange(cohort_idx)

post_bi_f_AC_df <- lapply(post_bi_f_AC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_f_AC_df <- do.call(rbind.data.frame, post_bi_f_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_f_AC_plot <- post_bi_f_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_f_AC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))

bi_f_AC_age_lo <- post_bi_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_AC_age_logodds.png", bi_f_AC_age_lo)


bi_f_AC_coh_lo <- post_bi_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_AC_coh_logodds.png", bi_f_AC_coh_lo)


bi_f_AC_age_p <- post_bi_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_AC_age_prev.png", bi_f_AC_age_p)


bi_f_AC_coh_p <- post_bi_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_AC_coh_prev.png", bi_f_AC_coh_p)

#### PC ####

post_bi_f_PC <- inla.posterior.sample(n = 1000, mod_bi_f_PC)

period_idx <- mod_data %>% 
  select(period, period_idx) %>% 
  unique() %>% 
  arrange(period_idx)

post_bi_f_PC_df <- lapply(post_bi_f_PC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("period_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("period_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_f_PC_df <- do.call(rbind.data.frame, post_bi_f_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_f_PC_plot <- post_bi_f_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_f_PC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "period", "Period", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Period", period_idx$period[term_idx],
                                cohort_idx$cohort[term_idx]))


bi_f_PC_per_lo <- post_bi_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_PC_per_logodds.png", bi_f_PC_per_lo)


bi_f_PC_coh_lo <- post_bi_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_PC_coh_logodds.png", bi_f_PC_coh_lo)


bi_f_PC_per_p <- post_bi_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_PC_per_prev.png", bi_f_PC_per_p)


bi_f_PC_coh_p <- post_bi_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_PC_coh_prev.png", bi_f_PC_coh_p)

#### AP ####

post_bi_f_AP <- inla.posterior.sample(n = 1000, mod_bi_f_AP)

post_bi_f_AP_df <- lapply(post_bi_f_AP, function(draw){
  out <- draw$latent[grepl("period_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("period_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_f_AP_df <- do.call(rbind.data.frame, post_bi_f_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_f_AP_plot <- post_bi_f_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_f_AP_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Period")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                period_idx$period[term_idx]))

bi_f_AP_age_lo <- post_bi_f_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_AP_age_logodds.png", bi_f_AC_age_lo)


bi_f_AP_per_lo <- post_bi_f_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_AP_per_logodds.png", bi_f_AP_per_lo)



#### C ####

post_bi_f_C <- inla.posterior.sample(n = 1000, mod_bi_f_C)

post_bi_f_C_df <- lapply(post_bi_f_C, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_f_C_df <- do.call(rbind.data.frame, post_bi_f_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_f_C_plot <- post_bi_f_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_f_C_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "cohort", "Cohort", term)) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))



bi_f_C_coh_lo <- post_bi_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_C_coh_logodds.png", bi_f_C_coh_lo)

bi_f_C_coh_p <- post_bi_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_f_C_coh_prev.png", bi_f_C_coh_p)

## Bisexual: Male ####

mod_bi_m_null <- inla(mod_null,
                      data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_bi_m_P <- inla(mod_P,
                   data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_bi_m_C <- inla(mod_C,
                   data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_bi_m_A <- inla(mod_A,
                   data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_bi_m_AP <- inla(mod_AP,
                    data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_bi_m_AC <- inla(mod_AC,
                    data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_bi_m_PC <- inla(mod_PC,
                    data = mod_data %>% filter(so == "bi" & sex == "Male"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

#### Tables ####
bi_m_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_bi_m_null$mlik[2],
                                       mod_bi_m_A$mlik[2],
                                       mod_bi_m_P$mlik[2],
                                       mod_bi_m_C$mlik[2],
                                       mod_bi_m_AP$mlik[2],
                                       mod_bi_m_AC$mlik[2],
                                       mod_bi_m_PC$mlik[2]),
                            WAIC = c(mod_bi_m_null$waic$waic,
                                     mod_bi_m_A$waic$waic,
                                     mod_bi_m_P$waic$waic,
                                     mod_bi_m_C$waic$waic,
                                     mod_bi_m_AP$waic$waic,
                                     mod_bi_m_AC$waic$waic,
                                     mod_bi_m_PC$waic$waic),
                            DIC = c(mod_bi_m_null$dic$dic,
                                    mod_bi_m_A$dic$dic,
                                    mod_bi_m_P$dic$dic,
                                    mod_bi_m_C$dic$dic,
                                    mod_bi_m_AP$dic$dic,
                                    mod_bi_m_AC$dic$dic,
                                    mod_bi_m_PC$dic$dic))

write.csv(bi_m_criteria, file = paste0("tables/SO_Q1/",
                                       "bi_m_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q1/mod_bi_m_summaries.txt")
summary(mod_bi_m_null)
summary(mod_bi_m_A)
summary(mod_bi_m_P)
summary(mod_bi_m_C)
summary(mod_bi_m_AP)
summary(mod_bi_m_AC)
summary(mod_bi_m_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_bi_m_AC <- inla.posterior.sample(n = 1000, mod_bi_m_AC)

post_bi_m_AC_df <- lapply(post_bi_m_AC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_m_AC_df <- do.call(rbind.data.frame, post_bi_m_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_m_AC_plot <- post_bi_m_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_m_AC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))


bi_m_AC_age_lo <- post_bi_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_AC_age_logodds.png", bi_m_AC_age_lo)


bi_m_AC_coh_lo <- post_bi_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_AC_coh_logodds.png", bi_m_AC_coh_lo)


bi_m_AC_age_p <- post_bi_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_AC_age_prev.png", bi_m_AC_age_p)


bi_m_AC_coh_p <- post_bi_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_AC_coh_prev.png", bi_m_AC_coh_p)

#### PC ####

post_bi_m_PC <- inla.posterior.sample(n = 1000, mod_bi_m_PC)

post_bi_m_PC_df <- lapply(post_bi_m_PC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("period_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("period_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_m_PC_df <- do.call(rbind.data.frame, post_bi_m_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_m_PC_plot <- post_bi_m_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_m_PC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "period", "Period", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Period", period_idx$period[term_idx],
                                cohort_idx$cohort[term_idx]))


bi_m_PC_per_lo <- post_bi_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_PC_per_logodds.png", bi_m_PC_per_lo)


bi_m_PC_coh_lo <- post_bi_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_PC_coh_logodds.png", bi_m_PC_coh_lo)


bi_m_PC_per_p <- post_bi_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_PC_per_prev.png", bi_m_PC_per_p)


bi_m_PC_coh_p <- post_bi_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_PC_coh_prev.png", bi_m_PC_coh_p)


#### AP ####

post_bi_m_AP <- inla.posterior.sample(n = 1000, mod_bi_m_AP)

post_bi_m_AP_df <- lapply(post_bi_m_AP, function(draw){
  out <- draw$latent[grepl("period_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("period_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_m_AP_df <- do.call(rbind.data.frame, post_bi_m_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_m_AP_plot <- post_bi_m_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_m_AP_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Period")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                period_idx$period[term_idx]))

bi_m_AP_age_lo <- post_bi_m_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_AP_age_logodds.png", bi_m_AP_age_lo)


bi_m_AP_per_lo <- post_bi_m_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_AP_per_logodds.png", bi_m_AP_per_lo)


#### C ####

post_bi_m_C <- inla.posterior.sample(n = 1000, mod_bi_m_C)

post_bi_m_C_df <- lapply(post_bi_m_C, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_bi_m_C_df <- do.call(rbind.data.frame, post_bi_m_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_bi_m_C_plot <- post_bi_m_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_m_C_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "cohort", "Cohort", term)) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))



bi_m_C_coh_lo <- post_bi_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_C_coh_logodds.png", bi_m_C_coh_lo)

bi_m_C_coh_p <- post_bi_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Bisexual: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/Bi_m_C_coh_prev.png", bi_m_C_coh_p)


## Lesbian ####

mod_lg_f_null <- inla(mod_null,
                      data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                             config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_lg_f_P <- inla(mod_P,
                   data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_lg_f_C <- inla(mod_C,
                   data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_lg_f_A <- inla(mod_A,
                   data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_lg_f_AP <- inla(mod_AP,
                    data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_lg_f_AC <- inla(mod_AC,
                    data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_lg_f_PC <- inla(mod_PC,
                    data = mod_data %>% filter(so == "lesgay" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)


#### Tables ####
lg_f_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_lg_f_null$mlik[2],
                                       mod_lg_f_A$mlik[2],
                                       mod_lg_f_P$mlik[2],
                                       mod_lg_f_C$mlik[2],
                                       mod_lg_f_AP$mlik[2],
                                       mod_lg_f_AC$mlik[2],
                                       mod_lg_f_PC$mlik[2]),
                            WAIC = c(mod_lg_f_null$waic$waic,
                                     mod_lg_f_A$waic$waic,
                                     mod_lg_f_P$waic$waic,
                                     mod_lg_f_C$waic$waic,
                                     mod_lg_f_AP$waic$waic,
                                     mod_lg_f_AC$waic$waic,
                                     mod_lg_f_PC$waic$waic),
                            DIC = c(mod_lg_f_null$dic$dic,
                                    mod_lg_f_A$dic$dic,
                                    mod_lg_f_P$dic$dic,
                                    mod_lg_f_C$dic$dic,
                                    mod_lg_f_AP$dic$dic,
                                    mod_lg_f_AC$dic$dic,
                                    mod_lg_f_PC$dic$dic))

write.csv(lg_f_criteria, file = paste0("tables/SO_Q1/",
                                       "lg_f_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q1/mod_lg_f_summaries.txt")
summary(mod_lg_f_null)
summary(mod_lg_f_A)
summary(mod_lg_f_P)
summary(mod_lg_f_C)
summary(mod_lg_f_AP)
summary(mod_lg_f_AC)
summary(mod_lg_f_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_lg_f_AC <- inla.posterior.sample(n = 1000, mod_lg_f_AC)

post_lg_f_AC_df <- lapply(post_lg_f_AC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_f_AC_df <- do.call(rbind.data.frame, post_lg_f_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_f_AC_plot <- post_lg_f_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_lg_f_AC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))


lg_f_AC_age_lo <- post_lg_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_AC_age_logodds.png", lg_f_AC_age_lo)


lg_f_AC_coh_lo <- post_lg_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_AC_coh_logodds.png", lg_f_AC_coh_lo)


lg_f_AC_age_p <- post_lg_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_AC_age_prev.png", lg_f_AC_age_p)


lg_f_AC_coh_p <- post_lg_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_AC_coh_prev.png", lg_f_AC_coh_p)


#### PC ####

post_lg_f_PC <- inla.posterior.sample(n = 1000, mod_lg_f_PC)

post_lg_f_PC_df <- lapply(post_lg_f_PC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("period_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("period_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_f_PC_df <- do.call(rbind.data.frame, post_lg_f_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_f_PC_plot <- post_lg_f_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_f_PC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "period", "Period", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Period", period_idx$period[term_idx],
                                cohort_idx$cohort[term_idx]))


lg_f_PC_per_lo <- post_lg_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_PC_per_logodds.png", lg_f_PC_per_lo)


lg_f_PC_coh_lo <- post_lg_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_PC_coh_logodds.png", lg_f_PC_coh_lo)


lg_f_PC_per_p <- post_lg_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_PC_per_prev.png", lg_f_PC_per_p)


lg_f_PC_coh_p <- post_lg_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_PC_coh_prev.png", lg_f_PC_coh_p)


#### AP ####

post_lg_f_AP <- inla.posterior.sample(n = 1000, mod_lg_f_AP)

post_lg_f_AP_df <- lapply(post_lg_f_AP, function(draw){
  out <- draw$latent[grepl("period_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("period_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_f_AP_df <- do.call(rbind.data.frame, post_lg_f_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_f_AP_plot <- post_lg_f_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_lg_f_AP_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Period")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                period_idx$period[term_idx]))

lg_f_AP_age_lo <- post_lg_f_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_AP_age_logodds.png", lg_f_AC_age_lo)


lg_f_AP_per_lo <- post_lg_f_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_AP_per_logodds.png", lg_f_AP_per_lo)



#### C ####

post_lg_f_C <- inla.posterior.sample(n = 1000, mod_lg_f_C)

post_lg_f_C_df <- lapply(post_lg_f_C, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_f_C_df <- do.call(rbind.data.frame, post_lg_f_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_f_C_plot <- post_lg_f_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_f_C_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "cohort", "Cohort", term)) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))



lg_f_C_coh_lo <- post_lg_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_C_coh_logodds.png", lg_f_C_coh_lo)

lg_f_C_coh_p <- post_lg_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_f_C_coh_prev.png", lg_f_C_coh_p)

## Gay ####

mod_lg_m_null <- inla(mod_null,
                      data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                             config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_lg_m_P <- inla(mod_P,
                   data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_lg_m_C <- inla(mod_C,
                   data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_lg_m_A <- inla(mod_A,
                   data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_lg_m_AP <- inla(mod_AP,
                    data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_lg_m_AC <- inla(mod_AC,
                    data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_lg_m_PC <- inla(mod_PC,
                    data = mod_data %>% filter(so == "lesgay" & sex == "Male"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

#### Tables ####
lg_m_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_lg_m_null$mlik[2],
                                       mod_lg_m_A$mlik[2],
                                       mod_lg_m_P$mlik[2],
                                       mod_lg_m_C$mlik[2],
                                       mod_lg_m_AP$mlik[2],
                                       mod_lg_m_AC$mlik[2],
                                       mod_lg_m_PC$mlik[2]),
                            WAIC = c(mod_lg_m_null$waic$waic,
                                     mod_lg_m_A$waic$waic,
                                     mod_lg_m_P$waic$waic,
                                     mod_lg_m_C$waic$waic,
                                     mod_lg_m_AP$waic$waic,
                                     mod_lg_m_AC$waic$waic,
                                     mod_lg_m_PC$waic$waic),
                            DIC = c(mod_lg_m_null$dic$dic,
                                    mod_lg_m_A$dic$dic,
                                    mod_lg_m_P$dic$dic,
                                    mod_lg_m_C$dic$dic,
                                    mod_lg_m_AP$dic$dic,
                                    mod_lg_m_AC$dic$dic,
                                    mod_lg_m_PC$dic$dic))

write.csv(lg_m_criteria, file = paste0("tables/SO_Q1/",
                                       "lg_m_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q1/mod_lg_m_summaries.txt")
summary(mod_lg_m_null)
summary(mod_lg_m_A)
summary(mod_lg_m_P)
summary(mod_lg_m_C)
summary(mod_lg_m_AP)
summary(mod_lg_m_AC)
summary(mod_lg_m_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_lg_m_AC <- inla.posterior.sample(n = 1000, mod_lg_m_AC)

post_lg_m_AC_df <- lapply(post_lg_m_AC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_m_AC_df <- do.call(rbind.data.frame, post_lg_m_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_m_AC_plot <- post_lg_m_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_lg_m_AC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))


lg_m_AC_age_lo <- post_lg_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_AC_age_logodds.png", lg_m_AC_age_lo)


lg_m_AC_coh_lo <- post_lg_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_AC_coh_logodds.png", lg_m_AC_coh_lo)


lg_m_AC_age_p <- post_lg_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_AC_age_prev.png", lg_m_AC_age_p)


lg_m_AC_coh_p <- post_lg_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_AC_coh_prev.png", lg_m_AC_coh_p)

#### PC ####

post_lg_m_PC <- inla.posterior.sample(n = 1000, mod_lg_m_PC)

post_lg_m_PC_df <- lapply(post_lg_m_PC, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("period_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("period_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_m_PC_df <- do.call(rbind.data.frame, post_lg_m_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_m_PC_plot <- post_lg_m_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_m_PC_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "period", "Period", "Cohort")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Period", period_idx$period[term_idx],
                                cohort_idx$cohort[term_idx]))


lg_m_PC_per_lo <- post_lg_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_PC_per_logodds.png", lg_m_PC_per_lo)


lg_m_PC_coh_lo <- post_lg_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_PC_coh_logodds.png", lg_m_PC_coh_lo)


lg_m_PC_per_p <- post_lg_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_PC_per_prev.png", lg_m_PC_per_p)


lg_m_PC_coh_p <- post_lg_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_PC_coh_prev.png", lg_m_PC_coh_p)

#### AP ####

post_lg_m_AP <- inla.posterior.sample(n = 1000, mod_lg_m_AP)

post_lg_m_AP_df <- lapply(post_lg_m_AP, function(draw){
  out <- draw$latent[grepl("period_idx", rownames(draw$latent)) |
                       grepl("age_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("period_idx", rownames(draw$latent)) |
      grepl("age_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_m_AP_df <- do.call(rbind.data.frame, post_lg_m_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_m_AP_plot <- post_lg_m_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_lg_m_AP_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "age", "Age", "Period")) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                period_idx$period[term_idx]))

lg_m_AP_age_lo <- post_lg_m_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Gay male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_AP_age_logodds.png", lg_m_AC_age_lo)


lg_m_AP_per_lo <- post_lg_m_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Gay male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_AP_per_logodds.png", lg_f_AP_per_lo)

#### C ####

post_lg_m_C <- inla.posterior.sample(n = 1000, mod_lg_m_C)

post_lg_m_C_df <- lapply(post_lg_m_C, function(draw){
  out <- draw$latent[grepl("cohort_idx", rownames(draw$latent)) |
                       grepl("(Intercept)", rownames(draw$latent))]
  out_names <- rownames(draw$latent)[
    grepl("cohort_idx", rownames(draw$latent)) |
      grepl("(Intercept)", rownames(draw$latent))]
  out_df <- data.frame(rowname = out_names,
                       draw = out) %>% 
    mutate(term = str_split_i(rowname, "_", 1),
           term_idx = str_split_i(rowname, ":", 2),
           term = ifelse(grepl("Intercept", term), "Intercept", term))
  return(out_df)
})

post_lg_m_C_df <- do.call(rbind.data.frame, post_lg_m_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_lg_m_C_plot <- post_lg_m_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_bi_m_C_df %>% 
              filter(term == "Intercept"),
            by = "draw_no",
            suffix = c("", "_int")) %>% 
  group_by(term, term_idx, draw_no) %>% 
  dplyr::summarize(draw_sum = draw + draw_int,
                   term_idx = as.numeric(term_idx),
                   term = ifelse(term == "cohort", "Cohort", term)) %>% 
  ungroup() %>% 
  group_by(term, term_idx) %>% 
  dplyr::summarize(med_lo = median(draw_sum),
                   lower_lo = quantile(draw_sum, 0.025),
                   upper_lo = quantile(draw_sum, 0.975),
                   med_p = median(expit(draw_sum)),
                   lower_p = quantile(expit(draw_sum), 0.025),
                   upper_p = quantile(expit(draw_sum), 0.975)) %>% 
  mutate(term_idx_name = ifelse(term == "Age", age_idx$age[term_idx],
                                cohort_idx$cohort[term_idx]))



lg_m_C_coh_lo <- post_lg_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_C_coh_logodds.png", lg_m_C_coh_lo)

lg_m_C_coh_p <- post_lg_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Gay") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q1/LG_m_C_coh_prev.png", lg_m_C_coh_p)

# Manuscript Plots ####

## AC ####
AC_ranges <- bind_rows(post_bi_f_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual females"),
                       post_bi_m_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual males"),
                       post_lg_f_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Lesbians"),
                       post_lg_m_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Gay males"))
## Across groups 
AC_ranges %>% 
  group_by(term) %>% 
  dplyr::summarize(lower = min(lower),
            upper = max(upper))

AC_ylims <- c(-7.5, 1)
A_xlims <- c(14, 79) + c(-1, 1)
C_xlims <- c(1935, 2007) + c(-1,1)


# par(mfrow = c(2,4),lend = 1)

# sgac

png("plots/SO_Q1/AC_8panel_logodds.png", 
    width = 300*8, height = 300*6, res=300)
{
  par(mfrow = c(2,4), lend = 1)
  par(mar=c(5,0,2,0))
  par(oma=c(0,4,2,1))
   
  ## Age 
  {
    { 
      ## Bi F
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("a. Bisexual female", x=13, y=1, cex=.9, adj=0)
      # title("Age", adj = 0, line = 4)
      #mtext("Log Odds", adj = 0.275, side = 2,
      #      line = 2.5, cex = 0.8, outer = TRUE)
      #mtext("Log Odds", adj = 0.85, side = 2,
      #      line = 2.5, cex = 0.8, outer = TRUE)
      mtext("Age", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
      
      ## Age axis
      #axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = .9)
      mtext("Age", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      axis(2, at = seq(-7, 1, 1), cex=0.5, labels = FALSE)
      mtext(side = 2, seq(-7, 1, 1), at = seq(-7, 1, 1), 
            cex=0.5, line=0.5)
      mtext("Log Odds", side = 2, line = 2, cex = 0.6)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_bi_f_AC_plot$term_idx_name[post_bi_f_AC_plot$term == "Age"],
            post_bi_f_AC_plot$med_lo[post_bi_f_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_bi_f_AC_plot$term_idx_name[post_bi_f_AC_plot$term == "Age"],
                    rev(post_bi_f_AC_plot$term_idx_name[post_bi_f_AC_plot$term == 
                                                          "Age"])),
              y = c(post_bi_f_AC_plot$upper_lo[post_bi_f_AC_plot$term == "Age"],
                    rev(post_bi_f_AC_plot$lower_lo[post_bi_f_AC_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Bi M
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("b. Bisexual male", x=13, y=1, cex=.9, adj=0)
      
      ## Age axis
      #axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      mtext("Age", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_bi_m_AC_plot$term_idx_name[post_bi_m_AC_plot$term == "Age"],
            post_bi_m_AC_plot$med_lo[post_bi_m_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_bi_m_AC_plot$term_idx_name[post_bi_m_AC_plot$term == "Age"],
                    rev(post_bi_m_AC_plot$term_idx_name[post_bi_m_AC_plot$term == 
                                                          "Age"])),
              y = c(post_bi_m_AC_plot$upper_lo[post_bi_m_AC_plot$term == "Age"],
                    rev(post_bi_m_AC_plot$lower_lo[post_bi_m_AC_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## LG F
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("c. Lesbian", x=13, y=1, cex=.9, adj=0)
      
      ## Age axis
      mtext("Age", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
            cex=0.5, line=0.5)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_lg_f_AC_plot$term_idx_name[post_lg_f_AC_plot$term == "Age"],
            post_lg_f_AC_plot$med_lo[post_lg_f_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_lg_f_AC_plot$term_idx_name[post_lg_f_AC_plot$term == "Age"],
                    rev(post_lg_f_AC_plot$term_idx_name[post_lg_f_AC_plot$term == 
                                                          "Age"])),
              y = c(post_lg_f_AC_plot$upper_lo[post_lg_f_AC_plot$term == "Age"],
                    rev(post_lg_f_AC_plot$lower_lo[post_lg_f_AC_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## LG M
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("d. Gay male", x=13, y=1, cex=.9, adj=0)
      
      ## Age axis
      #axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      mtext("Age", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_lg_m_AC_plot$term_idx_name[post_lg_m_AC_plot$term == "Age"],
            post_lg_m_AC_plot$med_lo[post_lg_m_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_lg_m_AC_plot$term_idx_name[post_lg_m_AC_plot$term ==
                                                      "Age"],
                    rev(post_lg_m_AC_plot$term_idx_name[post_lg_m_AC_plot$term == 
                                                          "Age"])),
              y = c(post_lg_m_AC_plot$upper_lo[post_lg_m_AC_plot$term == "Age"],
                    rev(post_lg_m_AC_plot$lower_lo[post_lg_m_AC_plot$term == 
                                                     "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
  }
  
  
  ## Cohort 
  {
    { 
      ## Bi F
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("e. Bisexual female", x=1935, y=1, cex=.9, adj=0)
      mtext("Cohort", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
      
      ## Age axis
      #axis(1, at = seq(1940, 2010, 10))
      mtext("Cohort", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
            cex=0.5, line=0.5)
      # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
      
      ## Log odds axis
      axis(2, at = seq(-7, 1, 1), cex=0.5, labels = FALSE)
      mtext(side = 2, seq(-7, 1, 1), at = seq(-7, 1, 1), 
            cex=0.5, line=0.5)
      mtext("Log Odds", side = 2, line = 2, cex = 0.6)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_bi_f_AC_plot$term_idx_name[post_bi_f_AC_plot$term ==
                                              "Cohort"],
            post_bi_f_AC_plot$med_lo[post_bi_f_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_bi_f_AC_plot$term_idx_name[post_bi_f_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_bi_f_AC_plot$term_idx_name[post_bi_f_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_bi_f_AC_plot$upper_lo[post_bi_f_AC_plot$term == 
                                                 "Cohort"],
                    rev(post_bi_f_AC_plot$lower_lo[post_bi_f_AC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Bi M
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("f. Bisexual male", x=1935, y=1, cex=.9, adj=0)
      
      ## Age axis
      #axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
      mtext("Cohort", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_bi_m_AC_plot$term_idx_name[post_bi_m_AC_plot$term == "Cohort"],
            post_bi_m_AC_plot$med_lo[post_bi_m_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_bi_m_AC_plot$term_idx_name[post_bi_m_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_bi_m_AC_plot$term_idx_name[post_bi_m_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_bi_m_AC_plot$upper_lo[post_bi_m_AC_plot$term == 
                                                 "Cohort"],
                    rev(post_bi_m_AC_plot$lower_lo[post_bi_m_AC_plot$term ==
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## LG F
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("g. Lesbian", x=1935, y=1, cex=.9, adj=0)
      
      ## Age axis
      #axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      mtext("Cohort", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_lg_f_AC_plot$term_idx_name[post_lg_f_AC_plot$term == "Cohort"],
            post_lg_f_AC_plot$med_lo[post_lg_f_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_lg_f_AC_plot$term_idx_name[post_lg_f_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_lg_f_AC_plot$term_idx_name[post_lg_f_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_lg_f_AC_plot$upper_lo[post_lg_f_AC_plot$term == "Cohort"],
                    rev(post_lg_f_AC_plot$lower_lo[post_lg_f_AC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## LG M
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("h. Gay male", x=1935, y=1, cex=.9, adj=0)
      
      ## Age axis
      #axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      mtext("Cohort", side = 1, line = 2, cex=0.6)
      axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
      mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_lg_m_AC_plot$term_idx_name[post_lg_m_AC_plot$term == "Cohort"],
            post_lg_m_AC_plot$med_lo[post_lg_m_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_lg_m_AC_plot$term_idx_name[post_lg_m_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_lg_m_AC_plot$term_idx_name[post_lg_m_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_lg_m_AC_plot$upper_lo[post_lg_m_AC_plot$term == "Cohort"],
                    rev(post_lg_m_AC_plot$lower_lo[post_lg_m_AC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

## PC ####
PC_ranges <- bind_rows(post_bi_f_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual females"),
                       post_bi_m_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual males"),
                       post_lg_f_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Lesbians"),
                       post_lg_m_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Gay males"))
## Across groups 
PC_ranges %>% 
  group_by(term) %>% 
  dplyr::summarize(lower = min(lower),
            upper = max(upper))

PC_ylims <- c(-7.5, 1)
P_xlims <- c(2014, 2021) + c(-1, 1)
C_xlims <- c(1935, 2007) + c(-1,1)
P_labs <- c("\'14","\'15","\'16","\'17","\'18","\'19","\'20","\'21")



# par(mfrow = c(2,4),lend = 1)

# sgpc

png("plots/SO_Q1/PC_8panel_logodds.png", 
    width = 300*8, height = 300*6, res=300)
{
  par(mfrow = c(2,4), lend = 1)
  par(mar=c(5,0,2,0))
  par(oma=c(0,4,2,1))
  
  ## Period 
  {
    { 
      ## Bi F

      plot(NA, xlim = P_xlims, ylim = PC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)

      ## title
      text("a. Bisexual female", x=2013, y=1, cex=.9, adj=0)
      mtext("Period", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
      
      ## Period axis
      mtext("Period", side = 1, line = 2, cex=0.6)
      axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
      mtext(side = 1, P_labs, at = 2014:2021, 
            cex=0.5, line=0.5)
      
      ## Log odds axis
      axis(2, at = seq(-7, 1, 1), cex=0.5, labels = FALSE)
      mtext(side = 2, seq(-7, 1, 1), at = seq(-7, 1, 1), 
            cex=0.5, line=0.5)
      mtext("Log Odds", side = 2, line = 2, cex = 0.6)
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_bi_f_PC_plot$term_idx_name[post_bi_f_PC_plot$term == "Period"],
            post_bi_f_PC_plot$med_lo[post_bi_f_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_bi_f_PC_plot$term_idx_name[post_bi_f_PC_plot$term == "Period"],
                    rev(post_bi_f_PC_plot$term_idx_name[post_bi_f_PC_plot$term == 
                                                          "Period"])),
              y = c(post_bi_f_PC_plot$upper_lo[post_bi_f_PC_plot$term ==
                                                 "Period"],
                    rev(post_bi_f_PC_plot$lower_lo[post_bi_f_PC_plot$term ==
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Bi M
      
      plot(NA, xlim = P_xlims, ylim = PC_ylims,
           xlab = "", ylab = "",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)

      ## title
      text("b. Bisexual male", x=2013, y=1, cex=.9, adj=0)

      ## Period axis
      mtext("Period", side = 1, line = 2, cex=0.6)
      axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
      mtext(side = 1, P_labs, at = 2014:2021, 
            cex=0.5, line=0.5)
      
      ## y = 0
      abline(h = 0)

      
      ## Ests
      lines(post_bi_m_PC_plot$term_idx_name[post_bi_m_PC_plot$term == "Period"],
            post_bi_m_PC_plot$med_lo[post_bi_m_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_bi_m_PC_plot$term_idx_name[post_bi_m_PC_plot$term == "Period"],
                    rev(post_bi_m_PC_plot$term_idx_name[post_bi_m_PC_plot$term == 
                                                          "Period"])),
              y = c(post_bi_m_PC_plot$upper_lo[post_bi_m_PC_plot$term == "Period"],
                    rev(post_bi_m_PC_plot$lower_lo[post_bi_m_PC_plot$term == "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## LG F
       plot(NA, xlim = P_xlims, ylim = PC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("c. Lesbian", x=2013, y=1, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, P_labs, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
    
      ## Ests
      lines(post_lg_f_PC_plot$term_idx_name[post_lg_f_PC_plot$term == 
                                              "Period"],
            post_lg_f_PC_plot$med_lo[post_lg_f_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_lg_f_PC_plot$term_idx_name[post_lg_f_PC_plot$term == 
                                                      "Period"],
                    rev(post_lg_f_PC_plot$term_idx_name[post_lg_f_PC_plot$term == 
                                                          "Period"])),
              y = c(post_lg_f_PC_plot$upper_lo[post_lg_f_PC_plot$term ==
                                                 "Period"],
                    rev(post_lg_f_PC_plot$lower_lo[post_lg_f_PC_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## LG M
       plot(NA, xlim = P_xlims, ylim = PC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("d. Gay male", x=2013, y=1, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, P_labs, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_m_PC_plot$term_idx_name[post_lg_m_PC_plot$term ==
                                              "Period"],
            post_lg_m_PC_plot$med_lo[post_lg_m_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_lg_m_PC_plot$term_idx_name[post_lg_m_PC_plot$term ==
                                                      "Period"],
                    rev(post_lg_m_PC_plot$term_idx_name[post_lg_m_PC_plot$term == 
                                                          "Period"])),
              y = c(post_lg_m_PC_plot$upper_lo[post_lg_m_PC_plot$term == "Period"],
                    rev(post_lg_m_PC_plot$lower_lo[post_lg_m_PC_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
  }
  
  ## Cohort 
  {
    { 
      ## Bi F
       plot(NA, xlim = C_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("e. Bisexual female", x=1935, y=1, cex=.9, adj=0)
       mtext("Cohort", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## Cohort axis
       #axis(1, at = seq(1940, 2010, 10))
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
       
       ## Log odds axis
       axis(2, at = seq(-7, 1, 1), cex=0.5, labels = FALSE)
       mtext(side = 2, seq(-7, 1, 1), at = seq(-7, 1, 1), 
             cex=0.5, line=0.5)
       mtext("Log Odds", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_bi_f_PC_plot$term_idx_name[post_bi_f_PC_plot$term ==
                                              "Cohort"],
            post_bi_f_PC_plot$med_lo[post_bi_f_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_bi_f_PC_plot$term_idx_name[post_bi_f_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_bi_f_PC_plot$term_idx_name[post_bi_f_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_bi_f_PC_plot$upper_lo[post_bi_f_PC_plot$term == 
                                                 "Cohort"],
                    rev(post_bi_f_PC_plot$lower_lo[post_bi_f_PC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Bi M
       plot(NA, xlim = C_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("f. Bisexual male", x=1935, y=1, cex=.9, adj=0)
       
       ## Age axis
       #axis(1, at = seq(1940, 2010, 10))
       # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       # axis(2, at = seq(-7, 1, 1), )
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_bi_m_PC_plot$term_idx_name[post_bi_m_PC_plot$term == "Cohort"],
            post_bi_m_PC_plot$med_lo[post_bi_m_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_bi_m_PC_plot$term_idx_name[post_bi_m_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_bi_m_PC_plot$term_idx_name[post_bi_m_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_bi_m_PC_plot$upper_lo[post_bi_m_PC_plot$term == 
                                                 "Cohort"],
                    rev(post_bi_m_PC_plot$lower_lo[post_bi_m_PC_plot$term ==
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## LG F
       plot(NA, xlim = C_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("g. Lesbian", x=1935, y=1, cex=.9, adj=0)
       
       ## Age axis
       #axis(1, at = seq(1940, 2010, 10))
       # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       # axis(2, at = seq(-7, 1, 1))
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_f_PC_plot$term_idx_name[post_lg_f_PC_plot$term == "Cohort"],
            post_lg_f_PC_plot$med_lo[post_lg_f_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_lg_f_PC_plot$term_idx_name[post_lg_f_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_lg_f_PC_plot$term_idx_name[post_lg_f_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_lg_f_PC_plot$upper_lo[post_lg_f_PC_plot$term == "Cohort"],
                    rev(post_lg_f_PC_plot$lower_lo[post_lg_f_PC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## LG M
       plot(NA, xlim = C_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("h. Gay male", x=1935, y=1, cex=.9, adj=0)
       
       ## Age axis
       #axis(1, at = seq(1940, 2010, 10))
       # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
       mtext("Cohort", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(1940, 2000, 10), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(1940, 2000, 10), at = seq(1940, 2000, 10), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       # axis(2, at = seq(-7, 1, 1))
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_m_PC_plot$term_idx_name[post_lg_m_PC_plot$term == "Cohort"],
            post_lg_m_PC_plot$med_lo[post_lg_m_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_lg_m_PC_plot$term_idx_name[post_lg_m_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_lg_m_PC_plot$term_idx_name[post_lg_m_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_lg_m_PC_plot$upper_lo[post_lg_m_PC_plot$term == "Cohort"],
                    rev(post_lg_m_PC_plot$lower_lo[post_lg_m_PC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
}
dev.off()


## AP ####
AP_ranges <- bind_rows(post_bi_f_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual females"),
                       post_bi_m_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual males"),
                       post_lg_f_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Lesbians"),
                       post_lg_m_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Gay males"))
## Across groups 
AP_ranges %>% 
  group_by(term) %>% 
  dplyr::summarize(lower = min(lower),
            upper = max(upper))

AP_ylims <- c(-7.5, 1)
A_xlims <- c(14, 79) + c(-1, 1)
P_xlims <- c(2014, 2021) + c(-1,1)

# sgap

png("plots/SO_Q1/AP_8panel_logodds.png", 
      width = 300*8, height = 300*6, res=300)
  {
     par(mfrow = c(2,4), lend = 1)
     par(mar=c(5,0,2,0))
     par(oma=c(0,4,2,1))
     
  
  ## Age 
  {
    { 
      ## Bi F
       plot(NA, xlim = A_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("a. Bisexual female", x=13, y=1, cex=.9, adj=0)
       # title("Age", adj = 0, line = 4)
       #mtext("Log Odds", adj = 0.275, side = 2,
       #      line = 2.5, cex = 0.8, outer = TRUE)
       #mtext("Log Odds", adj = 0.85, side = 2,
       #      line = 2.5, cex = 0.8, outer = TRUE)
       mtext("Age", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## Age axis
       #axis(1, at = seq(15, 75, 5))
       # mtext(side = 1, text = "Age", line = 2, cex = .9)
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       axis(2, at = seq(-7, 1, 1), cex=0.5, labels = FALSE)
       mtext(side = 2, seq(-7, 1, 1), at = seq(-7, 1, 1), 
             cex=0.5, line=0.5)
       mtext("Log Odds", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## y = 0
      # abline(h = 0)
      
      ## Ests
      lines(post_bi_f_AP_plot$term_idx_name[post_bi_f_AP_plot$term == "Age"],
            post_bi_f_AP_plot$med_lo[post_bi_f_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_bi_f_AP_plot$term_idx_name[post_bi_f_AP_plot$term == "Age"],
                    rev(post_bi_f_AP_plot$term_idx_name[post_bi_f_AP_plot$term == 
                                                          "Age"])),
              y = c(post_bi_f_AP_plot$upper_lo[post_bi_f_AP_plot$term == "Age"],
                    rev(post_bi_f_AP_plot$lower_lo[post_bi_f_AP_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Bi M
       plot(NA, xlim = A_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("b. Bisexual male", x=13, y=1, cex=.9, adj=0)
       
       ## Age axis
       #axis(1, at = seq(15, 75, 5))
       # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       # axis(2, at = seq(-7, 1, 1), )
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_bi_m_AP_plot$term_idx_name[post_bi_m_AP_plot$term == "Age"],
            post_bi_m_AP_plot$med_lo[post_bi_m_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_bi_m_AP_plot$term_idx_name[post_bi_m_AP_plot$term == "Age"],
                    rev(post_bi_m_AP_plot$term_idx_name[post_bi_m_AP_plot$term == 
                                                          "Age"])),
              y = c(post_bi_m_AP_plot$upper_lo[post_bi_m_AP_plot$term == "Age"],
                    rev(post_bi_m_AP_plot$lower_lo[post_bi_m_AP_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## LG F
       plot(NA, xlim = A_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("c. Lesbian", x=13, y=1, cex=.9, adj=0)
       
       ## Age axis
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
       
       ## Log odds axis
       # axis(2, at = seq(-7, 1, 1))
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_f_AP_plot$term_idx_name[post_lg_f_AP_plot$term == "Age"],
            post_lg_f_AP_plot$med_lo[post_lg_f_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_lg_f_AP_plot$term_idx_name[post_lg_f_AP_plot$term == "Age"],
                    rev(post_lg_f_AP_plot$term_idx_name[post_lg_f_AP_plot$term == 
                                                          "Age"])),
              y = c(post_lg_f_AP_plot$upper_lo[post_lg_f_AP_plot$term == "Age"],
                    rev(post_lg_f_AP_plot$lower_lo[post_lg_f_AP_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## LG M
       plot(NA, xlim = A_xlims, ylim = AC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       ## title
       text("d. Gay male", x=13, y=1, cex=.9, adj=0)
       
       ## Age axis
       #axis(1, at = seq(15, 75, 5))
       # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
       mtext("Age", side = 1, line = 2, cex=0.6)
       axis(1, at = seq(15, 75, 5), cex.axis=0.6, labels=FALSE)
       mtext(side = 1, seq(15, 75, 5), at = seq(15, 75, 5), 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       # axis(2, at = seq(-7, 1, 1))
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_m_AP_plot$term_idx_name[post_lg_m_AP_plot$term == "Age"],
            post_lg_m_AP_plot$med_lo[post_lg_m_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_lg_m_AP_plot$term_idx_name[post_lg_m_AP_plot$term ==
                                                      "Age"],
                    rev(post_lg_m_AP_plot$term_idx_name[post_lg_m_AP_plot$term == 
                                                          "Age"])),
              y = c(post_lg_m_AP_plot$upper_lo[post_lg_m_AP_plot$term == "Age"],
                    rev(post_lg_m_AP_plot$lower_lo[post_lg_m_AP_plot$term == 
                                                     "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
  }
  
  
  ## Period 
  {
    { 
       plot(NA, xlim = P_xlims, ylim = PC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("e. Bisexual female", x=2013, y=1, cex=.9, adj=0)
       mtext("Period", adj = 0, side = 3, line = 0.5, outer = FALSE, cex=0.8)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, P_labs, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## Log odds axis
       axis(2, at = seq(-7, 1, 1), cex=0.5, labels = FALSE)
       mtext(side = 2, seq(-7, 1, 1), at = seq(-7, 1, 1), 
             cex=0.5, line=0.5)
       mtext("Log Odds", side = 2, line = 2, cex = 0.6)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_bi_f_AP_plot$term_idx_name[post_bi_f_AP_plot$term == "Period"],
            post_bi_f_AP_plot$med_lo[post_bi_f_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_bi_f_AP_plot$term_idx_name[post_bi_f_AP_plot$term == "Period"],
                    rev(post_bi_f_AP_plot$term_idx_name[post_bi_f_AP_plot$term == 
                                                          "Period"])),
              y = c(post_bi_f_AP_plot$upper_lo[post_bi_f_AP_plot$term ==
                                                 "Period"],
                    rev(post_bi_f_AP_plot$lower_lo[post_bi_f_AP_plot$term ==
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Bi M
       plot(NA, xlim = P_xlims, ylim = PC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("f. Bisexual male", x=2013, y=1, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, P_labs, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_bi_m_AP_plot$term_idx_name[post_bi_m_AP_plot$term == "Period"],
            post_bi_m_AP_plot$med_lo[post_bi_m_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_bi_m_AP_plot$term_idx_name[post_bi_m_AP_plot$term == "Period"],
                    rev(post_bi_m_AP_plot$term_idx_name[post_bi_m_AP_plot$term == 
                                                          "Period"])),
              y = c(post_bi_m_AP_plot$upper_lo[post_bi_m_AP_plot$term == "Period"],
                    rev(post_bi_m_AP_plot$lower_lo[post_bi_m_AP_plot$term == "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## LG F
       plot(NA, xlim = P_xlims, ylim = PC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("g. Lesbian", x=2013, y=1, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, P_labs, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_f_AP_plot$term_idx_name[post_lg_f_AP_plot$term == 
                                              "Period"],
            post_lg_f_AP_plot$med_lo[post_lg_f_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_lg_f_AP_plot$term_idx_name[post_lg_f_AP_plot$term == 
                                                      "Period"],
                    rev(post_lg_f_AP_plot$term_idx_name[post_lg_f_AP_plot$term == 
                                                          "Period"])),
              y = c(post_lg_f_AP_plot$upper_lo[post_lg_f_AP_plot$term ==
                                                 "Period"],
                    rev(post_lg_f_AP_plot$lower_lo[post_lg_f_AP_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## LG M
       plot(NA, xlim = P_xlims, ylim = PC_ylims,
            xlab = "", ylab = "",
            main = "", type = "n", frame.plot = TRUE, axes = FALSE)
       
       ## title
       text("h. Gay male", x=2013, y=1, cex=.9, adj=0)
       
       ## Period axis
       mtext("Period", side = 1, line = 2, cex=0.6)
       axis(1, at = 2014:2021, cex.axis=0.6, labels=FALSE)
       mtext(side = 1, P_labs, at = 2014:2021, 
             cex=0.5, line=0.5)
       
       ## y = 0
       abline(h = 0)
       
      ## Ests
      lines(post_lg_m_AP_plot$term_idx_name[post_lg_m_AP_plot$term ==
                                              "Period"],
            post_lg_m_AP_plot$med_lo[post_lg_m_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_lg_m_AP_plot$term_idx_name[post_lg_m_AP_plot$term ==
                                                      "Period"],
                    rev(post_lg_m_AP_plot$term_idx_name[post_lg_m_AP_plot$term == 
                                                          "Period"])),
              y = c(post_lg_m_AP_plot$upper_lo[post_lg_m_AP_plot$term == "Period"],
                    rev(post_lg_m_AP_plot$lower_lo[post_lg_m_AP_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

