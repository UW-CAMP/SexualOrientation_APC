## Fit null, A, P, C, AP, AC, PC models
## Prevalence by age x period (survey) x cohort
## Jessica Godwin

# Setup ####
rm(list = ls())

if(!dir.exists("tables/SO_Q5")){
  if(!dir.exists("tables/")){
    dir.create("tables/")
  }
  dir.create("tables/SO_Q5")
}

if(!dir.exists("plots/SO_Q5")){
  if(!dir.exists("plots/")){
    dir.create("plots/")
  }
  dir.create("plots/SO_Q5")
}

# ## Libraries ####
# install.packages("INLA",
#                  repos=c(getOption("repos"),
#                          INLA="https://inla.r-inla-download.org/R/stable"))
library(INLA) 
library(tidyverse)
library(ggpubr)


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
prev_data_AP_yrbs <- read.csv("data - clean/YRBS_HT_AP_prevs.csv")
prev_data_AP_yrbs_2021 <- read.csv("data - clean/YRBS2021_HT_AP_prevs.csv")

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


# YRBS 2021 descriptives ####

yrbs_f <- prev_data_AP_yrbs_2021 %>%
  bind_rows(prev_data_AP_yrbs %>% 
              filter(period != 2021)) %>% 
  filter(so != "lesgay" & so != "bi") %>% 
  mutate(so = case_when(so == "dko" ~ "Not sure",
                        so == "dkns" ~ "Not sure about sexual identity",
                        so == "ref" ~ "Decline to answer",
                        so == "pnta" ~ "Decline to answer",
                        so == "dkwtm" ~ "Don't know what this means",
                        TRUE  ~ "Some other way"),
         so = factor(so, levels = c("Not sure", 
                                    "Not sure about sexual identity",
                                    "Some other way",
                                    "Don't know what this means",
                                    "Decline to answer")), 
         period = factor(period)) %>% 
  filter(sex == "Female") %>% 
  group_by(age, period) %>% 
  mutate(pos = round(sum(mean),3) + 0.01,
         lab = paste0(round(sum(mean)*100, 1), "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = period, y = mean, group = so, fill = so), color = "white") +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = lab, y = pos), size = 3) +
  scale_fill_manual(name = "Sexual Orientation",
                    values = c("Not sure" = "navy",
                               "Not sure about sexual identity" = alpha("navy", 0.65),
                               "Some other way" = alpha("navy", 0.45),
                               "Don't know what this means" = alpha("navy", 0.25), 
                               "Decline to answer" = "grey50")) +
  facet_wrap(~age) +
  xlab("Period") +
  ylab("Prevalence") +
  ggtitle("Female") +
  theme_classic() +
  ylim(c(0,.2))

ggsave("plots/SO_Q5/YRBS_f_dko_pnta.png", yrbs_f)


yrbs_m <- prev_data_AP_yrbs_2021 %>%
  bind_rows(prev_data_AP_yrbs %>% 
              filter(period != 2021)) %>% 
  filter(so != "lesgay" & so != "bi") %>% 
  mutate(so = case_when(so == "dko" ~ "Not sure",
                        so == "dkns" ~ "Not sure about sexual identity",
                        so == "ref" ~ "Decline to answer",
                        so == "pnta" ~ "Decline to answer",
                        so == "dkwtm" ~ "Don't know what this means",
                        TRUE  ~ "Some other way"),
         so = factor(so, levels = c("Not sure",
                                    "Not sure about sexual identity",
                                    "Some other way",
                                    "Don't know what this means",
                                    "Decline to answer")), 
         period = factor(period)) %>% 
  filter(sex == "Male") %>% 
  group_by(age, period) %>% 
  mutate(pos = round(sum(mean),3) + 0.01,
         lab = paste0(round(sum(mean)*100, 1), "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = period, y = mean, group = so, fill = so), color = "white") +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = lab, y = pos), size = 3) +
  scale_fill_manual(name = "Sexual Orientation",
                    values = c("Not sure" = "navy",
                               "Not sure about sexual identity" = alpha("navy", 0.65),
                               "Some other way" = alpha("navy", 0.45),
                               "Don't know what this means" = alpha("navy", 0.25), 
                               "Decline to answer" = "grey50")) +
  facet_wrap(~age) +
  xlab("Period") +
  ylab("Prevalence") +
  ylim(c(0, 0.2)) +
  ggtitle("Male") +
  theme_classic()

ggsave("plots/SO_Q5/YRBS_m_dko_pnta.png", yrbs_m)


yrbs_both <- ggarrange(yrbs_f, yrbs_m, nrow = 1, ncol = 2, 
                       common.legend = TRUE, legend = "right",
                       labels = c("a", "b"))

ggsave(plot = yrbs_both, filename = "plots/SO_Q5/YRBS_both_dko_pnta.png")

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



## DKO: Female ####

mod_dko_f_null <- inla(mod_null,
                      data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_dko_f_P <- inla(mod_P,
                   data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_dko_f_C <- inla(mod_C,
                   data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_dko_f_A <- inla(mod_A,
                   data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_dko_f_AP <- inla(mod_AP,
                    data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_dko_f_AC <- inla(mod_AC,
                    data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_dko_f_PC <- inla(mod_PC,
                    data = mod_data %>% filter(so == "dko" & sex == "Female"), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

#### Tables ####
dko_f_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_dko_f_null$mlik[2],
                                       mod_dko_f_A$mlik[2],
                                       mod_dko_f_P$mlik[2],
                                       mod_dko_f_C$mlik[2],
                                       mod_dko_f_AP$mlik[2],
                                       mod_dko_f_AC$mlik[2],
                                       mod_dko_f_PC$mlik[2]),
                            WAIC = c(mod_dko_f_null$waic$waic,
                                     mod_dko_f_A$waic$waic,
                                     mod_dko_f_P$waic$waic,
                                     mod_dko_f_C$waic$waic,
                                     mod_dko_f_AP$waic$waic,
                                     mod_dko_f_AC$waic$waic,
                                     mod_dko_f_PC$waic$waic),
                            DIC = c(mod_dko_f_null$dic$dic,
                                    mod_dko_f_A$dic$dic,
                                    mod_dko_f_P$dic$dic,
                                    mod_dko_f_C$dic$dic,
                                    mod_dko_f_AP$dic$dic,
                                    mod_dko_f_AC$dic$dic,
                                    mod_dko_f_PC$dic$dic))

write.csv(dko_f_criteria, file = paste0("tables/SO_Q5/",
                                       "dko_f_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q5/mod_dko_f_summaries.txt")
summary(mod_dko_f_null)
summary(mod_dko_f_A)
summary(mod_dko_f_P)
summary(mod_dko_f_C)
summary(mod_dko_f_AP)
summary(mod_dko_f_AC)
summary(mod_dko_f_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_dko_f_AC <- inla.posterior.sample(n = 1000, mod_dko_f_AC)

age_idx <- mod_data %>% 
  select(age, age_idx) %>% 
  unique()
cohort_idx <- mod_data %>% 
  select(cohort, cohort_idx) %>% 
  unique() %>% 
  arrange(cohort_idx)

post_dko_f_AC_df <- lapply(post_dko_f_AC, function(draw){
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

post_dko_f_AC_df <- do.call(rbind.data.frame, post_dko_f_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_f_AC_plot <- post_dko_f_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_f_AC_df %>% 
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

dko_f_AC_age_lo <- post_dko_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_AC_age_logodds.png", dko_f_AC_age_lo)


dko_f_AC_coh_lo <- post_dko_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_AC_coh_logodds.png", dko_f_AC_coh_lo)


dko_f_AC_age_p <- post_dko_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_AC_age_prev.png", dko_f_AC_age_p)


dko_f_AC_coh_p <- post_dko_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_AC_coh_prev.png", dko_f_AC_coh_p)

#### PC ####

post_dko_f_PC <- inla.posterior.sample(n = 1000, mod_dko_f_PC)

period_idx <- mod_data %>% 
  select(period, period_idx) %>% 
  unique() %>% 
  arrange(period_idx)

post_dko_f_PC_df <- lapply(post_dko_f_PC, function(draw){
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

post_dko_f_PC_df <- do.call(rbind.data.frame, post_dko_f_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_f_PC_plot <- post_dko_f_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_f_PC_df %>% 
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


dko_f_PC_per_lo <- post_dko_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_PC_per_logodds.png", dko_f_PC_per_lo)


dko_f_PC_coh_lo <- post_dko_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_PC_coh_logodds.png", dko_f_PC_coh_lo)


dko_f_PC_per_p <- post_dko_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_PC_per_prev.png", dko_f_PC_per_p)


dko_f_PC_coh_p <- post_dko_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_PC_coh_prev.png", dko_f_PC_coh_p)

#### AP ####

post_dko_f_AP <- inla.posterior.sample(n = 1000, mod_dko_f_AP)

post_dko_f_AP_df <- lapply(post_dko_f_AP, function(draw){
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

post_dko_f_AP_df <- do.call(rbind.data.frame, post_dko_f_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_f_AP_plot <- post_dko_f_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_f_AP_df %>% 
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

dko_f_AP_age_lo <- post_dko_f_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_AP_age_logodds.png", dko_f_AC_age_lo)


dko_f_AP_per_lo <- post_dko_f_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_AP_per_logodds.png", dko_f_AP_per_lo)



#### C ####

post_dko_f_C <- inla.posterior.sample(n = 1000, mod_dko_f_C)

post_dko_f_C_df <- lapply(post_dko_f_C, function(draw){
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

post_dko_f_C_df <- do.call(rbind.data.frame, post_dko_f_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_f_C_plot <- post_dko_f_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_f_C_df %>% 
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



dko_f_C_coh_lo <- post_dko_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_C_coh_logodds.png", dko_f_C_coh_lo)

dko_f_C_coh_p <- post_dko_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_f_C_coh_prev.png", dko_f_C_coh_p)

## DKO: Male ####

mod_dko_m_null <- inla(mod_null,
                      data = mod_data %>%
                        filter(so == "dko" & sex == "Male") %>% 
                        drop_na(), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_dko_m_P <- inla(mod_P,
                   data = mod_data %>%
                     filter(so == "dko" & sex == "Male") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_dko_m_C <- inla(mod_C,
                   data = mod_data %>% 
                     filter(so == "dko" & sex == "Male") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_dko_m_A <- inla(mod_A,
                   data = mod_data %>%
                     filter(so == "dko" & sex == "Male") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_dko_m_AP <- inla(mod_AP,
                    data = mod_data %>% 
                      filter(so == "dko" & sex == "Male") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_dko_m_AC <- inla(mod_AC,
                    data = mod_data %>%
                      filter(so == "dko" & sex == "Male") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_dko_m_PC <- inla(mod_PC,
                    data = mod_data %>%
                      filter(so == "dko" & sex == "Male") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

#### Tables ####
dko_m_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_dko_m_null$mlik[2],
                                       mod_dko_m_A$mlik[2],
                                       mod_dko_m_P$mlik[2],
                                       mod_dko_m_C$mlik[2],
                                       mod_dko_m_AP$mlik[2],
                                       mod_dko_m_AC$mlik[2],
                                       mod_dko_m_PC$mlik[2]),
                            WAIC = c(mod_dko_m_null$waic$waic,
                                     mod_dko_m_A$waic$waic,
                                     mod_dko_m_P$waic$waic,
                                     mod_dko_m_C$waic$waic,
                                     mod_dko_m_AP$waic$waic,
                                     mod_dko_m_AC$waic$waic,
                                     mod_dko_m_PC$waic$waic),
                            DIC = c(mod_dko_m_null$dic$dic,
                                    mod_dko_m_A$dic$dic,
                                    mod_dko_m_P$dic$dic,
                                    mod_dko_m_C$dic$dic,
                                    mod_dko_m_AP$dic$dic,
                                    mod_dko_m_AC$dic$dic,
                                    mod_dko_m_PC$dic$dic))

write.csv(dko_m_criteria, file = paste0("tables/SO_Q5/",
                                       "dko_m_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q5/mod_dko_m_summaries.txt")
summary(mod_dko_m_null)
summary(mod_dko_m_A)
summary(mod_dko_m_P)
summary(mod_dko_m_C)
summary(mod_dko_m_AP)
summary(mod_dko_m_AC)
summary(mod_dko_m_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_dko_m_AC <- inla.posterior.sample(n = 1000, mod_dko_m_AC)

post_dko_m_AC_df <- lapply(post_dko_m_AC, function(draw){
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

post_dko_m_AC_df <- do.call(rbind.data.frame, post_dko_m_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_m_AC_plot <- post_dko_m_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_m_AC_df %>% 
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


dko_m_AC_age_lo <- post_dko_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_AC_age_logodds.png", dko_m_AC_age_lo)


dko_m_AC_coh_lo <- post_dko_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_AC_coh_logodds.png", dko_m_AC_coh_lo)

dko_m_AC_age_p <- post_dko_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_AC_age_prev.png", dko_m_AC_age_p)

dko_m_AC_coh_p <- post_dko_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_AC_coh_prev.png", dko_m_AC_coh_p)

#### PC ####

post_dko_m_PC <- inla.posterior.sample(n = 1000, mod_dko_m_PC)

post_dko_m_PC_df <- lapply(post_dko_m_PC, function(draw){
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

post_dko_m_PC_df <- do.call(rbind.data.frame, post_dko_m_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_m_PC_plot <- post_dko_m_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_m_PC_df %>% 
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


dko_m_PC_per_lo <- post_dko_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_PC_per_logodds.png", dko_m_PC_per_lo)


dko_m_PC_coh_lo <- post_dko_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_PC_coh_logodds.png", dko_m_PC_coh_lo)


dko_m_PC_per_p <- post_dko_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_PC_per_prev.png", dko_m_PC_per_p)


dko_m_PC_coh_p <- post_dko_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_PC_coh_prev.png", dko_m_PC_coh_p)


#### AP ####

post_dko_m_AP <- inla.posterior.sample(n = 1000, mod_dko_m_AP)

post_dko_m_AP_df <- lapply(post_dko_m_AP, function(draw){
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

post_dko_m_AP_df <- do.call(rbind.data.frame, post_dko_m_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_m_AP_plot <- post_dko_m_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_m_AP_df %>% 
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

dko_m_AP_age_lo <- post_dko_m_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_AP_age_logodds.png", dko_m_AP_age_lo)


dko_m_AP_per_lo <- post_dko_m_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_AP_per_logodds.png", dko_m_AP_per_lo)


#### C ####

post_dko_m_C <- inla.posterior.sample(n = 1000, mod_dko_m_C)

post_dko_m_C_df <- lapply(post_dko_m_C, function(draw){
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

post_dko_m_C_df <- do.call(rbind.data.frame, post_dko_m_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_dko_m_C_plot <- post_dko_m_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_m_C_df %>% 
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



dko_m_C_coh_lo <- post_dko_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_C_coh_logodds.png", dko_m_C_coh_lo)

dko_m_C_coh_p <- post_dko_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Don't Know/Other: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/dko_m_C_coh_prev.png", dko_m_C_coh_p)


## Decline: Female####

mod_ref_f_null <- inla(mod_null,
                      data = mod_data %>%
                        filter(so == "ref" & sex == "Female") %>% 
                        drop_na(), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                             config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_ref_f_P <- inla(mod_P,
                   data = mod_data %>%
                     filter(so == "ref" & sex == "Female") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_ref_f_C <- inla(mod_C,
                   data = mod_data %>% 
                     filter(so == "ref" & sex == "Female") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_ref_f_A <- inla(mod_A,
                   data = mod_data %>%
                     filter(so == "ref" & sex == "Female") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_ref_f_AP <- inla(mod_AP,
                    data = mod_data %>%
                      filter(so == "ref" & sex == "Female") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_ref_f_AC <- inla(mod_AC,
                    data = mod_data %>%
                      filter(so == "ref" & sex == "Female") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_ref_f_PC <- inla(mod_PC,
                    data = mod_data %>%
                      filter(so == "ref" & sex == "Female") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)


#### Tables ####
ref_f_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_ref_f_null$mlik[2],
                                       mod_ref_f_A$mlik[2],
                                       mod_ref_f_P$mlik[2],
                                       mod_ref_f_C$mlik[2],
                                       mod_ref_f_AP$mlik[2],
                                       mod_ref_f_AC$mlik[2],
                                       mod_ref_f_PC$mlik[2]),
                            WAIC = c(mod_ref_f_null$waic$waic,
                                     mod_ref_f_A$waic$waic,
                                     mod_ref_f_P$waic$waic,
                                     mod_ref_f_C$waic$waic,
                                     mod_ref_f_AP$waic$waic,
                                     mod_ref_f_AC$waic$waic,
                                     mod_ref_f_PC$waic$waic),
                            DIC = c(mod_ref_f_null$dic$dic,
                                    mod_ref_f_A$dic$dic,
                                    mod_ref_f_P$dic$dic,
                                    mod_ref_f_C$dic$dic,
                                    mod_ref_f_AP$dic$dic,
                                    mod_ref_f_AC$dic$dic,
                                    mod_ref_f_PC$dic$dic))

write.csv(ref_f_criteria, file = paste0("tables/SO_Q5/",
                                       "ref_f_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q5/mod_ref_f_summaries.txt")
summary(mod_ref_f_null)
summary(mod_ref_f_A)
summary(mod_ref_f_P)
summary(mod_ref_f_C)
summary(mod_ref_f_AP)
summary(mod_ref_f_AC)
summary(mod_ref_f_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_ref_f_AC <- inla.posterior.sample(n = 1000, mod_ref_f_AC)

post_ref_f_AC_df <- lapply(post_ref_f_AC, function(draw){
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

post_ref_f_AC_df <- do.call(rbind.data.frame, post_ref_f_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_f_AC_plot <- post_ref_f_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_ref_f_AC_df %>% 
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


ref_f_AC_age_lo <- post_ref_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_AC_age_logodds.png", ref_f_AC_age_lo)


ref_f_AC_coh_lo <- post_ref_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_AC_coh_logodds.png", ref_f_AC_coh_lo)


ref_f_AC_age_p <- post_ref_f_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_AC_age_prev.png", ref_f_AC_age_p)


ref_f_AC_coh_p <- post_ref_f_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_AC_coh_prev.png", ref_f_AC_coh_p)


#### PC ####

post_ref_f_PC <- inla.posterior.sample(n = 1000, mod_ref_f_PC)

post_ref_f_PC_df <- lapply(post_ref_f_PC, function(draw){
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

post_ref_f_PC_df <- do.call(rbind.data.frame, post_ref_f_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_f_PC_plot <- post_ref_f_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_f_PC_df %>% 
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


ref_f_PC_per_lo <- post_ref_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_PC_per_logodds.png", ref_f_PC_per_lo)


ref_f_PC_coh_lo <- post_ref_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_PC_coh_logodds.png", ref_f_PC_coh_lo)


ref_f_PC_per_p <- post_ref_f_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_PC_per_prev.png", ref_f_PC_per_p)


ref_f_PC_coh_p <- post_ref_f_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_PC_coh_prev.png", ref_f_PC_coh_p)


#### AP ####

post_ref_f_AP <- inla.posterior.sample(n = 1000, mod_ref_f_AP)

post_ref_f_AP_df <- lapply(post_ref_f_AP, function(draw){
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

post_ref_f_AP_df <- do.call(rbind.data.frame, post_ref_f_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_f_AP_plot <- post_ref_f_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_ref_f_AP_df %>% 
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

ref_f_AP_age_lo <- post_ref_f_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_AP_age_logodds.png", ref_f_AC_age_lo)


ref_f_AP_per_lo <- post_ref_f_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Female") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_AP_per_logodds.png", ref_f_AP_per_lo)



#### C ####

post_ref_f_C <- inla.posterior.sample(n = 1000, mod_ref_f_C)

post_ref_f_C_df <- lapply(post_ref_f_C, function(draw){
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

post_ref_f_C_df <- do.call(rbind.data.frame, post_ref_f_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_f_C_plot <- post_ref_f_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_f_C_df %>% 
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



ref_f_C_coh_lo <- post_ref_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_C_coh_logodds.png", ref_f_C_coh_lo)

ref_f_C_coh_p <- post_ref_f_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Lesbian") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_f_C_coh_prev.png", ref_f_C_coh_p)

## Decline: Male ####

mod_ref_m_null <- inla(mod_null,
                      data = mod_data %>% 
                        filter(so == "ref" & sex == "Male") %>% 
                        drop_na(), 
                      family = "gaussian",
                      control.family = list(hyper = list(
                        prec = list(initial = log(1),
                                    fixed=TRUE))), 
                      control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                             config = TRUE),
                      scale = prec_logit_pijk,
                      verbose = TRUE)
mod_ref_m_P <- inla(mod_P,
                   data = mod_data %>%
                     filter(so == "ref" & sex == "Male") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_ref_m_C <- inla(mod_C,
                   data = mod_data %>%
                     filter(so == "ref" & sex == "Male") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE, 
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)


mod_ref_m_A <- inla(mod_A,
                   data = mod_data %>%
                     filter(so == "ref" & sex == "Male") %>% 
                     drop_na(), 
                   family = "gaussian",
                   control.family = list(hyper = list(
                     prec = list(initial = log(1),
                                 fixed=TRUE))), 
                   control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                          config = TRUE),
                   scale = prec_logit_pijk,
                   verbose = TRUE)

mod_ref_m_AP <- inla(mod_AP,
                    data = mod_data %>%
                      filter(so == "ref" & sex == "Male") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_ref_m_AC <- inla(mod_AC,
                    data = mod_data %>% 
                      filter(so == "ref" & sex == "Male") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

mod_ref_m_PC <- inla(mod_PC,
                    data = mod_data %>%
                      filter(so == "ref" & sex == "Male") %>% 
                      drop_na(), 
                    family = "gaussian",
                    control.family = list(hyper = list(
                      prec = list(initial = log(1),
                                  fixed=TRUE))), 
                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE,
                                           config = TRUE),
                    scale = prec_logit_pijk,
                    verbose = TRUE)

#### Tables ####
ref_m_criteria <- data.frame(Model = c("Null", "A", "P", "C",
                                      "AP", "AC", "PC"),
                            loglik = c(mod_ref_m_null$mlik[2],
                                       mod_ref_m_A$mlik[2],
                                       mod_ref_m_P$mlik[2],
                                       mod_ref_m_C$mlik[2],
                                       mod_ref_m_AP$mlik[2],
                                       mod_ref_m_AC$mlik[2],
                                       mod_ref_m_PC$mlik[2]),
                            WAIC = c(mod_ref_m_null$waic$waic,
                                     mod_ref_m_A$waic$waic,
                                     mod_ref_m_P$waic$waic,
                                     mod_ref_m_C$waic$waic,
                                     mod_ref_m_AP$waic$waic,
                                     mod_ref_m_AC$waic$waic,
                                     mod_ref_m_PC$waic$waic),
                            DIC = c(mod_ref_m_null$dic$dic,
                                    mod_ref_m_A$dic$dic,
                                    mod_ref_m_P$dic$dic,
                                    mod_ref_m_C$dic$dic,
                                    mod_ref_m_AP$dic$dic,
                                    mod_ref_m_AC$dic$dic,
                                    mod_ref_m_PC$dic$dic))

write.csv(ref_m_criteria, file = paste0("tables/SO_Q5/",
                                       "ref_m_critera.csv"),
          row.names = FALSE)

#### Save Model Outputs ####
sink(file = "tables/SO_Q5/mod_ref_m_summaries.txt")
summary(mod_ref_m_null)
summary(mod_ref_m_A)
summary(mod_ref_m_P)
summary(mod_ref_m_C)
summary(mod_ref_m_AP)
summary(mod_ref_m_AC)
summary(mod_ref_m_PC)
sink(file = NULL)

### Plots ####

#### AC ####

post_ref_m_AC <- inla.posterior.sample(n = 1000, mod_ref_m_AC)

post_ref_m_AC_df <- lapply(post_ref_m_AC, function(draw){
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

post_ref_m_AC_df <- do.call(rbind.data.frame, post_ref_m_AC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_m_AC_plot <- post_ref_m_AC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_ref_m_AC_df %>% 
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


ref_m_AC_age_lo <- post_ref_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_AC_age_logodds.png", ref_m_AC_age_lo)


ref_m_AC_coh_lo <- post_ref_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_AC_coh_logodds.png", ref_m_AC_coh_lo)


ref_m_AC_age_p <- post_ref_m_AC_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Prevalence") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_AC_age_prev.png", ref_m_AC_age_p)


ref_m_AC_coh_p <- post_ref_m_AC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_AC_coh_prev.png", ref_m_AC_coh_p)

#### PC ####

post_ref_m_PC <- inla.posterior.sample(n = 1000, mod_ref_m_PC)

post_ref_m_PC_df <- lapply(post_ref_m_PC, function(draw){
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

post_ref_m_PC_df <- do.call(rbind.data.frame, post_ref_m_PC_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_m_PC_plot <- post_ref_m_PC_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_m_PC_df %>% 
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


ref_m_PC_per_lo <- post_ref_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_PC_per_logodds.png", ref_m_PC_per_lo)


ref_m_PC_coh_lo <- post_ref_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_PC_coh_logodds.png", ref_m_PC_coh_lo)


ref_m_PC_per_p <- post_ref_m_PC_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Prevalence") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_PC_per_prev.png", ref_m_PC_per_p)


ref_m_PC_coh_p <- post_ref_m_PC_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_PC_coh_prev.png", ref_m_PC_coh_p)

#### AP ####

post_ref_m_AP <- inla.posterior.sample(n = 1000, mod_ref_m_AP)

post_ref_m_AP_df <- lapply(post_ref_m_AP, function(draw){
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

post_ref_m_AP_df <- do.call(rbind.data.frame, post_ref_m_AP_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_m_AP_plot <- post_ref_m_AP_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_ref_m_AP_df %>% 
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

ref_m_AP_age_lo <- post_ref_m_AP_plot %>%
  filter(term == "Age") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "navy") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "navy",
              color = "white", alpha = 0.3) +
  ggtitle("Gay male") +
  ylab("Log Odds") +
  xlab("Age")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_AP_age_logodds.png", ref_m_AC_age_lo)


ref_m_AP_per_lo <- post_ref_m_AP_plot %>%
  filter(term == "Period") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "goldenrod") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "goldenrod",
              color = "white", alpha = 0.3) +
  ggtitle("Gay male") +
  ylab("Log Odds") +
  xlab("Period")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_AP_per_logodds.png", ref_f_AP_per_lo)

#### C ####

post_ref_m_C <- inla.posterior.sample(n = 1000, mod_ref_m_C)

post_ref_m_C_df <- lapply(post_ref_m_C, function(draw){
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

post_ref_m_C_df <- do.call(rbind.data.frame, post_ref_m_C_df) %>% 
  group_by(term, term_idx) %>% 
  mutate(draw_no = 1:length(term))

post_ref_m_C_plot <- post_ref_m_C_df %>% 
  filter(term != "Intercept") %>% 
  left_join(post_dko_m_C_df %>% 
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



ref_m_C_coh_lo <- post_ref_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_lo)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_lo, ymax = upper_lo), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Log Odds") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_C_coh_logodds.png", ref_m_C_coh_lo)

ref_m_C_coh_p <- post_ref_m_C_plot %>%
  filter(term == "Cohort") %>% 
  ggplot(aes(x = term_idx_name, y = med_p)) +
  geom_line(color = "forestgreen") +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p), fill = "forestgreen",
              color = "white", alpha = 0.3) +
  ggtitle("Declined to Answer: Male") +
  ylab("Prevalence") +
  xlab("Cohort")+
  theme_classic()

ggsave(filename = "plots/SO_Q5/ref_m_C_coh_prev.png", ref_m_C_coh_p)

# Manuscript Plots ####

## AC ####
AC_ranges <- bind_rows(post_dko_f_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual females"),
                       post_dko_m_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual males"),
                       post_ref_f_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Lesbians"),
                       post_ref_m_AC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Gay males"))
## Across groups 
AC_ranges %>% 
  group_by(term) %>% 
  dplyr::summarize(lower = min(lower),
            upper = max(upper))

AC_ylims <- c(-8.5, 2.5)
A_xlims <- c(14, 79) + c(-1, 1)
C_xlims <- c(1935, 2007) + c(-1,1)


# par(mfrow = c(2,4),lend = 1)

png("plots/SO_Q5/AC_8panel_logodds.png", 
    width = 300*8, height = 300*6, res=300)
{
  par(mfrow = c(2,4), lend = 1)
  par(mar=c(5,0,0,0))
  par(oma=c(5,4,4,1))
  
  ## Age 
  {
    { 
      ## Other Response F
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("a. Other response - female", x= 35, y=2, cex=.9)
      # title("Age", adj = 0, line = 4)
      
      mtext("Log Odds", adj = 0.275, side = 2,
            line = 2.5, cex = 0.8, outer = TRUE)
      
      mtext("Log Odds", adj = 0.85, side = 2,
            line = 2.5, cex = 0.8, outer = TRUE)
      
      mtext("Age", adj = 0, side = 3, line = 0.5, outer = TRUE)
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = .9)
      
      ## Log odds axis
      axis(2, at = seq(-8, 2, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_f_AC_plot$term_idx_name[post_dko_f_AC_plot$term == "Age"],
            post_dko_f_AC_plot$med_lo[post_dko_f_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_dko_f_AC_plot$term_idx_name[post_dko_f_AC_plot$term == "Age"],
                    rev(post_dko_f_AC_plot$term_idx_name[post_dko_f_AC_plot$term == 
                                                          "Age"])),
              y = c(post_dko_f_AC_plot$upper_lo[post_dko_f_AC_plot$term == "Age"],
                    rev(post_dko_f_AC_plot$lower_lo[post_dko_f_AC_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Other response M
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("b. Other response - male", x=33.5, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_m_AC_plot$term_idx_name[post_dko_m_AC_plot$term == "Age"],
            post_dko_m_AC_plot$med_lo[post_dko_m_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_dko_m_AC_plot$term_idx_name[post_dko_m_AC_plot$term == "Age"],
                    rev(post_dko_m_AC_plot$term_idx_name[post_dko_m_AC_plot$term == 
                                                          "Age"])),
              y = c(post_dko_m_AC_plot$upper_lo[post_dko_m_AC_plot$term == "Age"],
                    rev(post_dko_m_AC_plot$lower_lo[post_dko_m_AC_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer F
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("c. Declined to answer - female", x = 37.5, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_f_AC_plot$term_idx_name[post_ref_f_AC_plot$term == "Age"],
            post_ref_f_AC_plot$med_lo[post_ref_f_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_ref_f_AC_plot$term_idx_name[post_ref_f_AC_plot$term == "Age"],
                    rev(post_ref_f_AC_plot$term_idx_name[post_ref_f_AC_plot$term == 
                                                          "Age"])),
              y = c(post_ref_f_AC_plot$upper_lo[post_ref_f_AC_plot$term == "Age"],
                    rev(post_ref_f_AC_plot$lower_lo[post_ref_f_AC_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer M
      plot(NA, xlim = A_xlims, ylim = AC_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("d. Declined to answer - male", x=36, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_m_AC_plot$term_idx_name[post_ref_m_AC_plot$term == "Age"],
            post_ref_m_AC_plot$med_lo[post_ref_m_AC_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_ref_m_AC_plot$term_idx_name[post_ref_m_AC_plot$term ==
                                                      "Age"],
                    rev(post_ref_m_AC_plot$term_idx_name[post_ref_m_AC_plot$term == 
                                                          "Age"])),
              y = c(post_ref_m_AC_plot$upper_lo[post_ref_m_AC_plot$term == "Age"],
                    rev(post_ref_m_AC_plot$lower_lo[post_ref_m_AC_plot$term == 
                                                     "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
  }
  
  
  ## Cohort 
  {
    { 
      ## Other Response F
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("e. Other response - female", x= 18 + 1940, y=2, cex=.9)
      mtext("Cohort", adj = 0, side = 3, line = 0.5)
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
      
      ## Log odds axis
      axis(2, at = seq(-8, 2, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_f_AC_plot$term_idx_name[post_dko_f_AC_plot$term ==
                                              "Cohort"],
            post_dko_f_AC_plot$med_lo[post_dko_f_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_dko_f_AC_plot$term_idx_name[post_dko_f_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_dko_f_AC_plot$term_idx_name[post_dko_f_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_dko_f_AC_plot$upper_lo[post_dko_f_AC_plot$term == 
                                                 "Cohort"],
                    rev(post_dko_f_AC_plot$lower_lo[post_dko_f_AC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Other Response M
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("f. Other response - male", x=16.5 + 1940, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_m_AC_plot$term_idx_name[post_dko_m_AC_plot$term == "Cohort"],
            post_dko_m_AC_plot$med_lo[post_dko_m_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_dko_m_AC_plot$term_idx_name[post_dko_m_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_dko_m_AC_plot$term_idx_name[post_dko_m_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_dko_m_AC_plot$upper_lo[post_dko_m_AC_plot$term == 
                                                 "Cohort"],
                    rev(post_dko_m_AC_plot$lower_lo[post_dko_m_AC_plot$term ==
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer F
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("g. Declined to answer - female", x = 21 + 1940, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_f_AC_plot$term_idx_name[post_ref_f_AC_plot$term == "Cohort"],
            post_ref_f_AC_plot$med_lo[post_ref_f_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_ref_f_AC_plot$term_idx_name[post_ref_f_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_ref_f_AC_plot$term_idx_name[post_ref_f_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_ref_f_AC_plot$upper_lo[post_ref_f_AC_plot$term == "Cohort"],
                    rev(post_ref_f_AC_plot$lower_lo[post_ref_f_AC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer M
      plot(NA, xlim = C_xlims, ylim = AC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("h. Declined to answer - male", x = 19.5 + 1940, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_m_AC_plot$term_idx_name[post_ref_m_AC_plot$term == "Cohort"],
            post_ref_m_AC_plot$med_lo[post_ref_m_AC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_ref_m_AC_plot$term_idx_name[post_ref_m_AC_plot$term == 
                                                      "Cohort"],
                    rev(post_ref_m_AC_plot$term_idx_name[post_ref_m_AC_plot$term == 
                                                          "Cohort"])),
              y = c(post_ref_m_AC_plot$upper_lo[post_ref_m_AC_plot$term == "Cohort"],
                    rev(post_ref_m_AC_plot$lower_lo[post_ref_m_AC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

## PC ####
PC_ranges <- bind_rows(post_dko_f_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual females"),
                       post_dko_m_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual males"),
                       post_ref_f_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Lesbians"),
                       post_ref_m_PC_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Gay males"))
## Across groups 
PC_ranges %>% 
  group_by(term) %>% 
  dplyr::summarize(lower = min(lower),
            upper = max(upper))

PC_ylims <- c(-8.5, 2.5)
P_xlims <- c(2014, 2021) + c(-1, 1)
C_xlims <- c(1935, 2007) + c(-1,1)


# par(mfrow = c(2,4),lend = 1)

png("plots/SO_Q5/PC_8panel_logodds.png", 
    width = 300*8, height = 300*6, res=300)
{
  par(mfrow = c(2,4), lend = 1)
  par(mar=c(5,0,0,0))
  par(oma=c(5,4,4,1))
  
  ## Period 
  {
    { 
      ## Other Response F
      plot(NA, xlim = P_xlims, ylim = PC_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("a. Other response - female", x=2016, y=2, cex=.9)
      # title("Age", adj = 0, line = 4)
      
      mtext("Log Odds", adj = 0.275, side = 2,
            line = 2.5, cex = 0.8, outer = TRUE)
      
      mtext("Log Odds", adj = 0.85, side = 2,
            line = 2.5, cex = 0.8, outer = TRUE)
      
      mtext("Period", adj = 0, side = 3, line = 0.5, outer = TRUE)
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = .9)
      
      ## Log odds axis
      axis(2, at = seq(-8, 2, 1))
      abline(h = 0)
      ## Ests
      lines(post_dko_f_PC_plot$term_idx_name[post_dko_f_PC_plot$term == "Period"],
            post_dko_f_PC_plot$med_lo[post_dko_f_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_dko_f_PC_plot$term_idx_name[post_dko_f_PC_plot$term == "Period"],
                    rev(post_dko_f_PC_plot$term_idx_name[post_dko_f_PC_plot$term == 
                                                          "Period"])),
              y = c(post_dko_f_PC_plot$upper_lo[post_dko_f_PC_plot$term ==
                                                 "Period"],
                    rev(post_dko_f_PC_plot$lower_lo[post_dko_f_PC_plot$term ==
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Other Response M
      plot(NA, xlim = P_xlims, ylim = PC_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("b. Other response - male", x=2015.75, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_m_PC_plot$term_idx_name[post_dko_m_PC_plot$term == "Period"],
            post_dko_m_PC_plot$med_lo[post_dko_m_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_dko_m_PC_plot$term_idx_name[post_dko_m_PC_plot$term == "Period"],
                    rev(post_dko_m_PC_plot$term_idx_name[post_dko_m_PC_plot$term == 
                                                          "Period"])),
              y = c(post_dko_m_PC_plot$upper_lo[post_dko_m_PC_plot$term == "Period"],
                    rev(post_dko_m_PC_plot$lower_lo[post_dko_m_PC_plot$term == "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer F
      plot(NA, xlim = P_xlims, ylim = PC_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("c. Declined to answer - female", x = 2016.25, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_f_PC_plot$term_idx_name[post_ref_f_PC_plot$term == 
                                              "Period"],
            post_ref_f_PC_plot$med_lo[post_ref_f_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_ref_f_PC_plot$term_idx_name[post_ref_f_PC_plot$term == 
                                                      "Period"],
                    rev(post_ref_f_PC_plot$term_idx_name[post_ref_f_PC_plot$term == 
                                                          "Period"])),
              y = c(post_ref_f_PC_plot$upper_lo[post_ref_f_PC_plot$term ==
                                                 "Period"],
                    rev(post_ref_f_PC_plot$lower_lo[post_ref_f_PC_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answre M
      plot(NA, xlim = P_xlims, ylim =PC_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("d. Declined to answer - male", x=2016.1, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_m_PC_plot$term_idx_name[post_ref_m_PC_plot$term ==
                                              "Period"],
            post_ref_m_PC_plot$med_lo[post_ref_m_PC_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_ref_m_PC_plot$term_idx_name[post_ref_m_PC_plot$term ==
                                                      "Period"],
                    rev(post_ref_m_PC_plot$term_idx_name[post_ref_m_PC_plot$term == 
                                                          "Period"])),
              y = c(post_ref_m_PC_plot$upper_lo[post_ref_m_PC_plot$term == "Period"],
                    rev(post_ref_m_PC_plot$lower_lo[post_ref_m_PC_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
  }
  
  
  ## Cohort 
  {
    { 
      ## Other Response F
      plot(NA, xlim = C_xlims, ylim = PC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("e. Other response - female", x= 18 + 1940, y=2, cex=.9)
      mtext("Cohort", adj = 0, side = 3, line = 0.5)
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
      
      ## Log odds axis
      
      axis(2, at = seq(-8, 2, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_f_PC_plot$term_idx_name[post_dko_f_PC_plot$term ==
                                              "Cohort"],
            post_dko_f_PC_plot$med_lo[post_dko_f_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_dko_f_PC_plot$term_idx_name[post_dko_f_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_dko_f_PC_plot$term_idx_name[post_dko_f_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_dko_f_PC_plot$upper_lo[post_dko_f_PC_plot$term == 
                                                 "Cohort"],
                    rev(post_dko_f_PC_plot$lower_lo[post_dko_f_PC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Other Response M
      plot(NA, xlim = C_xlims, ylim = PC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("f. Other response - male", x=16.5 + 1940, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Cohort", line = 2, cex = .9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_m_PC_plot$term_idx_name[post_dko_m_PC_plot$term == "Cohort"],
            post_dko_m_PC_plot$med_lo[post_dko_m_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_dko_m_PC_plot$term_idx_name[post_dko_m_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_dko_m_PC_plot$term_idx_name[post_dko_m_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_dko_m_PC_plot$upper_lo[post_dko_m_PC_plot$term == 
                                                 "Cohort"],
                    rev(post_dko_m_PC_plot$lower_lo[post_dko_m_PC_plot$term ==
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer F
      plot(NA, xlim = C_xlims, ylim = PC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("g. Declined to answer - female", x = 21 + 1940, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_f_PC_plot$term_idx_name[post_ref_f_PC_plot$term == "Cohort"],
            post_ref_f_PC_plot$med_lo[post_ref_f_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_ref_f_PC_plot$term_idx_name[post_ref_f_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_ref_f_PC_plot$term_idx_name[post_ref_f_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_ref_f_PC_plot$upper_lo[post_ref_f_PC_plot$term == "Cohort"],
                    rev(post_ref_f_PC_plot$lower_lo[post_ref_f_PC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer M
      plot(NA, xlim = C_xlims, ylim = PC_ylims,
           xlab = "Cohort", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("h. Declined to answer - male", x = 19.5 + 1940, y=2, cex=.9)  
      
      ## Age axis
      axis(1, at = seq(1940, 2010, 10))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      # y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_m_PC_plot$term_idx_name[post_ref_m_PC_plot$term == "Cohort"],
            post_ref_m_PC_plot$med_lo[post_ref_m_PC_plot$term == "Cohort"],
            lwd = 2, col = "forestgreen")
      polygon(x = c(post_ref_m_PC_plot$term_idx_name[post_ref_m_PC_plot$term == 
                                                      "Cohort"],
                    rev(post_ref_m_PC_plot$term_idx_name[post_ref_m_PC_plot$term == 
                                                          "Cohort"])),
              y = c(post_ref_m_PC_plot$upper_lo[post_ref_m_PC_plot$term == "Cohort"],
                    rev(post_ref_m_PC_plot$lower_lo[post_ref_m_PC_plot$term == 
                                                     "Cohort"])),
              col = alpha("forestgreen", 0.35), border = FALSE)
    }
    
  }
}
dev.off()


## AP ####
AP_ranges <- bind_rows(post_dko_f_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual females"),
                       post_dko_m_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Bisexual males"),
                       post_ref_f_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Lesbians"),
                       post_ref_m_AP_plot %>% 
                         group_by(term) %>% 
                         dplyr::summarize(lower = min(lower_lo),
                                   upper = max(upper_lo),
                                   Group = "Gay males"))
## Across groups 
AP_ranges %>% 
  group_by(term) %>% 
  dplyr::summarize(lower = min(lower),
            upper = max(upper))

AP_ylims <- c(-8.5, 2.5)
A_xlims <- c(14, 79) + c(-1, 1)
P_xlims <- c(2014, 2021) + c(-1,1)


# par(mfrow = c(2,4),lend = 1)

png("plots/SO_Q5/AP_8panel_logodds.png", 
    width = 300*8, height = 300*6, res=300)
{
  par(mfrow = c(2,4), lend = 1)
  par(mar=c(5,0,0,0))
  par(oma=c(5,4,4,1))
  
  ## Age 
  {
    { 
      ## Other Response F
      plot(NA, xlim = A_xlims, ylim = AP_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("a. Other response - female", x= 35, y=2, cex=.9)
      
      mtext("Log Odds", adj = 0.275, side = 2,
            line = 2.5, cex = 0.8, outer = TRUE)
      
      mtext("Log Odds", adj = 0.85, side = 2,
            line = 2.5, cex = 0.8, outer = TRUE)
      # title("Age", adj = 0, line = 4)
      mtext("Age", adj = 0, side = 3, line = 0.5, outer = FALSE)
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = .9)
      
      ## Log odds axis
      axis(2, at = seq(-8, 2, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_f_AP_plot$term_idx_name[post_dko_f_AP_plot$term == "Age"],
            post_dko_f_AP_plot$med_lo[post_dko_f_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_dko_f_AP_plot$term_idx_name[post_dko_f_AP_plot$term == "Age"],
                    rev(post_dko_f_AP_plot$term_idx_name[post_dko_f_AP_plot$term == 
                                                          "Age"])),
              y = c(post_dko_f_AP_plot$upper_lo[post_dko_f_AP_plot$term == "Age"],
                    rev(post_dko_f_AP_plot$lower_lo[post_dko_f_AP_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Other Response M
      plot(NA, xlim = A_xlims, ylim = AP_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("b. Other response - male", x=33.5, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_m_AP_plot$term_idx_name[post_dko_m_AP_plot$term == "Age"],
            post_dko_m_AP_plot$med_lo[post_dko_m_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_dko_m_AP_plot$term_idx_name[post_dko_m_AP_plot$term == "Age"],
                    rev(post_dko_m_AP_plot$term_idx_name[post_dko_m_AP_plot$term == 
                                                          "Age"])),
              y = c(post_dko_m_AP_plot$upper_lo[post_dko_m_AP_plot$term == "Age"],
                    rev(post_dko_m_AP_plot$lower_lo[post_dko_m_AP_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer F
      plot(NA, xlim = A_xlims, ylim = AP_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("c. Declined to answer - female", x = 37.5, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_f_AP_plot$term_idx_name[post_ref_f_AP_plot$term == "Age"],
            post_ref_f_AP_plot$med_lo[post_ref_f_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_ref_f_AP_plot$term_idx_name[post_ref_f_AP_plot$term == "Age"],
                    rev(post_ref_f_AP_plot$term_idx_name[post_ref_f_AP_plot$term == 
                                                          "Age"])),
              y = c(post_ref_f_AP_plot$upper_lo[post_ref_f_AP_plot$term == "Age"],
                    rev(post_ref_f_AP_plot$lower_lo[post_ref_f_AP_plot$term == "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer M
      plot(NA, xlim = A_xlims, ylim = AP_ylims,
           xlab = "Age", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("d. Declined to answer - male", x=36, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = seq(15, 75, 5))
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_m_AP_plot$term_idx_name[post_ref_m_AP_plot$term == "Age"],
            post_ref_m_AP_plot$med_lo[post_ref_m_AP_plot$term == "Age"],
            lwd = 2, col = "navy")
      polygon(x = c(post_ref_m_AP_plot$term_idx_name[post_ref_m_AP_plot$term ==
                                                      "Age"],
                    rev(post_ref_m_AP_plot$term_idx_name[post_ref_m_AP_plot$term == 
                                                          "Age"])),
              y = c(post_ref_m_AP_plot$upper_lo[post_ref_m_AP_plot$term == "Age"],
                    rev(post_ref_m_AP_plot$lower_lo[post_ref_m_AP_plot$term == 
                                                     "Age"])),
              col = alpha("navy", 0.35), border = FALSE)
    }
    
  }
  
  
  ## Period 
  {
    { 
      ## Other Response F
      plot(NA, xlim = P_xlims, ylim = AP_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("a. Other response - female", x=2016, y=2, cex=.9)
      # mtext("Log Odds", adj = 0.25, side = 2,
      #       line = 2.5, cex = 0.8, outer = TRUE)
      # 
      # mtext("Log Odds", adj = 0.75, side = 2,
      #       line = 2.5, cex = 0.8, outer = TRUE)
      
      # title("Age", adj = 0, line = 4)
      mtext("Period", adj = 0, side = 3, line = 0.5)
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = .9)
      
      ## Log odds axis
      axis(2, at = seq(-8, 2, 1))
      abline(h = 0)
      ## Ests
      lines(post_dko_f_AP_plot$term_idx_name[post_dko_f_AP_plot$term == "Period"],
            post_dko_f_AP_plot$med_lo[post_dko_f_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_dko_f_AP_plot$term_idx_name[post_dko_f_AP_plot$term == "Period"],
                    rev(post_dko_f_AP_plot$term_idx_name[post_dko_f_AP_plot$term == 
                                                          "Period"])),
              y = c(post_dko_f_AP_plot$upper_lo[post_dko_f_AP_plot$term ==
                                                 "Period"],
                    rev(post_dko_f_AP_plot$lower_lo[post_dko_f_AP_plot$term ==
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Other Response M
      plot(NA, xlim = P_xlims, ylim = AP_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("b. Other response - male", x=2015.75, y=2, cex=.9)
      
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1), )
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_dko_m_AP_plot$term_idx_name[post_dko_m_AP_plot$term == "Period"],
            post_dko_m_AP_plot$med_lo[post_dko_m_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_dko_m_AP_plot$term_idx_name[post_dko_m_AP_plot$term == "Period"],
                    rev(post_dko_m_AP_plot$term_idx_name[post_dko_m_AP_plot$term == 
                                                          "Period"])),
              y = c(post_dko_m_AP_plot$upper_lo[post_dko_m_AP_plot$term == "Period"],
                    rev(post_dko_m_AP_plot$lower_lo[post_dko_m_AP_plot$term == "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer F
      plot(NA, xlim = P_xlims, ylim = AP_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("c. Declined to answer - female", x = 2016.25, y=2, cex=.9)
      
      
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_f_AP_plot$term_idx_name[post_ref_f_AP_plot$term == 
                                              "Period"],
            post_ref_f_AP_plot$med_lo[post_ref_f_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_ref_f_AP_plot$term_idx_name[post_ref_f_AP_plot$term == 
                                                      "Period"],
                    rev(post_ref_f_AP_plot$term_idx_name[post_ref_f_AP_plot$term == 
                                                          "Period"])),
              y = c(post_ref_f_AP_plot$upper_lo[post_ref_f_AP_plot$term ==
                                                 "Period"],
                    rev(post_ref_f_AP_plot$lower_lo[post_ref_f_AP_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
    { 
      ## Declined to Answer M
      plot(NA, xlim = P_xlims, ylim = AP_ylims,
           xlab = "Period", ylab = "Log Odds",
           main = "", type = "n", frame.plot = TRUE, axes = FALSE)
      ## title
      text("d. Declined to answer - male", x=2016.1, y=2, cex=.9) 
      
      ## Age axis
      axis(1, at = 2014:2021)
      # mtext(side = 1, text = "Age", line = 2, cex = 0.9)
      
      ## Log odds axis
      # axis(2, at = seq(-7, 1, 1))
      
      ## y = 0
      abline(h = 0)
      
      ## Ests
      lines(post_ref_m_AP_plot$term_idx_name[post_ref_m_AP_plot$term ==
                                              "Period"],
            post_ref_m_AP_plot$med_lo[post_ref_m_AP_plot$term == "Period"],
            lwd = 2, col = "goldenrod")
      polygon(x = c(post_ref_m_AP_plot$term_idx_name[post_ref_m_AP_plot$term ==
                                                      "Period"],
                    rev(post_ref_m_AP_plot$term_idx_name[post_ref_m_AP_plot$term == 
                                                          "Period"])),
              y = c(post_ref_m_AP_plot$upper_lo[post_ref_m_AP_plot$term == "Period"],
                    rev(post_ref_m_AP_plot$lower_lo[post_ref_m_AP_plot$term == 
                                                     "Period"])),
              col = alpha("goldenrod", 0.35), border = FALSE)
    }
    
  }
}
dev.off()

# Grand proportions for #s in MS body

yrbs21 <- readRDS("data - raw/yrbs21_tomerge.RDS")
prop.table(tableNA(yrbs21$so_21, yrbs21$sex), 2)

