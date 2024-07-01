## Carry out chi-squared test for answer to 
## Jessica Godwin

# Setup ####
rm(list = ls())

if(!dir.exists("tables/SO_Q3")){
  if(!dir.exists("tables")){
    dir.create("tables")
  }
  dir.create("tables/SO_Q3")
}

if(!dir.exists("plots/SO_Q3")){
  if(!dir.exists("plots")){
    dir.create("plots")
  }
  dir.create("plots/SO_Q3")
}


## Libraries ####
library(survey)
library(tidyverse)


## Functions ####
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}

# Load Data ####

combo <- readRDS(file = "data - clean/combined.rds")

str(combo)
summary(combo)
head(combo)

# Prep for total ests ####

## Filter to analysis ages ####

mod_data <- combo %>% 
  filter(age < 26) %>% 
  filter(!(age == 18 & source == "YRBS"))


# Specify YRBS design ####

yrbs_des <- svydesign(ids = ~psu, strata = ~year + stratum, weights = ~weight,
                      data = mod_data %>% 
                        filter(age < 18), nest = TRUE)
yrbs_des

brfss_des <- svydesign(ids = ~1, strata = ~year + stratum, weights = ~weight,
                       data = mod_data %>% 
                         filter(age >= 18), nest = TRUE)
brfss_des


# Get totals by age and year ####

## YRBS ####
yrbs_totals <- list()
yrbs_E <- list()
for(age_calc in 14:17){
  yrbs_totals[[paste0("age_", age_calc)]] <- 
    svyby(~so_new, by = ~year+sex, design = subset(yrbs_des, age == age_calc), svytotal)
  yrbs_E[[paste0("age_", age_calc)]] <- 
    svyby(~so_new, by = ~sex,
          design = subset(yrbs_des, age == age_calc & year == 2015), svymean)
}

## BRFSS ####

brfss_totals <- brfss_E <- list()
for(age_calc in 18:25){
  brfss_totals[[paste0("age_", age_calc)]] <- 
    svyby(~so_new, by = ~year+sex, design = subset(brfss_des, age == age_calc),
          svytotal)
  brfss_E[[paste0("age_", age_calc)]] <- 
    svyby(~so_new,  by = ~sex,
          design = subset(brfss_des, age == age_calc & year == 2015),
          svymean)
}

# Get test statistics & p-values ####

## YRBS ####
yrbs_test <- list()
for(age_calc in 14:17){
  for(sex_loop in c("Female", "Male")){
    observed <- yrbs_totals[[paste0("age_", age_calc)]][,-1] %>% 
      filter(sex == sex_loop) %>% 
      select(!contains("se.")) %>% select(-sex) %>%  t()
    exp_prop <- yrbs_E[[paste0("age_", age_calc)]] %>% 
      filter(sex == sex_loop) %>% 
      select(!contains("se.")) %>% select(-sex) %>% t()
    expected <- apply(observed, 2, function(x){sum(x)*exp_prop})
    stats <- (observed - expected)^2/expected
    yrbs_test[[paste0("age_", age_calc, "_", sex_loop)]] <-
      1 - pchisq(colSums(stats), df = 4)
  }
}


## BRFSS ####
brfss_test <- list()
for(age_calc in 18:25){
  for(sex_loop in c("Female", "Male")){
    observed <- brfss_totals[[paste0("age_", age_calc)]][,-1] %>% 
      filter(sex == sex_loop) %>% 
      select(!contains("se.")) %>% select(-sex) %>% t()
    exp_prop <- brfss_E[[paste0("age_", age_calc)]] %>% 
      filter(sex == sex_loop) %>% 
      select(!contains("se.")) %>% select(-sex) %>% t()
    expected <- apply(observed, 2, function(x){sum(x)*exp_prop})
    stats <- (observed - expected)^2/expected
    brfss_test[[paste0("age_", age_calc, "_", sex_loop)]] <-
      1 - pchisq(colSums(stats), df = 4)
  }
}

## Save tables ####
### YRBS ####
yrbs_tab_list <- list()
for(age_calc in 14:17){
  for(sex_loop in c("Female", "Male")){
    yrbs_tab_list[[paste0("age_", age_calc,
                          "_", sex_loop)]] <- 
      yrbs_test[[paste0("age_", age_calc,
                        "_", sex_loop)]] %>% 
      t() %>% as.data.frame() %>% 
      pivot_longer(cols = contains("."),
                   names_to = c(".value", "sex"),
                   names_sep = "\\.") %>% 
      select(-`2015`) %>% 
      mutate(age = age_calc) %>% 
      relocate("age", .before = "2017")
  }
}

yrbs_tab <- do.call(rbind.data.frame, yrbs_tab_list)
write.csv(yrbs_tab, row.names = FALSE,
          file = paste0("tables/SO_Q3/ChiSquared_14_17.csv"))

### BRFSS ####
brfss_tab_list <- list()
for(age_calc in 18:25){
  for(sex_loop in c("Female", "Male")){
  brfss_tab_list[[paste0("age_", age_calc,
                         "_", sex_loop)]] <- 
    brfss_test[[paste0("age_", age_calc,
                       "_", sex_loop)]]  %>% 
    t() %>% as.data.frame() %>% 
    pivot_longer(cols = contains("."),
                 names_to = c(".value", "sex"),
                 names_sep = "\\.") %>% 
    select(-`2015`) %>% 
    mutate(age = age_calc) %>% 
    relocate("age", .before = "2014") 
  } 
}

brfss_tab <- do.call(rbind.data.frame, brfss_tab_list)
write.csv(brfss_tab, row.names = FALSE,
          file = paste0("tables/SO_Q3/ChiSquared_18_25.csv"))


# Plot changes ####
## Get means by age & year ####
### YRBS ####

yrbs_means <- svyby(~so_new, by = ~sex + age + year,
                    design = yrbs_des, svymean)
names(yrbs_means)[4:8] <- paste0("mean.", names(yrbs_means)[4:8])

yrbs_prev <- yrbs_means %>% 
  pivot_longer(cols = contains("so_new"),
               names_to = c("Measure", ".value"),
               names_sep = "\\.") %>% 
  pivot_longer(cols = contains("so_new"),
               names_to = "SO",
               names_prefix = "so_new(.)_",
               values_to = "Value") %>% 
  pivot_wider(id_cols = c("sex", "age", "year", "SO"),
              names_from = "Measure",
              values_from = "Value") %>% 
  mutate(SO = factor(SO),
         lower = mean - qnorm(.975)*se,
         upper = mean + qnorm(.975)*se)

#### Bisexual ####
##### ggplot ####
bi_14_prev <- yrbs_prev %>% 
  filter(SO == "bi") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2),
         Year = factor(year)) %>% 
  ggplot(aes(x = age, y = mean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(~sex) +
  scale_color_manual(values = c("2015" = "navy", "2017" = "forestgreen",
                                "2019" = "goldenrod", "2021" = "firebrick")) +
  xlab("Age") +
  ylab("Prevalence") +
  theme_classic()

ggsave(filename = paste0("plots/SO_Q3/", "Prev_bi_14to18.png"), bi_14_prev)

##### base R ####

year_cols <- c("navy", "forestgreen", "goldenrod", "firebrick")

plotdat <- yrbs_prev %>% 
  filter(SO == "bi") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(13, 18)

png("plots/SO_Q3/Prev_bi_14to18_baseR.png")
{
  
  par(mfrow = c(1,2) , lend = 1)
  plot(plotdat$age[plotdat$sex == "Female"], 
       plotdat$mean[plotdat$sex == "Female"],
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.255),
       type = "n", axes = FALSE,
       main = "Bisexual: Female")
  axis(1, at = 14:17, labels = as.character(14:17))
  axis(2, at = seq(0, 0.25, 0.05))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr & sex == "Female")
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 4, cex = 1)
  
  plot(plotdat$age[plotdat$sex == "Male"], 
       plotdat$mean[plotdat$sex == "Male"],
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.255),
       type = "n", axes = FALSE,
       main = "Male")
  axis(1, at = 14:17, labels = as.character(14:17))
  axis(2, at = seq(0, 0.25, 0.05))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr & sex == "Male")
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
}
dev.off()


#### Lesbian/Gay ####
##### ggplot ####
lg_14_prev <- yrbs_prev %>% 
  filter(SO == "lesgay") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2),
         Year = factor(year)) %>% 
  ggplot(aes(x = age, y = mean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  facet_wrap(~sex) +
  scale_color_manual(values = c("2015" = "navy", "2017" = "forestgreen",
                                "2019" = "goldenrod", "2021" = "firebrick")) +
  
  xlab("Age") +
  ylab("Prevalence") +
  theme_classic()

ggsave(filename = paste0("plots/SO_Q3/", "Prev_lg_14to18.png"), lg_14_prev)

##### base R ####
plotdat <- yrbs_prev %>% 
  filter(SO == "lesgay") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(13, 18)

png("plots/SO_Q3/Prev_lg_14to18_baseR.png")
{
  par(mfrow = c(1,2) , lend = 1)
  plot(plotdat$age[plotdat$sex == "Female"], 
       plotdat$mean[plotdat$sex == "Female"],
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.10),
       type = "n", axes = FALSE,
       main = "Lesbian: Female")
  axis(1, at = 14:17, labels = as.character(14:17))
  axis(2, at = seq(0, 0.1, 0.02))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr & sex == "Female")
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 4, cex = 1)
  
  plot(plotdat$age[plotdat$sex == "Male"], 
       plotdat$mean[plotdat$sex == "Male"],
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.1),
       type = "n", axes = FALSE,
       main = "Gay: Male")
  axis(1, at = 14:17, labels = as.character(14:17))
  axis(2, at = seq(0, 0.1, 0.02))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr & sex == "Male")
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
}
dev.off()

#### Straight ####

yrbs_prev %>% 
  filter(SO == "straight") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2),
         Year = factor(year)) %>% 
  ggplot(aes(x = age, y = mean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Age") +
  ylab("Prevalence") 

#### Don't Know ####
yrbs_prev %>% 
  filter(SO == "dko") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2),
         Year = factor(year)) %>% 
  ggplot(aes(x = age, y = mean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Age") +
  ylab("Prevalence") 

##### base R ####
plotdat <- yrbs_prev %>% 
  filter(SO == "dko") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(13, 18)

png("plots/SO_Q3/Prev_dko_14to18_baseR.png")
{
  plot(plotdat$age, plotdat$mean,
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.15),
       type = "n", axes = FALSE,
       main = "")
  axis(1, at = 14:17, labels = as.character(14:17))
  axis(2, at = seq(0, 0.15, 0.025))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr)
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 4, cex = 1)
  title(main = "Don't Know", adj = 0.5, font.main = 1)
}
dev.off()


#### Refuse to answer ####
yrbs_prev %>% 
  filter(SO == "ref") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2),
         Year = factor(year)) %>% 
  ggplot(aes(x = age, y = mean, col = Year)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Age") +
  ylab("Prevalence") 

##### base R ####
plotdat <- yrbs_prev %>% 
  filter(SO == "ref") %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(13, 18)

png("plots/SO_Q3/Prev_ref_14to18_baseR.png")
{
  plot(plotdat$age, plotdat$mean,
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.15),
       type = "n", axes = FALSE,
       main = "")
  axis(1, at = 14:17, labels = as.character(14:17))
  axis(2, at = seq(0, 0.15, 0.025))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr)
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 4, cex = 1)
  title(main = "Refuse to Answer", adj = 0.5, font.main = 1)
}
dev.off()


## Get means by age & year ####
### BRFSS ####

brfss_means <- svyby(~so_new, by = ~age + year,
                    design = brfss_des, svymean)
names(brfss_means)[3:7] <- paste0("mean.", names(brfss_means)[3:7])

brfss_prev <- brfss_means %>% 
  pivot_longer(cols = contains("so_new"),
               names_to = c("Measure", ".value"),
               names_sep = "\\.") %>% 
  pivot_longer(cols = contains("so_new"),
               names_to = "SO",
               names_prefix = "so_new(.)_",
               values_to = "Value") %>% 
  pivot_wider(id_cols = c("age", "year", "SO"),
              names_from = "Measure",
              values_from = "Value") %>% 
  mutate(SO = factor(SO),
         lower = mean - qnorm(.975)*se,
         upper = mean + qnorm(.975)*se)

#### Bisexual ####

##### base R ####

year_cols <- c("navy", "forestgreen", "goldenrod", "firebrick")

plotdat <- brfss_prev %>% 
  filter(SO == "bi" & year %in% seq(2015, 2021, 2)) %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(17, 26)

png("plots/SO_Q3/Prev_bi_18to25_baseR.png")
{
  plot(plotdat$age, plotdat$mean,
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.20),
       type = "n", axes = FALSE,
       main = "")
  axis(1, at = 18:25, labels = as.character(18:25))
  axis(2, at = seq(0, 0.20, 0.025))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr)
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 1, cex = 1)
  title(main = "Bisexual", adj = 0.5, font.main = 1)
}
dev.off()




#### Lesbian/Gay ####

##### base R ####

year_cols <- c("navy", "forestgreen", "goldenrod", "firebrick")

plotdat <- brfss_prev %>% 
  filter(SO == "lesgay" & year %in% seq(2015, 2021, 2)) %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(17, 26)

png("plots/SO_Q3/Prev_lg_18to25_baseR.png")
{
  plot(plotdat$age, plotdat$mean,
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.20),
       type = "n", axes = FALSE,
       main = "")
  axis(1, at = 18:25, labels = as.character(18:25))
  axis(2, at = seq(0, 0.20, 0.025))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr)
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 1, cex = 1)
  title(main = "Lesbian/Gay", adj = 0.5, font.main = 1)
}
dev.off()

#### Don't Know ####

##### base R ####

year_cols <- c("navy", "forestgreen", "goldenrod", "firebrick")

plotdat <- brfss_prev %>% 
  filter(SO == "dko" & year %in% seq(2015, 2021, 2)) %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(17, 26)

png("plots/SO_Q3/Prev_dko_18to25_baseR.png")
{
  plot(plotdat$age, plotdat$mean,
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.20),
       type = "n", axes = FALSE,
       main = "")
  axis(1, at = 18:25, labels = as.character(18:25))
  axis(2, at = seq(0, 0.20, 0.025))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr)
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 1, cex = 1)
  title(main = "Don't Know", adj = 0.5, font.main = 1)
}
dev.off()

#### Refuse to Answer ####

##### base R ####

year_cols <- c("navy", "forestgreen", "goldenrod", "firebrick")

plotdat <- brfss_prev %>% 
  filter(SO == "ref" & year %in% seq(2015, 2021, 2)) %>% 
  mutate(age = case_when(year == 2015 ~ age - .2,
                         year == 2017 ~ age - .075,
                         year == 2019 ~ age + 0.075,
                         TRUE ~ age + .2))
y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
x_lims <- c(17, 26)

png("plots/SO_Q3/Prev_ref_18to25_baseR.png")
{
  plot(plotdat$age, plotdat$mean,
       xlab = "Age", ylab = "Prevalence",
       ylim = c(0, 0.20),
       type = "n", axes = FALSE,
       main = "")
  axis(1, at = 18:25, labels = as.character(18:25))
  axis(2, at = seq(0, 0.20, 0.025))
  
  years <- seq(2015, 2021, 2)
  for(yr in years){
    year_idx <- match(yr, years)
    tmpdat <- plotdat %>% 
      filter(year == yr)
    points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
    segments(x0 = tmpdat$age, y0 = tmpdat$lower,
             x1 = tmpdat$age, y1 = tmpdat$upper,
             lwd = 2, col = year_cols[year_idx])
  }
  
  legend("topright", pch = 19, col = year_cols,
         legend = years, bty = "n",
         ncol = 1, cex = 1)
  title(main = "Refuse to Answer", adj = 0.5, font.main = 1)
}
dev.off()




################# Final paper figure 


png("plots/SO_Q3/Prev_bilg_14to17_baseR.png", 
    width = 300*12, height = 300*6, res=300)
{

   par(mfrow = c(1,4) , lend = 1)
   
   year_cols <- c("navy", "forestgreen", "goldenrod", "firebrick")
   
   plotdat <- yrbs_prev %>% 
      filter(SO == "bi") %>% 
      mutate(age = case_when(year == 2015 ~ age - .2,
                             year == 2017 ~ age - .075,
                             year == 2019 ~ age + 0.075,
                             TRUE ~ age + .2))
   y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
   x_lims <- c(13, 18)
   
   plot(plotdat$age[plotdat$sex == "Female"], 
        plotdat$mean[plotdat$sex == "Female"],
        xlab = "Age", ylab = "Prevalence",
        ylim = c(0, 0.255),
        type = "n", axes = FALSE,
        main = "Bisexual female")
   axis(1, at = 14:17, labels = as.character(14:17))
   axis(2, at = seq(0, 0.25, 0.05))
   
   years <- seq(2015, 2021, 2)
   for(yr in years){
      year_idx <- match(yr, years)
      tmpdat <- plotdat %>% 
         filter(year == yr & sex == "Female")
      points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
      segments(x0 = tmpdat$age, y0 = tmpdat$lower,
               x1 = tmpdat$age, y1 = tmpdat$upper,
               lwd = 2, col = year_cols[year_idx])
   }
   
#   legend("topright", pch = 19, col = year_cols,
#          legend = years, bty = "n",
#          ncol = 4, cex = 1)
   
   plot(plotdat$age[plotdat$sex == "Male"], 
        plotdat$mean[plotdat$sex == "Male"],
        xlab = "Age", ylab = "Prevalence",
        ylim = c(0, 0.1),
        type = "n", axes = FALSE,
        main = "Bisexual male")
   axis(1, at = 14:17, labels = as.character(14:17))
   axis(2, at = seq(0, 0.1, 0.02))
   
   years <- seq(2015, 2021, 2)
   for(yr in years){
      year_idx <- match(yr, years)
      tmpdat <- plotdat %>% 
         filter(year == yr & sex == "Male")
      points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
      segments(x0 = tmpdat$age, y0 = tmpdat$lower,
               x1 = tmpdat$age, y1 = tmpdat$upper,
               lwd = 2, col = year_cols[year_idx])
   }
      
   plotdat <- yrbs_prev %>% 
      filter(SO == "lesgay") %>% 
      mutate(age = case_when(year == 2015 ~ age - .2,
                             year == 2017 ~ age - .075,
                             year == 2019 ~ age + 0.075,
                             TRUE ~ age + .2))
   y_lims <- c(min(plotdat$lower), max(plotdat$upper) + 0.01)
   x_lims <- c(13, 18)
   
   plot(plotdat$age[plotdat$sex == "Female"], 
        plotdat$mean[plotdat$sex == "Female"],
        xlab = "Age", ylab = "Prevalence",
        ylim = c(0, 0.10),
        type = "n", axes = FALSE,
        main = "Lesbian")
   axis(1, at = 14:17, labels = as.character(14:17))
   axis(2, at = seq(0, 0.1, 0.02))
   
   years <- seq(2015, 2021, 2)
   for(yr in years){
      year_idx <- match(yr, years)
      tmpdat <- plotdat %>% 
         filter(year == yr & sex == "Female")
      points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
      segments(x0 = tmpdat$age, y0 = tmpdat$lower,
               x1 = tmpdat$age, y1 = tmpdat$upper,
               lwd = 2, col = year_cols[year_idx])
   }
   
   plot(plotdat$age[plotdat$sex == "Male"], 
        plotdat$mean[plotdat$sex == "Male"],
        xlab = "Age", ylab = "Prevalence",
        ylim = c(0, 0.1),
        type = "n", axes = FALSE,
        main = "Gay male")
   axis(1, at = 14:17, labels = as.character(14:17))
   axis(2, at = seq(0, 0.1, 0.02))
   
   years <- seq(2015, 2021, 2)
   for(yr in years){
      year_idx <- match(yr, years)
      tmpdat <- plotdat %>% 
         filter(year == yr & sex == "Male")
      points(tmpdat$age, tmpdat$mean, pch = 19, col = year_cols[year_idx])
      segments(x0 = tmpdat$age, y0 = tmpdat$lower,
               x1 = tmpdat$age, y1 = tmpdat$upper,
               lwd = 2, col = year_cols[year_idx])
   }

   legend("topright", pch = 19, col = year_cols,
             legend = years, bty = "n",
             ncol = 4, cex = 1)
   
}

dev.off()



