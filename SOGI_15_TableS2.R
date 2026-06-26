## Get sample sizes for Table S2?
## Jessica Godwin

# Setup ####
rm(list = ls())

## Libraries ####
library(tidyverse)
library(flextable)
library(officer)


## Functions ####
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}

# Load Data ####

## Full ####
combo <- readRDS(file = "data - clean/combined.rds")

str(combo)
summary(combo)
head(combo)
tableNA(combo$so)
tableNA(combo$so_new)

## Period Estimates ####
brfss_prev <- read_csv("data - clean/BRFSS_HT_P_prevs.csv")
brfss_prev_all <- read_csv("data - clean/BRFSS_HT_P_old_prevs.csv")
yrbs_prev <- read_csv("data - clean/YRBS_HT_P_prevs.csv")
yrbs_prev_21 <- read_csv("data - clean/YRBS2021_HT_P_prevs.csv")

# Tables ####

## Raw N ####
table_sampsize <- combo %>%
  filter(!(source == "YRBS" & age == 18)) %>% 
  group_by(year, so_new) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(Total = sum(n, na.rm = TRUE),
         sm_identity = Total - n[1]) %>% 
  pivot_wider(id_cols = c("year", "Total", "sm_identity"),
              names_from = "so_new",
              values_from = "n") %>% 
  select(-`1_straight`) %>% 
  rename("Year" = "year", "N" = "Total",
         "Sexual Minority" = "sm_identity",
         "Lesbian/Gay" = "2_lesgay",
         "Bisexual" = "3_bi",
         "Other response" = "4_dko",
         "Declined to Answer" = "5_ref") %>% 
  mutate(Straight = N - `Sexual Minority`)


table_sampsize <- combo %>%
  filter((source == "YRBS" & age != 18)) %>% 
  group_by(year, so_new) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(Total = sum(n, na.rm = TRUE),
         sm_identity = Total - n[1]) %>% 
  pivot_wider(id_cols = c("year", "Total", "sm_identity"),
              names_from = "so_new",
              values_from = "n") %>% 
  select(-`1_straight`) %>% 
  rename("Year" = "year", "N" = "Total",
         "Sexual Minority" = "sm_identity",
         "Lesbian/Gay" = "2_lesgay",
         "Bisexual" = "3_bi",
         "Other response" = "4_dko",
         "Declined to Answer" = "5_ref") %>% 
  mutate(Straight = N - `Sexual Minority`)

table_sampsize %>% 
  View()
table_sampsize_y %>% 
  View()

## Raw Proportion ####

table_prop <- table_sampsize %>%
  pivot_longer(cols = 3:ncol(.),
               names_to = "SO",
               values_to = "Count") %>% 
  group_by(Year) %>% 
  mutate(Prop = Count/N) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("Year", "N"),
              names_from = "SO",
              values_from = "Prop")

table_prop_y <- table_sampsize_y %>%
  pivot_longer(cols = 3:ncol(.),
               names_to = "SO",
               values_to = "Count") %>% 
  group_by(Year) %>% 
  mutate(Prop = Count/N) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("Year", "N"),
              names_from = "SO",
              values_from = "Prop")


table_prop %>% 
  View()

## Weighted Proportions
brfss_w <- brfss_prev_all %>% 
  mutate(ci = paste0(format(round(mean, 3), 3), " (",
                     format(round(mean - 1.96*se, 3), 3), ",",
                     format(round(mean + 1.96*se, 3), 3), ")")) %>% 
  pivot_wider(id_cols = c("sex", "so"),
              names_from = "period",
              values_from = "ci") %>% 
  mutate(so = case_when(so == "str" ~ "Straight",
                        so == "lesgay" ~ "Lesbian/\nGay",
                        so == "bi" ~ "Bisexual",
                        so == "dko" ~ "Other responses\n(Combined)",
                        so == "dk" ~ "Don't know/\nNot sure",
                        so == "o" ~ "Other",
                        so == "ref" ~ "Declined to\nanswer"))

brfss_w_land <- brfss_prev_all %>% 
  mutate(ci = paste0(format(round(mean, 3), 3), " (",
                     format(round(mean - 1.96*se, 3), 3), ",",
                     format(round(mean + 1.96*se, 3), 3), ")")) %>% 
  mutate(so = case_when(so == "str" ~ "Straight",
                        so == "lesgay" ~ "Lesbian/\nGay",
                        so == "bi" ~ "Bisexual",
                        so == "dko" ~ "Other responses\n(Combined)",
                        so == "dk" ~ "Don't know/\nNot sure",
                        so == "o" ~ "Other",
                        so == "ref" ~ "Declined to\nanswer")) %>% 
  pivot_wider(id_cols = c("sex", "period"),
              names_from = "so",
              values_from = "ci") 

yrbs_w <- bind_rows(yrbs_prev, yrbs_prev_21) %>% 
  unique() %>% 
  filter(so != "pnta") %>% 
  mutate(ci = paste0(format(round(mean, 3), 3), " (",
                     format(round(mean - 1.96*se, 3), 3), ",",
                     format(round(mean + 1.96*se, 3), 3), ")")) %>%
  mutate(so = case_when(so == "str" ~ "Straight",
                        so == "lesgay" ~ "Lesbian/\nGay",
                        so == "bi" ~ "Bisexual",
                        so %in% c("dko") ~ "Other responses\n(Combined)",
                        so == "dkns" ~ "Don't know/\nNot sure",
                        so == "dkwtm" ~ "Don't know\nwhat this\nmeans",
                        so == "somethingelse" ~ "Other",
                        so %in% c("ref") ~ "Declined to\nanswer")) %>% 
  pivot_wider(id_cols = c("sex", "period"),
              names_from = "so",
               values_from = "ci") #%>% 


flextable(brfss_w) %>% 
  set_header_labels(j = 1:2, 
                    values = c(sex = "Sex", so = "SO")) %>% 
  merge_v(j = 1) %>% 
  hline(i = 7) %>%
  hline(i = c(1, 3, 6, 8, 10, 13), j = 2:10) %>% 
  # autofit() %>% 
  width(j = 1, 0.6) %>% 
  width(j = 2, 1) %>% 
  width(j = 3:10, width = 0.6) %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all") %>% 
  bold(part = "header") %>% 
  bold(j = 1:2, part = "body") %>% 
  save_as_docx(path = "BRFSS_P_tab.docx")

flextable(brfss_w_land %>% 
            filter(sex == "Female") %>% 
            mutate(period = as.character(period))) %>% 
  set_header_labels(j = 1:2, 
                    values = c(sex = "Sex", period = "Period")) %>% 
  merge_v(j = 1) %>% 
  hline(i = 8) %>%
  vline(j = c(3, 5, 8), part = "body") %>% 
  # autofit() %>% 
  width(j = 1, 0.6) %>% 
  width(j = 2, 0.6) %>% 
  width(j = 3:9, width = 0.9) %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all") %>% 
  bold(part = "header") %>% 
  bold(j = 1:2, part = "body") %>% 
  save_as_docx(path = "BRFSS_P_tab_F_land.docx",
               pr_section = prop_section(page_size = 
                                           page_size(orient = "landscape",
                                                     width = 8.3, 
                                           )))

flextable(brfss_w_land %>% 
            filter(sex == "Male") %>% 
            mutate(period = as.character(period))) %>% 
  set_header_labels(j = 1:2, 
                    values = c(sex = "Sex", period = "Period")) %>% 
  merge_v(j = 1) %>% 
  hline(i = 8) %>%
  vline(j = c(3, 5, 8), part = "body") %>% 
  # autofit() %>% 
  width(j = 1, 0.6) %>% 
  width(j = 2, 0.6) %>% 
  width(j = 3:9, width = 0.9) %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all") %>% 
  bold(part = "header") %>% 
  bold(j = 1:2, part = "body") %>% 
  save_as_docx(path = "BRFSS_P_tab_M_land.docx",
               pr_section = prop_section(page_size = 
                                           page_size(orient = "landscape",
                                                     width = 8.3, 
                                           )))
flextable(yrbs_w %>% 
            arrange(sex) %>% 
            select(1:6, 8:10, 7) %>% 
            mutate(period = as.character(period))) %>% 
  set_header_labels(j = 1:2, 
                    values = c(sex = "Sex", period = "Period")) %>% 
  merge_v(j = 1) %>% 
  hline(i = 4) %>%  
  vline(j = c(3, 5, 9), part = "body") %>% 
  width(j = 1, 0.6) %>% 
  width(j = 2, 0.6) %>% 
  width(j = 3:10, width = .9) %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all") %>%
  # fontsize(size = 9, part = "body") %>%
  bold(part = "header") %>% 
  bold(j = 1:2, part = "body") %>% 
  save_as_docx(path = "YRBS_P_tab.docx",
               pr_section = prop_section(page_size = 
                                           page_size(orient = "landscape",
                                                     width = 8.3, 
                                                     height = 11.7)))

