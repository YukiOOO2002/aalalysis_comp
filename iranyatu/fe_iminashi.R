rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(modelsummary)
library(plm)
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/merge_long.csv" 
df <- read_csv(path, show_col_types = FALSE) 

df2 <- df %>% 
  mutate(
    contribute_full = if_else(contribute == 10,1,0),
    contribute_non = if_else(contribute == 0,1,0),
  )

df3 <- df2 %>%
  mutate(
    is_base = as.integer(grepl("base", group_type, ignore.case = TRUE)),
    a015 = as.integer(grepl("015", group_type)),
    a03 = as.integer(grepl("03", group_type)),
    a045 = as.integer(grepl("045", group_type))
  )

fe_norm <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  model = "within",
  index = c("participant.label", "round"),
  effect = "time"
)

random_norm <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  model = "random",
  index = c("participant.label", "round"),
  effect = "time"
)

stargazer(fe_norm,random_norm,type = "text", title = "Probit Model Results (contribute_full)")

hausman_test <- phtest(fe_norm, random_norm)
print(hausman_test)
model_lm_arr_multi <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3,
  model = "random",
  index = c("participant.label", "round")
)
