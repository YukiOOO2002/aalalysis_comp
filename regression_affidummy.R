rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(broom)
library(gt)
library(modelsummary)
library(plm)
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 

modelglm_norm <- glm(
  formula = contribute_full~
    a015 + a03 + a045+
    science + graduate +
    age + gender ,
  data = df,
  family = binomial(link = "probit") 
)
modelglm_arr <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science + graduate +
    age + gender ,
  data = df,
  family = binomial(link = "probit") 
)

modelglm_arr11 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    a015*beta_positive + a03*beta_positive + a045*beta_positive+
    science + graduate +
    age + gender ,
  data = df,
  family = binomial(link = "probit") 
)

modelglm_arrnon <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science + graduate +
    age + gender  ,
  data = df,
  family = binomial(link = "probit") 
)

modelglm_arr11non <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    a015*beta_positive + a03*beta_positive + a045*beta_positive+
    science + graduate +
    age + gender ,
  data = df,
  family = binomial(link = "probit") 
)


save_dir <- "015/regression/" 
file_name <- "resultsaffidummmy.html"
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr, modelglm_arr11, modelglm_arrnon, modelglm_arr11non,
          type = "html",
          out = full_path, 
          title = "Probit Model Results WITH AFFILIATION DUMMY")

model_plm_dum <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science + graduate + science*graduate+
    age + gender  ,
  data = df,
  effect = "time",
  model = "within", 
  index = c("participant.label", "round")
)

model_plm_arr_multi <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science + graduate + 
    age + gender  ,
  data = df,
  effect = "time",
  model = "within", 
  index = c("participant.label", "round")
)

model_plm_dumr <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science + graduate +
    age + gender  ,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multir <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science + graduate +
    age + gender  ,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round"))

model_plm_arr_multii <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science + graduate + science*graduate+
    age + gender  ,
  data = df,
  effect = "time",
  model = "within", 
  index = c("participant.label", "round")
)

model_plm_arr_multiri <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science + graduate + science*graduate+
    age + gender  ,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

file_name1 <- "FEtime_afficumyy.html"
full_path1 <- paste0(save_dir, file_name1)


stargazer(model_plm_arr_multii,model_plm_arr_multiri,
          type = "html",
          out = full_path1,
          title = "FEtime Regression")

hausman <- phtest(model_plm_dum,model_plm_dumr)
print(hausman)

hausman2 <- phtest(model_plm_arr_multi,model_plm_arr_multir)
print(hausman2)
