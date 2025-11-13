rm(list = ls())
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)


setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") 

path <- "015/data/merge_final_dummy.csv" # ファイルパスを適切に設定してください
df <- read_csv(path, show_col_types = FALSE)

path2 <- "015/data/merge_final_dummy_c0.csv"
df2 <- read_csv(path2, show_col_types = FALSE)

df_long <- df %>%
  pivot_longer(
    # 'round n_'で始まる全ての列を指定
    cols = starts_with("round"),
    
    # 正規表現で列名を分解し、新しい列名を定義
    names_to = c("round", ".value"), 
    names_pattern = "round (\\d+)_(.*)"
    # (\\d+): ラウンド番号 (n) を抽出し、'round' 列へ
    # (.*): 変数名部分 (contribute, mpcrなど) を抽出し、新しい変数列へ
  ) %>%
  # round列を整数型に変換
  mutate(round = as.integer(round)) %>%
  # participant.labelとroundでソート
  arrange(`participant.label`, round)

df_long2 <- df2 %>%
  pivot_longer(
    # 'round n_'で始まる全ての列を指定
    cols = starts_with("round"),
    
    # 正規表現で列名を分解し、新しい列名を定義
    names_to = c("round", ".value"), 
    names_pattern = "round (\\d+)_(.*)"
    # (\\d+): ラウンド番号 (n) を抽出し、'round' 列へ
    # (.*): 変数名部分 (contribute, mpcrなど) を抽出し、新しい変数列へ
  ) %>%
  # round列を整数型に変換
  mutate(round = as.integer(round)) %>%
  # participant.labelとroundでソート
  arrange(`participant.label`, round)

write.csv(
  df_long, 
  file = "015/data/merge_long.csv",
  row.names = FALSE,   
  quote = FALSE       
)
write.csv(
  df_long2, 
  file = "015/data/merge_long_c0.csv",
  row.names = FALSE,   
  quote = FALSE       
)
