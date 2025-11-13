# 必要なライブラリを読み込みます
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)

# -----------------------------------------------------------
# ステップ 1: 初期設定とデータ読み込み、前処理（再掲）
# -----------------------------------------------------------
# rm(list = ls()) # 実行環境によってはこの行はコメントアウトを推奨します
# setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") # 実行環境に合わせて変更してください

path <- "015/data/merge_final.csv" # ファイルパスを適切に設定してください
df <- read_csv(path, show_col_types = FALSE)

# group_type 列の作成
df <- df %>%
  mutate(
    group_type = str_extract(`participant.label`, "^base|^0\\d{2}|^\\d{2}")
  )

num_rounds <- 10

# calculate_max_flags 関数（修正版を再定義）
calculate_max_flags <- function(data, round_num) {
  r_contribute <- paste0("round ", round_num, "_contribute")
  r_payoff <- paste0("round ", round_num, "_payoff")
  r_id_subsession <- paste0("round ", round_num, "_id_subsession")
  
  new_col_contribute <- paste0("round ", round_num, "_max_contribute")
  new_col_payoff <- paste0("round ", round_num, "_max_payoff")
  
  data <- data %>%
    group_by(group_type, across(all_of(r_id_subsession))) %>%
    mutate(
      max_contribute_group = max(.data[[r_contribute]], na.rm = TRUE),
      
      # contributeの最大値と比較し、さらに「contributeが0でない」ことを条件に追加
      !!new_col_contribute := ifelse(
        .data[[r_contribute]] == max_contribute_group & .data[[r_contribute]] != 0,
        1,
        0
      ),
      
      !!new_col_payoff := ifelse(
        .data[[r_payoff]] == max(.data[[r_payoff]], na.rm = TRUE),
        1,
        0
      ),
      
      max_contribute_group = NULL 
    ) %>%
    ungroup()
  
  return(data)
}

# 全てのラウンドに対して最大値フラグの計算処理を適用
df2 <- df
for (i in 1:num_rounds) {
  df2 <- calculate_max_flags(df2, i)
}

# max_c_and_p列の作成
for (i in 1:num_rounds) {
  col_contribute <- paste0("round ", i, "_max_contribute")
  col_payoff <- paste0("round ", i, "_max_payoff")
  new_col_combined <- paste0("round ", i, "_max_c_and_p")
  
  df2 <- df2 %>%
    mutate(
      !!new_col_combined := ifelse(
        .data[[col_contribute]] == 1 & .data[[col_payoff]] == 1,
        1,
        0
      )
    )
}

# -----------------------------------------------------------
# ステップ 3: グループフラグ列の作成関数と適用 (修正部分)
# -----------------------------------------------------------

#' participant.labelに基づいてバイナリフラグ列を作成する関数
#'
#' @param data データフレーム
#' @param group_names フラグ列を作成したいグループ名のベクトル (例: c("base", "015", "03", "045"))
#' @return 新しいフラグ列が追加されたデータフレーム
create_group_flag_columns <- function(data, group_names) {
  
  for (group in group_names) {
    # 【修正点】: '015', '03', '045' に X プレフィックスを付けず、そのままの列名を使用
    # Rで数字から始まる列名を扱う際はバッククォートを使用
    col_name <- ifelse(group == "base", "Base", group) 
    
    # Rで動的な列名とバッククォートを組み合わせる
    data <- data %>%
      mutate(
        # !!as.name(col_name) はバッククォートで囲まれた列名として解釈される
        !!as.name(col_name) := ifelse(
          group_type == group,
          1,
          0
        )
      )
  }
  
  return(data)
}

# 関数を適用して新しい列を作成
target_groups <- c("base", "015", "03", "045")
df2 <- create_group_flag_columns(df2, target_groups)



write.csv(
  df2, 
  file = "015/data/merge_final_dummy_c0.csv",
  row.names = FALSE,   
  quote = FALSE       
)