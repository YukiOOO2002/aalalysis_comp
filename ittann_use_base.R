rm(list = ls())
library(tidyverse)
library(readr) 
library(dplyr)
library(stringr)

#データ読み込み（メイン＋質問コントロール＋）
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path_1st <- "015/Oct28_1330_base_1st/comp_demo_base_2025-10-28.csv"
df_1st <- read_csv(path_1st, show_col_types = FALSE)
pathq1 <- "015/Oct28_1330_base_1st/q_2025-10-28.csv"
df_q1 <- read_csv(pathq1, show_col_types = FALSE)



#不必要な列をドロップ
cols_to_drop <- c(
  "participant._is_bot",
  "participant._index_in_pages",
  "participant._max_page_index",
  "participant._current_app_name",
  "participant._current_page_name",
  "participant.time_started_utc",
  "participant.visited",
  "participant.mturk_worker_id",
  "participant.mturk_assignment_id",
  "player.role",
  "session.code",
  "session.label",
  "session.mturk_HITId",
  "session.mturk_HITGroupId",
  "session.comment",
  "session.is_demo",
  "player.payoff",
  "player.round_data",
  "group.individual_share"
)

#列を被験者番号順に
df_1st <- df_1st %>% 
  arrange(participant.label)

#質問も同様にマージ＆並べ替え

df_q1 <- df_q1 %>% 
  arrange(participant.label) %>% 
  mutate(
    participant.label = paste0("015", participant.label)
  )



#列名分かりやすく
df_wide <- df_1st %>%
  rename(
    contribute = player.individual_choice, 
    mpcr = player.assigned_mpcr,
    payoff = player.round_payoff,
    total_contribution = group.total_contribution 
  ) %>%
  pivot_wider(
    id_cols = c(participant.label), 
    names_from = subsession.round_number,
    values_from = c(contribute, mpcr, payoff, total_contribution),
    names_glue = "round {subsession.round_number}_{.value}"
  )

#質問とマージ
df_wide2 <- df_wide %>% 
  left_join(
    df_q1 %>% 
      select(participant.label,player.gender, player.age,player.affiliation),
    by = "participant.label"
  )
df_wide2 <- df_wide2 %>%
  
  mutate(
    participant.label = paste0("base", participant.label)
  )

write.csv(
  df_wide2, 
  file = "015/cleaning/base_ittan2.csv",
  row.names = FALSE,   
  quote = FALSE       
)

