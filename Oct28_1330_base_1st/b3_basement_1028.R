# 1. パッケージのロードと初期設定
library(tidyverse) 
library(readr)
rm(list = ls())
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ/exp_base/Oct28_1330_base_1st")

# 2. ファイルの読み込み
df_exp1 <- read_csv("b3_socialPt_2025-10-28.csv")
df_exp1_f <- subset(df_exp1, participant._current_page_name == "final")

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
  "session.is_demo"
)

df_exp1_cleaned <- df_exp1_f %>%
  select(-all_of(cols_to_drop))
# 4. ワイド化のインデックス（ID変数）として残す列
id_vars_for_pivot <- c(
  "participant.id_in_session", 
  "participant.code", 
  "participant.label",
  "participant.payoff" # participant.payoffはラウンド間で同じ値を持つためIDとします
)

# 5. ワイド型への変換と列名の変更（_1 -> _α, _2 -> _β）
df_exp1_wide <- df_exp1_cleaned %>%
  pivot_wider(
    id_cols = all_of(id_vars_for_pivot),
    names_from = `subsession.round_number`,
    # values_from で id_cols と names_from 以外のすべての列を指定
    values_from = -c(all_of(id_vars_for_pivot), `subsession.round_number`),
    names_prefix = "",
    names_sep = "_",
    names_repair = "unique"
  ) %>%
  # 列名内のラウンド番号接尾辞を一括置換
  rename_with(.fn = ~gsub("_1", "_α", .x), .cols = everything()) %>%
  rename_with(.fn = ~gsub("_2", "_β", .x), .cols = everything())

df_exp1_wide <- df_exp1_wide %>%
  mutate(
    alpha_med = case_when(
      player.select_num_α == 1 ~ -0.313,
      player.select_num_α == 2 ~ -0.3015,
      player.select_num_α == 3 ~ -0.2785,
      player.select_num_α == 4 ~ -0.254,
      player.select_num_α == 5 ~ -0.2275,
      player.select_num_α == 6 ~ -0.1995,
      player.select_num_α == 7 ~ -0.1695,
      player.select_num_α == 8 ~ -0.137,
      player.select_num_α == 9 ~ -0.1015,
      player.select_num_α == 10 ~ -0.063,
      player.select_num_α == 11 ~ -0.0215,
      player.select_num_α == 12 ~ 0.024,
      player.select_num_α == 13 ~ 0.074,
      player.select_num_α == 14 ~ 0.129,
      player.select_num_α == 15 ~ 0.19,
      player.select_num_α == 16 ~ 0.258,
      player.select_num_α == 17 ~ 0.3345,
      player.select_num_α == 18 ~ 0.421,
      player.select_num_α == 19 ~ 0.519,
      player.select_num_α == 20 ~ 0.6315,
      player.select_num_α == 21 ~ 0.7625,
      player.select_num_α == 22 ~ 0.9165,
      player.select_num_α == 23 ~ 1.1,
      player.select_num_α == 24 ~ 1.322,
      player.select_num_α == 25 ~ 1.597,
      player.select_num_α == 26 ~ 1.9465,
      player.select_num_α == 27 ~ 2.405,
      player.select_num_α == 28 ~ 3.0335,
      player.select_num_α == 29 ~ 3.95,
      player.select_num_α == 30 ~ 5.4165,
      player.select_num_α == 31 ~ 8.1665,
      player.select_num_α == 32 ~ 10,
      # 31以外の値が入った場合の処理
      TRUE ~ NA_real_
    )
  )
df_exp1_wide <- df_exp1_wide %>%
  mutate(
    beta_med = case_when(
      player.select_num_β == 1 ~ -7,
      player.select_num_β == 2 ~ -5,
      player.select_num_β == 3 ~ -2.3335,
      player.select_num_β == 4 ~ -1.3335,
      player.select_num_β == 5 ~ -0.8,
      player.select_num_β == 6 ~ -0.4665,
      player.select_num_β == 7 ~ -0.238,
      player.select_num_β == 8 ~ -0.0715,
      player.select_num_β == 9 ~ 0.0555,
      player.select_num_β == 10 ~ 0.1555,
      player.select_num_β == 11 ~ 0.2365,
      player.select_num_β == 12 ~ 0.303,
      player.select_num_β == 13 ~ 0.359,
      player.select_num_β == 14 ~ 0.407,
      player.select_num_β == 15 ~ 0.448,
      player.select_num_β == 16 ~ 0.4835,
      player.select_num_β == 17 ~ 0.5145,
      player.select_num_β == 18 ~ 0.5425,
      player.select_num_β == 19 ~ 0.5675,
      player.select_num_β == 20 ~ 0.5895,
      player.select_num_β == 21 ~ 0.6095,
      player.select_num_β == 22 ~ 0.6275,
      player.select_num_β == 23 ~ 0.644,
      player.select_num_β == 24 ~ 0.6595,
      player.select_num_β == 25 ~ 0.678467,
      player.select_num_β == 26 ~ 0.70194,
      player.select_num_β == 27 ~ 0.726381,
      player.select_num_β == 28 ~ 0.751756,
      player.select_num_β == 29 ~ 0.778209,
      player.select_num_β == 30 ~ 0.805909,
      player.select_num_β == 31 ~ 0.835046,
      player.select_num_β == 32 ~ 0.85,
      TRUE ~ NA_real_ # 31以外の値が入った場合はNA
    )
  )
summary(df_exp1_wide$player.payoff_β)

# 8. alpha_medとbeta_medの組み合わせの頻度を計算
df_plot <- df_exp1_wide %>%
  group_by(alpha_med, beta_med) %>%
  summarise(
    count = n(),
    .groups = 'drop' 
  )

# 9. alpha_medとbeta_medの散布図を作成（凡例を非表示、軸範囲を調整）
scatter_plot <- df_plot %>%
  ggplot(aes(x = alpha_med, y = beta_med)) +
  geom_point(aes(size = count), color = "#0072B2", alpha = 0.8) +
  guides(size = "none") + 
  # 軸の範囲を調整 (今回の修正箇所)
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(-5, 2)) + 
  labs(
    x = expression(alpha[med]),
    y = expression(beta[med]),
  ) +
  scale_size_continuous(range = c(2, 10)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")

# プロットの保存 (R環境で実行する場合)
ggsave("alpha_med_vs_beta_med_scatter_zoomed.png", plot = scatter_plot, width = 7, height = 5, dpi = 300)
print(scatter_plot)
