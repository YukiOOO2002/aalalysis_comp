# 1. パッケージのロードと初期設定
library(tidyverse) 
library(readr)
rm(list = ls())
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ/015/Nov10_1330_045_2nd")

# 2. ファイルの読み込み
df_comp <- read_csv("comp_demo_045_2025-11-10_1330.csv")
df_comp_f <- subset(df_comp, participant._current_page_name == "final")

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

df_comp_cleaned <- df_comp_f %>%
  select(-all_of(cols_to_drop))

df_comp_wide <- df_comp_cleaned %>%
  # 3-1. pivot_widerで新しい列名を作成するために、元の列名をベース名にリネームします。
  #      最後の引数 (total_contribution) の後ろにはカンマを付けません。
  rename(
    contribute = player.individual_choice, # <--- 修正: contribute に統一し、カンマあり
    mpcr = player.assigned_mpcr,
    payoff = player.round_payoff,
    total_contribution = group.total_contribution # <--- 修正: ここにカンマは不要です
  ) %>%
  # 3-2. participant.labelをキーとしてロング型からワイド型へ変換します。
  pivot_wider(
    # 新しいデータセットの行を識別するキー（ID）
    id_cols = participant.label, 
    
    # 新しい列名の一部となる値を含む列（ラウンド数 n）
    names_from = subsession.round_number, 
    
    # ワイドに展開する列（リネーム後のベース名）
    # <--- 修正: contribute に統一
    values_from = c(contribute, mpcr, payoff, total_contribution), 
    
    # 新しい列名を作成するためのパターン定義
    names_glue = "round {subsession.round_number}_{.value}"
  )

# 4. ラウンドごとの平均貢献額の計算とグラフ描画

# 4-1. グラフ作成のために、ワイド型から貢献額の列だけを抽出してロング型に戻す
df_comp_long_contribute <- df_comp_wide %>%
  # 'contribute'で終わる全ての列（例: round 1_contribute, round 2_contribute, ...）を選択
  pivot_longer(
    cols = ends_with("contribute"),
    names_to = "round_col",
    values_to = "contribution"
  ) %>%
  # round_col (例: "round 1_contribute") からラウンド番号（n）を正規表現で抽出
  mutate(
    # str_extractで数値部分(\\d+)を抽出し、整数型に変換
    round_number = as.integer(str_extract(round_col, "\\d+"))
  )

# 4-2. ラウンドごとに貢献額の平均を計算
df_average_contribution <- df_comp_long_contribute %>%
  group_by(round_number) %>%
  summarise(
    average_contribution = mean(contribution, na.rm = TRUE) # 欠損値を除いて平均を計算
  ) %>%
  ungroup()

# 4-3. 折れ線グラフの描画（線と点を黒色、Y軸範囲固定）
plot_contribution <- df_average_contribution %>%
  ggplot(aes(x = round_number, y = average_contribution)) +
  
  # 折れ線グラフ（線）の色を黒色に設定
  geom_line(color = "black", linewidth = 1) +
  
  # 各データポイント（点）の色を黒色に設定
  geom_point(color = "black", size = 3) +
  
  # 軸ラベルとタイトルを設定
  labs(
    title = "Average Contribution by Round",
    x = "Round Number", # 横軸：round_number
    y = "Average Contribution" # 縦軸：the amount of average contribution
  ) +
  
  # 縦軸の範囲を0から10に固定
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) + 
  
  # 横軸の目盛りを整数値（ラウンド番号）のみに設定
  scale_x_continuous(breaks = unique(df_average_contribution$round_number)) 

# デフォルトテーマ (theme_grey()) を使用

# 4-4. グラフをPNG形式で保存
ggsave("average_contribution_plot1106.png", plot = plot_contribution, width = 8, height = 5)


library(dplyr) # summarizeなどの関数を使用するため

# 6. 全ラウンドのcontributionに関する記述統計量の計算 (標準偏差を含む)
# df_comp_long_contribute は前々回のコードで作成されています。

df_summary_contribute_sd <- df_comp_long_contribute %>%
  # グループ化はせず、データセット全体を対象とします
  summarise(
    # 平均値
    Mean = mean(contribution, na.rm = TRUE),
    
    # 中央値
    Median = median(contribution, na.rm = TRUE),
    
    # 標準偏差 (sd = Standard Deviation)
    Standard_Deviation = sd(contribution, na.rm = TRUE),
    
    # サンプル数 (欠損値を除いた観測値の数)
    N = sum(!is.na(contribution))
  )

