# 1. パッケージのロードと初期設定
library(tidyverse) 
library(readr)
rm(list = ls())
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ/015/cleaning")

# 2. ファイルの読み込み
df_comp <- read_csv("merge_045.csv")

# 4. ラウンドごとの平均貢献額の計算とグラフ描画

# 4-1. グラフ作成のために、ワイド型から貢献額の列だけを抽出してロング型に戻す
df_comp_long_contribute <- df_comp %>%
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
    y = "The amount of average contribution" # 縦軸：the amount of average contribution
  ) +
  
  # 縦軸の範囲を0から10に固定
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) + 
  
  # 横軸の目盛りを整数値（ラウンド番号）のみに設定
  scale_x_continuous(breaks = unique(df_average_contribution$round_number)) 

# デフォルトテーマ (theme_grey()) を使用

# 4-4. グラフをPNG形式で保存
ggsave("average_contribution_plot_045.png", plot = plot_contribution, width = 8, height = 5)


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

