#1
install.packages("tidyverse")  # パッケージのインストール
library("tidyverse")  # ライブラリの読み出し
df <- read_csv("https://raw.githubusercontent.com/s1ok69oo/causal_inference_100knock/main/data/causal_knock1.csv")  # データの読み込み
?head
head(df, n = 5)

#----------------------------------------------
#2
install.packages("ggplot2")
install.packages("dagitty")
install.packages("dagitty")

library("ggplot2")
library("dagitty")
library("ggdag")

dag <- dagitty("dag {
  x -> y <- t  # RCTであるから矢印はこれだけ
  }")

tidy_dagitty(dag)
ggdag(dag)

#----------------------------------------------
#3
df[c('y', 'x')]
dif_y_x <- df$y - df$x
mean(dif_y_x)
# パイプ演算子を用いた方法
c(df$y - df$x) %>%
  mean()

#----------------------------------------------
#4
library("dplyr")
colnames(df)  # 列の名前の確認
df_tmp <- df %>% 
  select(y_t0, y_t1)  # 後ほどdiff関数を使うのでこの順にしている
df_tmp

apply(df_tmp, 1, diff)  # 行ごとに差をとる

# パイプ演算子を用いて以下のように簡潔に記述できる
ite <- df %>% 
  select(y_t0, y_t1)  %>% 
  apply(1, diff)  # 行ごとに差をとる
ite

#----------------------------------------------
#5
ate <- mean(ite)
ate

#----------------------------------------------
#6
# 準備
df_t1 <- df %>% filter(t == 1)
head(df_t1)
nrow(df_t1)  # 行数を確認
df_t1$t

# attを求める
att <- df_t1 %>%
  select(y_t0, y_t1) %>%
  apply(1, diff) %>%
  mean()
att

#----------------------------------------------
#7
cate <- df %>%
  filter(x > 10) %>%
  select(y_t0, y_t1) %>%
  apply(1, diff) %>%
  mean()
cate

#----------------------------------------------
#8
# 準備
df_t0 <- df %>% 
  filter(t == 0)

# ヒストグラムの描画
df_t1$y %>%
  hist(
    main = "Histogram of y",
    xlab = "y",
    ylim = c(0,10),  # df_t0の為にy軸に少し余裕を持たせる
    col = "#ffa00050"
    )
df_t0$y %>%
  hist(
    col = "#00a0ff50",
    add = T  # 前のヒストグラムに上書き
  )

#----------------------------------------------
#9
#分析レポートの提供はランダムに割り振られている
e_y1 <- mean(df_t1$y)  # 観測されたE[Y_1]を求める
e_y0 <- mean(df_t0$y)  # 観測されたE[Y_0]を求める
ate_rct <- e_y1 - e_y0
ate_rct

# 確認作業
df_t1 %>% 
  apply(2, mean)  # 列に対する平均を求める. e_y1とy,y_t1の平均が等しいことを確認
df_t0 %>% 
  apply(2, mean)  # 列に対する平均を求める. e_y0とy,y_t0の平均が等しいことを確認


#----------------------------------------------
#10
t.test(
  df_t1$y, df_t0$y,  # 処置群と対照群それぞれの結果変数の平均に対して差があるか検定
  alternative = "greater"  # 対立仮説がE[Y_1] > E[Y_0]なので右片側検定
)
