#------------------------------------------------------------
#This R script was presented as a poster at the 76th Annual Meeting of the Japanese Association for Mathematical Sociology on March 17, 2024. 
#The title was "Reevaluation of the 'Under-Enrollment' Problem in Private Universities: Focusing on Time-Dependent Treatments and Confounding Factors."
#If you need a translation of the following description, which is in Japanese, please feel free to contact me, and I will provide you with an English translation.
#------------------------------------------------------------

#------------------------------------------------------------
#準備
#------------------------------------------------------------
#Rのバージョン確認
R.version

#ワークスペースのクリア: オブジェクト一括削除
rm(list = ls())

#データ取り込み
d <- read.csv(file.choose()) 
str(d)

#------------------------------------------------------------
#パッケージの読み込み
#------------------------------------------------------------
library("WeightIt")

#------------------------------------------------------------
#変数の生成
#------------------------------------------------------------
#欠測値を含む行を削除
d_no_na <- na.omit(d)
#入学定員充足率を計算
admission_rate <- d_no_na$enrolment / d_no_na$capa
#exposureを定義
exposure <- ifelse(admission_rate >= 1, 0, 1) #1以上であれば0, 0未満であれば1
#wave変数をtime変数に変換
d_no_na$time <- d_no_na$wave - 2012
#結果の確認
summary(admission_rate)
summary(exposure)
summary(d_no_na$time)

#------------------------------------------------------------
#IPW
#------------------------------------------------------------
#IPWを実行
w.out <- weightit(exposure ~ confounder + students + status + scienced + hospitald + womend + cityd + foundation + time,
                  data = d_no_na,                  method = "ps",
                  estimand = "ATE")
#IPWで重み付けされたデータの作成
weighted_data <- weightit(exposure ~ confounder + students + status + scienced + hospitald + womend + cityd + foundation + time,
                          data = d_no_na,
                          method = "ps",
                          estimand = "ATE")$weighted.data
#IPWを使ったモデルの推定（線形回帰）
model_ipw <- lm(outcome ~ exposure + confounder + students + status + scienced + hospitald + womend + cityd + foundation + time,
                data = d_no_na,
                weights = weighted_data$weights)  #重みを適用
#結果の表示
summary(model_ipw)
#標本サイズを取得
n_samples <- nrow(d_no_na)
print(n_samples)
