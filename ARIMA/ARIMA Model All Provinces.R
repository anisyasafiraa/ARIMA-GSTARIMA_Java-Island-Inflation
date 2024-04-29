setwd("D:/0.coolyeah/0.skripsi/github")
source('Stationary Test.R')
source('ARIMA/Model.R')

library(readxl)
data <- read_excel("data.xlsx")
columns = c("AR", "MA", "Ljung", "SW", "rmse", "mape", "rmseTest", "mapeTest")

# --------------------------------Banten Province--------------------------------
banten <- varStationary(data$Banten[1:180])$data
banten <- meanStationary(banten)$data
stats::acf(banten) # for MA order
stats::pacf(banten) # for AR order
ar <- list(1, 2, 9, c(1, 2), c(1, 9), c(2, 9), c(1, 2, 9))
ma <- list(1, 9, 12, c(1, 9), c(1, 12), c(9, 12), c(1, 9, 12))

bantenModel <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(bantenModel) <- columns

for (i in 1:length(ar)) {
  for (j in 1:length(ma)) {
    model1 <- model(data = banten, ar = c(ar[i][[1]]),
                    d = meanStationary(banten)$diffOrder, ma = c(ma[j][[1]]))
    pred <- predict(model1$model, n.ahead = 12)
    pred <- (pred$pred)^(4)-1
    acc <- accuracy(pred, test[,1])
    if (model1$ljung > 0.05) {
      bantenModel[nrow(bantenModel) + 1,] <- c(toString(model1$ar), toString(model1$ma),
                                               model1$ljung, model1$shapiroWilk, model1$rmse,
                                               model1$mape, round(acc[2],3), round(acc[5],3))
    }
  }
}

# ---------------------------------DIY Province---------------------------------
diy <- varStationary(data$DIY[1:180])$data
diy <- varStationary(diy[1:180])$data
stats::acf(diy) # for MA order
stats::pacf(diy) # for AR order
ar <- list(1, 2, 5, 9, c(1, 2), c(1, 5), c(1, 9), c(2, 5), c(2, 9), c(5, 9),
           c(1, 2, 5), c(1, 2, 9), c(2, 5, 9), c(1, 2, 5, 9))
ma <- list(1, 5, 6, 9, 12, c(1, 5), c(1, 6), c(1, 9), c(1, 12), c(5, 6), c(5, 9), c(5, 12),
           c(6, 9), c(6, 12), c(9, 12), c(1, 5, 6), c(1, 5, 9), c(1, 5, 12), c(1, 6, 9), c(1, 6, 12),
           c(1, 9, 12), c(5, 6, 9), c(5, 6, 12), c(5, 9, 12), c(6, 9, 12), c(1, 5, 6, 9),
           c(1, 5, 6, 12), c(1, 5, 9, 12), c(1, 6, 9, 12), c(5, 6, 9, 12), c(1, 5, 6, 9, 12))

diyModel <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(diyModel) <- columns

for (i in 1:length(ar)) {
  for (j in 1:length(ma)) {
    model1 <- model(data = diy, ar = c(ar[i][[1]]),
                    d = meanStationary(diy)$diffOrder, ma = c(ma[j][[1]]))
    pred <- predict(model1$model, n.ahead = 12)
    pred <- exp(pred$pred)-1
    acc <- accuracy(pred, test[,2])
    if (model1$ljung > 0.05 & model1$shapiroWilk > 0.05) {
      diyModel[nrow(diyModel) + 1,] <- c(toString(model1$ar), toString(model1$ma),
                                         model1$ljung, model1$shapiroWilk, model1$rmse,
                                         model1$mape, round(acc[2],3), round(acc[5],3))
    }
  }
}

# -----------------------------DKI Jakarta Province-----------------------------
dki <- varStationary(data$`DKI Jakarta`[1:180])$data
dki <- varStationary(dki[1:180])$data
stats::acf(dki) # for MA order
stats::pacf(dki) # for AR order
ar <- list(1, 5, 9, c(1, 5), c(1, 9), c(5, 9), c(1, 5, 9))
ma <- list(1, 5, 6, 9, 12, c(1, 5), c(1, 6), c(1, 9), c(1, 12), c(5, 6), c(5, 9), c(5, 12), c(6, 9),
           c(6, 12), c(9, 12), c(1, 5, 6), c(1, 5, 9), c(1, 5, 12), c(1, 6, 9), c(1, 6, 12), c(1, 9, 12),
           c(5, 6, 9), c(5, 6, 12), c(5, 9, 12), c(6, 9, 12), c(1, 5, 6, 9), c(1, 5, 6, 12), c(1, 5, 9, 12),
           c(1, 6, 9, 12), c(5, 6, 9, 12), c(1, 5, 6, 9, 12))

dkiModel <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(dkiModel) <- columns

for (i in 1:length(ar)) {
  for (j in 1:length(ma)) {
    model1 <- model(data = dki, ar = c(ar[i][[1]]),
                    d = meanStationary(dki)$diffOrder, ma = c(ma[j][[1]]))
    pred <- predict(model1$model, n.ahead = 12)
    pred <- -1+(pred$pred)^(-2)
    acc <- accuracy(pred, test[,3])
    if (model1$ljung > 0.05 & model1$shapiroWilk > 0.05) {
      dkiModel[nrow(dkiModel) + 1,] <- c(toString(model1$ar), toString(model1$ma),
                                         model1$ljung, model1$shapiroWilk, model1$rmse,
                                         model1$mape, round(acc[2],3), round(acc[5],3))
    }
  }
}

# ------------------------------West Java Province------------------------------
jabar <- varStationary(data$`Jawa Barat`[1:180])$data
jabar <- varStationary(jabar[1:180])$data
stats::acf(jabar) # for MA order
stats::pacf(jabar) # for AR order
ar <- list(1, 2, 5, 8, 9, c(1, 2), c(1, 5), c(1, 8), c(1, 9), c(2, 5), c(2, 8), c(2, 9), c(5, 8),
           c(5, 9), c(8, 9), c(1, 2, 5), c(1, 2, 8), c(1, 2, 9), c(1, 5, 8), c(1, 5, 9), c(1, 8, 9),
           c(2, 5, 8), c(2, 5, 9), c(2, 8, 9), c(5, 8, 9), c(1, 2, 5, 8), c(1, 2, 5, 9), c(1, 2, 8, 9),
           c(1, 5, 8, 9), c(2, 5, 8, 9), c(1, 2, 5, 8, 9))
ma <- list(1, 3, 5, 9, c(1, 3), c(1, 5), c(1, 9), c(3, 5), c(3, 9), c(5, 9),
           c(1, 3, 5), c(1, 3, 9), c(3, 5, 9), c(1, 3, 5, 9))

jabarModel <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(jabarModel) <- columns

for (i in 1:length(ar)) {
  for (j in 1:length(ma)) {
    model1 <- model(data = jabar, ar = c(ar[i][[1]]),
                    d = meanStationary(jabar)$diffOrder, ma = c(ma[j][[1]]))
    pred <- predict(model1$model, n.ahead = 12)
    pred <- (pred$pred)^2
    pred <- -1+(pred)^2
    acc <- accuracy(pred, test[,6])
    if (model1$ljung > 0.05 & model1$shapiroWilk > 0.05) {
      jabarModel[nrow(jabarModel) + 1,] <- c(toString(model1$ar), toString(model1$ma),
                                             model1$ljung, model1$shapiroWilk, model1$rmse,
                                             model1$mape, round(acc[2],3), round(acc[5],3))
    }
  }
}

# -----------------------------Central Java Province-----------------------------
jateng <- varStationary(data$`Jawa Tengah`[1:180])$data
jateng <- varStationary(jateng[1:180])$data
stats::acf(jateng) # for MA order
stats::pacf(jateng) # for AR order
ar <- list(1, 2, 9, c(1, 2), c(1, 9), c(2, 9))
ma <- list(1, 3, 9, 12, c(1, 3), c(1, 9), c(1, 12), c(3, 9), c(3, 12), c(9, 12),
           c(1, 3, 9), c(1, 3, 12), c(3, 9, 12), c(1, 3, 9, 12))

jatengModel <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(jatengModel) <- columns

for (i in 1:length(ar)) {
  for (j in 1:length(ma)) {
    model1 <- model(data = jateng, ar = c(ar[i][[1]]),
                    d = meanStationary(jateng)$diffOrder, ma = c(ma[j][[1]]))
    pred <- predict(model1$model, n.ahead = 12)
    pred <- (pred$pred)^2
    pred <- -1+(pred)^2
    acc <- accuracy(pred, test[,6])
    if (model1$ljung > 0.05 & model1$shapiroWilk > 0.05) {
      jatengModel[nrow(jatengModel) + 1,] <- c(toString(model1$ar), toString(model1$ma),
                                               model1$ljung, model1$shapiroWilk, model1$rmse,
                                               model1$mape, round(acc[2],3), round(acc[5],3))
    }
  }
}

# ------------------------------East Java Province------------------------------
jatim <- varStationary(data$`Jawa Timur`[1:180])$data
jatim <- varStationary(jatim[1:180])$data
stats::acf(jatim) # for MA order
stats::pacf(jatim) # for AR order
ar <- list(1, 2, 5, 9, c(1, 2), c(1, 5), c(1, 9), c(2, 5), c(2, 9), c(5, 9),
           c(1, 2, 5), c(1, 2, 9), c(2, 5, 9), c(1, 2, 5, 9))
ma <- list(1, 3, 5, 6, 9, 12, c(1, 3), c(1, 5), c(1, 6), c(1, 9), c(1, 12), c(3, 5), c(3, 6),
           c(3, 9), c(3, 12), c(5, 6), c(5, 9), c(5, 12), c(6, 9), c(6, 12), c(9, 12),
           c(1, 3, 5), c(1, 3, 6), c(1, 3, 9), c(1, 3, 12), c(1, 5, 6), c(1, 5, 9), c(1, 5, 12),
           c(1, 6, 9), c(1, 6, 12), c(1, 9, 12), c(3, 5, 6), c(3, 5, 9), c(3, 5, 12), c(3, 6, 9),
           c(3, 6, 12), c(3, 9, 12), c(5, 6, 9), c(5, 6, 12), c(5, 9, 12), c(6, 9, 12),
           c(1, 3, 5, 6), c(1, 3, 5, 9), c(1, 3, 5, 12), c(1, 3, 6, 9), c(1, 3, 6, 12), c(1, 3, 9, 12),
           c(1, 5, 6, 9), c(1, 5, 6, 12), c(1, 5, 9, 12), c(1, 6, 9, 12), c(3, 5, 6, 9), c(3, 5, 6, 12),
           c(3, 5, 9, 12), c(3, 6, 9, 12), c(5, 6, 9, 12), c(1, 3, 5, 6, 9), c(1, 3, 5, 6, 12),
           c(1, 3, 5, 9, 12), c(1, 5, 6, 9, 12), c(3, 5, 6, 9, 12), c(1, 3, 5, 6, 9, 12))

jatimModel <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(jatimModel) <- columns

for (i in 1:length(ar)) {
  for (j in 1:length(ma)) {
    model1 <- model(data = jatim, ar = c(ar[i][[1]]),
                    d = meanStationary(jatim)$diffOrder, ma = c(ma[j][[1]]))
    pred <- predict(model1$model, n.ahead = 12)
    pred <- exp(pred$pred)-1
    acc <- accuracy(pred, test[,6])
    if (model1$ljung > 0.05 & model1$shapiroWilk > 0.05) {
      jatimModel[nrow(jatimModel) + 1,] <- c(toString(model1$ar), toString(model1$ma),
                                             model1$ljung, model1$shapiroWilk, model1$rmse,
                                             model1$mape, round(acc[2],3), round(acc[5],3))
    }
  }
}