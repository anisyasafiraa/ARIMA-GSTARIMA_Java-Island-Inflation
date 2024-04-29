library(stats)
library(forecast)
library(lmtest)
library(tseries)

model <- function(data, ar = 0, d = 0, ma = 0) {
  rep <- rep(0, ar[length(ar)] + ma[length(ma)])
  fixed <- replace(rep, c(ar, ma+max(ar)), NA)
  model <- Arima(data, order = c(max(ar), d, max(ma)),
                 include.mean = FALSE, fixed = fixed, method = 'CSS')
  res <- resid(model)
  res <- na.omit(res)
  acc <- forecast::accuracy(model)
  whiteNoise <- Box.test(res, type = "Ljung-Box")
  normSW <- shapiro.test(res)
  return(list(ar = ar, ma = ma, coef = coeftest(model), model = model,
              rmse = round(acc[2], 3), mape = round(acc[5], 3),
              ljung = round(whiteNoise$p.value, 3),
              shapiroWilk = round(normSW$p.value, 3)))
}