varStationary <- function(timeseries) {
  listLambda <- list()
  sum <- 0
  if (min(timeseries) < 0) {
    sum <- round(floor(min(timeseries)), 0)
    timeseries <- timeseries - sum
  }
  lambda <- forecast::BoxCox.lambda(timeseries, lower = -1, upper = 1)
  listLambda <- list(round(lambda, 2))
  while (round(lambda, 2) != 1) {
    if (round(lambda, 2) < -0.75) {
      timeseries <- 1/timeseries
      listLambda[length(listLambda)] = round(-1, 2)
    } else if (round(lambda, 2) < -0.25) {
      timeseries <- 1/sqrt(timeseries)
      listLambda[length(listLambda)] = round(-0.5, 2)
    } else if (round(lambda, 2) < 0.25) {
      timeseries <- log(timeseries)
      listLambda[length(listLambda)] = round(0, 2)
    } else if (round(lambda, 2) < 0.75) {
      timeseries <- sqrt(timeseries)
      listLambda[length(listLambda)] = round(0.5, 2)
    } else {
      listLambda[length(listLambda)] = round(1, 2)
      break
    }
    lambda <- forecast::BoxCox.lambda(timeseries, lower = -1, upper = 1)
    listLambda[length(listLambda)+1] = round(lambda, 2)
  }
  return(list(data = timeseries, lambda = listLambda, summation = -sum))
}

meanStationary <- function(timeseries) {
  adf <- tseries::adf.test(timeseries)
  d = 0
  while (adf$p.value > 0.05) {
    d = d + 1
    timeseries <- diff(timeseries, lag = 1)
    adf <- tseries::adf.test(timeseries)
  }
  return(list(data = timeseries, diffOrder = d))
}
