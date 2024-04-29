varStationary <- function(timeseries) {
  lambda <- forecast::BoxCox.lambda(timeseries, lower = -1, upper = 1)
  if (min(timeseries) < 0) {
    timeseries <- timeseries - floor(min(timeseries))
  }
  listLambda <- list(lambda)
  while (round(lambda, 1) != 1) {
    if (round(lambda, 2) < -0.75) {
      timeseries <- 1/timeseries
    } else if (round(lambda, 2) < -0.25) {
      timeseries <- 1/sqrt(timeseries)
    } else if (round(lambda, 2) < 0.25) {
      timeseries <- log(timeseries)
    } else if (round(lambda, 2) < 0.75) {
      timeseries <- sqrt(timeseries)
    } else {
      break
    }
    lambda <- forecast::BoxCox.lambda(timeseries, lower = -1, upper = 1)
    listLambda[[length(listLambda)+1]] = lambda
  }
  return(list(data = timeseries, lambda = listLambda))
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
