library(ggplot2)

f <- function(k) {
  ifelse(k == 0, 1/2, 1/(abs(k) * (abs(k) + 1) * (abs(k) + 2)))
}

X <- -10:10
Y <- sapply(X, f)
ggplot() + geom_point(aes(x = X, y = Y)) + geom_line(aes(x = X, y = Y), colour = "grey")

sample_from_Y <- function(unused) {
  u <- runif(1, 0, 1)
  sign <- 1
  if (u < 1/4) {
    sign <- sign * (-1)
  }
  else if (u > 1/2) {
    return (0)
  }

  v <- runif(1, 0, 1/4)
  temp <- 0
  k <- 1
  while (TRUE) {
    temp <- temp + f(k)
    if (temp >= v) {
      return (sign * k)
    }
    k <- k + 1
  }
}

N <- 10000
values <- sapply(1:N, sample_from_Y)
Y_sampled <- sapply(-10:10, function (i) length(values[values == i])) / N
ggplot() + geom_point(aes(x = X, y = Y)) + geom_line(aes(x = X, y = Y), colour = "grey") + geom_point(aes(x = X, y = Y_sampled), colour = "red")

partial_means <- cumsum(values) / (1:N)
ggplot() + geom_point(aes(x = 1:N, y = partial_means))

partial_medians <- sapply(1:N, function(i) {median(values[1:i])})
ggplot() + geom_point(aes(x = 1:N, y = partial_medians))
