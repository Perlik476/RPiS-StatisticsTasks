
library(ggplot2)


estimate_dnf <- function(N) {
  success <- 0
  for (i in 1:N) {
    x <- sample(c(-1, 1), M, replace = TRUE)
    for (clause in phi) {
      mul <- x[abs(clause)] * clause
      if (sum(mul <= 0) == 0) {
        success <- success + 1
        break
      }
    }
  }
  return (2^M * success / N)
}

estimate_clause <- function(N, i) {
  success <- 0
  clause_i <- phi[[i]]
  if (i == 1) {
    return (2^(M - length(clause_i)))
  }
  psi <- phi[1 : (i - 1)]
  for (j in 1 : N) {
    # print(success)
    x <- sample(c(-1, 1), M, replace = TRUE)
    for (k in clause_i) {
      x[abs(k)] <- k / abs(k)
    }
    for (clause in psi) {
      mul <- x[abs(clause)] * clause
      if (sum(mul <= 0) == 0) {
        success <- success - 1
        break
      }
    }
    success <- success + 1
  }
  return (2^(M - length(clause_i)) * success / N)
}

estimate_dnf2 <- function(N) {
  sum <- 0
  len <- length(phi)
  for (i in 1 : len) {
    sum <- sum + estimate_clause(N, i)
    print(sum)
  }
  return (sum)
}

M <- 4
phi <- list()
phi[[1]] <- c(1, -3, 4)
phi[[2]] <- c(2)
N <- 1000
REP <- 100


result1 <- sapply(1:REP, function (i) estimate_dnf(N))
result2 <- sapply(1:REP, function (i) estimate_dnf2(N))

ggplot() + geom_histogram(aes(x = result1), fill = "red", alpha = 0.8) + geom_histogram(aes(x = result2), fill = "blue", alpha = 0.8)