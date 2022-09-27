
check <- function(data) {
  dims <- dim(data)
  r_dim <- dims[1]
  c_dim <- dims[2]

  r <- sapply(1:r_dim, function(i) sum(data[i,]))
  c <- sapply(1:c_dim, function(j) sum(data[,j]))

  N <- sum(data)

  f <- sapply(1:c_dim, function(j) {sapply(1:r_dim, function(i) {r[i] * c[j] / N})})

  S <- sum(sapply(1:c_dim, function(j) {sapply(1:r_dim, function(i) {(f[i, j] - data[i, j])^2 / f[i, j]})}))

  degrees_of_freedom <- (r_dim - 1) * (c_dim - 1)

  result <- pchisq(S, degrees_of_freedom, lower.tail = FALSE)

  print(result)
  print(ifelse(result >= 0.05, "kobiety nie mają znaczenia", "kobiety mają znaczenie"))
}

data <- matrix(
  c(
    c(17508, 11642,  3308,  3131,  2911,  2205,  1852, 1235),
    c(17672,  9318,  4865,  3259,  3029,  2479,  1606, 3259)
  ), nrow = 2, byrow = TRUE
)

print("z Koalicją Odnowy Rzeczypospolitej Wolność i Nadzieja")
check(data)


data <- matrix(
  c(
    c(17508, 11642,  3308,  3131,  2911,  2205,  1852),
    c(17672,  9318,  4865,  3259,  3029,  2479,  1606)
  ), nrow = 2, byrow = TRUE
)

print("bez Koalicji Odnowy Rzeczypospolitej Wolność i Nadzieja")
check(data)