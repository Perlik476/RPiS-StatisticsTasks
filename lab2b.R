#Mateusz Perlik

library(ggplot2)

data <- read.csv("us_births_69_88.csv", header = TRUE)

number_of_days <- 372 #XD
max_number_of_births <- max(data$birth)
initial_vector_length <- 5000000
N <- 100000

vector_u <- sample(0 : max_number_of_births, initial_vector_length, replace = TRUE)
vector_k <- sample(1 : number_of_days, initial_vector_length, replace = TRUE)
combined_vector <- cbind(vector_u, vector_k)
combined_vector_good <- combined_vector[combined_vector[, 1] <= data$births[combined_vector[, 2]], ]


index <- 1
until_repeat <- function(unused) {
  times_repeated <- rep(0, number_of_days)
  return_value <- 1

  while (TRUE) {
    j <- combined_vector_good[index, 2]
    times_repeated[j] <- times_repeated[j] + 1

    if (times_repeated[j] == 2) {
      return (return_value)
    }

    index <<- index + 1
    return_value <- return_value + 1
  }
}


result <- sapply(rep(0, N), until_repeat)

ggplot() + geom_histogram(aes(x = result), binwidth = 1)