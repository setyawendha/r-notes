#library(dplyr)
upperBounds <- c(5, 7, 8, 2, 9)
upperBounds %>%
  map(function(x) {
    runif(5, max = x)
  })

upperBounds %>% map(~runif(n = 5, max = .))
lowerBounds <- upperBounds - 3
n <- 1:5
pmap(list(lowerBounds, upperBounds), runif, n = 5)
pmap(list(n, lowerBounds, upperBounds), runif)
pmap(list(n, lowerBounds), runif, max = 10)

upperBounds <- c(5, 7, 8, 2, 9)