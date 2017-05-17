asterisks <- function(starCounts) {
  for ( count in starCounts ) {
    line <- paste(rep("*", count), collapse = '')
    cat(line, "\n")
  }
}

pattern <- c(1:5, 6, 5:1)

asterisks(pattern)

numberNeeded <- function(target) {
  count <- 0
  sum <- 0
  while (sum < target) {
    count <- count + 1
    sum <- sum + runif(target)
  }
  count
}

target <- 1
reps <- 100000
counts <- numeric(reps)
for (i in 1:reps) {
  counts[i] <- numberNeeded(target)
}

sum(counts)/reps
