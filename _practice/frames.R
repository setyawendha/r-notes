## matrix version of appeals court simulation ----

courtSimMat <- function(reps = 10000,
                     seed = NULL,
                     table = FALSE,
                     probs = c(0.95, 0.94, 0.90, 0.90, 0.80),
                     weights = rep(1, length(probs))) {
  
  # validate input
  numberVoters <- length(probs)
  if ( numberVoters %% 2 != 1 ) {
    stop("Need an odd number of voters.")
  }
  if ( length(weights) != numberVoters ) {
    stop("weights must have the same length as the number of voters.")
  }
  if ( sum(pattern) != numberVoters ) {
    stop("weights should sum to the number of voters.")
  }
  
  # set the seed if none is provided
  if ( is.null(seed) ) {
    seed <- as.numeric(Sys.time())
  }
  
  # compute the vector of probabilities for rbinom:
  prob <- rep(probs, times = reps)
  # compute the number of rbinom samples required:
  n <- length(prob)
  # get decisions, arrange them in matrix:
  decisions <- matrix(rbinom(n, size = 1, prob), nrow = reps, byrow = T)
  
  # count the number of correct votes, according to pattern:
  correctVotes <- decisions %*% matrix(weights, ncol = 1)
  
  # determine whether court decided correctly, in each case:
  courtCorrect <- (correctVotes > numberVoters/2)
  
  # record results
  if ( table ) {
    cat("Here is a table of the results:\n\n")
    print(table(courtCorrect))
    cat("\n")
  }
  cat("The proportion of times the court was correct was ", 
      mean(courtCorrect), ".\n", sep = "")
}

system.time({
  courtSimMat(reps = 10000, seed = 2020, probs = c(0.95, 0.94, 0.90, 0.90, 0.80),
              weights = c(2, 1, 1, 1, 0))
})
courtSimMat(reps = 10000, seed = 2020, probs = c(0.95, 0.94, 0.90, 0.90, 0.80),
            weights = c(2, 1, 1, 1, 0))


cps <- mosaicData::CPS85
cpsSmall <- cps[cps$wage < 40, ]
library(ggplot2)
p <- ggplot(cpsSmall, aes(x = sector, y = wage)) + geom_violin() + geom_jitter()
print(p)

ggplot(cpsSmall, aes(x = sex, y = wage)) + geom_violin() + geom_jitter() + facet_wrap(~sector, nrow = 2)
with(cpsSmall, tapply(wage, list(sector, sex), mean))
