## drunken turtle sim --------

reps <- 10000
gotBack <- numeric(reps)
n <- 1000
vals <- c(-2, -1, 1, 2)

for (i in 1:reps) {
  steps <- sample(vals, size = n, replace = T)
  xSteps <- ifelse(steps > 0, 0, 2 * (steps + 1.5))
  ySteps <- ifelse(steps > 0, 2 * (steps - 1.5), 0)
  
  x <- cumsum(xSteps)
  y <- cumsum(ySteps)
  
  dist <- sqrt(x ^ 2 + y ^ 2)
  gotBack[i] <- length(dist[dist < 0.5])
}

prop.table(table(gotBack))

## drunken turtle sim 2 ------------

reps <- 10000
gotBack <- numeric(reps)
n <- 1000

for (i in 1:reps) {
  angle <- runif(n, 0 , 2*pi)
  xSteps <- cos(angle)
  ySteps <- sin(angle)
  
  x <- cumsum(xSteps)
  y <- cumsum(ySteps)
  
  dist <- sqrt(x ^ 2 + y ^ 2)
  closeReturn <- (dist < 0.5)
  returns[i] <- sum(closeReturn)
}

prop.table(table(returns))

## umbrella -------

umbrellaSim <- function(x, y, p, reps) {
  trips <- numeric(reps)
  atHome <- function(count) count %% 2 == 0
  for ( i in 1:reps ) {
    umbHome <- x
    umbOffice <- y
    count <- 0
    dry <- TRUE
    while( dry ) {
      rain <- rbinom(1, 1, p)
      if ( !rain ) {
        count <- count + 1
        next
      }
      if ( atHome(count) & (umbHome > 0) ) {
        count <- count + 1
        umbHome <- umbHome - 1
        umbOffice <- umbOffice + 1
      } else if ( !atHome(count) & (umbOffice > 0) ) {
        count <- count + 1
        umbHome <- umbHome + 1
        umbOffice <- umbOffice - 1
      } else dry <- FALSE
    }
    trips[i] <- count
  }
  # cat("The expected number of dry trips is:  ", 
  #     mean(trips), ".\n", sep = "")
  mean(trips)
}

prob <- seq(0.05, 0.95, by = 0.05)
exp <- numeric(length(prob))
for ( i in 1:length(prob) ) {
  exp[i] <- umbrellaSim(2, 2, prob[i], 10000)
}
data.frame(prob, exp)

## appeals court paradox ----

reps <- 1000000
a <- rbinom(reps, 1, 0.95)
b <- rbinom(reps, 1, 0.95)
c <- rbinom(reps, 1, 0.90)
d <- rbinom(reps, 1, 0.90)
e <- rbinom(reps, 1, 0.80)
correct1 <- (a + b + c + d + e >= 3)
mean(correct1)
correct2 <- (2*a + b + c + d >= 3)
mean(correct2)


reps <- 100000
a <- rbinom(reps, 1, 0.65)
b <- rbinom(reps, 1, 0.60)
c <- rbinom(reps, 1, 0.60)
d <- rbinom(reps, 1, 0.60)
e <- rbinom(reps, 1, 0.55)
correct1 <- (a + b + c + d + e >= 3)
mean(correct1)
correct2 <- (2*a + b + c + d >= 3)
mean(correct2)

absolute <- function(x) {
  ifelse(x >= 0, x, -x)
}
absolute(c(-3, 3, -5.7))

## meetup -------

# both willing to wait ten minutes
reps <- 10000
anna <- runif(reps, 0, 60)
raj <- runif(reps, 0, 60)
met <- (abs(anna - raj) < 10)
mean(met)

system.time({
  reps <- 100000
  anna <- runif(reps, 0, 60)
  raj <- runif(reps, 0, 60)
  met <- (abs(anna - raj) < 10)
  mean(met)
})

# slow way:
system.time({
  reps <- 100000
  met <- numeric(reps)
  for ( i in 1:reps ) {
    anna <- runif(1, 0, 60)
    raj <- runif(1, 0, 60)
    met[i] <- (abs(anna - raj) < 10)
  }
  mean(met)
})

# raj waits 10, anna 5
reps <- 10000
anna <- runif(reps, 0, 60)
raj <- runif(reps, 0, 60)
annaFirst <- (anna - raj < 0)
met <- ifelse(annaFirst, raj - anna < 5, anna - raj < 10 )
mean(met)

## dental floss ----
library(ggplot2)
# do this without matrix, for sake of students
flossSim <- function(len = 150, min = 1, 
                     max = 2, reps = 10000, seed = NULL) {
  if ( is.null(seed) ) {
    seed <- as.numeric(Sys.time())
  }
  set.seed(seed)
  whichOne <- function(x) {
    if (x < 0.5 ) {
      return("a")
    } else return("b")
  }
  leftover <- numeric(reps)
  for (i in 1:reps ) {
    a <- b <- len
    bothOK <- TRUE
    while ( bothOK ) {
      # determine which box is picked
      # < 0.5 is a; >= 0.5 is b;
      boxPicked <- whichOne(runif(1))
      
      # attempt to use floss from a
      if ( boxPicked == "a" ) {
        if ( a < 1 ) {
          leftover[i] <- b
          bothOK <- FALSE
        } else {
          useAmount <- min(runif(1, min, max), a)
          a <- a - useAmount
          if (abs(a) < 10^(-4)) {
            leftover[i] <- b
            bothOK <- FALSE
          }
        }
      }
      
      # use flosss from b  
      if ( boxPicked == "b" ) {
        if ( b < 1 ) {
          leftover[i] <- a
          bothOK <- FALSE
        } else {
          useAmount <- min(runif(1, min, max), b)
          b <- b - useAmount
          if (abs(b) < 10^(-4)) {
            leftover[i] <- a
            bothOK <- FALSE
          }
        }
      }
        
    }  # end while
  }    # end for loop
  
  # report results:
  plotTitle <- paste0("Dental-Floss Simulation with ", reps, " Repetitions")
  p <- ggplot(mapping = aes(leftover)) + geom_density() +
    labs(x = "Amount left in spare (ft)",
         title = plotTitle)
  if ( reps <= 100 ) {
    p <- p + geom_rug()
  }
  print(p)
  cat("The average amount left in the spare is:  ", 
      mean(leftover), ".", sep = "")
}      # end flossSim

## other targets besides 1 ----

numberNeededSim <- function(target = 1, reps = 1000) {
  
  # define helper function
  numberNeeded <- function(target) {
    mySum <- 0
    count <- 0
    while( mySum < target ) {
      number <- runif(1)
      mySum <- mySum + number
      count <- count + 1
    }
    count
  }
  
  #perform simulation
  needed <- numeric(reps)
  for (i in 1:reps ) {
    needed[i] <- numberNeeded(target = target)
  }
  mean(needed)
}

target <- seq(0.1, 1, by = 0.1 )
exp <- numeric(length(target))
for ( i in seq_along(target)) {
  exp[i] <- numberNeededSim(reps = 10000, target = target[i])
}

exponential <- exp(target)
data.frame(target, exp, exponential)

