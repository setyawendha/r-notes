## what gets copied -------------

library(pryr)

x <- 1:5
y <- 1:5
c(address(x), address(y))

x <- 1:10
y <- x
c(address(x), address(y))


df <- mtcars
c(address(df), address(mtcars))

c(address(df$mpg), address(mtcars$mpg))


## procedural ----------

# program to select and summarize the numerical variables in m111survey

cols <- length(names(m111survey))
isNumerical <- logical(cols)

for ( i in seq_along(isNumerical) ) {
  isNumerical[i] <- if (is.numeric(m111survey[, i])) TRUE else FALSE
}

numsm111 <- m111survey[, isNumerical]
str(numsm111)

## lapply and friends  -----------

lapply2 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

lst <- lapply(1:10000, function(x) runif(100))
system.time(lapply2(lst, mean))
system.time(lapply(lst, mean))

probs <- runif(100)
generateBinoms <- function(prob, n = 1, size = 100) {
  results <- rbinom(n = n, size = size, prob = prob)
  list(prob = prob, results = results)
}
lst <- lapply(probs, generateBinoms, size = 200)

system.time({
probs <- c(0.95, 0.90, 0.90, 0.90, 0.80)
reps <- 10000
mat <- sapply(probs, rbinom, n = reps, size = 1, simplify = "matrix")
pattern <- matrix(c(2, 1,1,1,0), nrow = 5)
mean(mat %*% pattern >= 2.5)
})

# to vary some other parameters, use Map:
reps <- 1:5 * 100
probs <- c(0.95, 0.90, 0.90, 0.90, 0.80)
sizes <- 5:1 * 1000
sims <- Map(rbinom, n = reps, size = sizes, prob = probs)

isTriangle <- function(x, y, z) {
  (x + y > z) & (x +z > y) & (y + z > x)
}

makesTriangle <- function(x, y) {
  a <- pmin(x, y)
  b <- pmax(x, y)
  side1 <- a
  side2 <- b-a
  side3 <- 1 - b
  isTriangle(x = side1, y = side2, z = side3)
}

# replicate is not so fast
system.time(
mean(replicate(10000, makesTriangle(runif(1), runif(1)), simplify = "vector"))
)

# Map is a bit faster:
system.time(
mean(simplify2array(Map(makesTriangle, x = runif(10000), y = runif(10000))))
)

# of course the fastest is to vectorize:
system.time(
  mean(makesTriangle(x = runif(10000), y = runif(10000)))
)

## lapply to data frame columns ----------

library(tigerstats)
unlist(lapply(m111survey, class))

## reduce ----------

Reduce(sum, rep(1, 5), accumulate = T)

sampleReplace <- function(...) {
  sample(replace = TRUE, ...)
}

set.seed(2020)
numberList <- lapply(30:35, sample, x = letters, replace = T)
Reduce(intersect, x = numberList)

## tapply ---------

sexFastest <- with(m111survey, tapply(fastest, INDEX = sex, FUN = mean, na.rm = T))
sexFastest

sexSeatFastest <- with(m111survey, 
                       tapply(fastest, 
                              INDEX = list(sex, seat), 
                              FUN = mean, 
                              na.rm = T))
sexSeatFastest

with(m111survey, split(fastest, f = list(sex, seat)))


## Iterative function application:
Funcall <- function(f, ...) f(...)
## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1)
  function(x) Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
Iterate(function(x) 1 + 1 / x, 30)(1)

## filter ------
Filter(is.factor, m111survey)

## Iterative function application:
Funcall <- function(f, ...) f(...)
## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1)
  function(x) Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
Iterate(function(x) 1 + 1 / x, 30)(1)

## some recursion
fibonacci <- function(n) {
  if ( n %in% c(1,2) ) {
    return(1);
  }
  return(fibonacci(n - 1) + fibonacci(n -2))
}

system.time(fibonacci(40))

## wow:
# > system.time(fibonacci(20))
# user  system elapsed 
# 0.013   0.000   0.014 
# > system.time(fibonacci(30))
# user  system elapsed 
# 2.069   0.004   2.073 
# > system.time(fibonacci(40))
# user  system elapsed 
# 263.229   0.494 264.065 

## speed up with memoization
## (Patrick Burns, R Inferno, Chapter 6 Doing Global Assignments,
## http://www.burns-stat.com/pages/Tutor/R_inferno.pdf)

fibonacci2 <- local({
  memo <- c(1, 1, rep(NA, 100))
  f <- function(x) {
    if (x == 0) return(0)
    if (x < 0) return(NA)
    if (x > length(memo))
      stop("’x’ too big for implementation")
    if (!is.na(memo[x])) return(memo[x])
    ans <- f(x-2) + f(x-1)
    memo[x] <<- ans
    ans
  }
})

## big improvement
# > system.time(fibonacci2(20))
# user  system elapsed 
# 0.000   0.000   0.001 
# > system.time(fibonacci2(30))
# user  system elapsed 
# 0       0       0 
# > system.time(fibonacci2(40))
# user  system elapsed 
# 0       0       0 

## similar idea, via function factory:
fibber <- function(limit = 100) {
  memo <- c(1, 1, rep(NA, limit))
  f <- function(x) {
    if (x == 0) return(0)
    if (x < 0) return(NA)
    if (x > length(memo))
      stop("’x’ too big for implementation")
    if (!is.na(memo[x])) return(memo[x])
    ans <- f(x-2) + f(x-1)
    memo[x] <<- ans
    ans
  }
}

fibonacci3 <- fibber(limit = 500)

## same improvement
# > system.time(fibonacci3(20))
# user  system elapsed 
# 0       0       0 
# > system.time(fibonacci3(30))
# user  system elapsed 
# 0       0       0 
# > system.time(fibonacci3(40))
# user  system elapsed 
# 0       0       0 
