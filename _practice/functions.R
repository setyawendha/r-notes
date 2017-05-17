# Scoping Issues --------

rm(list = ls())
ls()

a <- 10
c  <- 30
myFun <- function(x) {
  b <- 15
  a <- 20
  envNames <- ls()
  for ( name in envNames ) {
    message <- paste(name, ":", get(name), "\n")
    cat(message)
  }
  cat(c)
}

myFun(10)
print(b)

rm(list = ls())
a <- 10
b <- 2
c <- 50

myFun <- function(x) {
  a*x+b
}

codetools::findGlobals(myFun)

rm(list = ls())
a <- 10
b <- 2
c <- 50
e <- 2

myFun <- function(d) {
  e <- 5
  f <- function(x) {
    e*x+d
  }
  return(f)
}

codetools::findGlobals(myFun)

g <- myFun(5)
codetools::findGlobals(g)
g(1)

# Matching Arguments -------

myFun <- function(apples = 3, color = "red", collar = "green") {
  cat(apples,color, collar)
}

myFun(3, coll = "blue", c = "purple")

myFun2 <- function(x = c("bool", "boor", "box", "cat", "canary", "dog")) {
  match.arg(x)
}

myFun2()

# Home-made SD with na.rm parameter ----

mySD <- function(x, na.rm = F) {
  if (na.rm == T && any(is.na(x))) {
    x <- x[!is.na(x)]
  }
  sqrt(sum((x - mean(x))^2))
}

a <- c(1:5, NA)
mySD(a, na.rm = T)

# Ellipses (maybe use in chapter on lists?) ----

HelloWorld <- function(...) {
  arguments <- list(...)
  print(arguments)
  paste(...)
}

HelloWorld(a = "Hello", b = "World", c = "!")

# pi ------
# ramanujan for 1/pi (use Downey's version in flow control chapter,
# this version in functions)

piRec <- function(n) {
  k <- 0:(n-1)
  2*sqrt(2)/9801*sum(factorial(4*k)*(1103 + 2690*k)/(factorial(k)^4*396^(4*k)))
}

1/piRec(1)
(piRec(2) - 1/pi)/(1/pi)

cat(sprintf("%20.15f%20.15f", piRec(1), 1/pi))

piApprox <- function(n) {
  k <- 1:n
  4*sum((-1)^(k-1)/(2*k-1))
}
system.time(piApprox(10000000))

piApproxSlow <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + (-1)^(i+1)/(2*i-1)
  }
  4*total
}
system.time(piApproxSlow(10000000))

# for pi^2/6
myEuler <- function(n) {
  k <- 1:n
  sum(1/k^2)
}
myEuler(100)
pi^2/6

# good star-printer:  not really possible yet
patCat <- function(char = "*", n) {
  stars <- rep(1:n, times = 1:n)
  stars
}
patCat(n = 5)

# play with random ------
propUnder <- function(seed = 2020, target = 0.5, n) {
  set.seed(seed)
  sum(runif(n) > target)/n
}
propGreater(0.7, 100000)

# do twice -------

doTwice <- function(f, value) {
  f(value)
  f(value)
}

spitBack <- function(stuff) {
  cat("Here you go: ", stuff, "\n")
}

doTwice(spitBack, "yow!")

catTwice <- function(value) {
  cat(value, "\n")
  cat(value, "\n")
}

doTwice(catTwice, "Yow!")

doFour <- function(f, value) {
  doTwice(f, value)
  doTwice(f, value)
}
doFour(catTwice, "Ow!")

# return values -----

a <- cat("hello")
a

a <- (b <- 5)
a
assign("a", b <- 5)

myFun <- function(x) {
  x^2
  b <- 5
}

a <- myFun(2)
a

a <- print(2+2)
a

# if-then with booleans
f <- function(vec, n) {
  a <- 5
  b <- 10
  print(search())
  length(vec) >= n || return(vec)
  return(vec[1:n])
}

f(letters[1:5], 10)
rep("h", times = turnips)

f()

d <- 1
f <- function(b) {
  a <- 10
  c <- 1000
  return(function(x) a*x + b + d)
}

g <- f(5)
g(4)

assign("a", 100, pos = environment(g), envir = environment(g))
g(4)
ls(environment(g))
d <- 2
g(4)
