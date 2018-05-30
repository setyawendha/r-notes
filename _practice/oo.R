## Teach myself R6 -----
library(R6)

Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    age = NULL,
                    desire = NULL,
                    initialize = function(name = NA, age = NA, desire = NA) {
                      self$name <- name
                      self$age <- age
                      self$desire <- desire
                      self$greet()
                    },
                    set_age = function(val) {
                      self$age <- val
                    },
                    set_desire = function(val) {
                      self$desire <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)

dorothy <- Person$new("Dorothy", 12, "Kansas")
dorothy2 <- dorothy
dorothy2$age <- 14
dorothy$age


Numbers <- R6Class("Numbers",
                   public = list(
                     x = 100
                   ),
                   active = list(
                     x2 = function(value) {
                       if (missing(value)) return(self$x * 2)
                       else self$x <- value/2
                     },
                     rand = function() rnorm(1)
                   )
)

n <- Numbers$new()
n$x
n$x2
n$x2 <- 800
n$x

Lion <- R6Class("Lion",
                inherit = Person,
                public = list(
                  weight = NULL,
                  set_weight = function(val) {
                    self$weight <- val
                    cat(self$name, " has weight:  ", val, ".\n", sep = "")
                  },
                  greet = function() {
                    cat("Grr!  My name is ", self$name, "!", sep = "")
                    }
                  )
                )

cowardlyLion <- Lion$new(name = "Cowardly Lion", age = 18, desire = "courage")
cowardlyLion$set_weight(350)
cowardlyLion$desire
cowardlyLion$weight
cowardlyLion$greet()


# when adding new member, don't initialize to NULL, use NA instead.
# overwrite it so I can re-run code without removing the class
Person$set("public", "color", NA, overwrite = T)
Person$set("public", "set_color", function(val) self$color <- val, overwrite = T)
Person
# new class member present only in instances created after set is called.
# throws error:
#dorothy$set_color("ruby")

glinda <- Person$new("Glinda", "500", "the good of all")
glinda$set_color("blue")
glinda$color


## illustrate method chaining -----

animal <-  c("zebra", "giraffe", "pig", "cape buffalo", "antelope", "wildebeast")
mass <- c(50, 100, 25, 60, 45, 55)
prey <- data.frame(animal, mass, stringsAsFactors = F)

Lion$set("public", "eaten", character(), overwrite = T)
Lion$set("public", "prey", prey, overwrite = T)
Lion$set("public", "eat", function() {
  n <- nrow(self$prey)
  item <- self$prey[sample(1:n, size = 1), ]
  initLetter <- substr(item$animal, 1, 1)
  article <- ifelse(initLetter %in% c("a", "e", "i", "o", "u"), "An ", "A ")
  cat(article, item$animal, " was eaten just now ...\n\n", sep = "")
  self$eaten <- c(self$eaten, item$animal)
  self$weight <- self$weight + item$mass
  return(invisible(self))
}, overwrite = T)

Lion$set("public", "report", function() {
  n <- length(self$eaten)
  if ( n >= 1 ) {
    cat("My name is ", self$name, ".\n", sep = "")
    cat("My most recent meal consisted of: ", self$eaten[n], ".\n", sep = "")
  }
  cat("I now weigh ", self$weight, " pounds.\n", sep = "")
  return(invisible(self))
}, overwrite = T)

simba <- Lion$new(name = "Simba", age = 10, desire = "Hakuna Matata")
simba$set_weight(300)

simba$eat()$report()

simba$eat()$eat()$report()

myFun <- function(lion) {
  lion$age <- 25
}

myFun(simba)
simba$age

## can we add a field to an object?  ----

simba$height <- 52

simba$unlock()

## play with S3 classes -----

df <- bcscr::m111survey
class(df) <- c("customDataFrame", "data.frame")


print.customDataFrame <- function(x, ...) {
  cat("Here is the data frame, as requested.\n")
  cat("But you get 10 rows at most!\n\n")
  x <- data.frame(x)
  n <- min(10, nrow(x))
  x <- x[1:n, ]
  print.data.frame(x, ...)
}

print(df, row.names = F)

a <- 4
f <- function(x) {
  a +7
  x + 4
}


## simulation method functions -----

library(ggplot2)
fm <- subset(bcscr::m111survey, seat != "3_back")
ggplot(fm, aes(x = seat)) + geom_bar() + scale_x_discrete(drop = F)

numberNeededSim <- function(target = 1, reps = 1000, 
                            seed = NULL) {
  
  #set the seed if none is provided
  if ( is.null(seed) ) {
    seed <- as.numeric(Sys.time())
  }
  set.seed(seed)
  
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
  
  needed <- numeric(reps)
  for (i in 1:reps ) {
    needed[i] <- numberNeeded(target)
  }
  results <- list(target = target, sims = needed)
  class(results) <- "numNeededSims"
  results
}

print.numNeededSims <- function(x) {
  cat("The target was ", x$target, ".\n", sep = "")
  sims <- x$sims
  reps <- length(sims)
  cat("Here is a table of the results, based on ", reps,
      " simulations.\n\n", sep = "")
  tab <- prop.table(table(sims))
  
  # for sake of pretty output,
  # remove "sims" variable name from top of table printout
  colNames <- dimnames(tab)
  names(colNames) <- NULL
  dimnames(tab) <- colNames
  
  print(tab)
  cat("\n")
  cat("The expected number needed is about ", 
      mean(sims), ".\n", sep = "")
}

plot.numNeededSims <- function(x) {
  if ( !"package:ggplot2" %in% search() ) {
    cat("Need to load package ggplot2 in order to plot.")
  }
  
  sims <- x$sims
  levels <- min(sims):max(sims)
  sims <- factor(sims, levels = levels)
  
  df <- data.frame(sims)
  plotTitle <- paste0("Results of ", length(sims), " Simulations")
  ggplot(df, aes(x = sims)) + geom_bar() + scale_x_discrete(drop = F) +
    labs(x = "Number Needed", title = plotTitle)
}

res <- numberNeededSim(target = 1, reps = 10000, seed = 2020)
res
plot(res)

numberNeededSim(target = 1, reps = 10000, seed = 2020)

meetupSim <- function(reps = 10000, seed = NULL) {
  if ( is.null(seed) ) {
    seed <- as.numeric(Sys.time())
  }
  set.seed(seed)
  anna <- runif(reps, 0, 60)
  raj <- runif(reps, 0, 60)
  connect <- (abs(anna - raj) < 10)
  
  diffs <- abs(anna - raj)
  noMeet <- diffs[diffs > 10] - 10
  
  results <- list(anna = anna, raj = raj, 
                  connect = connect, noMeet = noMeet)
  class(results) <- "meetupSims"
  results
}


print.meetupSims <- function(x) {
  reps <- length(x$connect)
  cat("Here is a table of the results, based on ", reps,
      " simulations.\n\n", sep = "")
  tab <- prop.table(table(x$connect))
  
  # remove "connect" variable name from table printout
  colNames <- list(c("Did not connect", "Connected"))
  dimnames(tab) <- colNames
  
  print(tab)
  cat("\n")
  cat("Summary of how many minutes they missed by:\n\n")
  print(summary(x$noMeet))
}

plot.meetupSims <- function(x) {
  if ( !"package:ggplot2" %in% search() ) {
    cat("Need to load package ggplot2 in order to plot.")
  }
  
  sims <- x$noMeet
  
  df <- data.frame(sims)
  plotTitle <- paste0("Results of ", length(sims), " Simulations")
  ggplot(df, aes(x = sims)) + geom_density() +
    labs(x = "Minutes Missed By", title = plotTitle)
}

res <- meetupSim(reps = 100000, seed = 3030)
res
plot(res)

## generics ----

df <- data.frame(name = c("Dorothy", "Cowardly Lion"), age = c(12, 18))
class(df)
lst <- as.list(df)
class(lst)
methods(print)
methods(class = "factor")
