## drunken man sim --------

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



## invisible -------

verboseSearch <- function(elem, vec) {
  
  # The following logical keeps track of whether
  # we have found the element.
  # We have not yet begun the search so start it
  # at FALSE.
  found <- FALSE
  
  # validate input:
  if ( length(vec) == 0 ) {
    cat("The vector empty.  No way does it contain ", 
        elem, ".", sep = "")
    return(-1)
  }
  
  # check the elements of vector:
  for ( i in 1: length(vec) ) {
    if ( vec[i] == elem ) {
      # record that we found the element:
      found <- TRUE
      break
    } else {
      # report no match at this index:
      cat("Checked index ", i, 
          " in the vector.  No match there ...\n", sep = "")
    }
  }
  
  if ( found ) {
    # report success:
    cat("Found ", elem, " at index ", i, ".", sep = "")
    return(i)
  } else {
    # report failure:
    cat(elem, " is not in the vector.", sep = "")
    return(-1)
  }
}


y <- verboseSearch(3, 1:5)
y


getInput <- function() {
  valid <- FALSE
  while ( !valid ) {
    input <- as.numeric(readline(
      prompt = "How many will you take (1 or 2)?  "))
    if ( input %in% c(1,2) ) {
      valid <- TRUE
    }
  }
  input
}

subtraction <- function(n = 12, userFirst = TRUE) {
  if ( userFirst ) {
    n <- n - getInput()
  }
  while ( n > 0 ) {
    remainder <- n %% 3
    takeAmount <- ifelse(remainder == 0,
                         sample(c(1,2), size = 1),
                         remainder)
    n <- n - takeAmount
    cat("I will take ", takeAmount, ".  There are ", n, " left.\n", sep = "")
    if ( n == 0 ) {
      return(cat("I win!"))
    }
    n <- n - getInput()
    if ( n == 0 ) {
      return(cat("You win!"))
    }
  }
}

## bball game? ----

library(jpeg)
library(png)

plot(0,0, axes = F, xlim = c(0,100), ylim = c(0,100), col = "transparent")
net <- readPNG(source = "/Users/homer/net.png")
playerImage <- readJPEG(source = "/Users/homer/player.jpg")
rasterImage(player, 0,0,20,30)

freeThrow <- function(player = c(10,27), target = c(90, 95),
                     tolerance = 4) {
  plot(0,0, axes = F, xlim = c(0,100), ylim = c(0,130), col = "transparent",
       xlab = "", ylab = "")
  rasterImage(image = playerImage, 0, 0, 12, 30)
  rasterImage(image = net, 85,85,95,95)
  box(col = "red")
  
  angle <- as.numeric(readline("Shot angle (between 0 and 90 degrees):  "))
  theta <- pi/180*angle
  v0 <- as.numeric(readline("release speed (feet/sec):  "))
  g <- 32.174
  a <- g/(2*v0^2*cos(theta)^2)
  b <- -tan(theta)
  c <- target[2] - player[2]
  disc <- b^2-4*a*c
  if ( disc < 0 ) {
    hit <- FALSE
    x <- target[1]
    xs <- seq(player[1], target[1], by = 0.1)
    ys <- player[2] - b*(xs-player[1]) - a*(xs-player[1])^2
  } else {
    x <- (-b + sqrt(disc))/(2*a) + player[1]
    hit <- (target[1] - tolerance <= x) && (x <= target[1] + tolerance)
    xs <- seq(player[1], x, by = 0.1)
    ys <- player[2] - b*(xs-player[1]) - a*(xs - player[1])^2
  }
  print(points(xs, ys, type = "l"))
  results <- list(x = x, hit = hit)
  results
}

freeThrow()
