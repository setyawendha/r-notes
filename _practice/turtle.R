# Mostly taken from the TurtleGraphics Vignette.
# See:  https://CRAN.R-project.org/package=TurtleGraphics

# Attach the package -------

library(TurtleGraphics)

# Initial Example ---------

# Starting Off ------------
turtle_init()

# Turtle Position and Angle ---------
turtle_getpos()
turtle_getangle()

# First Movements -------------
turtle_forward(dist = 30)
turtle_backward(dist = 10)

# Add a triange ----------
turtle_right(90)
turtle_forward(10)
turtle_left(angle = 135)
turtle_forward(14.14)
turtle_left(angle = 90)
turtle_forward(14.14)
turtle_left(angle = 135)
turtle_forward(10)

# Up and Down ---------
turtle_up()
turtle_right(angle = 90)
turtle_forward(dist = 10)
turtle_right(angle = 90)
turtle_forward(dist = 17)
turtle_down()
turtle_left(angle = 180)
turtle_forward(dist = 34)

# Changing Color ---------------
turtle_col(col = "green")

# Hide and Show ------------
turtle_hide()
turtle_left(angle = 150)
turtle_forward(dist = 20)
turtle_left(angle = 60)
turtle_forward(dist = 20)
turtle_show()

# Line type and line width ---------
turtle_left(angle = 150)
turtle_lty(lty = 4)
turtle_forward(dist = 17)
turtle_lwd(lwd = 3)
turtle_forward(dist = 15)

# Using turtle_do() ------------------
turtle_init()
turtle_do({
  turtle_move(10)
  turtle_turn(45)
  turtle_move(15)
})

# Making a Square -----------
turtle_init()
turtle_do({
  turtle_move(20)
  turtle_right(90)
  turtle_move(20)
  turtle_right(90)
  turtle_move(20)
  turtle_right(90)
  turtle_move(20)
  turtle_right(90)
})

# Square made with a loop ---------
turtle_init()
turtle_do({
  for(i in 1:4) {
    turtle_forward(dist = 20)
    turtle_right(angle = 90)
  }
})

# Regular Octagon ------------
turtle_init()
turtle_do({
  for(i in 1:8) {
    turtle_forward(dist = 20)
    turtle_right(angle = 45)
  }
})

# Make a circle ----------------
turtle_init()
turtle_setpos(x = 30, y = 50)
turtle_do({
  for(i in 1:180) {
    turtle_forward(dist = 1)
    turtle_right(angle = 2)
  }
})

# Random Motion ------------
turtle_init()
turtle_do({
  for (i in 1:5) {
    x <- runif(1)
    if (x > 0.5) {
      turtle_right(angle = 45)
      turtle_lwd(lwd = 1)
      turtle_col(col = "red")
    } else {
      turtle_left(angle = 45)
      turtle_lwd(lwd = 3)
      turtle_col(col = "purple")
    }
    turtle_forward(dist = 10)
  }
})

# Make Squares ------------------
turtle_square <- function(r) { for (i in 1:4) {
  turtle_forward(r)
  turtle_right(90) }
}

turtle_init()
turtle_square(10)
turtle_left(90)
turtle_forward(30)
turtle_square(5)

# A Drunken Turtle ----------------
turtle_drunk <- function() {
  turtle_init(mode = "clip")
  origin <- turtle_getpos()
  repeat {
    move <- readline(prompt = "Go Again? (enter q to quit):  ")
    if ( move == "q") break
    angle <- runif(1, min = 0, max = 360)
    turtle_left(angle)
    turtle_forward(10)
    cp <- turtle_getpos()
    print(cp)
    distance <- round(sqrt((cp[1] - origin[1])^2 + (cp[2] - origin[2])^2),3)
    message <- paste0("Distance from starting point is: ", distance)
    cat(message)
  }
  cat("All done!")
}

# A Drunken Turtle Bouncing Off Wall----------------

turtle_bounce <- function(side = 60, step= 10) {
  if ( (side/2) %% step != 0 ) {
    return(cat("Side-length divided by two must be a multiple of step."))
  }
  bounds <- c(0, side)
  turtle_init(side, side, mode = "clip")
  origin <- turtle_getpos()
  cp <- turtle_getpos()
  repeat {
    move <- readline(prompt = "Go Again? (enter q to quit):  ")
    if ( move == "q") break
    x <- cp["x"]
    y <- cp["y"]
    if (x %in% bounds | y %in% bounds) {
      angle <- 180
    } else {
      angle <- sample(c(0,90,180,270), 1)
    }
    turtle_right(angle)
    turtle_forward(step)
    cp <- round(turtle_getpos(), 0)
    print(cp)
    move <- readline(prompt = "Go Again? (enter q to quit):  ")
  }
  cat("All done!")
}

turtle_bounce(side=60, step=15)

# Make Stars -------------------
turtle_star <- function(intensity=1) {
  y <- sample(1:657, 360*intensity, replace=TRUE)
  for (i in 1:(360*intensity)) {
    turtle_right(90)
    turtle_col(colors()[y[i]])
    x <- sample(1:100,1)
    turtle_forward(x)
    turtle_up()
    turtle_backward(x)
    turtle_down()
    turtle_left(90)
    turtle_forward(1/intensity)
    turtle_left(1/intensity)
  }
}

turtle_init(500, 500)
turtle_do({
  turtle_left(90)
  turtle_star(0.5)
})

# zany!
turtle_init(1000, 1000, mode = "clip")
turtle_do({
  turtle_right(90)
  for (i in 1:2000) {
    turtle_right(i)
    turtle_forward(sqrt(i))
  }
})




# less zany
turtle_init(150, 150, mode = "clip")
turtle_do({
  turtle_right(90)
  for (i in 1:720) {
    turtle_left(1)
    turtle_forward(i/720)
  }
})

# star-like function
turtle_star <- function(n = 6, len = 20, color = "red", width = 1, type  = 1) {
  turtle_param(col = color, lwd = width, lty = type)
  for ( i in 1:n ) {
    turtle_forward(len)
    turtle_backward(len)
    turtle_left(360/n)
  }
}

turtle_init()
turtle_setpos(25,75)
turtle_star(n = 4, len = 20, color = "blue", width = 4, type = 3)
turtle_setpos(75,75)
turtle_star(n = 6, 15, color = "black", width = 2, type = 6)
turtle_setpos(25,25)
turtle_star(n = 8, 12, color = "red", width = 3, type = 2)
turtle_setpos(75,25)
turtle_star(n = 6, 22, color = "blue", width = 1, type = 1)
turtle_hide()

# random-length rays
turtle_rstar <- function(n = 6, color = "red", width = 1, type  = 1) {
  turtle_param(col = color, lwd = width, lty = type)
  for ( i in 1:n ) {
    len <- runif(1, min = 5, max = 25)
    turtle_forward(len)
    turtle_backward(len)
    turtle_left(360/n)
  }
}
set.seed(4040)
turtle_init(50, 50)
turtle_rstar(n = 12, width = 3)
turtle_hide()

#random-color rays
turtle_rstarColor <- function(n = 6, width = 1, type  = 1) {
  turtle_param(lwd = width, lty = type)
  for ( i in 1:n ) {
    turtle_col(col = sample(colors(), size = 1))
    len <- runif(1, min = 5, max = 25)
    turtle_forward(len)
    turtle_backward(len)
    turtle_left(360/n)
  }
}
set.seed(4040)
turtle_init(50, 50)
turtle_rstarColor(n = 12, width = 5)
turtle_hide()

turtle_poly <- function(side, n) {
  for ( i in 1:n ) {
    turtle_forward(side)
    turtle_right(360/n)
  }
}

turtle_init(mode = "clip")
turtle_setpos(30, 50)
turtle_poly(15, 12)
