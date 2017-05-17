myData <- data.frame(x = 1:3, y = letters[1:3])

lst <- list(letter = "a", 
            num = 1:5,
            bool = c(T, F, F),
            df = myData)

set.seed(2020)
lst <- list()
probs <- c(0.1, 0.5, 0.9)
sizes <- c(50, 100, 40)
reps <- 3:1
for (i in 1:3) {
  lst[[i]] <- rbinom(n = reps[i], size = sizes[i], prob = probs[i])
}


bySeat <- split(m111survey, f = m111survey$seat)
