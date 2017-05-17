

wordCounter <- function() {
  counts <- numeric()
  word <- readline(prompt = "Enter a word (press Enter to quit):  \n")
  while ( word != "" ) {
    if ( !is.na(counts[word]) ) {
      counts[word] <- counts[word] +1
    } else {
      counts[word] <- 1
    }
    word <- readline(prompt = "Enter a word (press Enter to quit):  \n")
  }
  
  #data.frame(word = names(counts), count = counts, row.names = NULL)
  as.table(counts)
}