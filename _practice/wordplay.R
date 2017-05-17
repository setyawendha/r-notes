download.file(url = "http://thinkpython2.com/code/words.txt", 
              destfile = "_practice/words.txt")
words <- readLines(con = "_practice/words.txt")

save(words, file = "_resources/words.Rda")
load("_resources/words.Rda")

words[1:10]
nchar("hello")

# words more than 2 characters long
isLong <- nchar(words) > 20
words[isLong]

# words with no e
noE <- !grepl(pattern = "e", x = words, perl = TRUE)
length(words[noE])

# words where q appears, not followed by u
notQU <- grepl(pattern = "q(?!u)", x = words, perl = TRUE)
length(words[notQU])
words[notQU]

p <- "This is a test   of the Tart tat tt twelfth tester 29 days of blah blah happy days."

(r <- gregexpr(pattern = "\\b[Tt]\\w*[^\\s]\\b", p, perl = T))
regmatches(p, r)

print("hell\ro")
cat("hell\ro")

print("hell\ao")
cat("hell\ao")

print("hell\to")
cat("hell\to")

print("hell\fo")
cat("hell\fo")

print("hell\vo")
cat("hell\vo")

print("hell\\o")
cat("hell\\o")

print("hell\"o")
cat("hell\"o")

print("hell\'o")
cat("hell\'o")

print("hell\`o")
cat("hell\`o")

print("hell`o")
cat("hell`o")

print("hell\yo")
cat("hell\yo")

print("hell\u2709o")
cat("hell\u2709o")

# \n	newline
# \r	carriage return
# \t	tab
# \b	backspace
# \a	alert (bell)
# \f	form feed
# \v	vertical tab
# \\	backslash \
# \'	ASCII apostrophe '
# \"	ASCII quotation mark "
# \`	ASCII grave accent (backtick) `
# \nnn	character with given octal code (1, 2 or 3 digits)
# \xnn	character with given hex code (1 or 2 hex digits)
# \unnnn	Unicode character with given code (1--4 hex digits)
# \Unnnnnnnn	Unicode character with given code (1--8 hex digits)

myWords <- c("hello", "there", "world")
paste(myWords, collapse = ":")
myWords2 <- strsplit(paste(myWords, collapse = ":"), split = ":")
myWords2
myWords == myWords2
myWords == unlist(myWords2)
myWords == myWords2[[1]]

firstNames <- c("Donald", "Gina", "Rohini")
lastNames <- c("Duck", "Gentoriouslyfine", "Lancaster")
ages <- c(17, 19, 20)
gpa <- c(3.7, 3.9, 3.823)

cat(sprintf("%15-s%20-s%5-s%5-s\n\n", "First Name", "Last Name", "Age", "GPA"))
cat(sprintf("%15-s%20-s%5-d%5-g\n", firstNames, lastNames, ages, gpa), sep = "")
cat(sprintf("%15-s%20-s%5-s%5-s\n\n", "First Name", "Last Name", "Age", "GPA"))
cat(sprintf("%15-s%20-s%5-d%5-1.2f\n", firstNames, lastNames, ages, gpa), sep = "")

printOut <- function(first, last, age, gpa) {
  cat(sprintf("%15-s%20-s%5-s%5-s\n\n", "First Name", "Last Name", "Age", "GPA"))
  cat(sprintf("%15-s%20-s%5-d%5-1.2f\n", first, last, age, gpa), sep = "")
}

printOut(firstNames, lastNames, ages, gpa)

printOut <- function(first, last, age, gpa) {
  vectors <- as.list(match.call())[-1]
  sameLength <- function(vecs) {
    lengths <- sapply(vecs, function(x) length(eval(x)))
    length(unique(lengths)) == 1
  }
  if ( !sameLength(vectors) ) {
    stop("All vectors must be the same length.")
  }
  
  chopString <- function(string) {
    len <- nchar(string)
    substr(string, 1, min(12, len))
  }
  n <- length(first)
  for ( i in 1:n ) {
    firstName <- chopString(first[i])
    lastName <- chopString(last[i])
    cat(sprintf("%15-s%15-s%6-d%6.2-f\n", firstName, lastName, age[i], gpa[i]))
  }
}

printOut(firstNames, lastNames, ages, gpa)



## pride and prejudice------


# Look over words, see if they make sense.  This function helps check:
context <- function(pattern, string, width) {
  m <- gregexpr(pattern, string, perl = T)
  spots <- unlist(m)
  hasMatch <- !( length(spots) == 1 & spots[1] == -1 ) 
  if (hasMatch) {
    for (i in seq_along(spots)) {
      print(substr(string, spots[i] - width, spots[i] + width))
      cat("\n---------------------------\n")
    }
    cat(length(spots), "matches.\n\n")
    } else {
      cat("No matches!")
    }
}

system.time({
fileName <- '_practice/pride.txt'
pride2 <- readLines(con = fileName)
pride2 <- pride2[pride2 != ""]
pride2 <- gsub(pattern = "Chapter \\d\\d?", replacement = "", x = pride2, perl = T)

# underscores are used for emphasis:  remove them.
pride2 <- gsub(pattern = "\\b_([a-zA-Z-]+)_\\b", replacement = "\\1", x = pride2, perl = T)

pride2 <- gsub(pattern = "\\b(\\w+)\u{2019}m", replacement = "\\1 am", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\b(\\w+)\u{2019}re", replacement = "\\1 are", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\b(\\w+)\u{2019}ll", replacement = "\\1 will", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\b(\\w+)\u{2019}s", replacement = "\\1", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\bwon\u{2019}t", replacement = "will not", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\bshan\u{2019}t", replacement = "shall not", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\b(\\w+)\u{2019}t", replacement = "\\1 not", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\b\u{2018}tis\\b", replacement = "it is", x = pride2, perl = T)

# possessive for anonymized officer appears as : "----\u{2018}s"
# remove both references:
pride2 <- gsub(pattern = "----\u{2018}s\\b", replacement = "", x = pride2, perl = T)

# times in the form hour o'clock appear.  Lets' remove the o':
pride2 <- gsub(pattern = "\\b([oO]\u{2019})(?=[Cc]lock)", replacement = "", x = pride2, perl = T)

#St is for Saint in a couple of place names.  Change to Saint:
pride2 <- gsub(pattern = "\\bSt\\b", replacement = "Saint", x = pride2, perl = T)

# e and edw alone are abbr for Edward.  Drop them.
pride2 <- gsub(pattern = "\\b[eE]\\b", replacement = "", x = pride2, perl = T)

# Mister for Mr., Mrs. for Mistress.  Keep them
# esq for Esquire, keep it.

# etc. is et cetera.  Replace
pride2 <- gsub(pattern = "\\betc\\b", replacement = "et cetera", x = pride2, perl = T)

# se'nnight and se'ennight (a week) to sennight
pride2 <- gsub(pattern = "se\u{2019}e?nnight", replacement = "sennight", x = pride2, perl = T)

# austin has dates like 3rd, 4th, etc..  Remove these
pride2 <- gsub(pattern = "\\b\\d+(st|nd|rd|th)\\b", replacement = " ", x = pride2, perl = T)

# de is only in "de Bourgh":  remove it
pride2 <- gsub(pattern = "de Bourgh", replacement = "Bourgh", x = pride2, perl = T)

# B, W, F, P and M are devices to anonymize people:  Colonel F, W for Wickham, etc.
# let's remove them
pride2 <- gsub(pattern = "\\b[BPWFM]\\b", replacement = "", x = pride2, perl = T)


#m <- gregexpr(pattern = "----\\w+\\b", text = pride, perl = T)
#regmatches(pride, m)
# "----You occurs, but really should have been --You:
pride2 <- gsub(pattern = "----You", replacement = "--You", x = pride2, perl = T)

# deal with the anonymous Shire(s):
pride2 <- gsub(pattern = "----shire", replacement = "XXXXshire", x = pride2, perl = T)

#there is one triple hyphenation:  tete-a-teta
# deal with it manually:
pride2 <- gsub(pattern = "tete-a-tete", replacement = "tete a tete", x = pride2, perl = T)

#m <- gregexpr(pattern = "\\b\\w+--\\w+\\b", text = pride, perl = T)
#regmatches(pride, m)
# remove emdash (--), replace with space (this also separates hyphenated words)
pride2 <- gsub(pattern = "\\b(\\w+)-{1,2}(\\w+)\\b", replacement = "\\1 \\2 ", x = pride2, perl = T)

pride2 <- tolower(pride2)

# remove all non-letter stuff (puncutation, etc.)
pride2 <- gsub(pattern = "[^a-z]", replacement = " ", x = pride2, perl = T)

# but restore Vingt-un, tete-a-tete and self-gratulation:
pride2 <- gsub(pattern = "\\bvingt un\\b", replacement = "vingt-un", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\btete a tete\\b", replacement = "tete-a-tete", x = pride2, perl = T)
pride2 <- gsub(pattern = "\\bself gratulation\\b", replacement = "self-gratulation", x = pride2, perl = T)

# get rid of stubborn edw:
pride2 <- gsub(pattern = "\\bedw\\b", replacement = "", x = pride2, perl = T)

pw <- unlist(strsplit(pride2, split = " +"))
pw <- prideWords[pw != ""]
})

# The above required 0.530 seconds on the Mac.


lexicon <- sort(unique(pw))
lexicon[!(lexicon %in% words)]

tab <- table(pw)
tab["pride"]
system.time(table(pw))

f1 <- function(words) {
  wordTable <- numeric()
  for ( word in words) {
    if ( is.na(wordTable[word]) ) {
      wordTable[word] <- 1
    } else {
      wordTable[word] <- wordTable[word] + 1
    }
  }
  wordTable
}

tab <- f1(prideWords)

system.time(f1(pw))


rep <- grepl(pattern = "(\\w{4,})\\1", words, perl = T)
words[rep]
