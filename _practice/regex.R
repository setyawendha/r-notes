p1 <- "Find our stuff at http://example.com, or visit https://nuther.org."
p2 <- "Call now:  502-863-8892 and hear 43.1 dogs bark 38.395 times each!"
p3 <- "yabbadabbadoo!"
p4 <- "The tort test theatre trumps tintinabulation."

m <- gregexpr("\\b[[:alpha:]]+\\b", p2, perl = T)
regmatches(p2, m)

m <- gregexpr("\\bt\\w+?t\\b", p4, perl = T)
regmatches(p4, m)

p5 <- "This is text with some some words repeated in in yo-yo maw2maw succession."

m <- gregexpr("(\\b\\S+\\b)\\s+\\b\\1\\b", p5, perl = T)
regmatches(p5, m)

m <- gregexpr("https?://(\\w+\\.)+\\w+", p1, perl = T)
regmatches(p1, m)

m <- gregexpr("\\b[a-df-zA-DF-Z0-9]+\\b", p4, perl = T)
regmatches(p4, m)

p6 <- "Hello there hello\nhello Hello"
m <- gregexpr("(?mi)hello", p6, perl=T)
regmatches(p6, m)

m <- grep("(\\b\\S+\\b)\\s+\\b\\1\\b", p5, perl = T)
m
regmatches(p5, m)


library(tigerData)
# find links
hunger <- subset(reviews, book == "hunger")
links <- list()
pattern <- "(?<=<a class=\"\"a-link-normal\"\" href=\"\"/)(.+?)(?=\"\">)"

hasLink <- grepl(pattern, x = hunger$content, perl = T)
hasLink

for (i in 1:nrow(hunger)) {
  text <- reviews$content[i]
  m <- gregexpr(pattern, text = text, perl=T)
  links[[i]] <- unlist(regmatches(text, m))
}

lens <- sapply(links, function(x) length(x))
table(lens)

#0      1      2      3      4      5      6      7      8      9 
#242798    343     64     34     13      5      5      3      2      2 


myPattern <-  "(?x) # ignore whitespace in regex
               \\b         # begin word
               [tT]        # upper or lower T to start
               \\w+        # the >= 1 word chars
               (?<!s)\\b   # but not ending in s
              "
myString <- "the tars temper Tanzania"
m <- gregexpr(myPattern, myString, perl = TRUE)
regmatches(myString, m)

## case conversion in gsub
myPattern <-  "(?x)        # ignore whitespace in regex
               (           # begin a capture
               \\b         # begin word
                [tT]        # upper or lower T to start
                \\w+        # the >= 1 word chars
                (?<!s)\\b   # but not ending in s
               )            # end the capture
                "
myString <- "the tars temper tories in Tanzania"
gsub(pattern = myPattern, replacement = "\\U\\1\\E", x = myString, perl = T)
