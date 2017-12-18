#library(dplyr)
#library(stringr)
library(magrittr)
c("aa", "ba", "b") %>% 
  str_subset(., "a") == "ba" %>% 
  length()
c("aa", "ba", "b") %>% 
  `==`(str_subset(., "a"), "ba") %>% 
  length()
`==`(1:3, 2)
`==`(str_subset(c("aa", "ba", "b"), "a"), "ba")
LETTERS[1:3] %>% tolower() == "b"
LETTERS[1:3] %>% tolower() == "b" %>% length()
1:3 %>% sqrt() + 2 %>% length()
1:3 %>% sqrt() + add(2) %>% length()
LETTERS[1:3] %>% equals(tolower(), "b") %>% length()
1:10 %>%  *2 %>% sum
1:10 %>% .*2 %>% sum
(1:3 %>% sqrt() + 3) %>% length()
library(stringr)
(c("aa", "ba", "b") %>% str_count("a") == 2) %>% sum()
