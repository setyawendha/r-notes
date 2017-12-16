library(tidyverse)
data("m111survey", package = "tigerstats")
m111survey %>% 
  filter(sex == "male") %>% 
  select(height, extra_life)
data("babynames", package = "babynames")
babynames %>% 
  filter(name == "Prince" & year >= 1978)
class(m111survey)
class(babynames)
m111survey <-
  as_tibble(m111survey)
m111survey

"Hello" %>% cat()
"Hello" %>% rep(times = 4)
1:20 %>% sum()
letters %>% .[4]
4 %>% rep("hello", times = .)
rep(letters, times = 1:length(letters))
letters %>% rep(times = 1:length(.))
