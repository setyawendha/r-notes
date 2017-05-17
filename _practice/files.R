library(readr)
alcGDP <- read_csv("https://query.data.world/s/b6plbxp3ym20s5a5iey36geul")
names(alcGDP)
names(alcGDP)[c(4,5)] <- c("liters", "gdp")
alcGDP$YearDisplay <- NULL
alcGDP$SexDisplay <- NULL

ggplot(alcGDP, aes(x = liters, y = gdp)) + geom_point()
