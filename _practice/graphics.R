library(htmlwidgets)
library(threejs)
library(mosaicData)
library(tigerstats)
library(plotly)
library(slickR)
library(svglite)
library(xml2)

n <- nrow(RailTrail)
seasoncolor <- character(n)
for ( i in 1:n ) {
  if ( RailTrail$spring[i] ) {
    seasoncolor[i] <- "yellow"
  } else if ( RailTrail$summer[i] ) {
    seasoncolor[i] <- "darkgreen"
  } else if ( RailTrail$fall[i] ) {
    seasoncolor[i] <- "red"
  }
}

RailTrail$logpc <- log(RailTrail$precip +1)
df <- RailTrail[, c("avgtemp","logpc","volume")]
mat <- as.matrix(df)

scatterplot3js(x = mat, color = seasoncolor)

df <- m111survey[, c("height", "fastest", "GPA", "sex")]
na.omit(df)
df$color <- ifelse(df$sex == "female", "lightblue", "burlywood")
mat <- as.matrix(df[, c("height", "fastest", "GPA")])
scatterplot3js(x = mat, color = df$color,
               size = 0.4)

small <- m111survey[, c("sex", "seat", "fastest")]
df1 <- subset(small, seat == "1_front")
df2 <- subset(small, seat == "2_middle")
df3 <- subset(small, seat == "3_back")

p1 <- ggplot(df1, aes(x = sex, y = fastest)) +
  geom_violin(fill = "burlywood") +
  geom_jitter(width = 0.25) +
  labs(y = "fastest speed driven (mph)",
       title = "Front-sitters")

p2 <- ggplot(df2, aes(x = sex, y = fastest)) +
  geom_violin(fill = "burlywood") +
  geom_jitter(width = 0.25) +
  labs(y = "fastest speed driven (mph)",
       title = "Middle-sitters")

p3 <- ggplot(df3, aes(x = sex, y = fastest)) +
  geom_violin(fill = "burlywood") +
  geom_jitter(width = 0.25) +
  labs(y = "fastest speed driven (mph)",
       title = "Back-sitters")

plotsToSVG <- list(
  xmlSVG(print(p1), standalone = TRUE),
  xmlSVG(print(p2), standalone = TRUE),
  xmlSVG(print(p3), standalone = TRUE)
)


fn <- fivenum(m111survey$fastest)
iqr <- fn[4] - fn[2]
ggplot(m111survey, aes(group = 1, x = factor(1), y = fastest)) +
  geom_boxplot(fill = "burlywood", width = 0.3) +
  annotate("text", label = "Q1 -->", x = 0.8, y = fn[2], size = 5) +
  annotate("text", label = "median -->", x = 0.765, y = fn[3], size = 5) +
  annotate("text", label = "Q3 -->", x = 0.8, y = fn[4], size = 5) +
  annotate("text", label = "Q3 + 1.5 * IQR -->", x = 0.85,
           y = fn[4] + 1.5*iqr - 3, size = 5) +
  annotate("text", label = "outlier -->", x = 0.91, y = fn[5], size = 5) +
  annotate("text", label = "min -->", x = 0.93, y = fn[1], size = 5)  +
  labs(y = "fastest speed driven (mph)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
