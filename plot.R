library(dplyr)
library(ggplot2)

points.per.side <- 30
total.points <- (points.per.side - 2) * 4 + 4

to.x.coord <- function(x) {
  if (x < points.per.side) {
    return (x * box.size)
  }

  return (points.per.side)
}

to.y.coord <- function(x) {
  if (x < points.per.side) {
    return (1)
  }

  return (x - points.per.side + 1)
}

points <- tibble(start = 1:(points.per.side - 1)) %>%
  mutate(end = start + points.per.side) %>%
  mutate(end = ifelse(end > total.points, end - total.points, end)) %>%
  mutate(
    start.x = sapply(start, to.x.coord),
    end.x = sapply(end, to.x.coord),
    start.y = sapply(start, to.y.coord),
    end.y = sapply(end, to.y.coord)
  )

points %>%
  ggplot() +
  aes(
    x = start.x,
    y = start.y,
    xend = end.x,
    yend = end.y,
    label = start
  ) +
  geom_segment() +
  theme_void()

ggsave('plot.png', width = 5, height = 5)
