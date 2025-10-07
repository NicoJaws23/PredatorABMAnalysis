#Model 2 v2 analysis, measuring distances between prey agents as the number of predators increases
library(tidyverse)
library(geosphere)
library(ggplot2)

#Distance analysis
p1 <- read.csv(file.choose(), header = TRUE)
p1 <- p1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p2 <- read.csv(file.choose(), header = TRUE)
p2 <- p2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p3 <- read.csv(file.choose(), header = TRUE)
p3 <- p3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p4 <- read.csv(file.choose(), header = TRUE)
p4 <- p4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p5 <- read.csv(file.choose(), header = TRUE)
p5 <- p5 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p6 <- read.csv(file.choose(), header = TRUE)
p6 <- p6 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p7 <- read.csv(file.choose(), header = TRUE)
p7 <- p7 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
p8 <- read.csv(file.choose(), header = TRUE)
p8 <- p8 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

piv <- function(df, predNum){
  df |>
    pivot_longer(
    cols = -id,
    names_to = "tick",
    values_to = "coord") |>
    mutate(
      tick = as.numeric(tick),
      coord = str_remove_all(coord, "[()]"),
      x = as.numeric(str_split_fixed(coord, ",", 2)[,1]),
      y = as.numeric(str_split_fixed(coord, ",", 2)[,2]),
      num_predators = predNum
    )
}

p1Long <- piv(p1, 1)
p2Long <- piv(p2, 2)
p3Long <- piv(p3, 3)
p4Long <- piv(p4, 4)
p5Long <- piv(p5, 5)
p6Long <- piv(p6, 6)
p7Long <- piv(p7, 7)
p8Long <- piv(p8, 8)


allData <- bind_rows(p1Long, p2Long, p3Long, p4Long, p5Long, p6Long, p7Long, p8Long)

mean_dists <- allData |>
  group_by(num_predators, tick) |>
  group_modify(~ {
    df <- .x
    if (nrow(df) > 1) {
      dist_mat <- as.matrix(dist(df[, c("x", "y")]))
      mean_dist <- mean(dist_mat[upper.tri(dist_mat)], na.rm = TRUE)
      tibble(mean_distance = mean_dist)
    } else {
      tivvle(mean_distance = NA_real_)
    }
  }) |>
  ungroup()

ggplot(mean_dists, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "Effect of predator number on prey density"
  ) +
  theme_minimal()


dist_summary <- mean_dists |>
  group_by(num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(dist_summary, aes(x = num_predators, y = avg_distance)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Average prey spacing vs predator number"
  ) +
  theme_classic()

mDist3000 <- mean_dists |>
  filter(tick >= 3000)

dist_summary3000 <- mDist3000 |>
  group_by(num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(dist_summary3000, aes(x = num_predators, y = avg_distance)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Average prey spacing vs predator number After 3000 Ticks"
  ) +
  theme_classic()


#Number of neighbors analysis
n1 <- read.csv(file.choose(), header = TRUE)
n1 <- n1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n2 <- read.csv(file.choose(), header = TRUE)
n2 <- n2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n3 <- read.csv(file.choose(), header = TRUE)
n3 <- n3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n4 <- read.csv(file.choose(), header = TRUE)
n4 <- n4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n5 <- read.csv(file.choose(), header = TRUE)
n5 <- n5 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n6 <- read.csv(file.choose(), header = TRUE)
n6 <- n6 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n7 <- read.csv(file.choose(), header = TRUE)
n7 <- n7 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
n8 <- read.csv(file.choose(), header = TRUE)
n8 <- n8 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pivNeighbor<- function(df, predNum) {
  df |>
    pivot_longer(
      cols = -id,
      names_to = "tick",
      values_to = "neighbors",
      names_transform = list(tick = as.numeric)) |>
    mutate(num_predators = predNum)
  
}
n1Long <- pivNeighbor(n1, 1)
n2Long <- pivNeighbor(n2, 2)
n3Long <- pivNeighbor(n3, 3)
n4Long <- pivNeighbor(n4, 4)
n5Long <- pivNeighbor(n5, 5)
n6Long <- pivNeighbor(n6, 6)
n7Long <- pivNeighbor(n7, 7)
n8Long <- pivNeighbor(n8, 8)

allNData <- bind_rows(n1Long, n2Long, n3Long, n4Long, n5Long, n6Long, n7Long, n8Long)

neighborsSum <- allNData |>
  group_by(num_predators, tick) |>
  summarise(meanNeighbors = mean(neighbors, na.rm = TRUE), sd_neighbors = sd(neighbors, na.rm=TRUE), .groups = "drop")

ggplot(neighborsSum, aes(x = tick, y = meanNeighbors, color = factor(num_predators))) +
  geom_line(size = 1.2) +
  labs(title = "Mean Number of Prey Neighbors within 5 Patches",
       x = "Tick",
       y = "Mean Neighbor Count")

nOverall <- neighborsSum |>
  group_by(num_predators) |>
  summarize(mean_overall = mean(meanNeighbors, na.rm = TRUE))

ggplot(nOverall, aes(x = num_predators, y = mean_overall)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Numb of Predators",
    y = "Average Neighbor Count",
    title = "Average prey neighbors vs predator number"
  ) +
  theme_classic()

nOverall3000 <- neighborsSum |>
  filter(tick >= 3000) |>
  group_by(num_predators) |>
  summarize(avg_Neighbors = mean(meanNeighbors, na.rm = TRUE))

ggplot(nOverall3000, aes(x = num_predators, y = avg_Neighbors)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Numb of Predators",
    y = "Average Neighbor Count",
    title = "Average prey neighbors vs predator number after 3000 ticks"
  ) +
  theme_classic()
