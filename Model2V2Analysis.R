#Model 2 v2 analysis, measuring distances between prey agents as the number of predators increases
library(tidyverse)
library(geosphere)
library(ggplot2)
library(igraph)
library(ggraph)

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


#Calculating networks and components analysis

pairDist <- function(df, predNum) {
  df |>
    filter(!is.na(x), !is.na(y)) |>
    group_by(tick) |>
    do({
      agents <- .
      n <- nrow(agents)
      out <- data.frame()
      if (n > 1) {
        for (i in 1:(n-1)) {
          for (j in (i+1):n) {
            d <- sqrt((agents$x[i] - agents$x[j])^2 + (agents$y[i] - agents$y[j])^2)
            out <- rbind(out, data.frame(
              tick = agents$tick[i],
              id1 = agents$id[i],
              id2 = agents$id[j],
              dist = d,
              num_predators = predNum
            ))
          }
        }
      }
      out
    }) |>
    ungroup()
}

d1 <- pairDist(p1Long, 1)
d2 <- pairDist(p2Long, 2)
d3 <- pairDist(p3Long, 3)
d4 <- pairDist(p4Long, 4)
d5 <- pairDist(p5Long, 5)
d6 <- pairDist(p6Long, 6)
d7 <- pairDist(p7Long, 7)
d8 <- pairDist(p8Long, 8)

allDist <- bind_rows(d1, d2, d3, d4, d5, d6, d7, d8)

ticks_all <- sort(unique(allDist$tick))
pred_levels <- sort(unique(allDist$num_predators))

buildNetwork <- function(Distdf, Coorddf, t, threshold) {
  edges_t <- Distdf |>
    filter(tick == t, dist < threshold)
  verts_t <- Coorddf |>
    filter(tick == t) |>
    distinct(id, x, y)
  
  g <- graph_from_data_frame(
    d <- edges_t |> select(id1, id2),
    vertices = verts_t |> rename(name = id),
    directed = FALSE
  )
  
  comps <- components(g)
  
  verts_t <- verts_t |>
    mutate(comp = comps$membership[as.character(id)],
           tick = t)
  
  list(graph = g, verts = verts_t)
} 

compSum <- function(networks) {
  comp_summary <- lapply(networks, function(net) {
    verts <- net$verts
    verts |>
      group_by(tick, comp) |>
      summarise(n_individuals = n(), .groups = "drop")
  }) |> bind_rows()
  
  return(comp_summary)
}

numComp <- function(compSummary) {
  compSummary |>
    group_by(tick) |>
    summarise(n_components = n(), .groups = "drop")
}

all_networks <- list()

for(pred in pred_levels) {
  Distdf_pred <- allDist |> filter(num_predators == pred)
  Coorddf_pred <- allData |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  all_networks[[as.character(pred)]] <- networks_pred
}

allCompSum <- lapply(names(all_networks), function(pred) {
  comp_summary <- compSum(all_networks[[pred]]) |>
    mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators
allCompSum3000 <- allCompSum |>
  filter(tick >= 3000)

allCompSum3000S <- allCompSum3000|>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

meanCompSumPred <- allCompSum3000 |>
  group_by(num_predators) |>
  summarise(mGroupSize = mean(n_individuals))

#Number of groups as changing with number of predators
allNumComp <- allCompSum |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

allNumComp3000 <- allNumComp |>
  filter(tick >= 3000)

allNumComp300m <- allNumComp3000 |>
  group_by(num_predators) |>
  summarise(meanGroups = mean(n_components))

#Metrics plots I need

#Changes in group size over time by number of predators after 3000 ticks
ggplot(allCompSum3000S, aes(x = tick, y = mGroupSize, color = factor(num_predators))) +
  geom_line(size = 1) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean Group Size",
    title = "Effect of predator number on prey density"
  ) +
  theme_minimal()

#Changes in average group size by predators after 3000 ticks
ggplot(meanCompSumPred, aes(x = num_predators, y = mGroupSize)) +
  geom_line(size = 1) +
  labs(x = "Number of Predators",
       y = "Mean Group Size",
       title = "Number of Predators and Group Size") +
  theme_minimal()

#Changes in the number of components over time by predators after 3000 ticks
ggplot(allNumComp3000, aes(x = tick, y = n_components, color = factor(num_predators))) +
  geom_line(size = 1) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Components",
    title = "Effect of predator number on Number of Groups") +
  theme_minimal()

#Changes in the average number of components by number of predators after 3000 ticks
ggplot(allNumComp300m, aes(x = num_predators, y = meanGroups)) +
  geom_line(size = 1) +
  labs(x = "Number of Predators",
       y = "Number of Groups",
       title = "Number of Predators and Number of Components") +
  theme_minimal()
