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
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "Effect of predator number on prey density"
  ) +
  theme_minimal()


dist_summary <- mean_dists |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

dist_summaryG <- mean_dists |>
  group_by(num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(dist_summaryG, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_point() +
  geom_line() +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Average prey spacing vs predator number"
  ) +
  theme_classic()

ggplot(dist_summary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
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

dist_summary3000G <- mDist3000 |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(dist_summary3000, aes(x = num_predators, y = avg_distance)) +
  geom_point(size = 3) +
  geom_line() +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Average prey spacing vs predator number After 3000 Ticks"
  ) +
  theme_classic()

ggplot(dist_summary3000G, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Average prey spacing vs predator number"
  ) +
  theme_classic()



#Calculating networks and components analysis

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
    title = "Effect of predator number on Group Size"
  ) +
  theme_minimal()

ggplot(allCompSum3000S, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "Effect of predator number on Group Size"
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

ggplot(allNumComp3000, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
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

##Patch densities for heatmaps
patch1 <- read.csv(file.choose(), header = TRUE)
patch2 <- read.csv(file.choose(), header = TRUE)
patch3 <- read.csv(file.choose(), header = TRUE)
patch4 <- read.csv(file.choose(), header = TRUE)
patch5 <- read.csv(file.choose(), header = TRUE)
patch6 <- read.csv(file.choose(), header = TRUE)
patch7 <- read.csv(file.choose(), header = TRUE)
patch8 <- read.csv(file.choose(), header = TRUE)

patch1Maps <- heatMap(df = patch1, numPred = 1, titleText = "Prey Density, 1 Predator")
patch1Maps$grid
patch1Maps$smooth
patch1Maps$preyDes

patch2Maps <- heatMap(df = patch2, numPred = 2, titleText = "Prey Density, 2 Predators")  
patch2Maps$grid
patch2Maps$smooth
patch2Maps$preyDes

patch3Maps <- heatMap(df = patch3, numPred = 3, titleText = "Prey Density, 3 Predators")  
patch3Maps$grid
patch3Maps$smooth
patch3Maps$preyDes

patch4Maps <- heatMap(df = patch4, numPred = 4, titleText = "Prey Density, 4 Predators")  
patch4Maps$grid
patch4Maps$smooth
patch4Maps$preyDes

patch5Maps <- heatMap(df = patch5, numPred = 5, titleText = "Prey Density, 5 Predators")  
patch5Maps$grid
patch5Maps$smooth
patch5Maps$preyDes

patch6Maps <- heatMap(df = patch6, numPred = 6, titleText = "Prey Density, 6 Predators")  
patch6Maps$grid
patch6Maps$smooth
patch6Maps$preyDes

patch7Maps <- heatMap(df = patch7, numPred = 7, titleText = "Prey Density, 7 Predators")  
patch7Maps$grid
patch7Maps$smooth
patch7Maps$preyDes

patch8Maps <- heatMap(df = patch8, numPred = 8, titleText = "Prey Density, 8 Predators")  
patch8Maps$grid
patch8Maps$smooth
patch8Maps$preyDes

