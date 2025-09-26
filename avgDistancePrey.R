#Analyzing data from territory ABM
library(tidyverse)
library(ggplot2)

################################################################################
################################################################################
################################################################################
#Measure distances between prey when predator have territory and prey move freely
#across the environment, Model 3

preyPointsM3 <- read.csv(file.choose(), header = TRUE)

predatorPointsM3 <- read.csv(file.choose(), header = TRUE)

predTerritoryM3 <- read.csv(file.choose(), header = TRUE)

#function to go through (x,y) coordinate values
parse_coords <- function(coord_str) {
  if (is.na(coord_str)) return(c(NA, NA))
  as.numeric(str_split(str_remove_all(coord_str, "[()]"), ",")[[1]])
}

#pivot table
preyLongM3 <- preyPointsM3 |>
  pivot_longer(cols = -id, names_to = "tick", values_to = "coords") |>
  mutate(tick = as.integer(str_remove(tick, "^X")), coords = na_if(coords, ""),
         X = map_dbl(coords, ~ ifelse(is.na(.x), NA, parse_coords(.x)[1])),
         Y = map_dbl(coords, ~ ifelse(is.na(.x), NA, parse_coords(.x)[2])))|>
  select(id, tick, X, Y)

#determine distance between individuals
preyDistM3 <- preyLongM3 |>
  filter(!is.na(X), !is.na(Y)) |>
  group_by(tick) |>
  do({
    agents <- .
    n <- nrow(agents)
    out <- data.frame()
    if (n > 1) {
      for (i in 1:(n-1)) {
        for (j in (i+1):n) {
          d <- sqrt((agents$X[i] - agents$X[j])^2 + (agents$Y[i] - agents$Y[j])^2)
          out <- rbind(out, data.frame(
            tick = agents$tick[i],
            id1 = agents$id[i],
            id2 = agents$id[j],
            dist = d
          ))
        }
      }
    }
    out
  }) |>
  ungroup()

#average NN distance
avgDistM3 <- preyDistM3 |>
  group_by(tick) |>
  summarise(avgDistance = mean(dist, na.rm = TRUE), .groups = "drop")

avgDistanceM3 <- ggplot(data = avgDistM3, mapping = aes(x = tick, y = avgDistance)) +
  geom_line()

#Number of times prey went into predator territory
predBounds <- predTerritoryM3 |>
  mutate(across(-id, ~ str_remove_all(., "[()]"))) |>
  separate(bottom.left, into = c("bl_x", "bl_y"), sep = ",", convert = TRUE) |>
  separate(bottom.right, into = c("br_x", "br_y"), sep = ",", convert = TRUE) |>
  separate(top.right, into = c("tr_x", "tr_y"), sep = ",", convert = TRUE) |>
  separate(top.left, into = c("tl_x", "tl_y"), sep = ",", convert = TRUE) |>
  transmute(
    predator_id = id,
    xmin = pmin(bl_x, tl_x),
    xmax = pmax(br_x, tr_x),
    ymin = pmin(bl_y, br_y),
    ymax = pmax(tl_y, tr_y)
  )

#seeing if and when prey are in territory
preyInTerritory <- preyLongM3 |>
  filter(!is.na(X), !is.na(Y)) |>
  crossing(predBounds) |>
  filter(X >= xmin, X <= xmax, Y >= ymin, Y <= ymax) |>
  select(prey_id = id, predator_id, tick, X, Y)

#summaries when prey were in predator territory 
preyInTerritorySum <- preyInTerritory |>
  group_by(prey_id, predator_id) |>
  summarise(
    ticks_inside = n(),
    first_tick = min(tick),
    last_tick = max(tick),
    .groups = "drop") |>
  arrange(prey_id, predator_id)

################################################################################
################################################################################
################################################################################
#Measure distances between prey when prey have a territory and predators
#roam freely across the environment, Model 5

preyPointsM5 <- read.csv(file.choose(), header = TRUE)

predatorPointsM5 <- read.csv(file.choose(), header = TRUE)

preyTerritoryM5 <- read.csv(file.choose(), header = TRUE)

#pivot table
preyLongM5 <- preyPointsM5 |>
  pivot_longer(cols = -id, names_to = "tick", values_to = "coords") |>
  mutate(tick = as.integer(str_remove(tick, "^X")), coords = na_if(coords, ""),
         X = map_dbl(coords, ~ ifelse(is.na(.x), NA, parse_coords(.x)[1])),
         Y = map_dbl(coords, ~ ifelse(is.na(.x), NA, parse_coords(.x)[2])))|>
  select(id, tick, X, Y)

#determine distance between individuals
preyDistM5 <- preyLongM5 |>
  filter(!is.na(X), !is.na(Y)) |>
  group_by(tick) |>
  do({
    agents <- .
    n <- nrow(agents)
    out <- data.frame()
    if (n > 1) {
      for (i in 1:(n-1)) {
        for (j in (i+1):n) {
          d <- sqrt((agents$X[i] - agents$X[j])^2 + (agents$Y[i] - agents$Y[j])^2)
          out <- rbind(out, data.frame(
            tick = agents$tick[i],
            id1 = agents$id[i],
            id2 = agents$id[j],
            dist = d
          ))
        }
      }
    }
    out
  }) |>
  ungroup()

#average NN distance
avgDistM5 <- preyDistM5 |>
  group_by(tick) |>
  summarise(avgDistance = mean(dist, na.rm = TRUE), .groups = "drop")

avgDistanceM5 <- ggplot(data = avgDistM5, mapping = aes(x = tick, y = avgDistance)) +
  geom_line()













