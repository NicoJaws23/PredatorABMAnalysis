#Analyzing data from territory ABM
library(tidyverse)
library(ggplot2)
library(lme4)

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
write.csv(predBounds, file = "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\outputs\\predBounds.csv")
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


#Time in pred teritory
preyInTerritoryFull <- preyLongM3 |>
  filter(!is.na(X), !is.na(Y)) |>
  crossing(predBounds) |>
  mutate(in_territory = X >= xmin & X <= xmax & Y >= ymin & Y <= ymax) |>
  group_by(id, tick) |>
  summarise(in_territory = any(in_territory), .groups = "drop")

ggplot(data = preyInTerritoryFull, mapping = aes(y = tick, x = in_territory)) +
  geom_point()

plot(preyInTerritoryFull$in_territory)

a1 <- preyLongM3 |>
  filter(id == 1:3)
plot(preyLongM3$X, preyLongM3$Y)

ggplot(data = preyLongM3, mapping = aes(X, Y, color = factor(id)))+
  geom_point()

predTerritoriesv2 <- predBounds %>%
  rowwise() %>%
  mutate(corners = list(data.frame(
    x = c(xmin, xmax, xmax, xmin, xmin),  # loop back to first point
    y = c(ymin, ymin, ymax, ymax, ymin)
  ))) %>%
  unnest(corners)

ggplot() +
  geom_point(data = preyLongM3,
             aes(x = X, y = Y, color = factor(id)), alpha = 0.6, size = 1) +
  geom_path(data = predTerritoriesv2,
            aes(x = x, y = y, group = predator_id),
            color = "red", linewidth = 1) +
  theme_minimal()

#Calculating predator area
predBounds <- predBounds |>
  mutate(area = abs(xmax - xmin) * abs(ymax - ymin))

total_pred_area <- sum(predBounds$area)

env_xmin <- min(preyLongM3$X, na.rm = TRUE)
env_xmax <- max(preyLongM3$X, na.rm = TRUE)
env_ymin <- min(preyLongM3$Y, na.rm = TRUE)
env_ymax <- max(preyLongM3$Y, na.rm = TRUE)
env_area <- (env_xmax - env_xmin) * (env_ymax - env_ymin)
percent_cover <- (total_pred_area / env_area) * 100

percent_cover