#Model 6 Analysis, Divided by Type of Model Run
library(tidyverse)
library(ggplot2)
library(lme4)
library(sf)
library(igraph)

#########################
###No Territory Memory###
#########################

##Basic Stats for Prey Distances Due to Predators##
ntm1 <- read.csv(file.choose(), header = TRUE)
ntm1 <- ntm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntm2 <- read.csv(file.choose(), header = TRUE)
ntm2 <- ntm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntm3 <- read.csv(file.choose(), header = TRUE)
ntm3 <- ntm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntm4 <- read.csv(file.choose(), header = TRUE)
ntm4 <- ntm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntm1Long <- piv(ntm1, 1)
ntm2Long <- piv(ntm2, 2)
ntm3Long <- piv(ntm3, 3)
ntm4Long <- piv(ntm4, 4)

NTM_all <- bind_rows(ntm1Long, ntm2Long, ntm3Long, ntm4Long)

NTM_meanDist <- meanDist(NTM_all)

ggplot(NTM_meanDist, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "No Territory With Memory, Effect of predator number on prey density"
  ) +
  theme_minimal()

NTM_distsummary <- NTM_meanDist |>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(NTM_distsummary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "No Territory With Memory, Average prey spacing vs predator number, post 3000 ticks"
  ) +
  theme_classic()

##Calc Networks and Components##

NTMd1 <- pairDist(ntm1Long, 1)
NTMd2 <- pairDist(ntm2Long, 2)
NTMd3 <- pairDist(ntm3Long, 3)
NTMd4 <- pairDist(ntm4Long, 4)

NTM_allDist <- bind_rows(NTMd1, NTMd2, NTMd3, NTMd4)

NTM_ticks_all <- sort(unique(NTM_allDist$tick))
NTM_pred_levels <- sort(unique(NTM_allDist$num_predators))

NTM_all_networks <- list()

for(pred in NTM_pred_levels) {
  Distdf_pred <- NTM_allDist |> filter(num_predators == pred)
  Coorddf_pred <- NTM_all |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  NTM_all_networks[[as.character(pred)]] <- networks_pred
}

NTM_allCompSum <- lapply(names(NTM_all_networks), function(pred) {
  comp_summary <- compSum(NTM_all_networks[[pred]]) |>
      mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators

NTM_groupSize3000 <- NTM_allCompSum|>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

ggplot(NTM_groupSize3000, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "No Territory With Memory, Effect of predator number on Group Size, post 3000 ticks"
  ) +
  theme_minimal()
#Number of groups as changing with number of predators
NTM_allNumComp <- NTM_allCompSum |>
  filter(tick >= 3000) |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

ggplot(NTM_allNumComp, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Components",
    title = "No Territory With Memory, Effect of predator number on Number of Groups, post 3000 ticks") +
  theme_minimal()

##Heatmaps Based on Number of Prey on a Patch##
NTM_patch1 <- read.csv(file.choose(), header = TRUE)
NTM_patch2 <- read.csv(file.choose(), header = TRUE)
NTM_patch3 <- read.csv(file.choose(), header = TRUE)
NTM_patch4 <- read.csv(file.choose(), header = TRUE)

NTM_patch1Maps <- heatMap(df = NTM_patch1, numPred = 1, titleText = "No Territory with Memory Prey Density, 1 Predator")
NTM_patch1Maps$grid
NTM_patch1Maps$smooth
NTM_patch1Maps$preyDes

NTM_patch2Maps <- heatMap(df = NTM_patch2, numPred = 2, titleText = "No Territory with Memory Prey Density, 2 Predators")
NTM_patch2Maps$grid
NTM_patch2Maps$smooth
NTM_patch2Maps$preyDes

NTM_patch3Maps <- heatMap(df = NTM_patch3, numPred = 3, titleText = "No Territory with Memory Prey Density, 3 Predators")
NTM_patch3Maps$grid
NTM_patch3Maps$smooth
NTM_patch3Maps$preyDes

NTM_patch4Maps <- heatMap(df = NTM_patch4, numPred = 4, titleText = "No Territory with Memory Prey Density, 4 Predators")
NTM_patch4Maps$grid
NTM_patch4Maps$smooth
NTM_patch4Maps$preyDes

############################
###No Territory No Memory###
############################

##Basic Stats for Prey Distances Due to Predators##
ntnm1 <- read.csv(file.choose(), header = TRUE)
ntnm1 <- ntnm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntnm2 <- read.csv(file.choose(), header = TRUE)
ntnm2 <- ntnm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntnm3 <- read.csv(file.choose(), header = TRUE)
ntnm3 <- ntnm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntnm4 <- read.csv(file.choose(), header = TRUE)
ntnm4 <- ntnm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

ntnm1Long <- piv(ntnm1, 1)
ntnm2Long <- piv(ntnm2, 2)
ntnm3Long <- piv(ntnm3, 3)
ntnm4Long <- piv(ntnm4, 4)

NTNM_all <- bind_rows(ntnm1Long, ntnm2Long, ntnm3Long, ntnm4Long)

NTNM_meanDist <- meanDist(NTNM_all)

ggplot(NTNM_meanDist, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "No Territory No Memory, Effect of predator number on prey density"
  ) +
  theme_minimal()

NTNM_distsummary <- NTNM_meanDist |>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(NTNM_distsummary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "No Territory No Memory, Average prey spacing vs predator number, post 3000 ticks"
  ) +
  theme_classic()

##Calc Networks and Components##

NTNMd1 <- pairDist(ntnm1Long, 1)
NTNMd2 <- pairDist(ntnm2Long, 2)
NTNMd3 <- pairDist(ntnm3Long, 3)
NTNMd4 <- pairDist(ntnm4Long, 4)

NTNM_allDist <- bind_rows(NTNMd1, NTNMd2, NTNMd3, NTNMd4)

NTNM_ticks_all <- sort(unique(NTNM_allDist$tick))
NTNM_pred_levels <- sort(unique(NTNM_allDist$num_predators))

NTNM_all_networks <- list()

for(pred in NTNM_pred_levels) {
  Distdf_pred <- NTNM_allDist |> filter(num_predators == pred)
  Coorddf_pred <- NTNM_all |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  NTNM_all_networks[[as.character(pred)]] <- networks_pred
}

NTNM_allCompSum <- lapply(names(NTNM_all_networks), function(pred) {
  comp_summary <- compSum(NTNM_all_networks[[pred]]) |>
    mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators

NTNM_groupSize3000 <- NTNM_allCompSum|>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

ggplot(NTNM_groupSize3000, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "No Territory No Memory, Effect of predator number on Group Size, post 3000 ticks"
  ) +
  theme_minimal()
#Number of groups as changing with number of predators
NTNM_allNumComp <- NTNM_allCompSum |>
  filter(tick >= 3000) |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

ggplot(NTNM_allNumComp, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Components",
    title = "No Territory No Memory, Effect of predator number on Number of Groups, post 3000 ticks") +
  theme_minimal()

##Heatmaps Based on Number of Prey on a Patch##
NTNM_patch1 <- read.csv(file.choose(), header = TRUE)
NTNM_patch2 <- read.csv(file.choose(), header = TRUE)
NTNM_patch3 <- read.csv(file.choose(), header = TRUE)
NTNM_patch4 <- read.csv(file.choose(), header = TRUE)

NTNM_patch1Maps <- heatMap(df = NTNM_patch1, numPred = 1, titleText = "No Territory No Memory,  Prey Density, 1 Predator")
NTNM_patch1Maps$grid
NTNM_patch1Maps$smooth
NTNM_patch1Maps$preyDes

NTNM_patch2Maps <- heatMap(df = NTNM_patch2, numPred = 2, titleText = "No Territory No Memory,  Prey Density, 2 Predators")
NTNM_patch2Maps$grid
NTNM_patch2Maps$smooth
NTNM_patch2Maps$preyDes

NTNM_patch3Maps <- heatMap(df = NTNM_patch3, numPred = 3, titleText = "No Territory No Memory,  Prey Density, 3 Predators")
NTNM_patch3Maps$grid
NTNM_patch3Maps$smooth
NTNM_patch3Maps$preyDes

NTNM_patch4Maps <- heatMap(df = NTNM_patch4, numPred = 4, titleText = "No Territory No Memory,  Prey Density, 4 Predators")
NTNM_patch4Maps$grid
NTNM_patch4Maps$smooth
NTNM_patch4Maps$preyDes


###############################
###Predator Territory Memory###
###############################

##Basic Stats for Prey Distances Due to Predators##
pdtm1 <- read.csv(file.choose(), header = TRUE)
pdtm1 <- pdtm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtm2 <- read.csv(file.choose(), header = TRUE)
pdtm2 <- pdtm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtm3 <- read.csv(file.choose(), header = TRUE)
pdtm3 <- pdtm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtm4 <- read.csv(file.choose(), header = TRUE)
pdtm4 <- pdtm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtm1Long <- piv(pdtm1, 1)
pdtm2Long <- piv(pdtm2, 2)
pdtm3Long <- piv(pdtm3, 3)
pdtm4Long <- piv(pdtm4, 4)

PDTM_all <- bind_rows(pdtm1Long, pdtm2Long, pdtm3Long, pdtm4Long)

PDTM_meanDist <- meanDist(PDTM_all)

ggplot(PDTM_meanDist, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "Predator Territory With Memory, Effect of predator number on prey density"
  ) +
  theme_minimal()

PDTM_distsummary <- PDTM_meanDist |>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(PDTM_distsummary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Predator Territory With Memory, Average prey spacing vs predator number, post 3000 ticks"
  ) +
  theme_classic()

##Calc Networks and Components##

PDTMd1 <- pairDist(pdtm1Long, 1)
PDTMd2 <- pairDist(pdtm2Long, 2)
PDTMd3 <- pairDist(pdtm3Long, 3)
PDTMd4 <- pairDist(pdtm4Long, 4)

PDTM_allDist <- bind_rows(PDTMd1, PDTMd2, PDTMd3, PDTMd4)

PDTM_ticks_all <- sort(unique(PDTM_allDist$tick))
PDTM_pred_levels <- sort(unique(PDTM_allDist$num_predators))

PDTM_all_networks <- list()

for(pred in PDTM_pred_levels) {
  Distdf_pred <- PDTM_allDist |> filter(num_predators == pred)
  Coorddf_pred <- PDTM_all |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  PDTM_all_networks[[as.character(pred)]] <- networks_pred
}

PDTM_allCompSum <- lapply(names(PDTM_all_networks), function(pred) {
  comp_summary <- compSum(PDTM_all_networks[[pred]]) |>
    mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators

PDTM_groupSize3000 <- PDTM_allCompSum|>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

ggplot(PDTM_groupSize3000, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "Predator Territory With Memory, Effect of predator number on Group Size, post 3000 ticks"
  ) +
  theme_minimal()

#Number of groups as changing with number of predators

PDTM_allNumComp <- PDTM_allCompSum |>
  filter(tick >= 3000) |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

ggplot(PDTM_allNumComp, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Components",
    title = "Predator Territory With Memory, Effect of predator number on Number of Groups, post 3000 ticks") +
  theme_minimal()

##Heatmaps Based on Number of Prey on a Patch##

#Get predator territories measured
#territory boundary data
PDTM_pT1 <- read.csv(file.choose(), header = TRUE)
PDTM_pT2 <- read.csv(file.choose(), header = TRUE)
PDTM_pT3 <- read.csv(file.choose(), header = TRUE)
PDTM_pT4 <- read.csv(file.choose(), header = TRUE)

#patch count data
PDTM_pC1 <- read.csv(file.choose(), header = TRUE)
PDTM_pC2 <- read.csv(file.choose(), header = TRUE)
PDTM_pC3 <- read.csv(file.choose(), header = TRUE)
PDTM_pC4 <- read.csv(file.choose(), header = TRUE)

PDTM_TB1 <- terrBounds(PDTM_pT1, wWidth = 100, wHeight = 100)
PDTM_TB2 <- terrBounds(PDTM_pT2, wWidth = 100, wHeight = 100)
PDTM_TB3 <- terrBounds(PDTM_pT3, wWidth = 100, wHeight = 100)
PDTM_TB4 <- terrBounds(PDTM_pT4, wWidth = 100, wHeight = 100)

#seeing if and when prey are in territory, use preyInTerr()
detach("package:igraph", unload = TRUE)

PDTM_pP1In <- preyInTerr(pdtm1Long, PDTM_TB1$raw, 3000)
PDTM_pP2In <- preyInTerr(pdtm2Long, PDTM_TB2$raw, 3000)
PDTM_pP3In <- preyInTerr(pdtm3Long, PDTM_TB3$raw, 3000)
PDTM_pP4In <- preyInTerr(pdtm4Long, PDTM_TB4$raw, 3000)

#summaries when prey were in predator territory, use preyInTerrSum()
PDTM_pP1InSum <- preyInTerrSum(PDTM_pP1In)
PDTM_pP2InSum <- preyInTerrSum(PDTM_pP2In)
PDTM_pP3InSum <- preyInTerrSum(PDTM_pP3In)
PDTM_pP4InSum <- preyInTerrSum(PDTM_pP4In)

#Time in pred teritory, use predInTerrTime()
PDTM_pP1T <- predInTerrTime(pdtm1Long, PDTM_TB1$raw)
PDTM_pP2T <- predInTerrTime(pdtm2Long, PDTM_TB2$raw)
PDTM_pP3T <- predInTerrTime(pdtm3Long, PDTM_TB3$raw)
PDTM_pP4T <- predInTerrTime(pdtm4Long, PDTM_TB4$raw)

#plotting heat maps with predator territory
PDTM_hm1 <- heatMapPredTerr(PDTM_pC1, numPred = 1, terrBoundsObj = PDTM_TB1, titleText = "Predator Territory With Memory, Prey Density, 1 Predator")
PDTM_hm1$raw
PDTM_hm1$smooth
PDTM_hm1$relative

PDTM_hm2 <- heatMapPredTerr(PDTM_pC2, numPred = 2, terrBoundsObj = PDTM_TB2, titleText = "Predator Territory With Memory, Prey Density, 2 Predators")
PDTM_hm2$raw
PDTM_hm2$smooth
PDTM_hm2$relative

PDTM_hm3 <- heatMapPredTerr(PDTM_pC3, numPred = 3, terrBoundsObj = PDTM_TB3, titleText = "Predator Territory With Memory, Prey Density, 3 Predators")
PDTM_hm3$raw
PDTM_hm3$smooth
PDTM_hm3$relative

PDTM_hm4 <- heatMapPredTerr(PDTM_pC4, numPred = 4, terrBoundsObj = PDTM_TB4, titleText = "Predator Territory With Memory, Prey Density, 4 Predators")
PDTM_hm4$raw
PDTM_hm4$smooth
PDTM_hm4$relative

library(igraph)
##################################
###Predator Territory No Memory###
##################################

##Basic Stats for Prey Distances Due to Predators##
pdtnm1 <- read.csv(file.choose(), header = TRUE)
pdtnm1 <- pdtnm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtnm2 <- read.csv(file.choose(), header = TRUE)
pdtnm2 <- pdtnm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtnm3 <- read.csv(file.choose(), header = TRUE)
pdtnm3 <- pdtnm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtnm4 <- read.csv(file.choose(), header = TRUE)
pdtnm4 <- pdtnm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pdtnm1Long <- piv(pdtnm1, 1)
pdtnm2Long <- piv(pdtnm2, 2)
pdtnm3Long <- piv(pdtnm3, 3)
pdtnm4Long <- piv(pdtnm4, 4)

PDTNM_all <- bind_rows(pdtnm1Long, pdtnm2Long, pdtnm3Long, pdtnm4Long)

PDTNM_meanDist <- meanDist(PDTNM_all)

ggplot(PDTNM_meanDist, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "Predator Territory No Memory, Effect of predator number on prey density"
  ) +
  theme_minimal()

PDTNM_distsummary <- PDTNM_meanDist |>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(PDTNM_distsummary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Predator Territory No Memory, Average prey spacing vs predator number, post 3000 ticks"
  ) +
  theme_classic()

##Calc Networks and Components##

PDTNMd1 <- pairDist(pdtnm1Long, 1)
PDTNMd2 <- pairDist(pdtnm2Long, 2)
PDTNMd3 <- pairDist(pdtnm3Long, 3)
PDTNMd4 <- pairDist(pdtnm4Long, 4)

PDTNM_allDist <- bind_rows(PDTNMd1, PDTNMd2, PDTNMd3, PDTNMd4)

PDTNM_ticks_all <- sort(unique(PDTNM_allDist$tick))
PDTNM_pred_levels <- sort(unique(PDTNM_allDist$num_predators))

PDTNM_all_networks <- list()

for(pred in PDTNM_pred_levels) {
  Distdf_pred <- PDTNM_allDist |> filter(num_predators == pred)
  Coorddf_pred <- PDTNM_all |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  PDTNM_all_networks[[as.character(pred)]] <- networks_pred
}

PDTNM_allCompSum <- lapply(names(PDTNM_all_networks), function(pred) {
  comp_summary <- compSum(PDTNM_all_networks[[pred]]) |>
    mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators

PDTNM_groupSize3000 <- PDTNM_allCompSum|>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

ggplot(PDTNM_groupSize3000, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "Predator Territory No Memory, Effect of predator number on Group Size, post 3000 ticks"
  ) +
  theme_minimal()

#Number of groups as changing with number of predators

PDTNM_allNumComp <- PDTNM_allCompSum |>
  filter(tick >= 3000) |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

ggplot(PDTNM_allNumComp, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Components",
    title = "Predator Territory No Memory, Effect of predator number on Number of Groups, post 3000 ticks") +
  theme_minimal()

##Heatmaps Based on Number of Prey on a Patch##

#Get predator territories measured
#territory boundary data
PDTNM_pT1 <- read.csv(file.choose(), header = TRUE)
PDTNM_pT2 <- read.csv(file.choose(), header = TRUE)
PDTNM_pT3 <- read.csv(file.choose(), header = TRUE)
PDTNM_pT4 <- read.csv(file.choose(), header = TRUE)

#patch count data
PDTNM_pC1 <- read.csv(file.choose(), header = TRUE)
PDTNM_pC2 <- read.csv(file.choose(), header = TRUE)
PDTNM_pC3 <- read.csv(file.choose(), header = TRUE)
PDTNM_pC4 <- read.csv(file.choose(), header = TRUE)

PDTNM_TB1 <- terrBounds(PDTNM_pT1, wWidth = 100, wHeight = 100)
PDTNM_TB2 <- terrBounds(PDTNM_pT2, wWidth = 100, wHeight = 100)
PDTNM_TB3 <- terrBounds(PDTNM_pT3, wWidth = 100, wHeight = 100)
PDTNM_TB4 <- terrBounds(PDTNM_pT4, wWidth = 100, wHeight = 100)

#seeing if and when prey are in territory, use preyInTerr()
detach("package:igraph", unload = TRUE)

PDTNM_pP1In <- preyInTerr(pdtnm1Long, PDTNM_TB1$raw, 3000)
PDTNM_pP2In <- preyInTerr(pdtnm2Long, PDTNM_TB2$raw, 3000)
PDTNM_pP3In <- preyInTerr(pdtnm3Long, PDTNM_TB3$raw, 3000)
PDTNM_pP4In <- preyInTerr(pdtnm4Long, PDTNM_TB4$raw, 3000)

#summaries when prey were in predator territory, use preyInTerrSum()
PDTNM_pP1InSum <- preyInTerrSum(PDTNM_pP1In)
PDTNM_pP2InSum <- preyInTerrSum(PDTNM_pP2In)
PDTNM_pP3InSum <- preyInTerrSum(PDTNM_pP3In)
PDTNM_pP4InSum <- preyInTerrSum(PDTNM_pP4In)

#Time in pred teritory, use predInTerrTime()
PDTNM_pP1T <- predInTerrTime(pdtnm1Long, PDTNM_TB1$raw)
PDTNM_pP2T <- predInTerrTime(pdtnm2Long, PDTNM_TB2$raw)
PDTNM_pP3T <- predInTerrTime(pdtnm3Long, PDTNM_TB3$raw)
PDTNM_pP4T <- predInTerrTime(pdtnm4Long, PDTNM_TB4$raw)

#plotting heat maps with predator territory
PDTNM_hm1 <- heatMapPredTerr(PDTNM_pC1, numPred = 1, terrBoundsObj = PDTNM_TB1, titleText = "Predator Territory No Memory, Prey Density, 1 Predator")
PDTNM_hm1$raw
PDTNM_hm1$smooth
PDTNM_hm1$relative

PDTNM_hm2 <- heatMapPredTerr(PDTNM_pC2, numPred = 2, terrBoundsObj = PDTNM_TB2, titleText = "Predator Territory No Memory, Prey Density, 2 Predators")
PDTNM_hm2$raw
PDTNM_hm2$smooth
PDTNM_hm2$relative

PDTNM_hm3 <- heatMapPredTerr(PDTNM_pC3, numPred = 3, terrBoundsObj = PDTNM_TB3, titleText = "Predator Territory No Memory, Prey Density, 3 Predators")
PDTNM_hm3$raw
PDTNM_hm3$smooth
PDTNM_hm3$relative

PDTNM_hm4 <- heatMapPredTerr(PDTNM_pC4, numPred = 4, terrBoundsObj = PDTNM_TB4, titleText = "Predator Territory No Memory, Prey Density, 4 Predators")
PDTNM_hm4$raw
PDTNM_hm4$smooth
PDTNM_hm4$relative

library(igraph)
###########################
###Prey Territory Memory###
###########################

##Basic Stats for Prey Distances Due to Predators##
pytm1 <- read.csv(file.choose(), header = TRUE)
pytm1 <- pytm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytm2 <- read.csv(file.choose(), header = TRUE)
pytm2 <- pytm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytm3 <- read.csv(file.choose(), header = TRUE)
pytm3 <- pytm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytm4 <- read.csv(file.choose(), header = TRUE)
pytm4 <- pytm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytm1Long <- piv(pytm1, 1)
pytm2Long <- piv(pytm2, 2)
pytm3Long <- piv(pytm3, 3)
pytm4Long <- piv(pytm4, 4)

PYTM_all <- bind_rows(pytm1Long, pytm2Long, pytm3Long, pytm4Long)

PYTM_meanDist <- meanDist(PYTM_all)

ggplot(PYTM_meanDist, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "Prey Territory With Memory, Effect of predator number on prey density"
  ) +
  theme_minimal()

PYTM_distsummary <- PYTM_meanDist |>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(PYTM_distsummary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Prey Territory With Memory, Average prey spacing vs predator number, post 3000 ticks"
  ) +
  theme_classic()

##Calc Networks and Components##

PYTMd1 <- pairDist(pytm1Long, 1)
PYTMd2 <- pairDist(pytm2Long, 2)
PYTMd3 <- pairDist(pytm3Long, 3)
PYTMd4 <- pairDist(pytm4Long, 4)

PYTM_allDist <- bind_rows(PYTMd1, PYTMd2, PYTMd3, PYTMd4)

PYTM_ticks_all <- sort(unique(PYTM_allDist$tick))
PYTM_pred_levels <- sort(unique(PYTM_allDist$num_predators))

PYTM_all_networks <- list()

for(pred in PYTM_pred_levels) {
  Distdf_pred <- PYTM_allDist |> filter(num_predators == pred)
  Coorddf_pred <- PYTM_all |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  PYTM_all_networks[[as.character(pred)]] <- networks_pred
}

PYTM_allCompSum <- lapply(names(PYTM_all_networks), function(pred) {
  comp_summary <- compSum(PYTM_all_networks[[pred]]) |>
    mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators

PYTM_groupSize3000 <- PYTM_allCompSum|>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

ggplot(PYTM_groupSize3000, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "Prey Territory With Memory, Effect of predator number on Group Size, post 3000 ticks"
  ) +
  theme_minimal()
#Number of groups as changing with number of predators
PYTM_allNumComp <- PYTM_allCompSum |>
  filter(tick >= 3000) |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

ggplot(PYTM_allNumComp, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Components",
    title = "Prey Territory With Memory, Effect of predator number on Number of Groups, post 3000 ticks") +
  theme_minimal()

##Heatmaps Based on Number of Prey on a Patch##
PYTM_patch1 <- read.csv(file.choose(), header = TRUE)
PYTM_patch2 <- read.csv(file.choose(), header = TRUE)
PYTM_patch3 <- read.csv(file.choose(), header = TRUE)
PYTM_patch4 <- read.csv(file.choose(), header = TRUE)

PYTM_patch1Maps <- heatMap(df = PYTM_patch1, numPred = 1, titleText = "Prey Territory with Memory Prey Density, 1 Predator")
PYTM_patch1Maps$grid
PYTM_patch1Maps$smooth
PYTM_patch1Maps$preyDes

PYTM_patch2Maps <- heatMap(df = PYTM_patch2, numPred = 2, titleText = "Prey Territory with Memory Prey Density, 2 Predators")
PYTM_patch2Maps$grid
PYTM_patch2Maps$smooth
PYTM_patch2Maps$preyDes

PYTM_patch3Maps <- heatMap(df = PYTM_patch3, numPred = 3, titleText = "Prey Territory with Memory Prey Density, 3 Predators")
PYTM_patch3Maps$grid
PYTM_patch3Maps$smooth
PYTM_patch3Maps$preyDes

PYTM_patch4Maps <- heatMap(df = PYTM_patch4, numPred = 4, titleText = "Prey Territory with Memory Prey Density, 4 Predators")
PYTM_patch4Maps$grid
PYTM_patch4Maps$smooth
PYTM_patch4Maps$preyDes


##############################
###Prey Territory No Memory###
##############################

##Basic Stats for Prey Distances Due to Predators##
pytnm1 <- read.csv(file.choose(), header = TRUE)
pytnm1 <- pytnm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytnm2 <- read.csv(file.choose(), header = TRUE)
pytnm2 <- pytnm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytnm3 <- read.csv(file.choose(), header = TRUE)
pytnm3 <- pytnm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytnm4 <- read.csv(file.choose(), header = TRUE)
pytnm4 <- pytnm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))

pytnm1Long <- piv(pytnm1, 1)
pytnm2Long <- piv(pytnm2, 2)
pytnm3Long <- piv(pytnm3, 3)
pytnm4Long <- piv(pytnm4, 4)

PYTNM_all <- bind_rows(pytnm1Long, pytnm2Long, pytnm3Long, pytnm4Long)

PYTNM_meanDist <- meanDist(PYTNM_all)

ggplot(PYTNM_meanDist, aes(x = tick, y = mean_distance, color = factor(num_predators))) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Tick",
    y = "Mean inter-prey distance",
    title = "Prey Territory No Memory, Effect of predator number on prey density"
  ) +
  theme_minimal()

PYTNM_distsummary <- PYTNM_meanDist |>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarize(avg_distance = mean(mean_distance, na.rm = TRUE))

ggplot(PYTNM_distsummary, aes(x = as.factor(num_predators), y = avg_distance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Numb of Predators",
    y = "Average inter-prey distance",
    title = "Prey Territory No Memory, Average prey spacing vs predator number, post 3000 ticks"
  ) +
  theme_classic()

##Calc Networks and Components##

PYTNMd1 <- pairDist(pytnm1Long, 1)
PYTNMd2 <- pairDist(pytnm2Long, 2)
PYTNMd3 <- pairDist(pytnm3Long, 3)
PYTNMd4 <- pairDist(pytnm4Long, 4)

PYTNM_allDist <- bind_rows(PYTNMd1, PYTNMd2, PYTNMd3, PYTNMd4)

PYTNM_ticks_all <- sort(unique(PYTNM_allDist$tick))
PYTNM_pred_levels <- sort(unique(PYTNM_allDist$num_predators))

PYTNM_all_networks <- list()

for(pred in PYTNM_pred_levels) {
  Distdf_pred <- PYTNM_allDist |> filter(num_predators == pred)
  Coorddf_pred <- PYTNM_all |> filter(num_predators == pred)
  
  ticks_all <- sort(unique(Distdf_pred$tick))
  
  networks_pred <- lapply(ticks_all, function(t) buildNetwork(Distdf_pred, Coorddf_pred, t, threshold = 5))
  
  PYTNM_all_networks[[as.character(pred)]] <- networks_pred
}

PYTNM_allCompSum <- lapply(names(PYTNM_all_networks), function(pred) {
  comp_summary <- compSum(PYTNM_all_networks[[pred]]) |>
    mutate(num_predators = as.numeric(pred))
  return(comp_summary)
}) |> bind_rows()

#Group sizes changing with number of predators

PYTNM_groupSize3000 <- PYTNM_allCompSum|>
  filter(tick >= 3000) |>
  group_by(tick, num_predators) |>
  summarise(mGroupSize = mean(n_individuals), .groups = "drop")

ggplot(PYTNM_groupSize3000, aes(x = as.factor(num_predators), y = mGroupSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Mean Group Size",
    title = "Prey Territory No Memory, Effect of predator number on Group Size, post 3000 ticks"
  ) +
  theme_minimal()
#Number of groups as changing with number of predators
PYTNM_allNumComp <- PYTNM_allCompSum |>
  filter(tick >= 3000) |>
  group_by(num_predators, tick) |>
  summarise(n_components = n(), .groups = "drop")

ggplot(PYTNM_allNumComp, aes(x = as.factor(num_predators), y = n_components)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  ylim(0, NA) +
  labs(
    x = "Predators",
    y = "Components",
    title = "Prey Territory No Memory, Effect of predator number on Number of Groups, post 3000 ticks") +
  theme_minimal()

##Heatmaps Based on Number of Prey on a Patch##
PYTNM_patch1 <- read.csv(file.choose(), header = TRUE)
PYTNM_patch2 <- read.csv(file.choose(), header = TRUE)
PYTNM_patch3 <- read.csv(file.choose(), header = TRUE)
PYTNM_patch4 <- read.csv(file.choose(), header = TRUE)

PYTNM_patch1Maps <- heatMap(df = PYTNM_patch1, numPred = 1, titleText = "Prey Territory No Memory Prey Density, 1 Predator")
PYTNM_patch1Maps$grid
PYTNM_patch1Maps$smooth
PYTNM_patch1Maps$preyDes

PYTNM_patch2Maps <- heatMap(df = PYTNM_patch2, numPred = 2, titleText = "Prey Territory No Memory Prey Density, 2 Predators")
PYTNM_patch2Maps$grid
PYTNM_patch2Maps$smooth
PYTNM_patch2Maps$preyDes

PYTNM_patch3Maps <- heatMap(df = PYTNM_patch3, numPred = 3, titleText = "Prey Territory No Memory Prey Density, 3 Predators")
PYTNM_patch3Maps$grid
PYTNM_patch3Maps$smooth
PYTNM_patch3Maps$preyDes

PYTNM_patch4Maps <- heatMap(df = PYTNM_patch4, numPred = 4, titleText = "Prey Territory No Memory Prey Density, 4 Predators")
PYTNM_patch4Maps$grid
PYTNM_patch4Maps$smooth
PYTNM_patch4Maps$preyDes
