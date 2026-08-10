#Prey Territory Code
library(tidyverse)
library(ggplot2)
library(lme4)
library(sf)
library(igraph)

##########################################
#Step 1: Load in data from all variations#
##########################################

#Prey Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\preyCoords\\p1"
pytm1 <- fileRead(path, numPred = 1, type = "coords")
pytm1 <- piv(pytm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\preyCoords\\p2"
pytm2 <- fileRead(path, numPred = 2, type = "coords")
pytm2 <- piv(pytm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\preyCoords\\p3"
pytm3 <- fileRead(path, numPred = 3, type = "coords")
pytm3 <- piv(pytm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\preyCoords\\p4"
pytm4 <- fileRead(path, numPred = 4, type = "coords")
pytm4 <- piv(pytm4, 4)

#Prey Terr Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemoryPredMemory\\preyCoords\\p1"
pytmpm1 <- fileRead(path, numPred = 1, type = "coords")
pytmpm1 <- piv(pytmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemoryPredMemory\\preyCoords\\p2"
pytmpm2 <- fileRead(path, numPred = 2, type = "coords")
pytmpm2 <- piv(pytmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemoryPredMemory\\preyCoords\\p3"
pytmpm3 <- fileRead(path, numPred = 3, type = "coords")
pytmpm3 <- piv(pytmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemoryPredMemory\\preyCoords\\p4"
pytmpm4 <- fileRead(path, numPred = 4, type = "coords")
pytmpm4 <- piv(pytmpm4, 4)

#Prey Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\preyCoords\\p1"
pytnm1 <- fileRead(path, numPred = 1, type = "coords")
pytnm1 <- piv(pytnm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\preyCoords\\p2"
pytnm2 <- fileRead(path, numPred = 2, type = "coords")
pytnm2 <- piv(pytnm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\preyCoords\\p3"
pytnm3 <- fileRead(path, numPred = 3, type = "coords")
pytnm3 <- piv(pytnm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\preyCoords\\p4"
pytnm4 <- fileRead(path, numPred = 4, type = "coords")
pytnm4 <- piv(pytnm4, 4)

#Prey Terr No Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemoryPredMemory\\preyCoords\\p1"
pytnmpm1 <- fileRead(path, numPred = 1, type = "coords")
pytnmpm1 <- piv(pytnmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemoryPredMemory\\preyCoords\\p2"
pytnmpm2 <- fileRead(path, numPred = 2, type = "coords")
pytnmpm2 <- piv(pytnmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemoryPredMemory\\preyCoords\\p3"
pytnmpm3 <- fileRead(path, numPred = 3, type = "coords")
pytnmpm3 <- piv(pytnmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemoryPredMemory\\preyCoords\\p4"
pytnmpm4 <- fileRead(path, numPred = 4, type = "coords")
pytnmpm4 <- piv(pytnmpm4, 4)

#Prey Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\preyCoords\\p1"
pytsm1 <- fileRead(path, numPred = 1, type = "coords")
pytsm1 <- piv(pytsm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\preyCoords\\p2"
pytsm2 <- fileRead(path, numPred = 2, type = "coords")
pytsm2 <- piv(pytsm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\preyCoords\\p3"
pytsm3 <- fileRead(path, numPred = 3, type = "coords")
pytsm3 <- piv(pytsm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\preyCoords\\p4"
pytsm4 <- fileRead(path, numPred = 4, type = "coords")
pytsm4 <- piv(pytsm4, 4)

#Prey Terr Shared Mem Pred Memory
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemoryPredMemory\\preyCoords\\p1"
pytsmpm1 <- fileRead(path, numPred = 1, type = "coords")
pytsmpm1 <- piv(pytsmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemoryPredMemory\\preyCoords\\p2"
pytsmpm2 <- fileRead(path, numPred = 2, type = "coords")
pytsmpm2 <- piv(pytsmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemoryPredMemory\\preyCoords\\p3"
pytsmpm3 <- fileRead(path, numPred = 3, type = "coords")
pytsmpm3 <- piv(pytsmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemoryPredMemory\\preyCoords\\p4"
pytsmpm4 <- fileRead(path, numPred = 4, type = "coords")
pytsmpm4 <- piv(pytsmpm4, 4)

PYTM_all <- bind_rows(pytm1, pytm2, pytm3, pytm4)
write_csv(PYTM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTM_all.csv")
PYTNM_all <- bind_rows(pytnm1, pytnm2, pytnm3, pytnm4)
write_csv(PYTNM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTNM_all.csv")
PYTSM_all <- bind_rows(pytsm1, pytsm2, pytsm3, pytsm4)
write_csv(PYTSM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTSM_all.csv")


####################################
#Step 2: Pairwise Distance Analysis#
####################################

#Prey Terr Mem
PYTMd1 <- pairDist(pytm1, 1)
PYTMd2 <- pairDist(pytm2, 2)
PYTMd3 <- pairDist(pytm3, 3)
PYTMd4 <- pairDist(pytm4, 4)
PYTM_allDist <- bind_rows(PYTMd1, PYTMd2, PYTMd3, PYTMd4)
write_csv(PYTM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTM_allDist.csv")

#Prey Terr Mem Pred Mem
#PYTMPMd1 <- pairDist(pytmpm1, 1)
#PYTMPMd2 <- pairDist(pytmpm2, 2)
#PYTMPMd3 <- pairDist(pytmpm3, 3)
#PYTMPMd4 <- pairDist(pytmpm4, 4)
#PYTMPM_allDist <- bind_rows(PYTMPMd1, PYTMPMd2, PYTMPMd3, PYTMPMd4)
#write_csv(PYTMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTMPM_allDist.csv")

#Prey Terr No Mem
PYTNMd1 <- pairDist(pytnm1, 1)
PYTNMd2 <- pairDist(pytnm2, 2)
PYTNMd3 <- pairDist(pytnm3, 3)
PYTNMd4 <- pairDist(pytnm4, 4)
PYTNM_allDist <- bind_rows(PYTNMd1, PYTNMd2, PYTNMd3, PYTNMd4)
write_csv(PYTNM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTNM_allDist.csv")

#Prey Terr No Mem Pred Mem
#PYTNMPMd1 <- pairDist(pytnmpm1, 1)
#PYTNMPMd2 <- pairDist(pytnmpm2, 2)
#PYTNMPMd3 <- pairDist(pytnmpm3, 3)
#PYTNMPMd4 <- pairDist(pytnmpm4, 4)
#PYTNMPM_allDist <- bind_rows(PYTNMPMd1, PYTNMPMd2, PYTNMPMd3, PYTNMPMd4)
#write_csv(PYTNMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTNMPM_allDist.csv")

#Prey Terr Shared Mem
PYTSMd1 <- pairDist(pytsm1, 1)
PYTSMd2 <- pairDist(pytsm2, 2)
PYTSMd3 <- pairDist(pytsm3, 3)
PYTSMd4 <- pairDist(pytsm4, 4)
PYTSM_allDist <- bind_rows(PYTSMd1, PYTSMd2, PYTSMd3, PYTSMd4)
write_csv(PYTSM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTSM_allDist.csv")

#Prey Terr Shared Mem Pred Mem
#PYTSMPMd1 <- pairDist(pytsmpm1, 1)
#PYTSMPMd2 <- pairDist(pytsmpm2, 2)
#PYTSMPMd3 <- pairDist(pytsmpm3, 3)
#PYTSMPMd4 <- pairDist(pytsmpm4, 4)
#PYTSMPM_allDist <- bind_rows(PYTSMPMd1, PYTSMPMd2, PYTSMPMd3, PYTSMPMd4)
#write_csv(PYTSMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTSMPM_allDist.csv")

########################################################
#Step 3: Components Analysis: Number of Components######
#Size of Components, Distance Between Component Members#
########################################################

#PYTM
PYTM_compDist3 <- PYTM_compDists |>
  filter(tick >= 3000)
ggplot(PYTM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "Prey Territory With Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()
PYTM_compDists <- PYTM_compDists |>
  filter(tick <= 3000)
ggplot(PYTM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Prey Territory With Memory, Inter-Individual Distance From 0-3000 Ticks")

PYTM_compSizes3 <- PYTM_compSizes |>
  filter(tick >= 3000)
ggplot(PYTM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "Prey Territory With Memory, Effect of predator number on group size"
  ) +
  theme_minimal()
PYTM_compSizes <- PYTM_compSizes |>
  filter(tick <= 3000)
ggplot(PYTM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "Prey Territory With Memory, Group Size From 0-3000 Ticks")


PYTM_compSummary3 <- PYTM_compSummary |>
  filter(tick >= 3000)
ggplot(PYTM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "Prey Territory With Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

PYTM_compSummary <- PYTM_compSummary |>
  filter(tick <= 3000)
ggplot(PYTM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "Prey Territory With Memory, Number of Groups From 0-3000 Ticks")

PYTM_compDists <- PYTM_compDists |>
  filter(tick <= 3000)
ggplot(PYTM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Prey Territory With Memory, Inter-Individual Distance From 0-3000 Ticks")

#PYTNM
PYTNM_compDist3 <- PYTNM_compDists |>
  filter(tick >= 3000)
ggplot(PYTNM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "Prey Territory With No Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

PYTNM_compSizes3 <- PYTNM_compSizes |>
  filter(tick >= 3000)
ggplot(PYTNM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "Prey Territory With No Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

PYTNM_compDists <- PYTNM_compDists |>
  filter(tick <= 3000)
ggplot(PYTNM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Prey Territory With No Memory, Inter-Individual Distance From 0-3000 Ticks")
PYTNM_compSizes <- PYTNM_compSizes |>
  filter(tick <= 3000)
ggplot(PYTNM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "Prey Territory With No Memory, Group Size From 0-3000 Ticks")
PYTNM_compSummary <- PYTNM_compSummary |>
  filter(tick <= 3000)
ggplot(PYTNM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "Prey Territory With No Memory, Number of Groups From 0-3000 Ticks")


#PYTSM
PYTSM_compDist3 <- PYTSM_compDists |>
  filter(tick >= 3000)
ggplot(PYTSM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "Prey Territory With Shared Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

PYTSM_compSizes3 <- PYTSM_compSizes |>
  filter(tick >= 3000)
ggplot(PYTSM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "Prey Territory With Shared Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

PYTSM_compSummary3 <- PYTSM_compSummary |>
  filter(tick >= 3000)
ggplot(PYTSM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "Prey Territory With Shared Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

PYTSM_compDists <- PYTSM_compDists |>
  filter(tick <= 3000)
ggplot(PYTSM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Prey Territory With Shared Memory, Inter-Individual Distance From 0-3000 Ticks")
PYTSM_compSizes <- PYTSM_compSizes |>
  filter(tick <= 3000)
ggplot(PYTSM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "Prey Territory With Shared Memory, Group Size From 0-3000 Ticks")
PYTSM_compSummary <- PYTSM_compSummary |>
  filter(tick <= 3000)
ggplot(PYTSM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "Prey Territory With Shared Memory, Number of Groups From 0-3000 Ticks")


############################
#Step 4: Space Use Analysis#
############################

#Prey Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p1"
PYTM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")
PYTM_patch1Maps <- heatMap(df = PYTM_patch1, numPred = 1, titleText = "Prey Territory with Memory Prey Density, 1 Predator")
PYTM_patch1Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p2"
PYTM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")
PYTM_patch2Maps <- heatMap(df = PYTM_patch2, numPred = 2, titleText = "Prey Territory with Memory Prey Density, 2 Predators")
PYTM_patch2Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p3"
PYTM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")
PYTM_patch3Maps <- heatMap(df = PYTM_patch3, numPred = 3, titleText = "Prey Territory with Memory Prey Density, 3 Predators")
PYTM_patch3Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p4"
PYTM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")
PYTM_patch4Maps <- heatMap(df = PYTM_patch4, numPred = 4, titleText = "Prey Territory with Memory Prey Density, 4 Predators")
PYTM_patch4Maps$preyDes

#Prey Terr Mem Pred Mem

#Prey Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p1"
PYTNM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")
PYTNM_patch1Maps <- heatMap(df = PYTNM_patch1, numPred = 1, titleText = "Prey Territory No Memory,  Prey Density, 1 Predator")
PYTNM_patch1Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p2"
PYTNM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")
PYTNM_patch2Maps <- heatMap(df = PYTNM_patch2, numPred = 2, titleText = "Prey Territory No Memory,  Prey Density, 2 Predators")
PYTNM_patch2Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p3"
PYTNM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")
PYTNM_patch3Maps <- heatMap(df = PYTNM_patch3, numPred = 3, titleText = "Prey Territory No Memory,  Prey Density, 3 Predators")
PYTNM_patch3Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p4"
PYTNM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")
PYTNM_patch4Maps <- heatMap(df = PYTNM_patch4, numPred = 4, titleText = "Prey Territory No Memory,  Prey Density, 4 Predators")
PYTNM_patch4Maps$preyDes

#Prey Terr No Mem Pred Mem

#Prey Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\patchCounts\\p1"
PYTSM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")
PYTSM_patch1Maps <- heatMap(df = PYTSM_patch1, numPred = 1, titleText = "Prey Territory with Shared Memory Prey Density, 1 Predator")
PYTSM_patch1Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\patchCounts\\p2"
PYTSM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")
PYTSM_patch2Maps <- heatMap(df = PYTSM_patch2, numPred = 2, titleText = "Prey Territory with Shared Memory Prey Density, 2 Predators")
PYTSM_patch2Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\patchCounts\\p3"
PYTSM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")
PYTSM_patch3Maps <- heatMap(df = PYTSM_patch3, numPred = 3, titleText = "Prey Territory with Shared Memory Prey Density, 3 Predators")
PYTSM_patch3Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p4"
PYTSM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")
PYTSM_patch4Maps <- heatMap(df = PYTSM_patch4, numPred = 4, titleText = "Prey Territory with Shared Memory Prey Density, 4 Predators")
PYTSM_patch4Maps$preyDes

#Prey Terr Shared Mem Pred Mem
