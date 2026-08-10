#Predator Territory Code
library(tidyverse)
library(ggplot2)
library(lme4)
library(sf)
library(igraph)

##########################################
#Step 1: Load in data from all variations#
##########################################

#Pred Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\preyCoords\\p1"
pdtm1 <- fileRead(path, numPred = 1, type = "coords")
pdtm1 <- piv(pdtm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\preyCoords\\p2"
pdtm2 <- fileRead(path, numPred = 2, type = "coords")
pdtm2 <- piv(pdtm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\preyCoords\\p3"
pdtm3 <- fileRead(path, numPred = 3, type = "coords")
pdtm3 <- piv(pdtm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\preyCoords\\p4"
pdtm4 <- fileRead(path, numPred = 4, type = "coords")
pdtm4 <- piv(pdtm4, 4)

#Pred Terr Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemoryPredMemory\\preyCoords\\p1"
pdtmpm1 <- fileRead(path, numPred = 1, type = "coords")
pdtmpm1 <- piv(pdtmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemoryPredMemory\\preyCoords\\p2"
pdtmpm2 <- fileRead(path, numPred = 2, type = "coords")
pdtmpm2 <- piv(pdtmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemoryPredMemory\\preyCoords\\p3"
pdtmpm3 <- fileRead(path, numPred = 3, type = "coords")
pdtmpm3 <- piv(pdtmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemoryPredMemory\\preyCoords\\p4"
pdtmpm4 <- fileRead(path, numPred = 4, type = "coords")
pdtmpm4 <- piv(pdtmpm4, 4)

#Pred Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\preyCoords\\p1"
pdtnm1 <- fileRead(path, numPred = 1, type = "coords")
pdtnm1 <- piv(pdtnm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\preyCoords\\p2"
pdtnm2 <- fileRead(path, numPred = 2, type = "coords")
pdtnm2 <- piv(pdtnm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\preyCoords\\p3"
pdtnm3 <- fileRead(path, numPred = 3, type = "coords")
pdtnm3 <- piv(pdtnm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\preyCoords\\p4"
pdtnm4 <- fileRead(path, numPred = 4, type = "coords")
pdtnm4 <- piv(pdtnm4, 4)

#Pred Terr No Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemoryPredMemory\\preyCoords\\p1"
pdtnmpm1 <- fileRead(path, numPred = 1, type = "coords")
pdtnmpm1 <- piv(pdtnmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemoryPredMemory\\preyCoords\\p2"
pdtnmpm2 <- fileRead(path, numPred = 2, type = "coords")
pdtnmpm2 <- piv(pdtnmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemoryPredMemory\\preyCoords\\p3"
pdtnmpm3 <- fileRead(path, numPred = 3, type = "coords")
pdtnmpm3 <- piv(pdtnmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemoryPredMemory\\preyCoords\\p4"
pdtnmpm4 <- fileRead(path, numPred = 4, type = "coords")
pdtnmpm4 <- piv(pdtnmpm4, 4)

#Pred Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\preyCoords\\p1"
pdtsm1 <- fileRead(path, numPred = 1, type = "coords")
pdtsm1 <- piv(pdtsm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\preyCoords\\p2"
pdtsm2 <- fileRead(path, numPred = 2, type = "coords")
pdtsm2 <- piv(pdtsm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\preyCoords\\p3"
pdtsm3 <- fileRead(path, numPred = 3, type = "coords")
pdtsm3 <- piv(pdtsm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\preyCoords\\p4"
pdtsm4 <- fileRead(path, numPred = 4, type = "coords")
pdtsm4 <- piv(pdtsm4, 4)

#Pred Terr Shared Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemoryPredMemory\\preyCoords\\p1"
pdtsmpm1 <- fileRead(path, numPred = 1, type = "coords")
pdtsmpm1 <- piv(pdtsmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemoryPredMemory\\preyCoords\\p2"
pdtsmpm2 <- fileRead(path, numPred = 2, type = "coords")
pdtsmpm2 <- piv(pdtsmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemoryPredMemory\\preyCoords\\p3"
pdtsmpm3 <- fileRead(path, numPred = 3, type = "coords")
pdtsmpm3 <- piv(pdtsmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemoryPredMemory\\preyCoords\\p4"
pdtsmpm4 <- fileRead(path, numPred = 4, type = "coords")
pdtsmpm4 <- piv(pdtsmpm4, 4)

PDTM_all <- bind_rows(pdtm1, pdtm2, pdtm3, pdtm4)
write_csv(PDTM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTM_all.csv")
PDTNM_all <- bind_rows(pdtnm1, pdtnm2, pdtnm3, pdtnm4)
write_csv(PDTNM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTNM_all.csv")
PDTSM_all <- bind_rows(pdtsm1, pdtsm2, pdtsm3, pdtsm4)
write_csv(PDTSM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTSM_all.csv")


####################################
#Step 2: Pairwise Distance Analysis#
####################################

#Pred Terr Mem
PDTMd1 <- pairDist(pdtm1, 1)
PDTMd2 <- pairDist(pdtm2, 2)
PDTMd3 <- pairDist(pdtm3, 3)
PDTMd4 <- pairDist(pdtm4, 4)
PDTM_allDist <- bind_rows(PDTMd1, PDTMd2, PDTMd3, PDTMd4)
write_csv(PDTM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTM_allDist.csv")

#Pred Terr Mem Pred Mem
#PDTMPMd1 <- pairDist(pdtmpm1, 1)
#PDTMPMd2 <- pairDist(pdtmpm2, 2)
#PDTMPMd3 <- pairDist(pdtmpm3, 3)
#PDTMPMd4 <- pairDist(pdtmpm4, 4)
#PDTMPM_allDist <- bind_rows(PDTMPMd1, PDTMPMd2, PDTMPMd3, PDTMPMd4)
#write_csv(PDTMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTMPM_allDist.csv")

#Pred Terr No Mem
PDTNMd1 <- pairDist(pdtnm1, 1)
PDTNMd2 <- pairDist(pdtnm2, 2)
PDTNMd3 <- pairDist(pdtnm3, 3)
PDTNMd4 <- pairDist(pdtnm4, 4)
PDTNM_allDist <- bind_rows(PDTNMd1, PDTNMd2, PDTNMd3, PDTNMd4)
write_csv(PDTNM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTNM_allDist.csv")

#Pred Terr No Mem Pred Mem
#PDTNMPMd1 <- pairDist(pdtnmpm1, 1)
#PDTNMPMd2 <- pairDist(pdtnmpm2, 2)
#PDTNMPMd3 <- pairDist(pdtnmpm3, 3)
#PDTNMPMd4 <- pairDist(pdtnmpm4, 4)
#PDTNMPM_allDist <- bind_rows(PDTNMPMd1, PDTNMPMd2, PDTNMPMd3, PDTNMPMd4)
#write_csv(PDTNMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTNMPM_allDist.csv")

#Pred Terr Shared Mem Pred Mem
PDTSMd1 <- pairDist(pdtsm1, 1)
PDTSMd2 <- pairDist(pdtsm2, 2)
PDTSMd3 <- pairDist(pdtsm3, 3)
PDTSMd4 <- pairDist(pdtsm4, 4)
PDTSM_allDist <- bind_rows(PDTSMd1, PDTSMd2, PDTSMd3, PDTSMd4)
write_csv(PDTSM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTSM_allDist.csv")

#Pred Terr Shared Mem Pred Mem
#PDTSMPMd1 <- pairDist(pdtsmpm1, 1)
#PDTSMPMd2 <- pairDist(pdtsmpm2, 2)
#PDTSMPMd3 <- pairDist(pdtsmpm3, 3)
#PDTSMPMd4 <- pairDist(pdtsmpm4, 4)
#PDTSMPM_allDist <- bind_rows(PDTSMPMd1, PDTSMPMd2, PDTSMPMd3, PDTSMPMd4)
#write_csv(PDTSMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTSMPM_allDist.csv")

########################################################
#Step 3: Components Analysis: Number of Components######
#Size of Components, Distance Between Component Members#
########################################################

#PDTM
PDTM_compDist3 <- PDTM_compDists |>
  filter(tick >= 3000)
ggplot(PDTM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "Predator Territory With Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

PDTM_compSizes3 <- PDTM_compSizes |>
  filter(tick >= 3000)
ggplot(PDTM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "Predator Territory With Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

PDTM_compSummary3 <- PDTM_compSummary |>
  filter(tick >= 3000)
ggplot(PDTM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "Predator Territory With Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

PDTM_compDists <- PDTM_compDists |>
  filter(tick <= 3000)
ggplot(PDTM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Predator Territory With Memory, Inter-Individual Distance From 0-3000 Ticks")
PDTM_compSizes <- PDTM_compSizes |>
  filter(tick <= 3000)
ggplot(PDTM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "Predator Territory With Memory, Group Size From 0-3000 Ticks")
PDTM_compSummary <- PDTM_compSummary |>
  filter(tick <= 3000)
ggplot(PDTM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "Predator Territory With Memory, Number of Groups From 0-3000 Ticks")


#PDTNM
PDTNM_compDist3 <- PDTNM_compDists |>
  filter(tick >= 3000)
ggplot(PDTNM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "Predator Territory With No Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

PDTNM_compSizes3 <- PDTNM_compSizes |>
  filter(tick >= 3000)
ggplot(PDTNM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "Predator Territory With No Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

PDTNM_compSummary3 <- PDTNM_compSummary |>
  filter(tick >= 3000)
ggplot(PDTNM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "Predator Territory With No Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

PDTNM_compDists <- PDTNM_compDists |>
  filter(tick <= 3000)
ggplot(PDTNM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Predator Territory With No Memory, Inter-Individual Distance From 0-3000 Ticks")
PDTNM_compSizes <- PDTNM_compSizes |>
  filter(tick <= 3000)
ggplot(PDTNM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "Predator Territory With No Memory, Group Size From 0-3000 Ticks")
PDTNM_compSummary <- PDTNM_compSummary |>
  filter(tick <= 3000)
ggplot(PDTNM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "Predator Territory With No Memory, Number of Groups From 0-3000 Ticks")

#PDTSM
PDTSM_compDist3 <- PDTSM_compDists |>
  filter(tick >= 3000)
ggplot(PDTSM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "Predator Territory With Shared Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

PDTSM_compSizes3 <- PDTSM_compSizes |>
  filter(tick >= 3000)
ggplot(PDTSM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "Predator Territory With Shared Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

PDTSM_compSummary3 <- PDTSM_compSummary |>
  filter(tick >= 3000)
ggplot(PDTSM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "Predator Territory With Shared Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

PDTSM_compDists <- PDTSM_compDists |>
  filter(tick <= 3000)
ggplot(PDTSM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "Predator Territory With Shared Memory, Inter-Individual Distance From 0-3000 Ticks")
PDTSM_compSizes <- PDTSM_compSizes |>
  filter(tick <= 3000)
ggplot(PDTSM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "Predator Territory With Shared Memory, Group Size From 0-3000 Ticks")
PDTSM_compSummary <- PDTSM_compSummary |>
  filter(tick <= 3000)
ggplot(PDTSM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "Predator Territory With Shared Memory, Number of Groups From 0-3000 Ticks")
############################
#Step 4: Space Use Analysis#
############################

#Pred Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p1"
PDTM_pc1 <- fileRead(path, numPred = 1, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\predTerr\\p1"
PDTM_pT1 <- fileRead(path, numPred = 1, type = "terr")
PDTM_TB1 <- terrBounds(PDTM_pT1, wWidth = 100, wHeight = 100)
PDTM_hm1 <- heatMapPredTerr(PDTM_pc1, numPred = 1, terrBoundsObj = PDTM_TB1, titleText = "Predator Territory With Memory, Prey Density, 1 Predator")
PDTM_hm1$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p2"
PDTM_pc2 <- fileRead(path, numPred = 2, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\predTerr\\p2"
PDTM_pT2 <- fileRead(path, numPred = 2, type = "terr")
PDTM_TB2 <- terrBounds(PDTM_pT2, wWidth = 100, wHeight = 100)
PDTM_hm2 <- heatMapPredTerr(PDTM_pc2, numPred = 2, terrBoundsObj = PDTM_TB2, titleText = "Predator Territory With Memory, Prey Density, 2 Predators")
PDTM_hm2$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p3"
PDTM_pc3 <- fileRead(path, numPred = 3, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\predTerr\\p3"
PDTM_pT3 <- fileRead(path, numPred = 3, type = "terr")
PDTM_TB3 <- terrBounds(PDTM_pT3, wWidth = 100, wHeight = 100)
PDTM_hm3 <- heatMapPredTerr(PDTM_pc3, numPred = 3, terrBoundsObj = PDTM_TB3, titleText = "Predator Territory With Memory, Prey Density, 3 Predators")
PDTM_hm3$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p4"
PDTM_pc4 <- fileRead(path, numPred = 4, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\predTerr\\p4"
PDTM_pT4 <- fileRead(path, numPred = 4, type = "terr")
PDTM_TB4 <- terrBounds(PDTM_pT4, wWidth = 100, wHeight = 100)
PDTM_hm4 <- heatMapPredTerr(PDTM_pc4, numPred = 4, terrBoundsObj = PDTM_TB4, titleText = "Predator Territory With Memory, Prey Density, 4 Predators")
PDTM_hm4$relative

#Pred Terr Mem Pred Mem

#Pred Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p1"
PDTNM_pc1 <- fileRead(path, numPred = 1, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\predTerr\\p1"
PDTNM_pT1 <- fileRead(path, numPred = 1, type = "terr")
PDTNM_TB1 <- terrBounds(PDTNM_pT1, wWidth = 100, wHeight = 100)
PDTNM_hm1 <- heatMapPredTerr(PDTNM_pc1, numPred = 1, terrBoundsObj = PDTNM_TB1, titleText = "Predator Territory No Memory, Prey Density, 1 Predator")
PDTNM_hm1$relative
PDTNM_hm1$raw

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p2"
PDTNM_pc2 <- fileRead(path, numPred = 2, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\predTerr\\p2"
PDTNM_pT2 <- fileRead(path, numPred = 2, type = "terr")
PDTNM_TB2 <- terrBounds(PDTNM_pT2, wWidth = 100, wHeight = 100)
PDTNM_hm2 <- heatMapPredTerr(PDTNM_pc2, numPred = 2, terrBoundsObj = PDTNM_TB2, titleText = "Predator Territory No Memory, Prey Density, 2 Predators")
PDTNM_hm2$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p3"
PDTNM_pc3 <- fileRead(path, numPred = 3, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\predTerr\\p3"
PDTNM_pT3 <- fileRead(path, numPred = 3, type = "terr")
PDTNM_TB3 <- terrBounds(PDTNM_pT3, wWidth = 100, wHeight = 100)
PDTNM_hm3 <- heatMapPredTerr(PDTNM_pc3, numPred = 3, terrBoundsObj = PDTNM_TB3, titleText = "Predator Territory No Memory, Prey Density, 3 Predators")
PDTNM_hm3$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p4"
PDTNM_pc4 <- fileRead(path, numPred = 4, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\predTerr\\p4"
PDTNM_pT4 <- fileRead(path, numPred = 4, type = "terr")
PDTNM_TB4 <- terrBounds(PDTNM_pT4, wWidth = 100, wHeight = 100)
PDTNM_hm4 <- heatMapPredTerr(PDTNM_pc4, numPred = 4, terrBoundsObj = PDTNM_TB4, titleText = "Predator Territory No Memory, Prey Density, 4 Predators")
PDTNM_hm4$relative

x <- bind_rows(PDTNM_pc1, PDTNM_pc2, PDTNM_pc3, PDTNM_pc4)
x <- x |>
  mutate(terr = "Pred", mem = "None") |>
  filter(tick == 5000)
#Pred Terr No Mem Pred Mem

#Pred Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p1"
PDTSM_pc1 <- fileRead(path, numPred = 1, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\predTerr\\p1"
PDTSM_pT1 <- fileRead(path, numPred = 1, type = "terr")
PDTSM_TB1 <- terrBounds(PDTSM_pT1, wWidth = 100, wHeight = 100)
PDTSM_hm1 <- heatMapPredTerr(PDTSM_pc1, numPred = 1, terrBoundsObj = PDTSM_TB1, titleText = "Predator Territory With Shared Memory, Prey Density, 1 Predator")
PDTSM_hm1$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p2"
PDTSM_pc2 <- fileRead(path, numPred = 2, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\predTerr\\p2"
PDTSM_pT2 <- fileRead(path, numPred = 2, type = "terr")
PDTSM_TB2 <- terrBounds(PDTSM_pT2, wWidth = 100, wHeight = 100)
PDTSM_hm2 <- heatMapPredTerr(PDTSM_pc2, numPred = 2, terrBoundsObj = PDTSM_TB2, titleText = "Predator Territory With Shared Memory, Prey Density, 2 Predators")
PDTSM_hm2$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p3"
PDTSM_pc3 <- fileRead(path, numPred = 3, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\predTerr\\p3"
PDTSM_pT3 <- fileRead(path, numPred = 3, type = "terr")
PDTSM_TB3 <- terrBounds(PDTSM_pT3, wWidth = 100, wHeight = 100)
PDTSM_hm3 <- heatMapPredTerr(PDTSM_pc3, numPred = 3, terrBoundsObj = PDTSM_TB3, titleText = "Predator Territory With Shared Memory, Prey Density, 3 Predators")
PDTSM_hm3$relative

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p4"
PDTSM_pc4 <- fileRead(path, numPred = 4, type = "patchCount")
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\predTerr\\p4"
PDTSM_pT4 <- fileRead(path, numPred = 4, type = "terr")
PDTSM_TB4 <- terrBounds(PDTSM_pT4, wWidth = 100, wHeight = 100)
PDTSM_hm4 <- heatMapPredTerr(PDTSM_pc4, numPred = 4, terrBoundsObj = PDTSM_TB4, titleText = "Predator Territory With Shared Memory, Prey Density, 4 Predators")
PDTSM_hm4$relative
x <- bind_rows(PDTSM_pc1, PDTSM_pc2, PDTSM_pc3, PDTSM_pc4)
x <- x |>
  filter(tick == 5000)
#Pred Terr Shared Mem Pred Mem

#######################################
##Step 5: Number of Prey in Pred Terr##
#######################################
detach("package:igraph", unload = TRUE)

#Pred Terr Mem
#seeing if and when prey are in territory, use preyInTerr()
PDTM_pP1In <- preyInTerr(pdtm1, PDTM_TB1$raw, 3000)
PDTM_pP2In <- preyInTerr(pdtm2, PDTM_TB2$raw, 3000)
PDTM_pP3In <- preyInTerr(pdtm3, PDTM_TB3$raw, 3000)
PDTM_pP4In <- preyInTerr(pdtm4, PDTM_TB4$raw, 3000)

#summaries when prey were in predator territory, use preyInTerrSum()
PDTM_pP1InSum <- preyInTerrSum(PDTM_pP1In)
PDTM_pP2InSum <- preyInTerrSum(PDTM_pP2In)
PDTM_pP3InSum <- preyInTerrSum(PDTM_pP3In)
PDTM_pP4InSum <- preyInTerrSum(PDTM_pP4In)

#Time in pred teritory, use predInTerrTime()
PDTM_pP1T <- predInTerrTime(pdtm1, PDTM_TB1$raw)
PDTM_pP2T <- predInTerrTime(pdtm2, PDTM_TB2$raw)
PDTM_pP3T <- predInTerrTime(pdtm3, PDTM_TB3$raw)
PDTM_pP4T <- predInTerrTime(pdtm4, PDTM_TB4$raw)

#Pred Terr Mem Pred Mem

#Pred Terr No Mem
#seeing if and when prey are in territory, use preyInTerr()
PDTNM_pP1In <- preyInTerr(pdtnm1, PDTNM_TB1$raw, 3000)
PDTNM_pP2In <- preyInTerr(pdtnm2, PDTNM_TB2$raw, 3000)
PDTNM_pP3In <- preyInTerr(pdtnm3, PDTNM_TB3$raw, 3000)
PDTNM_pP4In <- preyInTerr(pdtnm4, PDTNM_TB4$raw, 3000)

#summaries when prey were in predator territory, use preyInTerrSum()
PDTNM_pP1InSum <- preyInTerrSum(PDTNM_pP1In)
PDTNM_pP2InSum <- preyInTerrSum(PDTNM_pP2In)
PDTNM_pP3InSum <- preyInTerrSum(PDTNM_pP3In)
PDTNM_pP4InSum <- preyInTerrSum(PDTNM_pP4In)

#Time in pred teritory, use predInTerrTime()
PDTNM_pP1T <- predInTerrTime(pdtnm1, PDTNM_TB1$raw)
PDTNM_pP2T <- predInTerrTime(pdtnm2, PDTNM_TB2$raw)
PDTNM_pP3T <- predInTerrTime(pdtnm3, PDTNM_TB3$raw)
PDTNM_pP4T <- predInTerrTime(pdtnm4, PDTNM_TB4$raw)


#Pred Terr No Mem Pred Mem

#Pred Terr Shared Mem
#seeing if and when prey are in territory, use preyInTerr()
PDTSM_pP1In <- preyInTerr(pdtsm1, PDTSM_TB1$raw, 3000)
PDTSM_pP2In <- preyInTerr(pdtsm2, PDTSM_TB2$raw, 3000)
PDTSM_pP3In <- preyInTerr(pdtsm3, PDTSM_TB3$raw, 3000)
PDTSM_pP4In <- preyInTerr(pdtsm4, PDTSM_TB4$raw, 3000)

#summaries when prey were in predator territory, use preyInTerrSum()
PDTSM_pP1InSum <- preyInTerrSum(PDTSM_pP1In)
PDTSM_pP2InSum <- preyInTerrSum(PDTSM_pP2In)
PDTSM_pP3InSum <- preyInTerrSum(PDTSM_pP3In)
PDTSM_pP4InSum <- preyInTerrSum(PDTSM_pP4In)

#Time in pred teritory, use predInTerrTime()
PDTSM_pP1T <- predInTerrTime(pdtsm1, PDTSM_TB1$raw)
PDTSM_pP2T <- predInTerrTime(pdtsm2, PDTSM_TB2$raw)
PDTSM_pP3T <- predInTerrTime(pdtsm3, PDTSM_TB3$raw)
PDTSM_pP4T <- predInTerrTime(pdtsm4, PDTSM_TB4$raw)

#Pred Terr Shared Mem Pred Mem