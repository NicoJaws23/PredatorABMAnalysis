#Analyzing data from territory ABM
library(tidyverse)
library(ggplot2)
library(lme4)
library(sf)

################################################################################
################################################################################
################################################################################
#Measure distances between prey when predator have territory and prey move freely
#across the environment, Model 3

pP1 <- read.csv(file.choose(), header = TRUE)
pP1 <- pP1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
pC1 <- read.csv(file.choose(), header = TRUE)
pT1 <- read.csv(file.choose(), header = TRUE)

pP2 <- read.csv(file.choose(), header = TRUE)
pP2 <- pP2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
pC2 <- read.csv(file.choose(), header = TRUE)
pT2 <- read.csv(file.choose(), header = TRUE)

pP3 <- read.csv(file.choose(), header = TRUE)
pP3 <- pP3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
pC3 <- read.csv(file.choose(), header = TRUE)
pT3 <- read.csv(file.choose(), header = TRUE)

pP4 <- read.csv(file.choose(), header = TRUE)
pP4 <- pP4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X"))
pC4 <- read.csv(file.choose(), header = TRUE)
pT4 <- read.csv(file.choose(), header = TRUE)
#pivot table, use piv(df) function
pP1Long <- piv(pP1, 1)
pP2Long <- piv(pP2, 2)
pP3Long <- piv(pP3, 3)
pP4Long <- piv(pP4, 4)


#determine distance between individuals, use pairDist() function
pP1Dist <- pairDist(pP1Long, 1)
pP2Dist <- pairDist(pP1Long, 2)
pP3Dist <- pairDist(pP1Long, 3)
pP4Dist <- pairDist(pP1Long, 4)


#Number of times prey went into predator territory, use terrBounds() to get
#territory boundary data
TB1 <- terrBounds(pT1, wWidth = 100, wHeight = 100)
TB2 <- terrBounds(pT2, wWidth = 100, wHeight = 100)
TB3 <- terrBounds(pT3, wWidth = 100, wHeight = 100)
TB4 <- terrBounds(pT4, wWidth = 100, wHeight = 100)

#seeing if and when prey are in territory, use preyInTerr()
detach("package:igraph", unload = TRUE)

pP1In <- preyInTerr(pP1Long, TB1$raw, 3000)
pP2In <- preyInTerr(pP2Long, TB2$raw, 3000)
pP3In <- preyInTerr(pP3Long, TB3$raw, 3000)
pP4In <- preyInTerr(pP4Long, TB4$raw, 3000)

#summaries when prey were in predator territory, use preyInTerrSum()
pP1InSum <- preyInTerrSum(pP1In)
pP2InSum <- preyInTerrSum(pP2In)
pP3InSum <- preyInTerrSum(pP3In)
pP4InSum <- preyInTerrSum(pP4In)

#Time in pred teritory, use predInTerrTime()
pP1T <- predInTerrTime(pP1Long, TB1$raw)
pP2T <- predInTerrTime(pP2Long, TB2$raw)
pP3T <- predInTerrTime(pP3Long, TB3$raw)
pP4T <- predInTerrTime(pP4Long, TB4$raw)

#plotting heat maps with predator territory
NoMem_hm1 <- heatMapPredTerr(pC1, numPred = 1, terrBoundsObj = TB1, titleText = "Prey Density, 1 Predator, No Memory")
NoMem_hm1$raw
NoMem_hm1$smooth
NoMem_hm1$relative

NoMem_hm2 <- heatMapPredTerr(pC2, numPred = 2, terrBoundsObj = TB2, titleText = "Prey Density, 2 Predators, No Memory")
NoMem_hm2$raw
NoMem_hm2$smooth
NoMem_hm2$relative

NoMem_hm3 <- heatMapPredTerr(pC3, numPred = 3, terrBoundsObj = TB3, titleText = "Prey Density, 3 Predators, No Memory")
NoMem_hm3$raw
NoMem_hm3$smooth
NoMem_hm3$relative

NoMem_hm4 <- heatMapPredTerr(pC4, numPred = 4, terrBoundsObj = TB4, titleText = "Prey Density, 4 Predators, No Memory")
NoMem_hm4$raw
NoMem_hm4$smooth
NoMem_hm4$relative

#with mem 
Mem_hm1 <- heatMapPredTerr(pC1, numPred = 1, terrBoundsObj = TB1, titleText = "Prey Density, 1 Predator, Memory")
Mem_hm1$raw
Mem_hm1$smooth
Mem_hm1$relative

Mem_hm2 <- heatMapPredTerr(pC2, numPred = 2, terrBoundsObj = TB2, titleText = "Prey Density, 2 Predators, Memory")
Mem_hm2$raw
Mem_hm2$smooth
Mem_hm2$relative

Mem_hm3 <- heatMapPredTerr(pC3, numPred = 3, terrBoundsObj = TB3, titleText = "Prey Density, 3 Predators, Memory")
Mem_hm3$raw
Mem_hm3$smooth
Mem_hm3$relative

Mem_hm4 <- heatMapPredTerr(pC4, numPred = 4, terrBoundsObj = TB4, titleText = "Prey Density, 4 Predators, Memory")
Mem_hm4$raw
Mem_hm4$smooth
Mem_hm4$relative

