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
pytm1 <- read.csv(file.choose(), header = TRUE)
pytm1 <- pytm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

pytm2 <- read.csv(file.choose(), header = TRUE)
pytm2 <- pytm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

pytm3 <- read.csv(file.choose(), header = TRUE)
pytm3 <- pytm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

pytm4 <- read.csv(file.choose(), header = TRUE)
pytm4 <- pytm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

#Prey Terr No Mem
pytnm1 <- read.csv(file.choose(), header = TRUE)
pytnm1 <- pytnm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

pytnm2 <- read.csv(file.choose(), header = TRUE)
pytnm2 <- pytnm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

pytnm3 <- read.csv(file.choose(), header = TRUE)
pytnm3 <- pytnm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

pytnm4 <- read.csv(file.choose(), header = TRUE)
pytnm4 <- pytnm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

#Prey Terr Shared Mem
pytsm1 <- read.csv(file.choose(), header = TRUE)
pytsm1 <- pytsm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

pytsm2 <- read.csv(file.choose(), header = TRUE)
pytsm2 <- pytsm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

pytsm3 <- read.csv(file.choose(), header = TRUE)
pytsm3 <- pytsm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

pytsm4 <- read.csv(file.choose(), header = TRUE)
pytsm4 <- pytsm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

####################################
#Step 2: Pairwise Distance Analysis#
####################################

#Prey Terr Mem
PYTMd1 <- pairDist(pytm1, 1)
PYTMd2 <- pairDist(pytm2, 2)
PYTMd3 <- pairDist(pytm3, 3)
PYTMd4 <- pairDist(pytm4, 4)

#Prey Terr No Mem
PYTNMd1 <- pairDist(pytnm1, 1)
PYTNMd2 <- pairDist(pytnm2, 2)
PYTNMd3 <- pairDist(pytnm3, 3)
PYTNMd4 <- pairDist(pytnm4, 4)

#Prey Terr Shared Mem
PYTSMd1 <- pairDist(pytsm1, 1)
PYTSMd2 <- pairDist(pytsm2, 2)
PYTSMd3 <- pairDist(pytsm3, 3)
PYTSMd4 <- pairDist(pytsm4, 4)

########################################################
#Step 3: Components Analysis: Number of Components######
#Size of Components, Distance Between Component Members#
########################################################

############################
#Step 4: Space Use Analysis#
############################
