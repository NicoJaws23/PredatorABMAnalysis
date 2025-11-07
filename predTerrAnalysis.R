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
pdtm1 <- read.csv(file.choose(), header = TRUE)
pdtm1 <- pdtm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

pdtm2 <- read.csv(file.choose(), header = TRUE)
pdtm2 <- pdtm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

pdtm3 <- read.csv(file.choose(), header = TRUE)
pdtm3 <- pdtm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

pdtm4 <- read.csv(file.choose(), header = TRUE)
pdtm4 <- pdtm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

#Pred Terr No Mem
pdtnm1 <- read.csv(file.choose(), header = TRUE)
pdtnm1 <- pdtnm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

pdtnm2 <- read.csv(file.choose(), header = TRUE)
pdtnm2 <- pdtnm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

pdtnm3 <- read.csv(file.choose(), header = TRUE)
pdtnm3 <- pdtnm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

pdtnm4 <- read.csv(file.choose(), header = TRUE)
pdtnm4 <- pdtnm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

#Pred Terr Shared Mem
pdtsm1 <- read.csv(file.choose(), header = TRUE)
pdtsm1 <- pdtsm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

pdtsm2 <- read.csv(file.choose(), header = TRUE)
pdtsm2 <- pdtsm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

pdtsm3 <- read.csv(file.choose(), header = TRUE)
pdtsm3 <- pdtsm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

pdtsm4 <- read.csv(file.choose(), header = TRUE)
pdtsm4 <- pdtsm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

####################################
#Step 2: Pairwise Distance Analysis#
####################################

#Pred Terr Mem
PDTMd1 <- pairDist(pdtm1, 1)
PDTMd2 <- pairDist(pdtm2, 2)
PDTMd3 <- pairDist(pdtm3, 3)
PDTMd4 <- pairDist(pdtm4, 4)

#Pred Terr No Mem
#PDTNMd1 <- pairDist(pdtnm1, 1)
PDTNMd2 <- pairDist(pdtnm2, 2)
PDTNMd3 <- pairDist(pdtnm3, 3)
PDTNMd4 <- pairDist(pdtnm4, 4)

#Pred Terr Shared Mem
PDTSMd1 <- pairDist(pdtsm1, 1)
PDTSMd2 <- pairDist(pdtsm2, 2)
PDTSMd3 <- pairDist(pdtsm3, 3)
PDTSMd4 <- pairDist(pdtsm4, 4)

########################################################
#Step 3: Components Analysis: Number of Components######
#Size of Components, Distance Between Component Members#
########################################################

############################
#Step 4: Space Use Analysis#
############################

