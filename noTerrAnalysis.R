#No Territory No Memory Code
library(tidyverse)
library(ggplot2)
library(lme4)
library(sf)
library(igraph)

##########################################
#Step 1: Load in data from all variations#
##########################################

#No Terr Mem
ntm1 <- read.csv(file.choose(), header = TRUE)
ntm1 <- ntm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

ntm2 <- read.csv(file.choose(), header = TRUE)
ntm2 <- ntm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

ntm3 <- read.csv(file.choose(), header = TRUE)
ntm3 <- ntm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

ntm4 <- read.csv(file.choose(), header = TRUE)
ntm4 <- ntm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

#No Terr No Mem
ntnm1 <- read.csv(file.choose(), header = TRUE)
ntnm1 <- ntnm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

ntnm2 <- read.csv(file.choose(), header = TRUE)
ntnm2 <- ntnm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

ntnm3 <- read.csv(file.choose(), header = TRUE)
ntnm3 <- ntnm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

ntnm4 <- read.csv(file.choose(), header = TRUE)
ntnm4 <- ntnm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

#No Terr Shared Mem
ntsm1 <- read.csv(file.choose(), header = TRUE)
ntsm1 <- ntsm1 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 1)

ntsm2 <- read.csv(file.choose(), header = TRUE)
ntsm2 <- ntsm2 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 2)

ntsm3 <- read.csv(file.choose(), header = TRUE)
ntsm3 <- ntsm3 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 3)

ntsm4 <- read.csv(file.choose(), header = TRUE)
ntsm4 <- ntsm4 |>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  split_behaviorspace_runs("^id\t0")|>
  rename_with(~ str_remove(.x, "^X"), starts_with("X")) |>
  piv(predNum = 4)

####################################
#Step 2: Pairwise Distance Analysis#
####################################

#No Terr Mem
NTMd1 <- pairDist(ntm1, 1)
NTMd2 <- pairDist(ntm2, 2)
NTMd3 <- pairDist(ntm3, 3)
NTMd4 <- pairDist(ntm4, 4)

#No Terr No Mem
NTNMd1 <- pairDist(ntnm1, 1)
NTNMd2 <- pairDist(ntnm2, 2)
NTNMd3 <- pairDist(ntnm3, 3)
NTNMd4 <- pairDist(ntnm4, 4)

#No Terr Shared Mem
NTSMd1 <- pairDist(ntsm1, 1)
NTSMd2 <- pairDist(ntsm2, 2)
NTSMd3 <- pairDist(ntsm3, 3)
NTSMd4 <- pairDist(ntsm4, 4)

########################################################
#Step 3: Components Analysis: Number of Components######
#Size of Components, Distance Between Component Members#
########################################################

############################
#Step 4: Space Use Analysis#
############################

