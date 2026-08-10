#Space use models
library(tidyverse)
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p1"
NTM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p2"
NTM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p3"
NTM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p4"
NTM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")


NTM_pc <- bind_rows(NTM_patch1, NTM_patch2, NTM_patch3, NTM_patch4)
NTM_pc <- NTM_pc |>
  mutate(terr = "None", mem = "Individual")

#No Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p1"
NTNM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p2"
NTNM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p3"
NTNM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p4"
NTNM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")

NTNM_pc <- bind_rows(NTNM_patch1, NTNM_patch2, NTNM_patch3, NTNM_patch4)
NTNM_pc <- NTNM_pc |>
  mutate(terr = "None", mem = "None")

#No Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p1"
NTSM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p2"
NTSM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p3"
NTSM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p4"
NTSM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")

NTSM_pc <- bind_rows(NTSM_patch1, NTSM_patch2, NTSM_patch3, NTSM_patch4)
NTSM_pc <- NTSM_pc |>
  mutate(terr = "None", mem = "Shared")


#Pred Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p1"
PDTM_pc1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p2"
PDTM_pc2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p3"
PDTM_pc3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryMemory\\patchCounts\\p4"
PDTM_pc4 <- fileRead(path, numPred = 4, type = "patchCount")

PDTM_pc <- bind_rows(PDTM_pc1, PDTM_pc2, PDTM_pc3, PDTM_pc4)
PDTM_pc <- PDTM_pc |>
  mutate(terr = "Pred", mem = "Individual")

#Pred Terr Mem Pred Mem

#Pred Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p1"
PDTNM_pc1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p2"
PDTNM_pc2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p3"
PDTNM_pc3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritoryNoMemory\\patchCounts\\p4"
PDTNM_pc4 <- fileRead(path, numPred = 4, type = "patchCount")

PDTNM_pc <- bind_rows(PDTNM_pc1, PDTNM_pc2, PDTNM_pc3, PDTNM_pc4)
PDTNM_pc <- PDTNM_pc |>
  mutate(terr = "Pred", mem = "None")

#Pred Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p1"
PDTSM_pc1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p2"
PDTSM_pc2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p3"
PDTSM_pc3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PredTerritorySharedMemory\\patchCounts\\p4"
PDTSM_pc4 <- fileRead(path, numPred = 4, type = "patchCount")

PDTSM_pc <- bind_rows(PDTSM_pc1, PDTSM_pc2, PDTSM_pc3, PDTSM_pc4)
PDTSM_pc <- PDTSM_pc |>
  mutate(terr = "Pred", mem = "Shared")

#Prey Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p1"
PYTM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p2"
PYTM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p3"
PYTM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryMemory\\patchCounts\\p4"
PYTM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")

PYTM_pc <- bind_rows(PYTM_patch1, PYTM_patch2, PYTM_patch3, PYTM_patch4)
PYTM_pc <- PYTM_pc |>
  mutate(terr = "Prey", mem = "Individual")

#Prey Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p1"
PYTNM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p2"
PYTNM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p3"
PYTNM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritoryNoMemory\\patchCounts\\p4"
PYTNM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")

PYTNM_pc <- bind_rows(PYTNM_patch1, PYTNM_patch2, PYTNM_patch3, PYTNM_patch4)
PYTNM_pc <- PYTNM_pc |>
  mutate(terr = "Prey", mem = "None")

#Prey Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\patchCounts\\p1"
PYTSM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\patchCounts\\p2"
PYTSM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\PreyTerritorySharedMemory\\patchCounts\\p3"
PYTSM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p4"
PYTSM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")

PYTSM_pc <- bind_rows(PYTSM_patch1, PYTSM_patch2, PYTSM_patch3, PYTSM_patch4)
PYTSM_pc <- PYTSM_pc |>
  mutate(terr = "Prey", mem = "Shared")

allCount <- bind_rows(NTM_pc, NTNM_pc, NTSM_pc, PDTM_pc, PDTNM_pc, PDTSM_pc, PYTM_pc, PYTNM_pc, PYTSM_pc)
write_csv(allCount, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//allCount.csv")

####Attemp 1####
grid101 <- expand.grid(
  x = -50:50,
  y = -50:50
)

library(spdep)


library(dplyr)
library(stringr)
library(tidyr)

coords <- grid101[, c("x","y")]

nb <- dnearneigh(coords, d1 = 0, d2 = 1.1)
lw <- nb2listw(nb)

get_full_map <- function(df_one_run) {
  df_one_run %>%
    right_join(grid101, by = c("x","y")) %>%
    mutate(count = replace_na(count, 0)) %>%
    arrange(x, y)
}

compute_moran <- function(df_one_run) {
  fullMap <- get_full_map(df_one_run)
  moran.test(fullMap$count, lw)$estimate["Moran I statistic"]
}



compute_moran_runs <- function(df, terrType, memType) {
  # --- Create grid and neighbor list (constant for all runs) ---
  grid101 <- expand.grid(
    x = -50:50,
    y = -50:50
  )
  
  coords <- grid101[, c("x","y")]
  nb <- dnearneigh(coords, d1 = 0, d2 = 1.1)   # rook neighbors
  lw <- nb2listw(nb)
  
  # --- Extract numeric coordinates from patch-id ---
  df_xy <- df %>%
    mutate(
      patch.id = str_remove_all(`patch-id`, "[()]"),
      x = as.numeric(str_split_fixed(patch.id, ",", 2)[,1]),
      y = as.numeric(str_split_fixed(patch.id, ",", 2)[,2])
    )
  
  # --- Aggregate counts within each unique cell for each run ---
  df_xy_agg <- df_xy %>%
    group_by(behaviorSpaceRun, x, y) %>%
    summarise(count = mean(count), .groups = "drop")
  
  # --- Helper function to create complete map for one run ---
  get_full_map <- function(df_one_run) {
    df_one_run %>%
      right_join(grid101, by = c("x","y")) %>%
      mutate(count = replace_na(count, 0)) %>%
      arrange(x, y)
  }
  
  # --- Helper function to compute Moran's I for one run ---
  compute_moran_single <- function(df_one_run) {
    fullMap <- get_full_map(df_one_run)
    moran.test(fullMap$count, lw)$estimate["Moran I statistic"]
  }
  
  # --- Compute Moran's I for all runs ---
  moran_raw <- df_xy_agg %>%
    group_by(behaviorSpaceRun) %>%
    summarise(
      moranI = compute_moran_single(cur_data()),
      .groups = "drop"
    ) |>
    mutate(terr = terrType, mem = memType,
           numPred = case_when(
             behaviorSpaceRun >= 0 & behaviorSpaceRun <= 50 ~ 1,
             behaviorSpaceRun >= 51 & behaviorSpaceRun <= 100 ~ 2,
             behaviorSpaceRun >= 101 & behaviorSpaceRun <= 150 ~ 3,
             behaviorSpaceRun >= 151 & behaviorSpaceRun <= 200 ~ 4
           )  
           )
  
  return(moran_raw)
}

compute_moran_runs_prey <- function(df, memType) {
  
  # --- Define prey territory grid (50x50 centered at 0,0) ---
  # If your prey territory is exactly -25:24 or -25:25, adjust here
  grid_prey <- expand.grid(
    x = -25:25,
    y = -25:25
  )
  
  # --- Neighbor list (rook adjacency) ---
  coords <- grid_prey[, c("x", "y")]
  nb <- dnearneigh(coords, d1 = 0, d2 = 1.1)
  lw <- nb2listw(nb)
  
  # --- Extract numeric coordinates from patch-id ---
  df_xy <- df %>%
    mutate(
      patch.id = str_remove_all(`patch-id`, "[()]"),
      x = as.numeric(str_split_fixed(patch.id, ",", 2)[, 1]),
      y = as.numeric(str_split_fixed(patch.id, ",", 2)[, 2])
    ) %>%
    # keep only prey territory patches
    filter(x >= -25, x <= 25, y >= -25, y <= 25)
  
  # --- Aggregate counts per patch per run (no spatial aggregation) ---
  df_xy_agg <- df_xy %>%
    group_by(behaviorSpaceRun, x, y) %>%
    summarise(count = mean(count), .groups = "drop")
  
  # --- Helper: complete prey grid for one run ---
  get_full_map <- function(df_one_run) {
    df_one_run %>%
      right_join(grid_prey, by = c("x", "y")) %>%
      mutate(count = replace_na(count, 0)) %>%
      arrange(x, y)
  }
  
  # --- Helper: Moran's I for one run ---
  compute_moran_single <- function(df_one_run) {
    fullMap <- get_full_map(df_one_run)
    vals <- fullMap$count
    
    # avoid failures when variance is zero
    if (var(vals) == 0) return(NA_real_)
    
    as.numeric(moran.test(vals, lw)$estimate["Moran I statistic"])
  }
  
  # --- Compute Moran's I across runs ---
  df_xy_agg %>%
    group_by(behaviorSpaceRun) %>%
    summarise(
      moranI = compute_moran_single(cur_data()),
      .groups = "drop"
    ) %>%
    mutate(
      terr = "Prey",
      mem  = memType,
      numPred = case_when(
        behaviorSpaceRun <= 50  ~ 1,
        behaviorSpaceRun <= 100 ~ 2,
        behaviorSpaceRun <= 150 ~ 3,
        behaviorSpaceRun <= 200 ~ 4
      )
    )
}

NTM <- allCount |>
  filter(terr == "None", mem == "Individual", tick >= 3000)
NTNM <- allCount |>
  filter(terr == "None", mem == "None", tick >= 3000)
NTSM <- allCount |>
  filter(terr == "None", mem == "Shared", tick >= 3000)
PDTM <- allCount |>
  filter(terr == "Pred", mem == "Individual", tick >= 3000)
PDTNM <- allCount |>
  filter(terr == "Pred", mem == "None", tick >= 3000)
PDTSM <- allCount |>
  filter(terr == "Pred", mem == "Shared", tick >= 3000)
PYTM <- allCount |>
  filter(terr == "Prey", mem == "Individual", tick >= 3000)
PYTNM <- allCount |>
  filter(terr == "Prey", mem == "None", tick >= 3000)
PYTSM <- allCount |>
  filter(terr == "Prey", mem == "Shared", tick >= 3000)


NTM_mor1 <- compute_moran_runs(df = NTM, terrType = "None", memType = "Individual")
NTNM_mor1 <- compute_moran_runs(df = NTNM, terrType = "None", memType = "None")
NTSM_mor1 <- compute_moran_runs(df = NTSM, terrType = "None", memType = "Shared")
PDTM_mor1 <- compute_moran_runs(df = PDTM, terrType = "Pred", memType = "Individual")
PDTNM_mor1 <- compute_moran_runs(df = PDTNM, terrType = "Pred", memType = "None")
PDTSM_mor1 <- compute_moran_runs(df = PDTSM, terrType = "Pred", memType = "Shared")
PYTM_mor1 <- compute_moran_runs_prey(df = PYTM, memType = "Individual")
PYTNM_mor1 <- compute_moran_runs_prey(df = PYTNM, memType = "None")
PYTSM_mor1 <- compute_moran_runs_prey(df = PYTSM, memType = "Shared")

allMoranI <- bind_rows(NTM_mor1, NTNM_mor1, NTSM_mor1, PDTM_mor1, PDTNM_mor1, PDTSM_mor1,
                       PYTM_mor1, PYTNM_mor1, PYTSM_mor1)
write_csv(allMoranI, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//allMoranI.csv")
allMoranI <- allMoranI |>
  mutate(logMI = log(moranI))
mM <- lm(moranI ~ as.factor(terr) + as.factor(mem) + as.factor(numPred), data = allMoranI)
summary(mM)
mMem <- lm(moranI ~ as.factor(mem), data = allMoranI)
emm_mem <- emmeans(mMem, ~ mem)
pairs(emm_mem)
standardize_parameters(mMem)
eta_squared(mM, partial = TRUE)

x <- stepAIC(mM, direction = "both")

terr_glht <- glht(mM, linfct = mcp("as.factor(terr)" = "Tukey"))
mem_glht  <- glht(mM, linfct = mcp("as.factor(mem)" = "Tukey"))
num_glht  <- glht(mM, linfct = mcp("as.factor(numPred)" = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory on the Space Use (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for Memory on Space Use (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for Number of Predators on Space Use (Tukey-adjusted)")
gt_num

ggplot(allMoranI, aes(x = factor(numPred), y = moranI)) +
  geom_boxplot() +
  #geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
  facet_grid(
    terr ~ mem,
    labeller = labeller(
      terr = c(
        "None" = "No Territory",
        "Pred" = "Predator Territory",
        "Prey" = "Prey Territory"
      ),
      mem = c(
        "Individual" = "Individual Memory",
        "None" = "No Memory",
        "Shared" = "Shared Memory"
      )
    )
  ) +  # rows = territory, cols = memory
  labs(
    x = "Number of Predators",
    y = "Moran's I",
    title = "Effects on Number of Groups"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )
library(report)
library(MASS)
library(multcomp)
library(igraph)
cite_packages()
####Aggregating cells####


compute_moran_runsAGG <- function(df, terrType, memType, cell_size = 10) {
  
  # --- Create coarse grid ---
  grid_coarse <- expand.grid(
    x_coarse = seq(-50, 40, by = cell_size),
    y_coarse = seq(-50, 40, by = cell_size)
  )
  
  # --- Neighbors on coarse grid ---
  coords <- grid_coarse[, c("x_coarse", "y_coarse")]
  
  nb <- dnearneigh(
    coords,
    d1 = 0,
    d2 = cell_size * 1.1
  )
  
  lw <- nb2listw(nb)
  
  # --- Extract numeric coordinates and assign coarse cells ---
  df_xy <- df %>%
    mutate(
      patch.id = str_remove_all(`patch-id`, "[()]"),
      x = as.numeric(str_split_fixed(patch.id, ",", 2)[,1]),
      y = as.numeric(str_split_fixed(patch.id, ",", 2)[,2]),
      x_coarse = floor(x / cell_size) * cell_size,
      y_coarse = floor(y / cell_size) * cell_size
    )
  
  # --- Aggregate within coarse cells ---
  df_xy_agg <- df_xy %>%
    group_by(behaviorSpaceRun, x_coarse, y_coarse) %>%
    summarise(count = sum(count), .groups = "drop")
  
  # --- Complete map for a single run ---
  get_full_map <- function(df_one_run) {
    df_one_run %>%
      right_join(grid_coarse, by = c("x_coarse", "y_coarse")) %>%
      mutate(count = replace_na(count, 0)) %>%
      arrange(x_coarse, y_coarse)
  }
  
  # --- Moran's I for one run ---
  compute_moran_single <- function(df_one_run) {
    fullMap <- get_full_map(df_one_run)
    moran.test(fullMap$count, lw)$estimate["Moran I statistic"]
  }
  
  # --- Compute Moran's I for all runs ---
  moran_raw <- df_xy_agg %>%
    group_by(behaviorSpaceRun) %>%
    summarise(
      moranI = compute_moran_single(cur_data()),
      .groups = "drop"
    ) %>%
    mutate(
      terr = terrType,
      mem  = memType,
      numPred = case_when(
        behaviorSpaceRun <= 50  ~ 1,
        behaviorSpaceRun <= 100 ~ 2,
        behaviorSpaceRun <= 150 ~ 3,
        behaviorSpaceRun <= 200 ~ 4
      )
    )
  
  moran_raw
}

compute_moran_runsAGG_prey <- function(df, memType, cell_size = 10) {
  
  # --- Prey territory: 50x50, aligned to coarse bins ---
  min_coord <- -20
  max_coord <-  20
  
  grid_coarse <- expand.grid(
    x_coarse = seq(min_coord, max_coord, by = cell_size),
    y_coarse = seq(min_coord, max_coord, by = cell_size)
  )
  
  coords <- grid_coarse[, c("x_coarse", "y_coarse")]
  
  nb <- dnearneigh(
    coords,
    d1 = 0,
    d2 = cell_size * 1.1
  )
  
  lw <- nb2listw(nb)
  
  df_xy <- df %>%
    mutate(
      patch.id = str_remove_all(`patch-id`, "[()]"),
      x = as.numeric(str_split_fixed(patch.id, ",", 2)[,1]),
      y = as.numeric(str_split_fixed(patch.id, ",", 2)[,2]),
      x_coarse = floor(x / cell_size) * cell_size,
      y_coarse = floor(y / cell_size) * cell_size
    )
  
  df_xy_agg <- df_xy %>%
    group_by(behaviorSpaceRun, x_coarse, y_coarse) %>%
    summarise(count = sum(count), .groups = "drop")
  
  get_full_map <- function(df_one_run) {
    df_one_run %>%
      right_join(grid_coarse, by = c("x_coarse", "y_coarse")) %>%
      mutate(count = replace_na(count, 0)) %>%
      arrange(x_coarse, y_coarse)
  }
  
  compute_moran_single <- function(df_one_run) {
    fullMap <- get_full_map(df_one_run)
    vals <- fullMap$count
    
    if (var(vals) == 0) return(NA_real_)
    
    as.numeric(moran.test(vals, lw)$estimate["Moran I statistic"])
  }
  
  df_xy_agg %>%
    group_by(behaviorSpaceRun) %>%
    summarise(
      moranI = compute_moran_single(cur_data()),
      .groups = "drop"
    ) %>%
    mutate(
      terr = "Prey",
      mem  = memType,
      numPred = case_when(
        behaviorSpaceRun <= 50  ~ 1,
        behaviorSpaceRun <= 100 ~ 2,
        behaviorSpaceRun <= 150 ~ 3,
        behaviorSpaceRun <= 200 ~ 4
      )
    )
}


NTM_mor1AGG <- compute_moran_runsAGG(df = NTM, terrType = "None", memType = "Individual", cell_size = 10)
NTNM_mor1AGG <- compute_moran_runsAGG(df = NTNM, terrType = "None", memType = "None", cell_size = 10)
NTSM_mor1AGG <- compute_moran_runsAGG(df = NTSM, terrType = "None", memType = "Shared", cell_size = 10)
PDTM_mor1AGG <- compute_moran_runsAGG(df = PDTM, terrType = "Pred", memType = "Individual", cell_size = 10)
PDTNM_mor1AGG <- compute_moran_runsAGG(df = PDTNM, terrType = "Pred", memType = "None", cell_size = 10)
PDTSM_mor1AGG <- compute_moran_runsAGG(df = PDTSM, terrType = "Pred", memType = "Shared", cell_size = 10)
PYTM_mor1AGG <- compute_moran_runsAGG_prey(df = PYTM, memType = "Individual", cell_size = 10)
PYTNM_mor1AGG <- compute_moran_runsAGG_prey(df = PYTNM, memType = "None", cell_size = 10)
PYTSM_mor1AGG <- compute_moran_runsAGG_prey(df = PYTSM, memType = "Shared", cell_size = 10)

allMoranIAGG <- bind_rows(NTM_mor1AGG, NTNM_mor1AGG, NTSM_mor1AGG, PDTM_mor1AGG, PDTNM_mor1AGG, PDTSM_mor1AGG,
                       PYTM_mor1AGG, PYTNM_mor1AGG, PYTSM_mor1AGG)
library(effectsize)
m2 <- lm(moranI ~ as.factor(terr) + as.factor(mem) + as.factor(numPred), data = allMoranIAGG)
summary(m2)
eta_squared(m2)
omega_squared(m2)
cohens_d(moranI ~ as.factor(mem), data = allMoranI)
x <- stepAIC(m2, direction = "both")

terr_glht <- glht(m2, linfct = mcp("as.factor(terr)" = "Tukey"))
mem_glht  <- glht(m2, linfct = mcp("as.factor(mem)" = "Tukey"))
num_glht  <- glht(m2, linfct = mcp("as.factor(numPred)" = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory on the Space Use (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for Memory on Space Use (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for Number of Predators on Space Use (Tukey-adjusted)")
gt_num

ggplot(allMoranIAGG, aes(x = factor(numPred), y = moranI)) +
  geom_boxplot() +
  #geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
  facet_grid(
    terr ~ mem,
    labeller = labeller(
      terr = c(
        "None" = "No Territory",
        "Pred" = "Predator Territory",
        "Prey" = "Prey Territory"
      ),
      mem = c(
        "Individual" = "Individual Memory",
        "None" = "No Memory",
        "Shared" = "Shared Memory"
      )
    )
  ) +  # rows = territory, cols = memory
  labs(
    x = "Number of Predators",
    y = "Moran's I",
    title = "Effects on Aggregated Space Use"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

ggplot(allMoranIAGG, aes(as.factor(numPred), moranI)) +
  geom_boxplot() +
  facet_wrap(~ terr)


# pick a run to inspect
run_to_check <- 1

# rebuild the aggregated prey data exactly as used in Moran's I
cell_size <- 20

prey_check <- PYTM %>%
  mutate(
    patch.id = str_remove_all(`patch-id`, "[()]"),
    x = as.numeric(str_split_fixed(patch.id, ",", 2)[,1]),
    y = as.numeric(str_split_fixed(patch.id, ",", 2)[,2]),
    x_coarse = floor(x / cell_size) * cell_size,
    y_coarse = floor(y / cell_size) * cell_size
  ) %>%
  group_by(behaviorSpaceRun, x_coarse, y_coarse) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  filter(behaviorSpaceRun == run_to_check)

ggplot(prey_check, aes(x_coarse, y_coarse, fill = count)) +
  geom_tile(color = "grey40") +
  coord_equal() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = paste("Prey coarse grid sanity check (run", run_to_check, ")"),
    x = "x (coarse)",
    y = "y (coarse)",
    fill = "Agent count"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)
library(stringr)

# choose dataset and run
df_check <- PDTM          # or PDTM, NTNM, PDTSM, etc.
run_to_check <- 160
cell_size <- 25

full_check <- df_check %>%
  mutate(
    patch.id = str_remove_all(`patch-id`, "[()]"),
    x = as.numeric(str_split_fixed(patch.id, ",", 2)[,1]),
    y = as.numeric(str_split_fixed(patch.id, ",", 2)[,2]),
    x_coarse = floor(x / cell_size) * cell_size,
    y_coarse = floor(y / cell_size) * cell_size
  ) %>%
  group_by(behaviorSpaceRun, x_coarse, y_coarse) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  filter(behaviorSpaceRun == run_to_check)

ggplot(full_check, aes(x_coarse, y_coarse, fill = count)) +
  geom_tile(color = "grey40") +
  coord_equal() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = paste("Full landscape coarse grid sanity check (run", run_to_check, ")"),
    x = "x (coarse)",
    y = "y (coarse)",
    fill = "Agent count"
  ) +
  theme_minimal()

