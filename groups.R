###New Network Analysis##

library(data.table)
library(igraph)
library(dplyr)
library(purrr)
library(scales)
library(stringr)
library(MASS)
library(ggplot2)
library(effects)
library(emmeans)
library(multcomp)
library(gt)

###Functions to Use###
#build_network_and_communities()
#mean_within_component
#plot_network_component
#Extract_components_info

###Workflow###
#Step 1: Load in allDist data set for a particular model variation
#Step 2: Pass data through build_network_and_communities()
#Step 3: Use extract_components_info() to get number of comps, size of comps
#and comp membership
#Step 4: Use compute_within_component_distances() to figure out average distance between
#group members
#Step 5: Use mutate and case when to assigned numPred and terr/mem to data
#Step 6: Bind data together
#Step 7: Linear models 

build_network_and_communities <- function(
    df,
    tick,
    weight_method = "inv",     # "inv", "scale01"
    eps = 1e-6,
    community_algorithm
) {
  
  # ---- 1. Filter to the tick we want ----
  df_tick <- df %>% filter(tick == !!tick)
  
  # ---- 2. Compute weights per run ----
  df_weighted <-
    df_tick %>%
    mutate(weight_raw = 1 / (dist + eps)) %>%
    group_by(behaviorSpaceRun) %>%
    mutate(weight =
             case_when(
               weight_method == "inv" ~ weight_raw,
               weight_method == "scale01" ~ scales::rescale(weight_raw, to = c(0, 1)),
               TRUE ~ weight_raw
             )
    ) %>%
    ungroup()
  
  # ---- 3. Split dataset into list, one per run ----
  runs <- df_weighted %>% group_split(behaviorSpaceRun)
  
  # ---- 4. Process each run independently ----
  results <- map(runs, function(run_df) {
    
    run_id <- unique(run_df$behaviorSpaceRun)
    
    # Edge list for igraph
    edges <- run_df %>%
      select(id1, id2, weight)
    
    # Build graph
    g <- graph_from_data_frame(edges, directed = FALSE)
    
    # ---- Community detection ----
    comm <- if (community_algorithm == "louvain") {
      cluster_louvain(g)
    } else if (community_algorithm == "walktrap") {
      cluster_walktrap(g)        # keep as communities object
    } else {
      stop("Need method: choose 'louvain' or 'walktrap'")
    }
    
    comm_membership <- membership(comm)  # works for both Louvain and Walktrap
    
    # To get members per community:
    communities_summary <- tibble(
      community = unique(comm_membership),
      members = map(unique(comm_membership), ~ names(comm_membership)[comm_membership == .x])
    ) %>%
      mutate(
        comm_size = map_int(members, length),
        run = run_id
      )
    
    list(
      run = run_id,
      edges = edges,
      graph = g,
      community_membership = comm_membership,
      communities_summary = communities_summary
    )
  })
  
  return(results)
}

# Function to compute mean distance for a given component
compute_within_component_distances <- function(component_df, pairwise_df, tick_use) {
    component_df %>%
    # Add a column with the mean distance for each component
    mutate(
      meanDist = map2_dbl(behaviorSpaceRun, members, function(run_id, members_vec) {
        members_vec <- as.numeric(members_vec)
        
        pairwise_df %>%
          filter(behaviorSpaceRun == run_id, tick == tick_use) %>%
          filter(id1 %in% members_vec & id2 %in% members_vec) %>%
          summarize(meanDist = mean(dist), .groups = "drop") %>%
          pull(meanDist)
      })
    )
}

plot_network_components <- function(network_obj, scale_edges = TRUE, edge_factor = 5, node_factor = 3) {
  library(igraph)
  
  g <- network_obj$graph
  membership_vec <- network_obj$community_membership
  
  # Assign colors to components
  num_components <- length(unique(membership_vec))
  palette <- rainbow(num_components)
  V(g)$color <- palette[membership_vec]
  
  # Node size proportional to degree
  V(g)$size <- degree(g) * node_factor
  
  # Edge width optionally scaled by weight
  if (scale_edges && "weight" %in% edge_attr_names(g)) {
    E(g)$width <- E(g)$weight * edge_factor
  } else {
    E(g)$width <- 1
  }
  
  # Plot
  plot(
    g,
    vertex.label = V(g)$name,
    vertex.color = V(g)$color,
    vertex.size = V(g)$size,
    edge.width = E(g)$width,
    main = paste("BehaviorSpaceRun", network_obj$run)
  )
}

extract_components_info <- function(network_list, terrType, memType) {
  
  # ---- 1) Number of components per run ----
  num_components_per_run <- map_dfr(network_list, function(x) {
    tibble(
      behaviorSpaceRun = x$run,
      numComponents = nrow(x$communities_summary),
      terr = terrType,
      mem = memType,
      numPred = case_when(
        behaviorSpaceRun >= 0 & behaviorSpaceRun <= 50 ~ 1,
        behaviorSpaceRun >= 51 & behaviorSpaceRun <= 100 ~ 2,
        behaviorSpaceRun >= 101 & behaviorSpaceRun <= 150 ~ 3,
        behaviorSpaceRun >= 151 & behaviorSpaceRun <= 200 ~ 4
      )
    )
  })
  
  # ---- 2) Component sizes per run ----
  component_sizes_per_run <- map_dfr(network_list, function(x) {
    x$communities_summary %>%
      select(run, community, comm_size) %>%
      rename(
        behaviorSpaceRun = run,
        componentID = community,
        componentSize = comm_size
      )
  })
  component_sizes_per_run <- component_sizes_per_run |>
    mutate(terr = terrType,
           mem = memType,
           numPred = case_when(
             behaviorSpaceRun >= 0 & behaviorSpaceRun <= 50 ~ 1,
             behaviorSpaceRun >= 51 & behaviorSpaceRun <= 100 ~ 2,
             behaviorSpaceRun >= 101 & behaviorSpaceRun <= 150 ~ 3,
             behaviorSpaceRun >= 151 & behaviorSpaceRun <= 200 ~ 4
           ))
  
  # ---- 3) Component members per run ----
  component_members_per_run <- map_dfr(network_list, function(x) {
    tibble(
      behaviorSpaceRun = x$run,
      componentID = seq_along(x$communities_summary$members),
      members = x$communities_summary$members,
      terr = terrType,
      mem = memType,
      numPred = case_when(
        behaviorSpaceRun >= 0 & behaviorSpaceRun <= 50 ~ 1,
        behaviorSpaceRun >= 51 & behaviorSpaceRun <= 100 ~ 2,
        behaviorSpaceRun >= 101 & behaviorSpaceRun <= 150 ~ 3,
        behaviorSpaceRun >= 151 & behaviorSpaceRun <= 200 ~ 4
      )
    )
  })
  
  # Return as a list of three data frames
  list(
    num_components = num_components_per_run,
    component_sizes = component_sizes_per_run,
    component_members = component_members_per_run
  )
}

####No Territory Memory#####
NTM_net <- build_network_and_communities(df = NTM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
NTM_info <- extract_components_info(NTM_net, terrType = "None", memType = "Individual")
NTM_compMem <- NTM_info$component_members
NTM_compNum <- NTM_info$num_components
NTM_compSize <- NTM_info$component_sizes
NTM_compDist <- compute_within_component_distances(component_df = NTM_compMem, pairwise_df = NTM_allDist, tick_use = 5000)

####No Territory No Memory####
NTNM_net <- build_network_and_communities(df = NTNM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
NTNM_info <- extract_components_info(NTNM_net, terrType = "None", memType = "None")
NTNM_compMem <- NTNM_info$component_members
NTNM_compNum <- NTNM_info$num_components
NTNM_compSize <- NTNM_info$component_sizes
NTNM_compDist <- compute_within_component_distances(component_df = NTNM_compMem, pairwise_df = NTNM_allDist, tick_use = 5000)

####No Territory Shared Memory####
NTSM_net <- build_network_and_communities(df = NTSM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
NTSM_info <- extract_components_info(NTSM_net, terrType = "None", memType = "Shared")
NTSM_compMem <- NTSM_info$component_members
NTSM_compNum <- NTSM_info$num_components
NTSM_compSize <- NTSM_info$component_sizes
NTSM_compDist <- compute_within_component_distances(component_df = NTSM_compMem, pairwise_df = NTSM_allDist, tick_use = 5000)

####Pred Territory Memory####
PDTM_net <- build_network_and_communities(df = PDTM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
PDTM_info <- extract_components_info(PDTM_net, terrType = "Pred", memType = "Individual")
PDTM_compMem <- PDTM_info$component_members
PDTM_compNum <- PDTM_info$num_components
PDTM_compSize <- PDTM_info$component_sizes
PDTM_compDist <- compute_within_component_distances(component_df = PDTM_compMem, pairwise_df = PDTM_allDist, tick_use = 5000)

####Pred Territory No Memory####
PDTNM_net <- build_network_and_communities(df = PDTNM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
PDTNM_info <- extract_components_info(PDTNM_net, terrType = "Pred", memType = "None")
PDTNM_compMem <- PDTNM_info$component_members
PDTNM_compNum <- PDTNM_info$num_components
PDTNM_compSize <- PDTNM_info$component_sizes
PDTNM_compDist <- compute_within_component_distances(component_df = PDTNM_compMem, pairwise_df = PDTNM_allDist, tick_use = 5000)

####Pred Territory Shared Memory####
PDTSM_net <- build_network_and_communities(df = PDTSM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
PDTSM_info <- extract_components_info(PDTSM_net, terrType = "Pred", memType = "Shared")
PDTSM_compMem <- PDTSM_info$component_members
PDTSM_compNum <- PDTSM_info$num_components
PDTSM_compSize <- PDTSM_info$component_sizes
PDTSM_compDist <- compute_within_component_distances(component_df = PDTSM_compMem, pairwise_df = PDTSM_allDist, tick_use = 5000)

####Prey Territory Memory####
PYTM_net <- build_network_and_communities(df = PYTM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
PYTM_info <- extract_components_info(PYTM_net, terrType = "Prey", memType = "Individual")
PYTM_compMem <- PYTM_info$component_members
PYTM_compNum <- PYTM_info$num_components
PYTM_compSize <- PYTM_info$component_sizes
PYTM_compDist <- compute_within_component_distances(component_df = PYTM_compMem, pairwise_df = PYTM_allDist, tick_use = 5000)

####Prey Territory No Memory####
PYTNM_net <- build_network_and_communities(df = PYTNM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
PYTNM_info <- extract_components_info(PYTNM_net, terrType = "Prey", memType = "None")
PYTNM_compMem <- PYTNM_info$component_members
PYTNM_compNum <- PYTNM_info$num_components
PYTNM_compSize <- PYTNM_info$component_sizes
PYTNM_compDist <- compute_within_component_distances(component_df = PYTNM_compMem, pairwise_df = PYTNM_allDist, tick_use = 5000)

####Prey Territory Shared Memory####
PYTSM_net <- build_network_and_communities(df = PYTSM_allDist, tick = 5000, weight_method = "inv", eps = 1e-6, community_algorithm = "walktrap")
PYTSM_info <- extract_components_info(PYTSM_net, terrType = "Prey", memType = "Shared")
PYTSM_compMem <- PYTSM_info$component_members
PYTSM_compNum <- PYTSM_info$num_components
PYTSM_compSize <- PYTSM_info$component_sizes
PYTSM_compDist <- compute_within_component_distances(component_df = PYTSM_compMem, pairwise_df = PYTSM_allDist, tick_use = 5000)

####Binding Data####
allComps <- bind_rows(NTM_compNum, NTNM_compNum, NTSM_compNum, PDTM_compNum, PDTNM_compNum, PDTSM_compNum,
                      PYTM_compNum, PYTNM_compNum, PYTSM_compNum)
write_csv(allComps, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//allComps_v2.csv")
m1 <- lm(numComponents ~ as.factor(terr) + as.factor(mem) + as.factor(numPred), data = allComps_v2)
summary(m1)
m1A <- stepAIC(m1, direction = "both")

ggplot(allComps_v2, aes(x = factor(numPred), y = numComponents)) +
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
    y = "Number of Groups",
    title = "Effects on Number of Groups"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

terr_glht <- glht(m1, linfct = mcp("as.factor(terr)" = "Tukey"))
mem_glht  <- glht(m1, linfct = mcp("as.factor(mem)" = "Tukey"))
num_glht  <- glht(m1, linfct = mcp("as.factor(numPred)" = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory on the Number of Groups (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for Memory on the Number of Groups (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for Number of Predators on the Number of Groups (Tukey-adjusted)")
gt_num

allSizes <- bind_rows(NTM_compSize, NTNM_compSize, NTSM_compSize, PDTM_compSize, PDTNM_compSize, PDTSM_compSize,
                      PYTM_compSize, PYTNM_compSize, PYTSM_compSize)
write_csv(allSizes, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//allSizes_v2.csv")
m2 <- lm(componentSize ~ as.factor(terr) + as.factor(mem) + as.factor(numPred), data = allSizes_v2)
summary(m2)
m2A <- stepAIC(m2, direction = "both")

ggplot(allSizes_v2, aes(x = factor(numPred), y = componentSize)) +
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
    y = "Group Size",
    title = "Effects on Group Size"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

terr_glht <- glht(m2, linfct = mcp("as.factor(terr)" = "Tukey"))
mem_glht  <- glht(m2, linfct = mcp("as.factor(mem)" = "Tukey"))
num_glht  <- glht(m2, linfct = mcp("as.factor(numPred)" = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory on Group Size (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for Memory on Group Size (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for Number of Predators on Group Size (Tukey-adjusted)")
gt_num


allDist <- bind_rows(NTM_compDist, NTNM_compDist, NTSM_compDist, PDTM_compDist, PDTNM_compDist, PDTSM_compDist,
                     PYTM_compDist, PYTNM_compDist, PYTSM_compDist)
write_csv(allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//allDist_v2.csv")
m3 <- lm(meanDist ~ as.factor(terr) + as.factor(mem) + as.factor(numPred), data = d)
summary(m3)
m3A <- stepAIC(m3, direction = "both")
ggplot(d, aes(x = factor(numPred), y = meanDist)) +
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
    y = "Inter-Individual Distance",
    title = "Effects on Distance"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

terr_glht <- glht(m3, linfct = mcp("as.factor(terr)" = "Tukey"))
mem_glht  <- glht(m3, linfct = mcp("as.factor(mem)" = "Tukey"))
num_glht  <- glht(m3, linfct = mcp("as.factor(numPred)" = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory on Within-Group Distance (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for Memory on Within-Group Distance (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for Number of Predators on Within-Group Distance (Tukey-adjusted)")
gt_num

d <- allDist_v2 |>
  filter(meanDist >= 0)
mean(d$meanDist)
min(d$meanDist)
max(d$meanDist)
