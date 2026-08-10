#Just functions

#Pivot table function, used to show individual prey agent location at a given tick
piv <- function(df, predNum){
  df |>
    pivot_longer(
      cols = -c(behaviorSpaceRun, predNum, id),
      names_to = "tick",
      values_to = "coord"
    ) |>
    mutate(
      tick = as.numeric(tick),
      coord = str_remove_all(coord, "[()]"),
      x = as.numeric(str_split_fixed(coord, ",", 2)[, 1]),
      y = as.numeric(str_split_fixed(coord, ",", 2)[, 2]),
      num_predators = predNum
    )
}

#Pivot table function for counts of neighbors for every character at tick interval
pivNeighbor<- function(df, predNum) {
  df |>
    pivot_longer(
      cols = -id,
      names_to = "tick",
      values_to = "neighbors",
      names_transform = list(tick = as.numeric)) |>
    mutate(num_predators = predNum)
}

#Pivot table function for model 3
pivM3 <- function(df){
  data <- df
  data |>
    pivot_longer(cols = -id, names_to = "tick", values_to = "coords") |>
    mutate(tick = as.integer(str_remove(tick, "^X")), coords = na_if(coords, ""),
           X = map_dbl(coords, ~ ifelse(is.na(.x), NA, parse_coords(.x)[1])),
           Y = map_dbl(coords, ~ ifelse(is.na(.x), NA, parse_coords(.x)[2])))|>
    select(id, tick, X, Y)
}


#Used to calcuate distances between dyads at a tick interval
pairDist <- function(df, predNum) {
  df |>
    filter(!is.na(x), !is.na(y)) |>
    group_by(behaviorSpaceRun, tick) |>  # ← group within each run AND tick
    do({
      agents <- .
      n <- nrow(agents)
      out <- data.frame()
      if (n > 1) {
        for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
            d <- sqrt((agents$x[i] - agents$x[j])^2 + (agents$y[i] - agents$y[j])^2)
            out <- rbind(out, data.frame(
              behaviorSpaceRun = agents$behaviorSpaceRun[i],
              tick = agents$tick[i],
              id1 = agents$id[i],
              id2 = agents$id[j],
              dist = d,
              num_predators = predNum
            ))
          }
        }
      }
      out
    }) |>
    ungroup()
}


#Uses individual coordinates and distance of dyads to build networks at a given
#tick and distance threshold 
buildNetwork <- function(Distdf, Coorddf, threshold = 6) {
  # edges: only pairs closer than threshold
  edges_t <- Distdf |> 
    filter(dist < threshold) |> 
    mutate(across(c(id1, id2), as.character))
  
  # verts: include ALL individuals present in Coorddf for this run & tick
  verts_t <- Coorddf |>
    mutate(id = as.character(id)) |>
    distinct(id, x, y, behaviorSpaceRun, tick, .keep_all = TRUE)
  
  # If no individuals at this tick -> nothing
  if (nrow(verts_t) == 0) return(NULL)
  
  # Build graph: if no edges, create a graph with only vertices (isolated nodes)
  g <- tryCatch({
    if (nrow(edges_t) == 0) {
      graph_from_data_frame(d = data.frame(), vertices = verts_t |> rename(name = id), directed = FALSE)
    } else {
      graph_from_data_frame(
        d = select(edges_t, id1, id2),
        vertices = verts_t |> rename(name = id),
        directed = FALSE
      )
    }
  }, error = function(e) {
    message("⚠️ igraph error for run=", unique(verts_t$behaviorSpaceRun), 
            " tick=", unique(verts_t$tick), ": ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(g)) return(NULL)
  
  comps <- components(g)
  
  verts_t <- verts_t |> 
    mutate(comp = comps$membership[as.character(id)],
           tick = unique(verts_t$tick))
  
  list(graph = g, verts = verts_t)
}

#Determines the size of components in the networks, as in how many individuals 
# are present in a component at a given tick interval
compSum <- function(networks) {
  comp_summary <- pblapply(names(networks), function(run_name) {
    networks_run <- networks[[run_name]]
    pblapply(networks_run, function(net) {
      if (is.null(net)) return(NULL)
      verts <- net$verts
      verts |>
        group_by(behaviorSpaceRun, tick, comp) |>
        summarise(n_individuals = n(), .groups = "drop")
    }) |> bind_rows()
  }) |> bind_rows()
  
  return(comp_summary)
}

#Counts the number of components
numComp <- function(compSummary) {
  compSummary |>
    group_by(tick) |>
    summarise(n_components = n(), .groups = "drop")
}

#Generates heatmaps of prey locations
heatMap <- function(df, numPred, titleText) {
  preds <- numPred
  pDF <- df
  pDF <- pDF |>
    mutate(patch.id = str_remove_all(`patch-id`, "[()]"),
           x = as.numeric(str_split_fixed(patch.id, ",", 2)[,1]),
           y = as.numeric(str_split_fixed(patch.id, ",", 2)[,2]))
  
  pDFsum <- pDF |>
    group_by(x, y) |>
    summarise(total_prey = sum(count), .groups = "drop")
  
  #Shows the true number of prey on a patch (ex: 1 prey here, 5 here)
  plot1 <- ggplot(pDFsum, aes(x = x, y = y, fill = total_prey)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    coord_fixed(ratio = 1, xlim = c(-50, 50), ylim = c(-50, 50)) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
    labs(
      title = titleText,
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "Total Prey"
    ) +
    theme_minimal(base_size = 14)
  
  #Shows smoothed prey distribution of prey, probability density of occupied patches
  #Using KDE
  plot2 <- ggplot(pDFsum, aes(x = x, y = y)) +
    stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
    scale_fill_viridis_c(option = "magma") +
    coord_fixed(xlim = c(-50, 50), ylim = c(-50, 50)) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
    labs(
      title = titleText,
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "Density"
    ) +
    theme_minimal(base_size = 14)
  
  #Shows a weighted spatial density, prey were here and this is where they were
  #concentrated
  plot3 <- ggplot(pDFsum, aes(x = x, y = y)) +
    stat_density_2d(
      aes(fill = after_stat(level)),
      geom = "polygon",
      contour = TRUE,
      # simulate weighting by repeating high-count locations
      data = pDFsum[rep(1:nrow(pDFsum), pDFsum$total_prey), ]
    ) +
    scale_fill_viridis_c(option = "magma") +
    coord_fixed(xlim = c(-50, 50), ylim = c(-50, 50)) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
    labs(
      title = titleText,
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "Relative Density"
    ) +
    theme_minimal(base_size = 14)
  
  
  return(list(grid = plot1, smooth = plot2, preyDes = plot3))
}

#Plot networks on a grid resembling the netlogo environment
plot_spatial_network <- function(all_networks, pred_to_plot, tick_to_plot, limits = c(-50, 50)) {
  # Extract target network
  network_to_plot <- all_networks[[as.character(pred_to_plot)]][[which(
    sort(unique(allDist$tick)) == tick_to_plot
  )]]
  
  verts <- network_to_plot$verts %>%
    mutate(id = as.character(id))  # ensure character IDs
  
  edges <- igraph::as_data_frame(network_to_plot$graph, what = "edges")
  
  # Join coordinates for each edge endpoint
  edges_xy <- edges %>%
    left_join(verts, by = c("from" = "id")) %>%
    left_join(verts, by = c("to" = "id"), suffix = c(".from", ".to"))
  
  # Plot
  plot <- ggplot() +
    geom_segment(data = edges_xy,
                 aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
                 color = "gray70", alpha = 0.4) +
    geom_point(data = verts, aes(x = x, y = y, color = as.factor(comp)), size = 3) +
    scale_color_brewer(palette = "Set2") +
    coord_equal(xlim = limits, ylim = limits) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Spatial Prey Network —", pred_to_plot, "Predator(s) at Tick", tick_to_plot),
      x = "X coordinate",
      y = "Y coordinate",
      color = "Group"
    )
}

#Parse coordinates for predator or prey territory
parse_coords <- function(coord_str) {
  if (is.na(coord_str)) return(c(NA, NA))
  as.numeric(str_split(str_remove_all(coord_str, "[()]"), ",")[[1]])
}

#Extract the coordinates of the predator territory
terrBounds <- function(df, wWidth, wHeight) {
  
  # --- Clean and prepare the input dataframe ---
  data_clean <- df %>%
    # rename dash-based columns to dot-based
    rename(
      bottom.left  = `bottom-left`,
      bottom.right = `bottom-right`,
      top.right    = `top-right`,
      top.left     = `top-left`
    ) %>%
    # drop behaviorSpaceRun if it exists
    select(-any_of("behaviorSpaceRun")) %>%
    # keep only distinct polygons
    distinct()
  
  # --- Extract numeric coordinates ---
  data <- data_clean %>%
    mutate(across(-id, ~ str_remove_all(., "[()]"))) %>%
    
    separate(bottom.left,  into = c("bl_x", "bl_y"), sep = ",", convert = TRUE) %>%
    separate(bottom.right, into = c("br_x", "br_y"), sep = ",", convert = TRUE) %>%
    separate(top.right,    into = c("tr_x", "tr_y"), sep = ",", convert = TRUE) %>%
    separate(top.left,     into = c("tl_x", "tl_y"), sep = ",", convert = TRUE) %>%
    
    transmute(
      predator_id = id,
      xmin = pmin(bl_x, tl_x),
      xmax = pmax(br_x, tr_x),
      ymin = pmin(bl_y, br_y),
      ymax = pmax(tl_y, tr_y)
    )
  
  # --- Polygon vertices for plotting ---
  terrV2 <- data %>%
    rowwise() %>%
    mutate(corners = list(data.frame(
      x = c(xmin, xmax, xmax, xmin, xmin),
      y = c(ymin, ymin, ymax, ymax, ymin)
    ))) %>%
    unnest(corners)
  
  # --- Create sf polygon objects ---
  preds_sf <- data %>%
    rowwise() %>%
    mutate(geometry = list(st_polygon(list(matrix(
      c(
        xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin
      ),
      ncol = 2, byrow = TRUE
    ))))) %>%
    st_as_sf()
  
  # --- Compute union area (remove overlap) ---
  combined_union <- st_union(preds_sf)
  
  totalArea_noOverlap <- as.numeric(st_area(combined_union))
  totalPercentCover_noOverlap <- (totalArea_noOverlap / (wWidth * wHeight)) * 100
  
  # --- Individual predictor area ---
  PredArea <- data %>%
    mutate(
      area = abs(xmax - xmin) * abs(ymax - ymin),
      percentCov = (area / (wWidth * wHeight)) * 100
    )
  
  return(list(
    raw = data,
    tCover = terrV2,
    area = PredArea,
    totalAreaNoOverlap = totalArea_noOverlap,
    totalPercentCoverNoOverlap = totalPercentCover_noOverlap,
    PredsSF = preds_sf,
    union_sf = combined_union
  ))
}


#Determine prey in predator territroy
preyInTerr <- function(df, predBoundsDF, threshold) {
  data <- df |>
    filter(!is.na(x), !is.na(y)) |>
    crossing(predBoundsDF) |>
    filter(x >= xmin, x <= xmax, y >= ymin, y <= ymax) |>
    filter(tick >= threshold) |>
    select(prey_id = id, predator_id, tick, x, y)
  
  return(data)
}

#sum number of prey in territory at a tick
preyInTerrSum <- function(df){
  data <- df |>
    group_by(prey_id, predator_id) |>
    summarise(
      ticks_inside = n(),
      first_tick = min(tick),
      last_tick = max(tick),
      .groups = "drop") |>
    arrange(prey_id, predator_id)
  
  return(data)
}

#how long prey were in predator territory
predInTerrTime <- function(df, predBounds) {
  data <- df |>
    filter(!is.na(x), !is.na(y)) |>
    crossing(predBounds) |>
    mutate(in_territory = x >= xmin & x <= xmax & y >= ymin & y <= ymax) |>
    group_by(id, tick) |>
    summarise(in_territory = any(in_territory), .groups = "drop")
  
  return(data)
}

#mapping prey locations with territory
heatMapPredTerr <- function(df, numPred, terrBoundsObj, titleText) {
  # df = prey patch data
  # numPred = number of predators (for labeling)
  # terrBoundsObj = the output from terrBounds(), e.g., TB1 or TB4
  
  # Prey data prep (same as before)
  pDF <- df |>
    mutate(patch.id = str_remove_all(`patch-id`, "[()]"),
           x = as.numeric(str_split_fixed(`patch.id`, ",", 2)[,1]),
           y = as.numeric(str_split_fixed(`patch.id`, ",", 2)[,2]))
  
  pDFsum <- pDF |>
    group_by(x, y) |>
    summarise(total_prey = sum(count), .groups = "drop")
  
  # Predator territory polygons
  terr_poly <- terrBoundsObj$PredsSF
  
  # Plot 1: raw prey density
  plot1 <- ggplot() +
    geom_tile(data = pDFsum, aes(x = x, y = y, fill = total_prey)) +
    geom_sf(data = terr_poly, aes(color = "Predator Territory"), fill = NA, linewidth = 2) +
    scale_fill_viridis_c(option = "plasma") +
    scale_color_manual(name = "", values = c("Predator Territory" = "red")) +
    coord_sf(xlim = c(-50, 50), ylim = c(-50, 50), expand = FALSE) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
    labs(
      title = titleText,
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "Total Prey"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")
  
  # Plot 2: smoothed density
  plot2 <- ggplot() +
    stat_density_2d(data = pDFsum,
                    aes(x = x, y = y, fill = after_stat(level)),
                    geom = "polygon") +
    geom_sf(data = terr_poly, aes(color = "Predator Territory"), fill = NA, linewidth = 2) +
    scale_fill_viridis_c(option = "plasma") +
    scale_color_manual(name = "", values = c("Predator Territory" = "red")) +
    scale_fill_viridis_c(option = "magma") +
    coord_sf(xlim = c(-50, 50), ylim = c(-50, 50), expand = FALSE) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
    labs(
      title = titleText,
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "Density"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")
  
  # Plot 3: weighted density (relative concentration)
  plot3 <- ggplot() +
    stat_density_2d(
      data = pDFsum[rep(1:nrow(pDFsum), pDFsum$total_prey), ],
      aes(x = x, y = y, fill = after_stat(level)),
      geom = "polygon",
      contour = TRUE
    ) +
    geom_sf(data = terr_poly, aes(color = "Predator Territory"), fill = NA, linewidth = 2) +
    scale_fill_viridis_c(option = "plasma") +
    scale_color_manual(name = "", values = c("Predator Territory" = "red")) +
    scale_fill_viridis_c(option = "magma") +
    coord_sf(xlim = c(-50, 50), ylim = c(-50, 50), expand = FALSE) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
    labs(
      title = titleText,
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "Relative Density"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")
  
  return(list(raw = plot1, smooth = plot2, relative = plot3))
}


meanDist <- function(df) {
  data <- df
  data <- data |>
    group_by(num_predators, tick) |>
    group_modify(~ {
      df <- .x
      if (nrow(df) > 1) {
        dist_mat <- as.matrix(dist(df[, c("x", "y")]))
        mean_dist <- mean(dist_mat[upper.tri(dist_mat)], na.rm = TRUE)
        tibble(mean_distance = mean_dist)
      } else {
        tivvle(mean_distance = NA_real_)
      }
    }) |>
    ungroup()
}

#Split behavior space exports
#If prey/pred coordinates: header_pattern = "^id\t0"
#If patch count: header_pattern = "^tick\tpatch-id"
#If territory bounds: header_pattern = "^id\tbottom-left"
split_behaviorspace_runs <- function(df, header_pattern) {
  # Convert the data frame back into text lines
  tmpfile <- tempfile(fileext = ".csv")
  write.table(df, tmpfile, sep = "\t", row.names = FALSE, quote = FALSE)
  raw_lines <- readLines(tmpfile)
  unlink(tmpfile)
  
  # Find where each new BehaviorSpace run starts
  header_lines <- grep(header_pattern, raw_lines)
  
  # Split by run and read each section
  chunks <- lapply(seq_along(header_lines), function(i) {
    start <- header_lines[i]
    end <- ifelse(i < length(header_lines), header_lines[i + 1] - 1, length(raw_lines))
    read.table(text = raw_lines[start:end], header = TRUE, sep = "\t")
  })
  
  # Add run number column and combine
  chunks <- lapply(seq_along(chunks), function(i) {
    df <- chunks[[i]]
    df$behaviorspaceRun <- i
    df <- df[, c("behaviorspaceRun", setdiff(names(df), "behaviorspaceRun"))] # make first column
    df
  })
  
  # Return one cleaned data frame
  do.call(rbind, chunks)
}

#Distance between agents within the same component or group
withinCompDist <- function(networks, predNum) {
  all_dists <- pblapply(names(networks), function(run_name) {
    networks_run <- networks[[run_name]]
    if (is.null(networks_run) || length(networks_run) == 0) return(NULL)
    
    pblapply(networks_run, function(net) {
      if (!is.list(net) || !"verts" %in% names(net)) return(NULL)
      
      verts <- net$verts
      if (is.null(verts) || nrow(verts) == 0 || !"comp" %in% names(verts)) return(NULL)
      
      out <- list()
      comps <- unique(verts$comp)
      
      for (c in comps) {
        comp_verts <- verts |> filter(comp == c)
        n <- nrow(comp_verts)
        if (n > 1) {
          dists <- combn(n, 2, simplify = FALSE) |> 
            lapply(function(idx) {
              i <- idx[1]; j <- idx[2]
              d <- sqrt((comp_verts$x[i] - comp_verts$x[j])^2 + 
                          (comp_verts$y[i] - comp_verts$y[j])^2)
              data.frame(
                behaviorSpaceRun = comp_verts$behaviorSpaceRun[i],
                tick = comp_verts$tick[i],
                compID = c,
                id1 = comp_verts$id[i],
                id2 = comp_verts$id[j],
                dist = d,
                n_individuals = n,
                num_predators = predNum
              )
            })
          out <- c(out, dists)
        }
      }
      if (length(out) > 0) bind_rows(out) else NULL
    }) |> bind_rows()
  }) |> bind_rows()
  
  if (nrow(all_dists) == 0) message("⚠️ No distances calculated for predNum = ", predNum)
  return(all_dists)
}

fileRead <- function(path, numPred, type = "coords") {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  
  # Helper to remove header rows repeated inside files
  drop_header_rows <- function(df) {
    df %>%
      filter(!if_all(everything(), ~ .x %in% names(df)))  # remove rows matching header names
  }
  
  if (type == "coords") {
    df <- imap_dfr(files, ~ {
      raw <- read_csv(.x, col_types = cols(.default = "c")) %>%
        distinct() %>%
        drop_header_rows()
      
      run_id <- str_extract(basename(.x), "(?<=run)\\d+") |> as.numeric()
      
      raw %>%
        mutate(
          behaviorSpaceRun = run_id,
          predNum = numPred
        ) %>%
        select(behaviorSpaceRun, predNum, everything())
    })
    
  } else if (type == "patchCount") {
    df <- imap_dfr(files, ~ {
      raw <- read_csv(.x, col_types = cols(.default = "c")) %>%
        distinct() %>%
        drop_header_rows()
      
      run_id <- str_extract(basename(.x), "(?<=run)\\d+") |> as.numeric()
      
      raw %>%
        mutate(
          tick = as.numeric(tick),
          count = as.numeric(count),
          behaviorSpaceRun = run_id,
          predNum = numPred
        ) %>%
        select(behaviorSpaceRun, predNum, everything())
    })
    
  } else if (type == "terr") {
    df <- imap_dfr(files, ~ {
      raw <- read_csv(.x, col_types = cols(.default = "c")) %>%
        distinct() %>%
        drop_header_rows()
      
      run_id <- str_extract(basename(.x), "(?<=run)\\d+") |> as.numeric()
      
      raw %>%
        mutate(
          id = as.numeric(id),
          behaviorSpaceRun = run_id,
          predNum = numPred
        ) %>%
        select(behaviorSpaceRun, predNum, everything())
    })
    
  } else {
    stop("Invalid type, must be 'coords', 'patchCount', or 'terr'")
  }
  
  return(df)
}



library(dplyr)
library(igraph)

analyze_networks_tidy <- function(Distdf, threshold = 6, pred) {
  
  # Ensure character
  Distdf <- Distdf %>%
    mutate(
      id1 = as.character(id1),
      id2 = as.character(id2),
      behaviorSpaceRun = as.character(behaviorSpaceRun),
      tick = as.integer(tick)
    )
  
  # Unique run × tick combos
  run_tick_combos <- Distdf %>% distinct(behaviorSpaceRun, tick)
  
  results_summary <- list()
  results_comp_sizes <- list()
  results_within <- list()
  
  for(i in seq_len(nrow(run_tick_combos))) {
    run_i <- run_tick_combos$behaviorSpaceRun[i]
    tick_i <- run_tick_combos$tick[i]
    
    edges_sub <- Distdf %>%
      filter(behaviorSpaceRun == run_i, tick == tick_i, dist < threshold)
    
    if(nrow(edges_sub) == 0) {
      # Empty network
      results_summary[[i]] <- tibble(
        behaviorSpaceRun = run_i,
        tick = tick_i,
        num_components = NA_integer_,
        mean_comp_size = NA_real_,
        sd_comp_size = NA_real_
      )
      results_comp_sizes[[i]] <- tibble(
        behaviorSpaceRun = run_i,
        tick = tick_i,
        compID = NA_integer_,
        compSize = NA_integer_
      )
      results_within[[i]] <- tibble(
        behaviorSpaceRun = run_i,
        tick = tick_i,
        compID = NA_integer_,
        mean_within_dist = NA_real_,
        n_pairs = NA_integer_
      )
      next
    }
    
    # Map unique IDs to numeric indices
    all_ids <- unique(c(edges_sub$id1, edges_sub$id2))
    id_map <- tibble(id = all_ids, idx = seq_along(all_ids))
    
    edges_idx <- edges_sub %>%
      left_join(id_map, by = c("id1" = "id")) %>%
      rename(from = idx) %>%
      left_join(id_map, by = c("id2" = "id")) %>%
      rename(to = idx)
    
    # Build igraph with numeric vertex IDs
    g <- graph_from_data_frame(edges_idx %>% select(from, to), 
                               vertices = tibble(idx = seq_along(all_ids)),
                               directed = FALSE)
    
    comps <- components(g)
    
    # --- Summary stats ---
    results_summary[[i]] <- tibble(
      numPred = pred,
      behaviorSpaceRun = run_i,
      tick = tick_i,
      num_components = comps$no,
      mean_comp_size = mean(comps$csize),
      sd_comp_size = sd(comps$csize)
    )
    
    # --- Component sizes ---
    results_comp_sizes[[i]] <- tibble(
      numPred = pred,
      behaviorSpaceRun = run_i,
      tick = tick_i,
      compID = seq_along(comps$csize),
      compSize = comps$csize,
    )
    
    # --- Within-component distances ---
    edges_idx <- edges_idx %>%
      mutate(
        compID_from = comps$membership[from],
        compID_to = comps$membership[to]
      ) %>%
      filter(compID_from == compID_to) %>%
      group_by(compID_from) %>%
      summarise(
        mean_within_dist = mean(dist, na.rm = TRUE),
        n_pairs = n(),
        .groups = "drop"
      ) %>%
      rename(compID = compID_from) %>%
      mutate(numPred = pred, behaviorSpaceRun = run_i, tick = tick_i)
    
    results_within[[i]] <- edges_idx
  }
  
  list(
    summary = bind_rows(results_summary),
    comp_sizes = bind_rows(results_comp_sizes),
    within_comp_dist = bind_rows(results_within)
  )
}

predPreyDist <- function(prey_df, pred_df, numPred) {
  
  coord_cols <- setdiff(colnames(pred_df), 
                        c("behaviorSpaceRun", "predNum", "id"))
  new_ticks <- 3000 + (seq_along(coord_cols) - 1) * 50
  names(pred_df)[match(coord_cols, names(pred_df))] <- as.character(new_ticks)
  pred_df <- piv(pred_df, 1)
  
  prey_df <- prey_df |>
    filter(tick >= 3000)
  # Make sure columns match
  prey <- prey_df |> 
    filter(!is.na(x), !is.na(y)) |> 
    mutate(id = as.character(id))
  
  preds <- pred_df |> 
    filter(!is.na(x), !is.na(y)) |> 
    mutate(id = as.character(id))
  
  # Join predators with prey on run + tick
  crossed <- prey |> 
    inner_join(preds, 
               by = c("behaviorSpaceRun", "tick"),
               suffix = c("_prey", "_pred"))
  
  # Compute Euclidean distances
  crossed |> 
    mutate(dist = sqrt((x_prey - x_pred)^2 + (y_prey - y_pred)^2), predNum = numPred) |>
    select(predNum,behaviorSpaceRun, tick,
           prey_id = id_prey,
           predator_id = id_pred,
           dist)
}

tidy_glht <- function(glht_obj) {
  s <- summary(glht_obj)
  
  tibble(
    Contrast = names(s$test$coefficients),
    Estimate = s$test$coefficients,
    Std_Error = s$test$sigma,
    t_value = s$test$tstat,
    p_value = s$test$pvalues
  )
}

make_gt <- function(df, title) {
  df %>%
    mutate(p_value = scales::pvalue(p_value)) %>% 
    gt() %>%
    tab_header(title = title) %>%
    cols_label(
      Std_Error = "Standard Error",
      t_value   = "t value",
      p_value   = "p value"
    ) %>%
    fmt_number(columns = c(Estimate, Std_Error, t_value), decimals = 3)
}
