#Just functions

#Pivot table function, used to show individual prey agent location at a given tick
piv <- function(df, predNum){
  df |>
    pivot_longer(
      cols = -id,
      names_to = "tick",
      values_to = "coord") |>
    mutate(
      tick = as.numeric(tick),
      coord = str_remove_all(coord, "[()]"),
      x = as.numeric(str_split_fixed(coord, ",", 2)[,1]),
      y = as.numeric(str_split_fixed(coord, ",", 2)[,2]),
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
    group_by(tick) |>
    do({
      agents <- .
      n <- nrow(agents)
      out <- data.frame()
      if (n > 1) {
        for (i in 1:(n-1)) {
          for (j in (i+1):n) {
            d <- sqrt((agents$x[i] - agents$x[j])^2 + (agents$y[i] - agents$y[j])^2)
            out <- rbind(out, data.frame(
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
buildNetwork <- function(Distdf, Coorddf, t, threshold) {
  edges_t <- Distdf |>
    filter(tick == t, dist < threshold)
  verts_t <- Coorddf |>
    filter(tick == t) |>
    distinct(id, x, y)
  
  g <- graph_from_data_frame(
    d <- edges_t |> select(id1, id2),
    vertices = verts_t |> rename(name = id),
    directed = FALSE
  )
  
  comps <- components(g)
  
  verts_t <- verts_t |>
    mutate(comp = comps$membership[as.character(id)],
           tick = t)
  
  list(graph = g, verts = verts_t)
} 

#Determines the size of components in the networks, as in how many individuals 
# are present in a component at a given tick interval
compSum <- function(networks) {
  comp_summary <- lapply(networks, function(net) {
    verts <- net$verts
    verts |>
      group_by(tick, comp) |>
      summarise(n_individuals = n(), .groups = "drop")
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
    mutate(patch.id = str_remove_all(`patch.id`, "[()]"),
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
  data <- df |>
    mutate(across(-id, ~ str_remove_all(., "[()]"))) |>
      separate(bottom.left, into = c("bl_x", "bl_y"), sep = ",", convert = TRUE) |>
      separate(bottom.right, into = c("br_x", "br_y"), sep = ",", convert = TRUE) |>
      separate(top.right, into = c("tr_x", "tr_y"), sep = ",", convert = TRUE) |>
      separate(top.left, into = c("tl_x", "tl_y"), sep = ",", convert = TRUE) |>
      transmute(
        predator_id = id,
        xmin = pmin(bl_x, tl_x),
        xmax = pmax(br_x, tr_x),
        ymin = pmin(bl_y, br_y),
        ymax = pmax(tl_y, tr_y)
      )
  
  terrV2 <- data |>
    rowwise() %>%
    mutate(corners = list(data.frame(
      x = c(xmin, xmax, xmax, xmin, xmin),  # loop back to first point
      y = c(ymin, ymin, ymax, ymax, ymin)
    ))) %>%
    unnest(corners)
  
  #account for overlaps
  preds_sf <- data |>
    rowwise() |>
    mutate(geometry = list(st_polygon(list(matrix(
      c(xmin, ymin, 
        xmax, ymin, 
        xmax, ymax, 
        xmin, ymax, 
        xmin, ymin),
      ncol = 2, byrow= TRUE))))) |>
    st_as_sf()
  
  combined_union <- st_union(preds_sf)
  
  totalArea_noOverlap <- as.numeric(st_area(combined_union))
  totalPercentCover_noOverlap <- (totalArea_noOverlap / (wWidth*wHeight))*100
  
  PredArea <- data |>
    mutate(area = abs(xmax - xmin) * abs(ymax - ymin), percentCov = (area/(wWidth*wHeight))*100)
  
    
  return(list(raw = data, tCover = terrV2, area = PredArea, 
              totalAreaNoOverlap = totalArea_noOverlap, totalPercentCoverNoOverlap = totalPercentCover_noOverlap,
              PredsSF = preds_sf, union_sf = combined_union))
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
    mutate(patch.id = str_remove_all(`patch.id`, "[()]"),
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
