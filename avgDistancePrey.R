#Analyzing data from territory ABM
library(tidyverse)
library(igraph)

f <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\outputs\\M3_prey_coords_matrix.csv"
preyPoints <- read_csv(f)
f1 <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\outputs\\M3_predator_territories.csv"
predatorTerritory <-read_csv(f1)

#How often does a prey agetn fall within a predators territory
library(dplyr)
library(tidyr)
library(sf)

# Convert wide → long
prey_long <- preyPoints %>%
  pivot_longer(-id, names_to = "tick", values_to = "coords") %>%
  mutate(coords = gsub("[()]", "", coords)) %>%
  separate(coords, into = c("x", "y"), sep = ",", convert = TRUE)

PL_clean <- na.omit(prey_long)
# Convert to sf points
prey_sf <- st_as_sf(PL_clean, coords = c("x", "y"), crs = 4326) # arbitrary CRS


### --- 2. Predator territories ---
# Helper to parse corners into polygon
parse_poly <- function(id, corners) {
  coords <- gsub("[()]", "", corners) %>%
    strsplit(",") %>%
    lapply(as.numeric)
  mat <- do.call(rbind, coords)
  st_polygon(list(rbind(mat, mat[1,]))) |> st_sfc(crs = 4326) |> st_sf(id = id, geometry = _)
}

# Apply to each row
pred_sf <- do.call(rbind, lapply(1:nrow(predatorTerritory), function(i) {
  parse_poly(predatorTerritory$id[i], predatorTerritory[i,2:5])
}))


### --- 3. Spatial join (point-in-polygon) ---
joined <- st_join(prey_sf, pred_sf, join = st_within)

### --- 4. Count how many times each prey was inside each predator territory
counts <- joined %>%
  filter(!is.na(id.y)) %>%   # keep only prey inside a territory
  count(prey_id = id.x, predator_id = id.y)

counts
