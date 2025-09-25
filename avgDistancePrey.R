#Analyzing data from territory ABM
library(tidyverse)
library(igraph)

f <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\outputs\\M3_prey_coords_matrix.csv"
df <- read_csv(f)

#Determine average distance between individuals

# --- Step 1: Long format ---
df_long <- df %>%
  pivot_longer(-id, names_to = "tick", values_to = "coord") %>%
  mutate(coord = str_remove_all(coord, "[()]")) %>%
  separate(coord, into = c("x","y"), sep = ",", convert = TRUE)

# --- Step 2: Self-join by tick to get all pairs ---
pairs <- df_long %>%
  rename(x1 = x, y1 = y, id1 = id) %>%
  inner_join(df_long %>% rename(x2 = x, y2 = y, id2 = id),
             by = "tick") %>%
  filter(id1 < id2) %>%   # keep each dyad once
  mutate(dist = sqrt((x1-x2)^2 + (y1-y2)^2))

# --- Step 3: Average distance across ticks for each dyad ---
dyad_avg <- pairs %>%
  group_by(id1, id2) %>%
  summarise(mean_dist = mean(dist, na.rm = TRUE), .groups = "drop")

dyad_avg

g <- graph_from_data_frame(dyad_avg, directed = FALSE)

# Quick plot
plot(g, edge.width = 1/(E(g)$mean_dist/10), 
     vertex.size = 20, vertex.label.cex = 0.8,
     main = "Social network based on average inter-individual distance")