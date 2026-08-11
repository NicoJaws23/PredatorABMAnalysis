####Tables and Such###

library(tidyverse)
library(gt)
library(ggplot2)

####Inputs####
x <- read_csv(file.choose())
t <- x |>
  gt()|>
  tab_header(
    title = "Model Inputs"
  )

####Inter-Individual Distances####
ggplot(allDist, aes(x = factor(numPred), y = logDist)) +
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
    y = "Inter-Individual Distance, Log Transformed",
    title = "Predictors of Log Transformed Inter-Individual Distance"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

fixed_tbl <- broom.mixed::tidy(m3, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "as.factor(terr)Pred" = "Territory: Predator",
                  "as.factor(terr)Prey" = "Territory: Prey",
                  "as.factor(mem)None" = "Memory: None",
                  "as.factor(mem)Shared" = "Memory: Shared",
                  "as.factor(numPred)2" = "2 Predators", 
                  "as.factor(numPred)3" = "3 Predators",
                  "as.factor(numPred)4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  dplyr::select(Term, Estimate, `95% CI`, statistic, p)

m3T <- fixed_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Predictors of Inter-Individual Distance**"),
    subtitle = "Fixed effects with 95% CI"
  ) %>%
  fmt_number(columns = Estimate, decimals = 3) %>%
  fmt_number(columns = statistic, decimals = 2) %>%
  cols_label(
    Term = "Parameter",
    statistic = "t value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p,
      rows = p < 0.05
    )
  ) %>%
  tab_options(
    table.font.size = 14,
    data_row.padding = px(3)
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


####GroupSize####
fixed_tbl <- broom.mixed::tidy(m2, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "as.factor(terr)Pred" = "Territory: Predator",
                  "as.factor(terr)Prey" = "Territory: Prey",
                  "as.factor(mem)None" = "Memory: None",
                  "as.factor(mem)Shared" = "Memory: Shared",
                  "as.factor(numPred)2" = "2 Predators", 
                  "as.factor(numPred)3" = "3 Predators",
                  "as.factor(numPred)4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  dplyr::select(Term, Estimate, `95% CI`, statistic, p)

m2T <- fixed_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Predictors of Group Size**"),
    subtitle = "Fixed effects with 95% CI"
  ) %>%
  fmt_number(columns = Estimate, decimals = 3) %>%
  fmt_number(columns = statistic, decimals = 2) %>%
  cols_label(
    Term = "Parameter",
    statistic = "t value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p,
      rows = p < 0.05
    )
  ) %>%
  tab_options(
    table.font.size = 14,
    data_row.padding = px(3)
  )

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
    title = "Predictors of Group Size"
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


####Number of Groups####
fixed_tbl <- broom.mixed::tidy(m1, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "as.factor(terr)Pred" = "Territory: Predator",
                  "as.factor(terr)Prey" = "Territory: Prey",
                  "as.factor(mem)None" = "Memory: None",
                  "as.factor(mem)Shared" = "Memory: Shared",
                  "as.factor(numPred)2" = "2 Predators", 
                  "as.factor(numPred)3" = "3 Predators",
                  "as.factor(numPred)4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  dplyr::select(Term, Estimate, `95% CI`, statistic, p)

m1T <- fixed_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Predictors of Number of Groups**"),
    subtitle = "Fixed effects with 95% CI"
  ) %>%
  fmt_number(columns = Estimate, decimals = 3) %>%
  fmt_number(columns = statistic, decimals = 2) %>%
  cols_label(
    Term = "Parameter",
    statistic = "t value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p,
      rows = p < 0.05
    )
  ) %>%
  tab_options(
    table.font.size = 14,
    data_row.padding = px(3)
  )

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
    title = "Predictors of the Number of Groups"
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


####Space Use####
fixed_tbl <- broom.mixed::tidy(mM, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "as.factor(terr)Pred" = "Territory: Predator",
                  "as.factor(terr)Prey" = "Territory: Prey",
                  "as.factor(mem)None" = "Memory: None",
                  "as.factor(mem)Shared" = "Memory: Shared",
                  "as.factor(numPred)2" = "2 Predators", 
                  "as.factor(numPred)3" = "3 Predators",
                  "as.factor(numPred)4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  dplyr::select(Term, Estimate, `95% CI`, statistic, p)

mMT <- fixed_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Predictors of Space Use Based on Morans I**"),
    subtitle = "Fixed effects with 95% CI"
  ) %>%
  fmt_number(columns = Estimate, decimals = 3) %>%
  fmt_number(columns = statistic, decimals = 2) %>%
  cols_label(
    Term = "Parameter",
    statistic = "t value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p,
      rows = p < 0.05
    )
  ) %>%
  tab_options(
    table.font.size = 14,
    data_row.padding = px(3)
  )

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
    title = "Effects on Space Use Based on Morans I"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

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

####Example Exponential Learning Curve####
# P(t) = P_max * (1 - exp(-t / tau))

# Parameters
P_max <- 100   # maximum performance (ceiling)
tau   <- 20    # time constant (smaller = faster learning)

# Time steps (e.g., practice sessions, days, trials)
t <- seq(0, 100, by = 1)

# Compute performance at each time step
P <- P_max * (1 - exp(-t / tau))

# Plot the curve
plot(t, P, type = "l", lwd = 2, col = "steelblue",
     xlab = "Time (practice trials)",
     ylab = "Performance",
     main = "Exponential Learning Curve",
     ylim = c(0, P_max))

abline(h = P_max, lty = 2, col = "gray50")  # ceiling reference line
legend("bottomright", legend = c("Performance", "Max ceiling"),
       col = c("steelblue", "gray50"), lty = c(1, 2), lwd = c(2, 1), bty = "n")

# Example Exponential Forgetting Curve
# P(t) = P0 * exp(-t / tau)

# Parameters
P0  <- 100   # initial performance/retention right after learning
tau <- 20    # time constant (smaller = faster forgetting)

# Time steps (e.g., days since learning)
t <- seq(0, 100, by = 1)

# Compute retention at each time step
P <- P0 * exp(-t / tau)

# Plot the curve
plot(t, P, type = "l", lwd = 2, col = "firebrick",
     xlab = "Time",
     ylab = "Retention",
     main = "Exponential Forgetting Curve",
     ylim = c(0, P0))

legend("topright", legend = c("Retention"),
       col = c("firebrick"), lty = c(1, 2), lwd = c(2, 1), bty = "n")

# Example Exponential Forgetting Curve (ggplot2 version)
# P(t) = P0 * exp(-t / tau)

# Example Exponential Forgetting Curve (ggplot2 version)
# P(t) = P0 * exp(-t / tau)

# Example Exponential Forgetting Curve (ggplot2 version)
# Using the original equation: P = e^(-(u/t))

library(ggplot2)
library(extrafont)  # needed to register/use Times New Roman

# --- One-time font setup (uncomment if Times New Roman isn't registered yet) ---
# font_import()   # scans system fonts, only needs to be run once, takes a while
# loadfonts(device = "win")   # on Windows
# loadfonts(device = "pdf")   # if exporting to PDF on Mac/Linux

# Parameters
t <- 800              # fixed denominator (from the original equation)
u <- seq(0, 2400, by = 1) # numerator, ranging (e.g., time/units elapsed)

# Compute P using the original equation
P <- exp(-(u / t))

df <- data.frame(u = u, P = P)

# Plot
ggplot(df, aes(x = u, y = P)) +
  geom_line(color = "firebrick", linewidth = 1) +
  labs(
    title = "Exponential Forgetting Curve",
    x = "Time",
    y = "Memory Strength"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# To save the plot:
# ggsave("forgetting_curve.png", width = 6, height = 4, dpi = 300)

# Example Exponential Forgetting Curve (ggplot2 version)
# Using the original equation: P = e^(-(u/t))

library(ggplot2)
library(extrafont)  # needed to register/use Times New Roman

# --- One-time font setup (uncomment if Times New Roman isn't registered yet) ---
# font_import()   # scans system fonts, only needs to be run once, takes a while
# loadfonts(device = "win")   # on Windows
# loadfonts(device = "pdf")   # if exporting to PDF on Mac/Linux

# Parameters
t <- 800               # fixed denominator (from the original equation)
u <- seq(0, 2400, by = 1)  # numerator, ranging (e.g., time/units elapsed)

# Compute P using the original equation
P <- exp(-(u / t))

df <- data.frame(u = u, P = P)

# Plot
ggplot(df, aes(x = u, y = P)) +
  geom_line(color = "firebrick", linewidth = 1) +
  labs(
    title = "Memory Strength Over Time",
    x = "Time (ticks)",
    y = "Memory Strength"
  ) +
  scale_x_continuous(breaks = seq(0, 2500, by = 250), limits = c(0, 2500)) +
  ylim(0, 1) +
  theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# To save the plot:
ggsave("forgetting_curve.png", width = 7, height = 5, dpi = 300)
