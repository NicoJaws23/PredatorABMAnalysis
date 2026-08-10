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
