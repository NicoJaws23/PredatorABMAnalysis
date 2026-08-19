#Using this for analysis, no plotting#
library(tidyverse)
library(MuMIn)      # dredge, AICc, get.models
library(emmeans)
library(broom)
library(infer)       # permutation-based inference
library(car)         # Anova(type = 2) for joint/omnibus term tests

# =====================================================================
# General pipeline applied to each response variable:
#   1. Fit a global lm() with the full terr*mem*numPred interaction
#   2. dredge() -> AICc-ranked table of all subset models, with
#      delta AICc and Akaike weights computed automatically
#   3. Pull out the best (lowest-AICc) model
#   4. Normality diagnostics (Shapiro-Wilk, QQ plot) on that model
#   5. Permutation-based inference on that model's coefficients,
#      via infer::specify() %>% hypothesize() %>% generate() %>% fit()
#   6. emmeans on that best model, as in your original code
#
# NOTE on the permutation test: generate(type = "permute") shuffles the
# response against ALL predictors jointly, then refits the full formula.
# get_p_value() then compares each coefficient's observed magnitude to its
# null distribution. This tests each term against a null of "no
# relationship to any predictor" rather than a partial/marginal test
# controlling for the other terms (i.e. it isn't equivalent to
# car::Anova(type = 2/3)). For a fully-saturated interaction model this is
# a reasonable simplification; flag if you need Freedman-Lane style
# partial permutation instead.
# =====================================================================

run_response_analysis <- function(data, response_var,
                                  predictors = c("terr", "mem", "numPred"),
                                  n_perm = 1000, model_select = NULL) {
  
  global_formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " * ")))
  
  cat("\n==================================================\n")
  cat("Response:", response_var, "\n")
  cat("Global model:", deparse(global_formula), "\n")
  cat("==================================================\n")
  
  # Drop incomplete cases on exactly the variables entering the model, and say
  # so out loud, rather than letting na.fail (below) error out opaquely.
  model_vars <- all.vars(global_formula)
  complete_idx <- complete.cases(data[, model_vars])
  n_dropped <- sum(!complete_idx)
  if (n_dropped > 0) {
    cat(sprintf("\nDropping %d of %d rows with missing values in %s\n",
                n_dropped, nrow(data), paste(model_vars, collapse = ", ")))
    data <- data[complete_idx, ]
  }
  
  # dredge() requires na.action = na.fail on the fitted model
  global_model <- lm(global_formula, data = data, na.action = na.fail)
  
  cat("\nGlobal model df check: n =", nobs(global_model),
      " | params =", length(coef(global_model)), "\n")
  
  # ---- 2 & 3. dredge, ranked by AICc ----
  dredge_result <- dredge(global_model, rank = "AICc")
  cat("\n--- Model selection table (sorted by AICc) ---\n")
  print(dredge_result)
  
  # ---- pick which row of the dredge table becomes "best_model" ----
  n_candidates <- nrow(dredge_result)
  if (!is.null(model_select)) {
    if (!is.numeric(model_select) || model_select < 1 || model_select > n_candidates) {
      stop(sprintf("model_select must be an integer between 1 and %d", n_candidates))
    }
    sel <- as.integer(model_select)
    cat(sprintf("\nUsing model row %d (as specified via model_select)\n", sel))
  } else if (interactive()) {
    repeat {
      raw <- readline(prompt = sprintf(
        "Select model row to use as best model [1-%d] (Enter = 1, top AICc): ", n_candidates))
      if (raw == "") { sel <- 1L; break }
      sel <- suppressWarnings(as.integer(raw))
      if (!is.na(sel) && sel >= 1 && sel <= n_candidates) break
      cat("Invalid selection, try again.\n")
    }
  } else {
    sel <- 1L
    cat("\nNon-interactive session and model_select not set -> defaulting to row 1 (top AICc)\n")
  }
  
  best_model <- get.models(dredge_result, subset = sel)[[1]]
  best_formula <- formula(best_model)
  cat("\nSelected model formula:", deparse(best_formula), "\n")
  
  # ---- 4. Diagnostics ----
  cat("\n--- Diagnostics on best model ---\n")
  r <- residuals(best_model)
  # shapiro.test() errors above n = 5000, so subsample for the test itself;
  # the QQ plot below still uses the full residual vector.
  r_test <- if (length(r) > 5000) sample(r, 5000) else r
  sw <- shapiro.test(r_test)
  cat(sprintf("Shapiro-Wilk (n = %d%s): W = %.4f, p = %.4g\n",
              length(r_test), if (length(r) > 5000) ", subsampled" else "",
              sw$statistic, sw$p.value))
  qqnorm(r, main = paste("QQ:", response_var)); qqline(r, col = "red")
  
  shapiro_result <- data.frame(
    response   = response_var,
    n          = length(r_test),
    subsampled = length(r) > 5000,
    W          = unname(sw$statistic),
    p_value    = sw$p.value
  )
  
  # ---- 5. Permutation-based inference on best model's terms, via infer ----
  cat("\n--- Permutation inference on best model (infer::fit) ---\n")
  
  # Observed coefficients from the best model, fit inside the infer pipeline
  obs_fit <- data %>%
    specify(best_formula) %>%
    fit()
  
  # Null distribution: shuffle the response, refit the same formula n_perm times
  null_fits <- data %>%
    specify(best_formula) %>%
    hypothesize(null = "independence") %>%
    generate(reps = n_perm, type = "permute") %>%
    fit()
  
  perm_result <- get_p_value(null_fits, obs_stat = obs_fit, direction = "two-sided") %>%
    left_join(obs_fit, by = "term") %>%
    relocate(term, estimate, p_value)
  
  print(perm_result)
  
  # ---- 5b. Joint (omnibus) permutation test per term ----
  # infer's fit() only ever returns one row per dummy coefficient, which can't
  # answer "does this whole term (e.g. mem, which spans 2 dummy coefficients,
  # or mem:terr, which spans 4) matter?" This reuses infer::generate() to
  # produce the permuted datasets (same permutation engine as above), then
  # computes a Type II F-statistic per full term (via car::Anova) on each
  # permuted replicate to build a null distribution for each term as a whole.
  cat("\n--- Joint permutation test per term (infer::generate + car::Anova) ---\n")
  
  obs_anova <- car::Anova(best_model, type = 2)
  obs_anova <- obs_anova[rownames(obs_anova) != "Residuals", , drop = FALSE]
  obs_F <- setNames(obs_anova[["F value"]], rownames(obs_anova))
  
  permuted_sets <- data %>%
    specify(best_formula) %>%
    hypothesize(null = "independence") %>%
    generate(reps = n_perm, type = "permute")
  
  null_F <- permuted_sets %>%
    group_by(replicate) %>%
    group_modify(~ {
      m <- lm(best_formula, data = .x)
      a <- car::Anova(m, type = 2)
      a <- a[rownames(a) != "Residuals", , drop = FALSE]
      tibble(term = rownames(a), F = a[["F value"]])
    }) %>%
    ungroup()
  
  # p = (# permuted F >= observed F + 1) / (# permutations + 1), so p can
  # never come out as exactly 0
  obs_anova_df <- obs_anova %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    rename(Sum_Sq = `Sum Sq`, term_df = Df, observed_F = `F value`)
  
  joint_result <- null_F %>%
    left_join(tibble(term = names(obs_F), observed_F = obs_F), by = "term") %>%
    group_by(term, observed_F) %>%
    summarise(p_value = (sum(F >= observed_F) + 1) / (n() + 1), .groups = "drop") %>%
    left_join(obs_anova_df %>% select(term, Sum_Sq, term_df), by = "term") %>%
    arrange(p_value)
  
  # Round for display/reporting - the unrounded values are still used
  # upstream (e.g. partial R2 calc, if you add that) since this happens
  # after all the actual math is done
  joint_result <- joint_result %>%
    mutate(across(c(observed_F, Sum_Sq), ~ round(., 2)))
  
  print(joint_result)
  
  # ---- 6. emmeans on best model ----
  emm_list <- lapply(predictors, function(p) {
    if (p %in% all.vars(best_formula)) emmeans(best_model, as.formula(paste("~", p))) else NULL
  })
  names(emm_list) <- predictors
  
  list(
    dredge_table = dredge_result,
    best_model   = best_model,
    shapiro      = shapiro_result,
    permutation  = perm_result,
    null_fits    = null_fits,
    emmeans      = emm_list
  )
}

####Inter-Individual Distance####
allDist_v2 <- allDist_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None"),
    logDist = log(meanDist)
  )
res_dist <- run_response_analysis(allDist_v2, response_var = "logDist")

emtrends(res_dist$best_model, ~ mem, var = "numPred")
emmeans(res_dist$best_model, ~ mem | terr)

emm <- emmeans(
  res_dist$best_model,
  ~ mem | terr,
  type = "response"
)
pairs(emm, adjust = "tukey")

emtrends(
  res_dist$best_model,
  ~ mem | terr,
  var = "numPred"
)

pairs(
  emtrends(
    res_dist$best_model,
    ~ mem | terr,
    var = "numPred"
  ),
  adjust = "tukey"
)

####Component Size####
allSizes_v2 <- allSizes_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None"),
    logSize = log(componentSize)
  )
res_size <- run_response_analysis(allSizes_v2, response_var = "logSize")
emmeans(res_size$best_model, ~ mem | terr)

####Number of Components####
allComps_v2 <- allComps_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None"),
    logNum = log(numComponents)
  )
res_comps <- run_response_analysis(allComps_v2, response_var = "logNum")

####Moran's I####
allMoranI <- allMoranI |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None"),
    logMor = log(moranI)
  )
res_moran <- run_response_analysis(allMoranI, response_var = "logMor")

emtrends(
  res_moran$best_model,
  ~ mem * terr,
  var = "numPred"
)
emmip(
  res_moran$best_model,
  mem ~ numPred | terr,
  at = list(numPred = 1:4),
  CIs = TRUE
)

# =====================================================================
# Pulling results together at the end
# =====================================================================
all_results <- list(dist = res_dist, size = res_size, comps = res_comps, moran = res_moran)

# Quick summary of each response's best model + delta AICc of runner-up
lapply(names(all_results), function(nm) {
  d <- all_results[[nm]]$dredge_table
  cat("\n", nm, ": best model =", deparse(formula(all_results[[nm]]$best_model)),
      "| delta AICc of 2nd-best =", round(d$delta[2], 2), "\n")
})

# ---- Shapiro-Wilk summary table across all four best models, for write-up ----
shapiro_summary <- bind_rows(lapply(all_results, function(res) res$shapiro), .id = "model") %>%
  mutate(
    W = round(W, 4),
    p_value = signif(p_value, 3),
    normality_rejected = p_value < 0.05
  )

cat("\n--- Shapiro-Wilk normality summary (best model per response) ---\n")
print(shapiro_summary)

# Optional: write straight to CSV for pasting into a manuscript table
write_csv(shapiro_summary, "shapiro_wilk_summary.csv")

#################
# Extract the isolated (marginal) effect of each predictor from a single
# response's best model:
#   - mem, terr (factors)  -> emmeans, averaged over the other predictor(s)
#   - numPred (continuous) -> emtrends (overall slope), NOT emmeans, since
#     emmeans() on a continuous term just evaluates it at its mean rather
#     than giving a trend
get_isolated_effects <- function(res, response_name) {
  model <- res$best_model
  model_vars <- all.vars(formula(model))
  out <- list()
  
  if ("mem" %in% model_vars) {
    out$mem <- emmeans(model, ~ mem) %>%
      as.data.frame() %>%
      rename(level = mem, estimate = emmean) %>%
      mutate(response = response_name, predictor = "mem")
  }
  
  if ("terr" %in% model_vars) {
    out$terr <- emmeans(model, ~ terr) %>%
      as.data.frame() %>%
      rename(level = terr, estimate = emmean) %>%
      mutate(response = response_name, predictor = "terr")
  }
  
  if ("numPred" %in% model_vars) {
    out$numPred <- emtrends(model, ~ 1, var = "numPred") %>%
      as.data.frame() %>%
      rename(estimate = numPred.trend) %>%
      mutate(response = response_name, predictor = "numPred", level = "slope")
  }
  
  bind_rows(out) %>%
    select(response, predictor, level, estimate, SE, df, lower.CL, upper.CL)
}

# Run across all four stored results
isolated_effects <- map2_dfr(all_results, names(all_results), get_isolated_effects)

print(isolated_effects, n = Inf)
write_csv(isolated_effects, "isolated_predictor_effects.csv")

# Optional: wide tables, one per predictor, for side-by-side comparison
# across the four response variables (nice for a manuscript table)
mem_table <- isolated_effects %>%
  filter(predictor == "mem") %>%
  select(response, level, estimate) %>%
  pivot_wider(names_from = response, values_from = estimate)

terr_table <- isolated_effects %>%
  filter(predictor == "terr") %>%
  select(response, level, estimate) %>%
  pivot_wider(names_from = response, values_from = estimate)

numPred_table <- isolated_effects %>%
  filter(predictor == "numPred") %>%
  select(response, estimate, SE, p = df)  # trend + SE per response

####################
get_interaction_effects <- function(res, response_name, numPred_vals = c(1, 2, 3, 4)) {
  model <- res$best_model
  model_vars <- all.vars(formula(model))
  has_numPred <- "numPred" %in% model_vars
  out <- list()
  
  # mem x terr (2-way), averaged over numPred if present - fine as-is
  if (all(c("mem", "terr") %in% model_vars)) {
    out$mem_terr <- emmeans(model, ~ mem * terr) %>%
      as.data.frame() %>%
      mutate(response = response_name, numPred = NA_real_)
  }
  
  # mem x terr x numPred (3-way) - numPred MUST be in the specs formula,
  # not just in `at`, or emmeans averages over the `at` values instead of
  # returning one row per value
  if (has_numPred) {
    out$mem_terr_numPred <- emmeans(
      model, ~ mem * terr * numPred,
      at = list(numPred = numPred_vals)
    ) %>%
      as.data.frame() %>%
      mutate(response = response_name)
  }
  
  bind_rows(out) %>%
    select(response, mem, terr, any_of("numPred"), emmean, SE, df, lower.CL, upper.CL)
}

interaction_effects <- map2_dfr(all_results, names(all_results), get_interaction_effects)
print(interaction_effects, n = Inf)
write_csv(interaction_effects, "interaction_cell_means.csv")

# =====================================================================
# Table 8a: Distance - Memory x Territory x Number of Predators
# (uses interaction_effects, already computed with numPred = c(1,2,3,4))
# =====================================================================
table_8a <- interaction_effects %>%
  filter(response == "dist", !is.na(numPred)) %>%
  mutate(emmean = round(emmean, 2)) %>%
  select(mem, terr, numPred, emmean) %>%
  pivot_wider(
    names_from = numPred,
    values_from = emmean,
    names_prefix = "Pred_"
  ) %>%
  arrange(terr, mem)

print(table_8a)
write_csv(table_8a, "table_8a_distance_mem_terr_numPred.csv")


# =====================================================================
# Table 8b: Component Size - Memory x Territory (no numPred in model)
# =====================================================================
table_8b <- interaction_effects %>%
  filter(response == "size") %>%
  mutate(emmean = round(emmean, 2)) %>%
  select(mem, terr, emmean) %>%
  pivot_wider(names_from = terr, values_from = emmean) %>%
  arrange(mem)

print(table_8b)
write_csv(table_8b, "table_8b_size_mem_terr.csv")


# =====================================================================
# Table 8c: Number of Components - Territory only (no interactions)
# Pull straight from the isolated_effects table you already built,
# since this response has no significant interaction term at all
# =====================================================================
table_8c <- isolated_effects %>%
  filter(response == "comps", predictor == "terr") %>%
  mutate(across(c(estimate, lower.CL, upper.CL), ~ round(., 2))) %>%
  select(terr = level, estimate, lower.CL, upper.CL)

print(table_8c)
write_csv(table_8c, "table_8c_comps_terr.csv")


# =====================================================================
# Table 8d: Moran's I - Memory x Territory x Number of Predators
# Same structure as 8a
# =====================================================================
table_8d <- interaction_effects %>%
  filter(response == "moran", !is.na(numPred)) %>%
  mutate(emmean = round(emmean, 4)) %>%   # more decimals - moranI is bounded 0-1
  select(mem, terr, numPred, emmean) %>%
  pivot_wider(
    names_from = numPred,
    values_from = emmean,
    names_prefix = "Pred_"
  ) %>%
  arrange(terr, mem)

print(table_8d)
write_csv(table_8d, "table_8d_moran_mem_terr_numPred.csv")


# =====================================================================
# Optional: condensed version of 8a/8d for main text, showing only the
# extremes (1 and 4 predators) instead of the full 4-column grid,
# with full version reserved for supplementary material
# =====================================================================
table_8a_condensed <- table_8a %>%
  select(mem, terr, Pred_1, Pred_4)

table_8d_condensed <- table_8d %>%
  select(mem, terr, Pred_1, Pred_4)
