#Using this for analysis, no plotting#
library(emmeans)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(broom)
library(sjPlot)
library(MASS)
library(MuMIn)     # dredge, AICc
library(permuco)   # permutation inference for lm models
library(car)        # Anova() for type II/III tests
library(DHARMa)    # simulated residuals for the Poisson GLM


# =====================================================================
# General pipeline applied to each response variable:
#   1. Fit a global model with the full terr*mem*numPred interaction
#   2. dredge() -> AICc-ranked table of all subset models, with
#      delta AICc and Akaike weights computed automatically
#   3. Pull out the best (lowest-AICc) model
#   4. Normality / distributional diagnostics on that best model
#   5. Permutation-based inference on that best model's terms
#   6. emmeans on that best model, as in your original code
# =====================================================================

run_response_analysis <- function(data, response_var, predictors = c("terr", "mem", "numPred"),
                                  family = gaussian(), log_transform = FALSE,
                                  n_perm = 10000, anova_type = 2, model_select = NULL) {
  # model_select: row number from the dredge table to use as the "best" model.
  #   - NULL + interactive session -> prompts you to pick after printing the table
  #   - NULL + non-interactive (e.g. Rscript, knitr) -> falls back to row 1 (top AICc)
  #   - integer -> uses that row directly, no prompt (for reproducible/scripted runs)
  
  resp_expr <- if (log_transform) paste0("log(", response_var, ")") else response_var
  global_formula <- as.formula(paste(resp_expr, "~", paste(predictors, collapse = " * ")))
  
  cat("\n==================================================\n")
  cat("Response:", response_var, ifelse(log_transform, "(log-transformed)", ""), "\n")
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
  
  is_gaussian <- identical(family$family, "gaussian")
  # dredge() will only accept quasi-likelihood families with QAIC/QAICc, never AICc
  is_quasi <- grepl("^quasi", family$family)
  
  # FIX #1: dredge() requires na.action = na.fail on the fitted model, otherwise
  # it errors out immediately with "na.action argument is not set...". Passing it
  # directly (rather than setting options(na.action=...) globally) avoids leaking
  # a side effect into the caller's session.
  global_model <- if (is_gaussian) {
    lm(global_formula, data = data, na.action = na.fail)
  } else {
    glm(global_formula, data = data, family = family, na.action = na.fail)
  }
  
  cat("\nGlobal model df check: n =", nobs(global_model),
      " | params =", length(coef(global_model)), "\n")
  
  # ---- 2 & 3. dredge, ranked by AICc (or QAICc for quasi-families) ----
  # FIX #4: AICc is undefined for quasi-families; dredge() errors unless you
  # switch to QAICc and supply a chat/dispersion argument.
  if (is_quasi) {
    chat_val <- summary(global_model)$dispersion
    dredge_result <- dredge(global_model, rank = "QAICc", chat = chat_val)
  } else {
    dredge_result <- dredge(global_model, rank = "AICc")
  }
  cat("\n--- Model selection table (sorted by", ifelse(is_quasi, "QAICc", "AICc"), ") ---\n")
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
  cat("\nSelected model formula:", deparse(formula(best_model)), "\n")
  
  # ---- 4. Diagnostics ----
  cat("\n--- Diagnostics on best model ---\n")
  if (is_gaussian) {
    r <- residuals(best_model)
    # shapiro.test() errors above n = 5000, so subsample for the test itself;
    # the QQ plot below still uses the full residual vector.
    r_test <- if (length(r) > 5000) sample(r, 5000) else r
    sw <- shapiro.test(r_test)
    cat(sprintf("Shapiro-Wilk (n = %d%s): W = %.4f, p = %.4g\n",
                length(r_test), if (length(r) > 5000) ", subsampled" else "",
                sw$statistic, sw$p.value))
    qqnorm(r, main = paste("QQ:", response_var)); qqline(r, col = "red")
  } else {
    sim <- simulateResiduals(best_model, n = 1000)
    plot(sim, main = paste("DHARMa:", response_var))
    print(testDispersion(sim))
    print(testUniformity(sim))
  }
  
  # ---- 5. Permutation-based inference on best model's terms ----
  cat("\n--- Permutation inference on best model ---\n")
  perm_result <- NULL
  
  if (is_gaussian) {
    perm_result <- lmperm(formula(best_model), data = data, np = n_perm)
    print(summary(perm_result))
  } else {
    obs_anova <- car::Anova(best_model, type = anova_type)
    stat_col <- if ("LR Chisq" %in% colnames(obs_anova)) "LR Chisq" else "F value"
    term_names <- rownames(obs_anova)
    obs_vals <- obs_anova[[stat_col]]
    
    perm_stats <- matrix(NA, nrow = n_perm, ncol = length(term_names),
                         dimnames = list(NULL, term_names))
    
    for (i in seq_len(n_perm)) {
      data_perm <- data
      data_perm[[response_var]] <- sample(data_perm[[response_var]])
      perm_model <- tryCatch(update(best_model, data = data_perm), error = function(e) NULL)
      if (!is.null(perm_model)) {
        perm_anova <- tryCatch(car::Anova(perm_model, type = anova_type), error = function(e) NULL)
        if (!is.null(perm_anova)) {
          perm_stats[i, ] <- perm_anova[[stat_col]][match(term_names, rownames(perm_anova))]
        }
      }
    }
    
    # FIX #2: add the "+1" correction so p can never come out as exactly 0.
    # p = (# permuted >= observed + 1) / (# valid permutations + 1)
    p_perm <- sapply(seq_along(term_names), function(j) {
      valid <- perm_stats[, j][!is.na(perm_stats[, j])]
      (sum(valid >= obs_vals[j]) + 1) / (length(valid) + 1)
    })
    
    perm_result <- data.frame(term = term_names, observed_stat = obs_vals, p_permutation = p_perm)
    print(perm_result)
  }
  
  # ---- 6. emmeans on best model ----
  emm_list <- lapply(predictors, function(p) {
    if (p %in% all.vars(formula(best_model))) emmeans(best_model, as.formula(paste("~", p))) else NULL
  })
  names(emm_list) <- predictors
  
  list(
    dredge_table = dredge_result,
    best_model   = best_model,
    permutation  = perm_result,
    emmeans      = emm_list
  )
}

####Inter-Individual Distance####
allDist_v2 <- allDist_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None")
  )
res_dist <- run_response_analysis(allDist_v2, response_var = "meanDist", log_transform = FALSE)

emtrends(res_dist$best_model, ~ mem, var = "numPred")

emmeans(res_dist$best_model, ~ mem | terr)

emm <- emmeans(
  res_dist$best_model,
  ~ mem | terr,
  type = "response"
)

pairs(emm, adjust = "tukey")

####Component Size####
allSizes_v2 <- allSizes_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None")
  )
res_size <- run_response_analysis(allSizes_v2, response_var = "componentSize", log_transform = FALSE)
emmeans(res_size$best_model, ~ mem | terr)

# If diagnostics on the untransformed model still look non-normal, try:
#res_size_log <- run_response_analysis(allSizes_v2, response_var = "componentSize", log_transform = TRUE)

####Number of Components####
allComps_v2 <- allComps_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None")
  )
res_comps <- run_response_analysis(allComps_v2, response_var = "numComponents", log_transform = FALSE)

####Moran's I####
allMoranI <- allMoranI |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem  = relevel(as.factor(mem), ref = "None")
  )
res_moran <- run_response_analysis(allMoranI, response_var = "moranI", log_transform = FALSE)
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
