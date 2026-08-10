#Models for all#
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(report)
library(modelsummary)
library(gt)
library(broom.mixed)
library(MASS)
library(MuMIn)
library(effects)
library(emmeans)
library(multcomp)
library(marginaleffects)
library(plotMElm)
library(sjPlot)
library(ggeffects)

#Distance Models#
NTM_compDists <- NTM_compDists |>
  mutate(terr = "None", mem = "Individual")

NTNM_compDists <- NTNM_compDists |>
  mutate(terr = "None", mem = "None")

NTSM_compDists <- NTSM_compDists |>
  mutate(terr = "None", mem = "Shared")

PDTM_compDists <- PDTM_compDists |>
  mutate(terr = "Pred", mem = "Individual")

PDTNM_compDists <- PDTNM_compDists |>
  mutate(terr = "Pred", mem = "None")

PDTSM_compDists <- PDTSM_compDists |>
  mutate(terr = "Pred", mem = "Shared")

PYTM_compDists <- PYTM_compDists |>
  mutate(terr = "Prey", mem = "Individual")

PYTNM_compDists <- PYTNM_compDists |>
  mutate(terr = "Prey", mem = "None")

PYTSM_compDists <- PYTSM_compDists |>
  mutate(terr = "Prey", mem = "Shared")

allDist <- bind_rows(NTM_compDists, NTNM_compDists, NTSM_compDists, 
                     PDTM_compDists, PDTNM_compDists, PDTSM_compDists,
                     PYTM_compDists, PYTNM_compDists, PYTSM_compDists)
write_csv(allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allDist.csv")
allDist <- allDist |>
  mutate(
    terr = as.factor(terr),
    mem = as.factor(mem),
    numPred = as.factor(numPred),
    logDist = log(mean_within_dist)
  ) |>
  filter(tick == 5000)

m1 <- lm(logDist ~ terr + mem, data = allDist)
summary(m1_full)
m1_full <- lm(logDist ~ terr + mem + numPred, data = allDist, na.action = na.fail)
m1_s <- stepAIC(m1_full, scope = .~., direction = "both")
m1_n <- stepAIC(m1_null, scope - .~., direction = "both")
steps_df <- as.data.frame(m1_s$anova)
gt(steps_df)

#Marginal effects plot

preds <- predictions(
  m1_full,
  newdata = datagrid(
    terr = unique(allDist$terr),
    mem = unique(allDist$mem),
    numPred = unique(allDist$numPred)
  )
)

preds$terr <- factor(preds$terr, levels = c("None", "Pred", "Prey"))
preds$mem  <- factor(preds$mem, levels = c("None", "Individual", "Shared"))

ggplot(preds, aes(x = terr, y = estimate, color = mem)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.3),
                width = 0.2) +
  facet_wrap(~numPred) +
  labs(y = "Logged Inter-Individuaual Distance", x = "Territory Type") +
  ggtitle("Marginal Effects of Predictor Variables on Logged Inter-Individual Distance")
  theme_minimal()


plot_predictions(m1_full, condition = "numPred")

comparisons(m1_full, variables = "terr")
comparisons(m1_full, variables = "mem")
comparisons(m1_full, variables = "terr", by = "mem")

x <- confint(m1_full)
plot(x)
effm1 <- allEffects(m1_full)
plot(effm1)
emm <- emmeans(m1_full, ~ mem)
plot(emm, comparisons = TRUE)

plot(ggpredict(m1_full, terms = c("terr", "mem", "numPred")))

eff_terr <- ggpredict(m1_full, terms = "terr")
eff_mem  <- ggpredict(m1_full, terms = "mem")
eff_pred <- ggpredict(m1_full, terms = "numPred")

plot(eff_terr) + ggtitle("Effect of terr on logDist")
plot(eff_mem)  + ggtitle("Effect of mem on logDist")
plot(eff_pred) + ggtitle("Effect of numPred on logDist")

z <- emmeans(m1_full, pairwise ~ terr)
plot(z)
emmeans(m1_full, pairwise ~ mem)
emmeans(m1_full, pairwise ~ numPred)

a <-summary(glht(m1_full, linfct = mcp(terr = "Tukey")))
summary(glht(m1_full, linfct = mcp(mem = "Tukey")))
summary(glht(m1_full, linfct = mcp(numPred = "Tukey")))

terr_glht <- glht(m1_full, linfct = mcp(terr = "Tukey"))
mem_glht  <- glht(m1_full, linfct = mcp(mem = "Tukey"))
num_glht  <- glht(m1_full, linfct = mcp(numPred = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory of Dist (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for MEM of Dist (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for numPred of Dist (Tukey-adjusted)")
gt_num

write_csv(m1df, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allDistModel.csv")
m1AV <- anova(m1, type = 2)
write_csv(m1AV, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allDistANOVA.csv")
report_table(m1)

fixed_tbl <- broom.mixed::tidy(m1_full, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "terrPred" = "Territory: Predator",
                  "terrPrey" = "Territory: Prey",
                  "memNone" = "Memory: None",
                  "memShared" = "Memory: Shared",
                  "numPred2" = "2 Predators", 
                  "numPred3" = "3 Predators",
                  "numPred4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low,3), ", ", round(conf.high,3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  select(Term, Estimate, `95% CI`, statistic, p)

x <- fixed_tbl %>%
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
gof <- performance::model_performance(m1) |> 
  select(AIC, BIC, R2_marginal, R2_conditional, RMSE)

gt_fixed <- fixed_tbl %>% gt()
gt_gof   <- gof %>% gt()

gt_fixed %>% 
  tab_spanner(label = "Goodness of Fit", columns = everything())  # optional

#GroupSize Models#
NTM_compSizes <- NTM_compSizes |>
  mutate(terr = "None", mem = "Individual")

NTNM_compSizes <- NTNM_compSizes |>
  mutate(terr = "None", mem = "None")

NTSM_compSizes <- NTSM_compSizes |>
  mutate(terr = "None", mem = "Shared")

PDTM_compSizes <- PDTM_compSizes |>
  mutate(terr = "Pred", mem = "Individual")

PDTNM_compSizes <- PDTNM_compSizes |>
  mutate(terr = "Pred", mem = "None")

PDTSM_compSizes <- PDTSM_compSizes |>
  mutate(terr = "Pred", mem = "Shared")

PYTM_compSizes <- PYTM_compSizes |>
  mutate(terr = "Prey", mem = "Individual")

PYTNM_compSizes <- PYTNM_compSizes |>
  mutate(terr = "Prey", mem = "None")

PYTSM_compSizes <- PYTSM_compSizes |>
  mutate(terr = "Prey", mem = "Shared")

allSizes <- bind_rows(NTM_compSizes, NTNM_compSizes, NTSM_compSizes, 
                      PDTM_compSizes, PDTNM_compSizes, PDTSM_compSizes,
                      PYTM_compSizes, PYTNM_compSizes, PYTSM_compSizes)
write_csv(allSizes, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allSizes.csv")
allSizes <- allSizes |>
  mutate(
    terr = as.factor(terr),
    mem = as.factor(mem),
    numPred = as.factor(numPred)
  ) |>
  filter(tick == 5000)
m2 <- lm(compSize ~ terr + mem + numPred, data = allSizes)
summary(m2)
m2_full <- lm(compSize ~ terr + mem + numPred, data = allSizes)
m2_s <- stepAIC(m2_full, scope = .~., direction = "both")

m2df <- tidy(m2, effects = "fixed", conf.int = TRUE)
write_csv(m2df, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allSizesModel.csv")
m2AV <- anova(m2, type = 2)
write_csv(m2AV, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allSizesANOVA.csv")
plot(m2AV)

terr_glht <- glht(m2_full, linfct = mcp(terr = "Tukey"))
mem_glht  <- glht(m2_full, linfct = mcp(mem = "Tukey"))
num_glht  <- glht(m2_full, linfct = mcp(numPred = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory Group Size (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for MEM Group Size (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for numPred Group Size (Tukey-adjusted)")
gt_num


report_table(m2)
fixed_tbl <- broom.mixed::tidy(m2, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "terrPred" = "Territory: Predator",
                  "terrPrey" = "Territory: Prey",
                  "memNone" = "Memory: None",
                  "memShared" = "Memory: Shared",
                  "numPred2" = "2 Predators", 
                  "numPred3" = "3 Predators",
                  "numPred4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low,3), ", ", round(conf.high,3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  select(Term, Estimate, `95% CI`, statistic, p)

x <- fixed_tbl %>%
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


#NumGroups Models#
NTM_compSummary <- NTM_compSummary |>
  mutate(terr = "None", mem = "Individual")

NTNM_compSummary <- NTNM_compSummary |>
  mutate(terr = "None", mem = "None")

NTSM_compSummary <- NTSM_compSummary |>
  mutate(terr = "None", mem = "Shared")

PDTM_compSummary <- PDTM_compSummary |>
  mutate(terr = "Pred", mem = "Individual")

PDTNM_compSummary <- PDTNM_compSummary |>
  mutate(terr = "Pred", mem = "None")

PDTSM_compSummary <- PDTSM_compSummary |>
  mutate(terr = "Pred", mem = "Shared")

PYTM_compSummary <- PYTM_compSummary |>
  mutate(terr = "Prey", mem = "Individual")

PYTNM_compSummary <- PYTNM_compSummary |>
  mutate(terr = "Prey", mem = "None")

PYTSM_compSummary <- PYTSM_compSummary |>
  mutate(terr = "Prey", mem = "Shared")

allGroups <- bind_rows(NTM_compSummary, NTNM_compSummary, NTSM_compSummary, 
                      PDTM_compSummary, PDTNM_compSummary, PDTSM_compSummary,
                      PYTM_compSummary, PYTNM_compSummary, PYTSM_compSummary)
write_csv(allGroups, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allGroups.csv")
allGroups <- allGroups |>
  mutate(
    terr = as.factor(terr),
    mem = as.factor(mem),
    numPred = as.factor(numPred)
  ) |>
  filter(tick == 5000)

m3 <- lmer(num_components ~ terr + mem + numPred + (1|behaviorSpaceRun), data = allGroups)
summary(m3)

m3_full <- lm(num_components ~ terr + mem + numPred, data = allGroups)
m3_s <- stepAIC(m3_full, scope = .~., direction = "both")

m3df <- tidy(m3, effects = "fixed", conf.int = TRUE)
write_csv(m3df, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allGroupsModel.csv")
m3AV <- anova(m3, type = 2)
write_csv(m3AV, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//compAnalysis//allGroupsANOVA.csv")

terr_glht <- glht(m3_full, linfct = mcp(terr = "Tukey"))
mem_glht  <- glht(m3_full, linfct = mcp(mem = "Tukey"))
num_glht  <- glht(m3_full, linfct = mcp(numPred = "Tukey"))

terr_tbl <- tidy_glht(terr_glht)
mem_tbl  <- tidy_glht(mem_glht)
num_tbl  <- tidy_glht(num_glht)

gt_terr <- make_gt(terr_tbl, "Pairwise Comparisons for Territory Number of Groups (Tukey-adjusted)")
gt_terr

gt_mem <- make_gt(mem_tbl, "Pairwise Comparisons for MEM Number of Groups (Tukey-adjusted)")
gt_mem

gt_num <- make_gt(num_tbl, "Pairwise Comparisons for numPred Number of Groups (Tukey-adjusted)")
gt_num

report_table(m3)
fixed_tbl <- broom.mixed::tidy(m3_full, effects = "fixed", conf.int = TRUE) %>%
  mutate(
    Term = recode(term,
                  "(Intercept)" = "Intercept",
                  "terrPred" = "Territory: Predator",
                  "terrPrey" = "Territory: Prey",
                  "memNone" = "Memory: None",
                  "memShared" = "Memory: Shared",
                  "numPred2" = "2 Predators", 
                  "numPred3" = "3 Predators",
                  "numPred4" = "4 Predators"
    ),
    Estimate = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low,3), ", ", round(conf.high,3), "]"),
    p = scales::pvalue(p.value)
  ) %>%
  select(Term, Estimate, `95% CI`, statistic, p)

x <- fixed_tbl %>%
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


inputTable <- gt(inputs) |>
  tab_header(
    title = "Model Inputs"
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "grey",
      weight = px (1.5),
      style = "solid"),
    locations = cells_body()
    )

