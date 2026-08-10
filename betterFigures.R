library(emmeans)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(broom)
library(sjPlot)
library(MASS)

####Inter-Individual Distance####
#Use allDist file, log transform mean_within_dist, set predictors as factors
allDist_v2 <- allDist_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem = relevel(as.factor(mem), ref = "None"),
    logDist = log(meanDist)
  )

#### USE LOG: Log transformed Distance####
#create full model using LM

m1_fullLOG <- lm(logDist ~ terr + mem + numPred, data = allDist_v2)
summary(m1_fullLOG)
plot(m1_fullLOG, which = 2)

#calc marginal means
emm_terr <- emmeans(m1_fullLOG, ~ terr)
emm_mem <- emmeans(m1_fullLOG, ~ mem)
emm_pred <- emmeans(m1_fullLOG, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Inter-Individual Distance (Logged)") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Inter-Individual Distance (Logged)") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Inter-Individual Distance (Logged)") +
  theme_minimal()

logDist <- plot_grid(terr, mem, numPred)

# Create a grid of numPred values, holding terr and mem at reference levels
pred_grid <- data.frame(
  numPred = seq(min(allDist_v2$numPred), max(allDist_v2$numPred), length.out = 100),
  terr    = factor("None", levels = levels(allDist_v2$terr)),
  mem     = factor("None",  levels = levels(allDist_v2$mem))
)

# Get predictions + standard error from the model
preds <- predict(m1_fullLOG, newdata = pred_grid, interval = "confidence", level = 0.95)
pred_grid <- cbind(pred_grid, preds)  # adds fit, lwr, upr columns

# Marginal effects plot
numPred_plot <- ggplot(pred_grid, aes(x = numPred, y = fit)) +
  geom_line(linewidth = 1, color = "blue") +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_jitter(data = allDist_v2, aes(x = numPred, y = logDist),  # raw data overlay
              width = 0.1, alpha = 0.3, size = 1.5, inherit.aes = FALSE) +
  labs(
    x     = "Number of Predators",
    y     = "Predicted Log Inter-Individual Distance",
    title = "Marginal Effect of Number of Predators on Inter-Individual Distance (Logged)"
  ) +
  theme_minimal()

#Boxplots of logDist
logDistBox <- ggplot(allDist_v2, aes(x = factor(numPred), y = logDist)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Log Transformed Inter-Individual Distance"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

##Plot Regression Coefficients##
coef_df <- tidy(m1_fullLOG, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred" = "Number of Predators (continuous)",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m1LOGcf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-0.75, 0.75, by = 0.1)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Logged Inter-Individiaul Distance"
  ) +
  theme_minimal()

####Not Logged Distance####
m1_full <- lm(meanDist ~ terr + mem + numPred, data = allDist_v2)
plot(m1_full)
#calc marginal means
emm_terr <- emmeans(m1_full, ~ terr)
emm_mem <- emmeans(m1_full, ~ mem)
emm_pred <- emmeans(m1_full, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Inter-Individual Distance") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Inter-Individual Distance") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Inter-Individual Distance") +
  theme_minimal()

Dist <- plot_grid(terr, mem, numPred)

#Boxplots of Dist
DistBox <- ggplot(allDist_v2, aes(x = factor(numPred), y = meanDist)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Inter-Individual Distance"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

coef_df <- tidy(m1_full, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m1cf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 1.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Inter-Individual Distance"
  ) +
  theme_minimal()
distCFs <- plot_grid(m1LOGcf, m1cf)
####Group Size####
#Use allSizes file, log transform compSize, set predictors as factors
allSizes_v2 <- allSizes_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem = relevel(as.factor(mem), ref = "None"),
    logSize = log(componentSize)
  )

####Log transformed group size####
#create full model using LM
m2_fullLOG <- lm(logSize ~ terr + mem, data = allSizes_v2)
summary(m2_fullLOG)
plot(m2_fullLOG, which = 2)
#calc marginal means
emm_terr <- emmeans(m2_fullLOG, ~ terr)
emm_mem <- emmeans(m2_fullLOG, ~ mem)
emm_pred <- emmeans(m2_fullLOG, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Group Size (Logged)") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Group Size (Logged)") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Group Size (Logged)") +
  theme_minimal()

logSize <- plot_grid(terr, mem, numPred)

#Boxplots of logSize
logSizeBox <- ggplot(allSizes_v2, aes(x = factor(numPred), y = logSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    y = "Group Size, Log Transformed",
    title = "Variation in Log Transformed Group Size"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

coef_df <- tidy(m2_fullLOG, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m2LOGcf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-1.75, 1.75, by = 0.1)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Group Size"
  ) +
  theme_minimal()

####STICK WITH THIS: Not Logged Group Sizes####
m2_full <- lm(componentSize ~ terr + mem, data = allSizes_v2)

#calc marginal means
emm_terr <- emmeans(m2_full, ~ terr)
emm_mem <- emmeans(m2_full, ~ mem)
emm_pred <- emmeans(m2_full, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Group Size") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Group Size") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Group Size") +
  theme_minimal()

Size <- plot_grid(terr, mem, numPred)

#Boxplots of Dist
SizeBox <- ggplot(allSizes_v2, aes(x = factor(numPred), y = componentSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Group Size"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

##Regression coefficient plots##
##pooppy head#

coef_df <- tidy(m2_full, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m2cf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Group Size"
  ) +
  theme_minimal()

sizeCFs <- plot_grid(m2LOGcf, m2cf)
####Group Size GLM: DO NOT USE####
m2_glm <- glm(componentSize ~ terr + mem, data = allSizes_v2, family = gaussian(link = "identity"))
plot(m2_glm, which = 2)
plot(m2_full, which  = 2)
stepAIC(m2_full)
stepAIC(m2_glm)
####Number of Groups####
#Use allGroups file, log transform num_components, set predictors as factors
allComps_v2 <- allComps_v2 |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem = relevel(as.factor(mem), ref = "None"),
    logNum = log(numComponents)
  )

##Log transformed version#####
#create full model using LM
m3_fullLOG <- lm(logNum ~ terr + mem, data = allComps_v2)
summary(m3_fullLOG)
#calc marginal means
emm_terr <- emmeans(m3_fullLOG, ~ terr)
emm_mem <- emmeans(m3_fullLOG, ~ mem)
emm_pred <- emmeans(m3_fullLOG, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Number of Groups (Logged)") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Number of Groups (Logged)") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Number of Groups (Logged)") +
  theme_minimal()

logNum <- plot_grid(terr, mem, numPred)

#Boxplots of logSize
logNumBox <- ggplot(allComps_v2, aes(x = factor(numPred), y = logNum)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    y = "Number of Groups, Log Transformed",
    title = "Variation in Log Transformed Number of Groups"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

coef_df <- tidy(m3_fullLOG, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m3LOGcf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-2, 2, by = 0.15)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Logged Number of Groups"
  ) +
  theme_minimal()
##Not Logged Group Number####
m3_full <- lm(numComponents ~ terr + mem, data = allComps_v2)
plot(m3_full, which = 2)
plot(m3_glm, which = 2)
#calc marginal means
emm_terr <- emmeans(m3_full, ~ terr)
emm_mem <- emmeans(m3_full, ~ mem)
emm_pred <- emmeans(m3_full, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Number of Groups") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Number of Groups") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Number of Groups") +
  theme_minimal()

Num <- plot_grid(terr, mem, numPred)

#Boxplots of Dist
NumBox <- ggplot(allComps_v2, aes(x = factor(numPred), y = numComponents)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Number of Groups"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

##Regression coefficient plots##

coef_df <- tidy(m3_full, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m3cf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-5, 5, by = 0.25)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Number of Groups"
  ) +
  theme_minimal()

numGroupcf <- plot_grid(m3LOGcf, m3cf)
#USE THIS: Group number glm####
m3_glm <- glm(numComponents ~ terr, data = allComps_v2, family = poisson(link = "log"))
plot(m3_glm, which = 2)


#calc marginal means
emm_terr <- emmeans(m3_glm, ~ terr)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
names(df_terr)
#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Number of Groups") +
  theme_minimal()

##Regression coefficient plots##

coef_df <- tidy(m3_glm, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey"
)
m3GLMcf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-5, 5, by = 0.25)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Number of Groups"
  ) +
  theme_minimal()

####Space Use####
#Use allMoranI file,
allMoranI <- allMoranI |>
  mutate(
    terr = relevel(as.factor(terr), ref = "None"),
    mem = relevel(as.factor(mem), ref = "None"),
    logMoranI = log(moranI)
  )

##Log transformed version####
#create full model using LM
m4_fullLOG <- lm(logMoranI ~ terr + mem + numPred, data = allMoranI)
summary(m4_fullLOG)
#calc marginal means
emm_terr <- emmeans(m4_fullLOG, ~ terr)
emm_mem <- emmeans(m4_fullLOG, ~ mem)
emm_pred <- emmeans(m4_fullLOG, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Moran's I (Logged)") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Moran's I (Logged)") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Moran's I (Logged)") +
  theme_minimal()

logMoran <- plot_grid(terr, mem, numPred)

#Boxplots of logDist
logMoranBox <- ggplot(allMoranI, aes(x = factor(numPred), y = logMoranI)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    y = "Moran's I, Log Transformed",
    title = "Vairation in Log Transformed Moran's I"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

coef_df <- tidy(m4_fullLOG, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m4LOGcf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-5, 5, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Logged Moran's I"
  ) +
  theme_minimal()
##USE THIS Not Logged Morans I#####
m4_full <- lm(moranI ~ terr + mem + numPred, data = allMoranI)
summary(m4_full)
#calc marginal means
emm_terr <- emmeans(m4_full, ~ terr)
emm_mem <- emmeans(m4_full, ~ mem)
emm_pred <- emmeans(m4_full, ~ numPred)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)
df_pred <- as.data.frame(emm_pred)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Moran's I") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Moran's I") +
  theme_minimal()

numPred <- ggplot(df_pred, aes(x = numPred, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Number of Predators") +
  ggtitle("Effects of Predator Number on Moran's I") +
  theme_minimal()

Moran <- plot_grid(terr, mem, numPred)

#Boxplots of Dist
MoranBox <- ggplot(allMoranI, aes(x = factor(numPred), y = moranI)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Moran's I"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

##Regression coefficient plots##

coef_df <- tidy(m4_full, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred1" = "Number of Predators: 1",
                       "numPred2" = "Number of Predators: 2",
                       "numPred3" = "Number of Predators: 3",
                       "numPred4" = "Number of Predators: 4",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
m4cf <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-2, 2, by = 0.15)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Moran's I"
  ) +
  theme_minimal()

moranCF <- plot_grid(m4LOGcf, m4cf)

##Morans I GLMS####
plot(m4_full, which = 2)
plot(m4_fullLOG, which = 2)
m4GLM <- glm(moranI ~ terr + mem + numPred, data = allMoranI, family = poisson(link = "log"))
plot(m4GLM, which = 2)
####stepAIC Model Testing####
m1logTest <- stepAIC(m1_fullLOG, scope = .~., direction = "both")

m2Test <- stepAIC(m2_full, scope = .~., direction = "both")

m3Test <- stepAIC(m3_glm, scope = .~., direction = "both")

m4Test <- stepAIC(m4_full, scope = .~., direction = "both")

####Model Re-Runs Based on AIC####
##Re run of log(grouSize) ~ terr + mem
m2LOG_v2 <- lm(logSize ~ terr + mem, data = allSizes_v2)
#calc marginal means
emm_terr <- emmeans(m2LOG_v2, ~ terr)
emm_mem <- emmeans(m2LOG_v2, ~ mem)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Group Size (Logged)") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Group Size (Logged)") +
  theme_minimal()

logSize_v2 <- plot_grid(terr, mem)

coef_df <- tidy(m2LOG_v2, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       )
m2LOGcf_v2 <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Logged Group Size"
  ) +
  theme_minimal()

##Re run of groupSize ~ terr + mem
m2Full_v2 <- lm(componentSize ~ terr + mem, data = allSizes_v2)

#calc marginal means
emm_terr <- emmeans(m2Full_v2, ~ terr)
emm_mem <- emmeans(m2Full_v2, ~ mem)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Group Size") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Group Size") +
  theme_minimal()

Size_v2 <- plot_grid(terr, mem)

coef_df <- tidy(m2Full_v2, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
)
m2FULLcf_v2 <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Group Size"
  ) +
  theme_minimal()

##Re run of log(#ofGroups) ~ terr
m3LOG_v2 <- lm(logNum ~ terr, data = allComps_v2)

#calc marginal means
emm_terr <- emmeans(m3LOG_v2, ~ terr)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Number of Groups") +
  theme_minimal()

coef_df <- tidy(m3LOG_v2, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       )
m3LOGcf_v2 <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Logged Number of Groups"
  ) +
  theme_minimal()

##Re run of #ofGroups ~ terr + mem
m3Full_v2 <- lm(numComponents ~ terr + mem, data = allComps_v2)

#calc marginal means
emm_terr <- emmeans(m3Full_v2, ~ terr)
emm_mem <- emmeans(m3Full_v2, ~ mem)

#Convert means to dataframes to plot using ggplot
df_terr <- as.data.frame(emm_terr)
df_mem <- as.data.frame(emm_mem)

#Plot using ggplot, combine ggplots using cowplot
terr <- ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Number of Groups") +
  theme_minimal()

mem <- ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Number of Groups") +
  theme_minimal()

Num_v2 <- plot_grid(terr, mem)

coef_df <- tidy(m3Full_v2, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
)
m3FULLcf_v2 <- ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates with 95% Confidence Intervals of Number of Groups"
  ) +
  theme_minimal()


#Plots####

#Fig 1 (log(dist) model QQ plot)
df <- data.frame(resid = residuals(m1_fullLOG))

ggplot(df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal(base_size = 10) +
  labs(title = "Log Transformed Prey Agent Inter-Individual Distance, Linear Model Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig1.png", width = 7, height = 5, dpi = 300)
#Fig 2 (group num glm QQ plot)
df <- data.frame(resid = residuals(m3_glm))

ggplot(df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal(base_size = 10) +
  labs(title = "Number of Prey Agent Groups, Generalized Linear Model (Poisson Distribution) Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig2.png", width = 7, height = 5, dpi = 300)
#Fig 3 (group size QQ plot)
df <- data.frame(resid = residuals(m2_full))

ggplot(df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal(base_size = 10) +
  labs(title = "Prey Agent Group Size, Linear Model Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig3.png", width = 7, height = 5, dpi = 300)
#Fig 4 (Morans I QQ plot)
df <- data.frame(resid = residuals(m4_full))

ggplot(df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal(base_size = 10) +
  labs(title = "Prey Agent Moran's I, Linear Model Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig4.png", width = 7, height = 5, dpi = 300)
#Fig 5 (reg dist boxplot)
ggplot(allDist_v2, aes(x = factor(numPred), y = meanDist)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Prey Agent Inter-Individual Distance"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    base_size = 10
  )
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig5.png", width = 7, height = 5, dpi = 300)
#Fig 6 (log(dist) boxplot)
ggplot(allDist_v2, aes(x = factor(numPred), y = logDist)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Log Transformed Prey Agent Inter-Individual Distance"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    base_size = 10
  )
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig6.png", width = 7, height = 5, dpi = 300)
#Fig 7 (log(dist) LM coefs)
coef_df <- tidy(m1_fullLOG, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred" = "Number of Predators (continuous)",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates of Logged Prey Agent Inter-Individual Distance"
  ) +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig7.png", width = 7, height = 5, dpi = 300)
#Fig 8 (log(dist) terr EMMs)
emm_terr <- emmeans(m1_fullLOG, ~ terr)
df_terr <- as.data.frame(emm_terr)
ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Prey Agent Inter-Individual Distance (Logged)") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig8.png", width = 7, height = 5, dpi = 300)

#Fig 9 (log(dist) mem EMMs)
emm_mem <- emmeans(m1_fullLOG, ~ mem)
df_mem <- as.data.frame(emm_mem)
ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Prey Agent Inter-Individual Distance (Logged)") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig9.png", width = 7, height = 5, dpi = 300)

#Fig 10 (log(dist) numPred EMMs)
# Create a grid of numPred values, holding terr and mem at reference levels
pred_grid <- data.frame(
  numPred = seq(min(allDist_v2$numPred), max(allDist_v2$numPred), length.out = 100),
  terr    = factor("None", levels = levels(allDist_v2$terr)),
  mem     = factor("None",  levels = levels(allDist_v2$mem))
)

# Get predictions + standard error from the model
preds <- predict(m1_fullLOG, newdata = pred_grid, interval = "confidence", level = 0.95)
pred_grid <- cbind(pred_grid, preds)  # adds fit, lwr, upr columns

# Marginal effects plot
ggplot(pred_grid, aes(x = numPred, y = fit)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_jitter(data = allDist_v2, aes(x = numPred, y = logDist),  # raw data overlay
              width = 0.1, alpha = 0.3, size = 1, inherit.aes = FALSE) +
  labs(
    x     = "Number of Predators",
    y     = "Predicted Log Inter-Individual Distance",
    title = "Marginal Effect of Number of Predators on Prey Agent Inter-Individual Distance (Logged)"
  ) +
  theme_minimal()
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig10.png", width = 7, height = 5, dpi = 300)

#Fig 11 (numGroups boxplot)
ggplot(allComps_v2, aes(x = factor(numPred), y = numComponents)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Number of Prey Agent Groups"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    base_size = 10
  )
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig11.png", width = 7, height = 5, dpi = 300)

#Fig 12 (numGroups glm coefs)
coef_df <- tidy(m3_glm, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey"
)
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates of Number of Prey Agent Groups"
  ) +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig12.png", width = 7, height = 5, dpi = 300)

#Fig 13 (numGrooups terr EMMs)
emm_terr <- emmeans(m3_glm, ~ terr)
df_terr <- as.data.frame(emm_terr)
ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Number of Prey Agent Groups") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig13.png", width = 7, height = 5, dpi = 300)

#Fig 14 (groupSize boxplot)
ggplot(allSizes_v2, aes(x = factor(numPred), y = componentSize)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Prey Agent Group Size"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    base_size = 10
  )
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig14.png", width = 7, height = 5, dpi = 300)

#Fig 15 (groupSize coefs)
coef_df <- tidy(m2_full, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(-6, 6, by = 0.5)) +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates of Prey Agent Group Size"
  ) +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig15.png", width = 7, height = 5, dpi = 300)

#Fig 16 (groupSize terr EMMs)
emm_terr <- emmeans(m2_full, ~ terr)
df_terr <- as.data.frame(emm_terr)
ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Prey Agent Group Size") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig16.png", width = 7, height = 5, dpi = 300)

#Fig 17 (groupSize mem EMMs)
emm_mem <- emmeans(m2_full, ~ mem)
df_mem <- as.data.frame(emm_mem)
ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Prey Agent Group Size") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig17.png", width = 7, height = 5, dpi = 300)

#Fig 18 (Morans I boxplot)
ggplot(allMoranI, aes(x = factor(numPred), y = moranI)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +  # adds individual points
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
    title = "Variation in Prey Agent Moran's I"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    base_size = 10
  )
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig18.png", width = 7, height = 5, dpi = 300)

#Fig 19 (Morans I coefs)
coef_df <- tidy(m4_full, conf.int = TRUE)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df$term <- factor(coef_df$term, levels = rev(coef_df$term))
coef_df$term <- recode(coef_df$term,
                       "terrNone" = "Territory: None",
                       "terrPred" = "Territory: Predator",
                       "terrPrey" = "Territory: Prey",
                       "memIndividual" = "Memory: Individual",
                       "memNone" = "Memory: None",
                       "memShared" = "Memory: Shared",
                       "numPred" = "Number of Predators (continuous)",
                       "(Intercept)" = "Baseline: Territory = None, Memory = None, 
                       Number of Predators = 1"
)
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient estimate (log scale)",
    y = "Predictor",
    title = "Predictor Estimates of Prey Agent Moran's I"
  ) +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig19.png", width = 7, height = 5, dpi = 300)

#Fig 20 (Morans I terr EMMs)
emm_terr <- emmeans(m4_full, ~ terr)
df_terr <- as.data.frame(emm_terr)
ggplot(df_terr, aes(x = terr, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Territory Type") +
  ggtitle("Effects of Territory Type on Prey Agent Moran's I") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig20.png", width = 7, height = 5, dpi = 300)

#Fig 21 (Morans I mem EMMs)
emm_mem <- emmeans(m4_full, ~ mem)
df_mem <- as.data.frame(emm_mem)
ggplot(df_mem, aes(x = mem, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "EMM", x = "Memory Type") +
  ggtitle("Effects of Memory Type on Prey Agent Moran's I") +
  theme_minimal(base_size = 10)
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig21.png", width = 7, height = 5, dpi = 300)

#Fig 22 (Morans I numPred EMMs)
# Create a grid of numPred values, holding terr and mem at reference levels
pred_grid <- data.frame(
  numPred = seq(min(allMoranI$numPred), max(allMoranI$numPred), length.out = 100),
  terr    = factor("None", levels = levels(allMoranI$terr)),
  mem     = factor("None",  levels = levels(allMoranI$mem))
)

# Get predictions + standard error from the model
preds <- predict(m4_full, newdata = pred_grid, interval = "confidence", level = 0.95)
pred_grid <- cbind(pred_grid, preds)  # adds fit, lwr, upr columns

# Marginal effects plot
ggplot(pred_grid, aes(x = numPred, y = fit)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_jitter(data = allMoranI, aes(x = numPred, y = moranI),  # raw data overlay
              width = 0.1, alpha = 0.3, size = 1, inherit.aes = FALSE) +
  labs(
    x     = "Number of Predators",
    y     = "Predicted Moran's I",
    title = "Marginal Effect of Number of Predators on Prey Agent Moran's I"
  ) +
  theme_minimal()
ggsave("C:/Users/Jawor/Desktop/R_repos/PredatorABMAnalysis/newPlots/fig22.png", width = 7, height = 5, dpi = 300)


stats <- allMoranI |>
  filter(numPred == 4) |>
  drop_na(moranI)
mean(stats$moranI)

