# ==============================================================================
# PILOT STUDY: RT AND ERROR-PROBABILITY ANALYSIS + FIGURE
# ==============================================================================
# Purpose:
# - Load combined pilot dataframe
# - Fit RT and error-probability GLMMs with sentence category as fixed effect
# - Include participant and item random intercepts
# - Print model summaries, response-scale emmeans, and pairwise comparisons
# - Visualize RT and error-rate results in the same violin-style format as the main study
# ==============================================================================

# ==============================================================================
# 0. LOAD LIBRARIES
# ==============================================================================
library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)
library(ggprism)
library(patchwork)

# svglite is used through svglite::svglite in ggsave().
# If needed, install once:
# install.packages("svglite")

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

pilot_dir <- "//nas.ads.mwn.de/ra38lap/MWN-PC/Desktop/EEG_Study/Pilot"
file_path <- file.path(pilot_dir, "combined_main_task_data.csv")

df_raw <- read.csv(file_path, stringsAsFactors = FALSE)

cat("\n--- RAW PILOT DATA ---\n")
cat("Rows: ", nrow(df_raw), "\n", sep = "")
cat("Columns: ", ncol(df_raw), "\n\n", sep = "")
print(names(df_raw))
cat("\n")
print(head(df_raw))

# ==============================================================================
# 2. PREPROCESSING
# ==============================================================================

# The uploaded pilot dataframe contains reaction_time values in seconds.
# The conditional check below keeps the script robust in case a future file is
# saved in milliseconds instead.
rt_median_raw <- median(df_raw$reaction_time, na.rm = TRUE)

if (rt_median_raw > 50) {
  cat("\nReaction time appears to be in milliseconds. Converting to seconds.\n")
  df_raw <- df_raw %>%
    mutate(RT_sec = reaction_time / 1000)
} else {
  cat("\nReaction time appears to be in seconds. Keeping as seconds.\n")
  df_raw <- df_raw %>%
    mutate(RT_sec = reaction_time)
}

df_analysis <- df_raw %>%
  mutate(
    participant_id = factor(participant_id),
    item_id = factor(item_id),
    condition = factor(condition, levels = c("Human", "AI", "Tool")),
    gender = factor(gender),
    Error_Flag = 1 - is_correct
  ) %>%
  drop_na(
    participant_id,
    item_id,
    condition,
    RT_sec,
    Error_Flag
  ) %>%
  filter(
    RT_sec > 0,
    Error_Flag %in% c(0, 1)
  )

cat("\n--- CLEANED PILOT ANALYSIS DATASET ---\n")
cat("Rows retained: ", nrow(df_analysis), "\n", sep = "")
cat("Participants retained: ", n_distinct(df_analysis$participant_id), "\n", sep = "")
cat("Items retained: ", n_distinct(df_analysis$item_id), "\n\n", sep = "")

cat("Trials per condition:\n")
print(table(df_analysis$condition, useNA = "ifany"))

cat("\nTrials per participant:\n")
print(table(df_analysis$participant_id))

cat("\nCorrectness by condition:\n")
print(table(df_analysis$is_correct, df_analysis$condition, useNA = "ifany"))

cat("\nRT summary in seconds:\n")
print(summary(df_analysis$RT_sec))

cat("\nNumber of RTs below 200 ms, inspected but not excluded from models:\n")
print(sum(df_analysis$RT_sec < 0.200, na.rm = TRUE))

# ==============================================================================
# 3. STATISTICAL MODELS
# ==============================================================================
# Because this is a small pilot sample, the models use random intercepts for
# participant and item. This is more stable than adding by-participant category
# slopes with only 10 participants.
#
# Human is the reference category because condition is coded as:
# c("Human", "AI", "Tool")

# ------------------------------------------------------------------------------
# A. RT MODEL: Gamma GLMM with log link
# ------------------------------------------------------------------------------
# 
# model_rt_pilot <- glmer(
#   RT_sec ~ condition +
#     (1 | participant_id) +
#     (1 | item_id),
#   data = df_analysis,
#   family = Gamma(link = "log"),
#   control = glmerControl(
#     optimizer = "bobyqa",
#     optCtrl = list(maxfun = 2e5)
#   )
# )
# 
# cat("\n============================\n")
# cat("PILOT RT MODEL: Gamma GLMM with log link\n")
# cat("============================\n")
# print(summary(model_rt_pilot))
# cat("\nRT model singular fit? ", isSingular(model_rt_pilot, tol = 1e-4), "\n", sep = "")
# 
# cat("\nRT model omnibus test for condition:\n")
# print(drop1(model_rt_pilot, test = "Chisq"))
# 
# rt_ci <- confint(model_rt_pilot, parm = "beta_", method = "Wald")
# 
# cat("\nRT MODEL: EXPONENTIATED FIXED EFFECTS + 95% WALD CIs\n")
# cat("Interpretation: ratios on the response-time scale.\n")
# print(exp(cbind(Estimate = fixef(model_rt_pilot), rt_ci)))
# 
# emm_rt_pilot <- emmeans(model_rt_pilot, ~ condition)
# 
# cat("\nRT MODEL: MODEL-ESTIMATED MEANS ON RESPONSE SCALE\n")
# print(summary(emm_rt_pilot, type = "response", infer = c(TRUE, TRUE)))
# 
# cat("\nRT MODEL: PAIRWISE CONTRASTS BETWEEN CONDITIONS\n")
# cat("Interpretation: RT ratios. Tukey-adjusted p-values.\n")
# pairs_rt_pilot <- pairs(emm_rt_pilot)
# print(summary(pairs_rt_pilot, type = "response", infer = c(TRUE, TRUE), adjust = "tukey"))

# ==============================================================================
# SIMPLIFIED PILOT RT MODEL: PARTICIPANT RANDOM INTERCEPT ONLY
# ==============================================================================

model_rt_pilot_simple <- glmer(
  RT_sec ~ condition + 
    (1 | participant_id),
  data = df_analysis,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

cat("\n============================\n")
cat("SIMPLIFIED PILOT RT MODEL: Gamma GLMM, participant random intercept only\n")
cat("============================\n")
print(summary(model_rt_pilot_simple))

cat("\nRT model singular fit? ", isSingular(model_rt_pilot_simple, tol = 1e-4), "\n", sep = "")

cat("\nRT model omnibus test for condition:\n")
print(drop1(model_rt_pilot_simple, test = "Chisq"))

rt_simple_ci <- confint(model_rt_pilot_simple, parm = "beta_", method = "Wald")

cat("\nRT MODEL: EXPONENTIATED FIXED EFFECTS + 95% WALD CIs\n")
print(exp(cbind(
  Estimate = fixef(model_rt_pilot_simple),
  rt_simple_ci
)))

emm_rt_pilot_simple <- emmeans(model_rt_pilot_simple, ~ condition)

cat("\nRT MODEL: MODEL-ESTIMATED MEANS ON RESPONSE SCALE\n")
print(summary(emm_rt_pilot_simple, type = "response", infer = c(TRUE, TRUE)))

cat("\nRT MODEL: PAIRWISE CONTRASTS BETWEEN CONDITIONS\n")
pairs_rt_pilot_simple <- pairs(emm_rt_pilot_simple)
print(summary(
  pairs_rt_pilot_simple,
  type = "response",
  infer = c(TRUE, TRUE),
  adjust = "tukey"
))


# ------------------------------------------------------------------------------
# B. ERROR-PROBABILITY MODEL: Binomial GLMM with logit link
# ------------------------------------------------------------------------------

model_error_pilot <- glmer(
  Error_Flag ~ condition +
    (1 | participant_id) +
    (1 | item_id),
  data = df_analysis,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

cat("\n============================\n")
cat("PILOT ERROR-PROBABILITY MODEL: Binomial GLMM with logit link\n")
cat("============================\n")
print(summary(model_error_pilot))
cat("\nError model singular fit? ", isSingular(model_error_pilot, tol = 1e-4), "\n", sep = "")

cat("\nError model omnibus test for condition:\n")
print(drop1(model_error_pilot, test = "Chisq"))

err_ci <- confint(model_error_pilot, parm = "beta_", method = "Wald")

cat("\nERROR MODEL: EXPONENTIATED FIXED EFFECTS + 95% WALD CIs\n")
cat("Interpretation: odds ratios.\n")
print(exp(cbind(Estimate = fixef(model_error_pilot), err_ci)))

emm_err_pilot <- emmeans(model_error_pilot, ~ condition)

cat("\nERROR MODEL: MODEL-ESTIMATED ERROR PROBABILITIES ON RESPONSE SCALE\n")
print(summary(emm_err_pilot, type = "response", infer = c(TRUE, TRUE)))

cat("\nERROR MODEL: PAIRWISE CONTRASTS BETWEEN CONDITIONS\n")
cat("Interpretation: odds ratios. Tukey-adjusted p-values.\n")
pairs_err_pilot <- pairs(emm_err_pilot)
print(summary(pairs_err_pilot, type = "response", infer = c(TRUE, TRUE), adjust = "tukey"))

# ==============================================================================
# 4. FIGURE: PILOT MODEL RESULTS
# ==============================================================================
# Same general style as the main-study figure:
# - RT panel: trial-level RT distribution, with model-estimated mean and 95% CI
# - Error panel: participant-level error-rate distribution, with model-estimated
#   error probability and 95% CI
#
# Important:
# - RT model estimates come from the simplified converged RT model:
#   model_rt_pilot_simple
# - Error model estimates come from:
#   model_error_pilot
# - RTs are trimmed for visualization only.
# - Error rates are computed from the full plotting dataset, not from RT-trimmed data.

# ------------------------------------------------------------------------------
# A. SETTINGS
# ------------------------------------------------------------------------------

plot_order <- c("Human", "AI", "Tool")

category_cols <- c(
  "AI"    = "#FFC4C4",
  "Human" = "lightgrey",
  "Tool"  = "#BFD8FF"
)

df_plot <- df_analysis %>%
  mutate(
    condition = factor(condition, levels = plot_order)
  )

# Recreate emmeans from the final models used for plotting
emm_rt_pilot_simple <- emmeans(model_rt_pilot_simple, ~ condition)
emm_err_pilot <- emmeans(model_error_pilot, ~ condition)

# ------------------------------------------------------------------------------
# B. RT DATA AND MODEL-BASED ESTIMATES
# ------------------------------------------------------------------------------

# Visualization-only RT trimming:
# - lower bound: 200 ms
# - upper bound: 97.5th percentile within each condition
df_plot_rt_trimmed <- df_plot %>%
  group_by(condition) %>%
  mutate(
    rt_lower_bound = 0.200,
    rt_upper_bound = quantile(RT_sec, probs = 0.975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(RT_sec > rt_lower_bound, RT_sec <= rt_upper_bound) %>%
  select(-rt_lower_bound, -rt_upper_bound)

cat(
  "\nRT visualization trimming: ",
  nrow(df_plot) - nrow(df_plot_rt_trimmed),
  " trials excluded (",
  round(100 * (nrow(df_plot) - nrow(df_plot_rt_trimmed)) / nrow(df_plot), 2),
  "%)\n",
  sep = ""
)

rt_violin_data <- df_plot_rt_trimmed %>%
  mutate(
    condition = factor(condition, levels = plot_order),
    RT_ms = RT_sec * 1000
  )

rt_emm_violin_df <- summary(
  emm_rt_pilot_simple,
  type = "response",
  infer = c(TRUE, FALSE)
) %>%
  as.data.frame() %>%
  transmute(
    condition = factor(condition, levels = plot_order),
    emmean_ms = response * 1000,
    lower_ms = asymp.LCL * 1000,
    upper_ms = asymp.UCL * 1000
  )

# ------------------------------------------------------------------------------
# C. ERROR-RATE DATA AND MODEL-BASED ESTIMATES
# ------------------------------------------------------------------------------

# Participant-level error rates per condition.
# These are computed from the full plotting dataset, not from RT-trimmed trials.
err_participant_rates <- df_plot %>%
  group_by(participant_id, condition) %>%
  summarise(
    participant_error_rate = mean(Error_Flag, na.rm = TRUE),
    .groups = "drop"
  )

err_violin_data <- err_participant_rates %>%
  mutate(
    condition = factor(condition, levels = plot_order),
    participant_error_rate_pct = participant_error_rate * 100
  )

err_emm_violin_df <- summary(
  emm_err_pilot,
  type = "response",
  infer = c(TRUE, FALSE)
) %>%
  as.data.frame() %>%
  transmute(
    condition = factor(condition, levels = plot_order),
    emmean_pct = prob * 100,
    lower_pct = asymp.LCL * 100,
    upper_pct = asymp.UCL * 100
  )

# ------------------------------------------------------------------------------
# D. DYNAMIC Y-AXIS LIMITS
# ------------------------------------------------------------------------------

rt_y_upper <- max(
  4000,
  ceiling(
    max(
      rt_violin_data$RT_ms,
      rt_emm_violin_df$upper_ms,
      na.rm = TRUE
    ) / 500
  ) * 500
)

err_y_upper <- max(
  25,
  ceiling(
    max(
      err_violin_data$participant_error_rate_pct,
      err_emm_violin_df$upper_pct,
      na.rm = TRUE
    ) / 5
  ) * 5
)

err_y_upper <- min(100, err_y_upper)

cat("\nFigure y-axis limits:\n")
cat("RT: 0 to ", rt_y_upper, " ms\n", sep = "")
cat("Error rate: 0 to ", err_y_upper, "%\n", sep = "")

# ------------------------------------------------------------------------------
# E. RT VIOLIN FIGURE
# ------------------------------------------------------------------------------

p_rt_violin <- ggplot() +
  geom_violin(
    data = rt_violin_data,
    aes(
      x = condition,
      y = RT_ms,
      fill = condition
    ),
    width = 0.75,
    alpha = 0.75,
    color = NA,
    trim = FALSE
  ) +
  geom_errorbar(
    data = rt_emm_violin_df,
    aes(
      x = condition,
      ymin = lower_ms,
      ymax = upper_ms
    ),
    width = 0.10,
    linewidth = 1.1,
    color = "black"
  ) +
  geom_point(
    data = rt_emm_violin_df,
    aes(
      x = condition,
      y = emmean_ms
    ),
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(values = category_cols) +
  scale_x_discrete(limits = plot_order) +
  labs(
    x = "Sentence Category",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, rt_y_upper)) +
  theme_prism() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    aspect.ratio = 1
  )

# ------------------------------------------------------------------------------
# F. ERROR-RATE VIOLIN FIGURE
# ------------------------------------------------------------------------------

p_err_violin <- ggplot() +
  geom_violin(
    data = err_violin_data,
    aes(
      x = condition,
      y = participant_error_rate_pct,
      fill = condition
    ),
    width = 0.75,
    alpha = 0.75,
    color = NA,
    trim = FALSE
  ) +
  geom_errorbar(
    data = err_emm_violin_df,
    aes(
      x = condition,
      ymin = lower_pct,
      ymax = upper_pct
    ),
    width = 0.10,
    linewidth = 1.1,
    color = "black"
  ) +
  geom_point(
    data = err_emm_violin_df,
    aes(
      x = condition,
      y = emmean_pct
    ),
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(values = category_cols) +
  scale_x_discrete(limits = plot_order) +
  labs(
    x = "Sentence Category",
    y = "Error Rate (%)"
  ) +
  coord_cartesian(ylim = c(0, err_y_upper)) +
  theme_prism() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    aspect.ratio = 1
  )

# ------------------------------------------------------------------------------
# G. COMBINED FIGURE
# ------------------------------------------------------------------------------

pilot_combined_figure <- (p_rt_violin + p_err_violin) &
  theme(
    plot.margin = margin(0, 0, 0, 0, unit = "pt")
  )

pilot_combined_figure <- pilot_combined_figure +
  plot_annotation(
    theme = theme(
      plot.margin = margin(0, 0, 0, 0, unit = "pt")
    )
  )

pilot_combined_figure

# ------------------------------------------------------------------------------
# H. SAVE FIGURE AS SVG
# ------------------------------------------------------------------------------

output_svg <- file.path(pilot_dir, "Pilot_RT_error_probability_figure.svg")

ggsave(
  filename = output_svg,
  plot = pilot_combined_figure,
  device = svglite::svglite,
  width = 24,
  height = 11,
  units = "cm"
)

cat("\nSaved pilot figure to:\n")
cat(output_svg, "\n")
