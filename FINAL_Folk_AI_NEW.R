# ==============================================================================
# 0. LOAD LIBRARIES
# ==============================================================================
library(tidyverse)    # For data manipulation and piping
library(lme4)         # For mixed-effects modeling
library(emmeans)      # For estimated marginal means and post-hocs
library(ggplot2)      # For data visualization
library(ggprism)      # For scientific publication themes
library(patchwork)    # For side-by-side plot alignment
library(ggsignif)     # For drawing significance brackets

# ==============================================================================
# 1. LOAD DATA 
# ==============================================================================
# Update the path to where your file is located
file_path <- "/Users/mustafacvbe/Downloads/Folk_AI_Full_data.csv"
df_full_data <- read.csv(file_path, stringsAsFactors = FALSE)

# ==============================================================================
# 2. PREPROCESSING: FACTORS, BASELINES, AND SCALING
# ==============================================================================
df_full_data <- df_full_data %>%
  mutate(
    # Set sentence category as a factor with Human as the baseline reference
    Sentence_Category = as.factor(Sentence_Category),
    Sentence_Category = relevel(Sentence_Category, ref = "Human"),
    
    # Convert RT from milliseconds to seconds (Highly recommended for Gamma GLMMs)
    RT_sec = Reaction_Time / 1000,
    
    # Create explicit Error column (1 = Error, 0 = Correct) for binomial modeling
    Error_Flag = 1 - Is_Correct 
  )

# ==============================================================================
# 3. INITIAL DATA EXCLUSION: FAILED PRACTICE OUTCOMES
# ==============================================================================
nrow_before <- nrow(df_full_data)

# Filter out participants who failed both practice rounds
df_cleaned <- df_full_data %>%
  filter(Practice_Outcome != "Failed_Both")

nrow_after <- nrow(df_cleaned)
rows_deleted <- nrow_before - nrow_after
participants_excluded <- rows_deleted / 66

# Print exact exclusion statistics to the console for the manuscript
cat("\n--- EXCLUSION REPORT (PRACTICE FAILURES) ---\n")
cat("Total rows deleted: ", rows_deleted, "\n")
cat("Total participants excluded: ", participants_excluded, "\n\n")

# ==============================================================================
# 4. INITIAL MODELING (PRE-TRIMMING)
# ==============================================================================

# A. Initial Reaction Time Model (Gamma distribution, log link)
model_rt_initial <- glmer(RT_sec ~ Sentence_Category + (1 + Sentence_Category | Participant_Private_ID), 
                          data = df_cleaned, 
                          family = Gamma(link = "log"))

cat("\n=== INITIAL REACTION TIME MODEL SUMMARY ===\n")
print(summary(model_rt_initial))

# Pre-registered error model with random intercept and slopes which results in 
# convergence failure, hence simplified model chosen. 
# Uncomment and run to see it for yourself:
# model_error_preregistered <- glmer(Error_Flag ~ Sentence_Category + (1 + Sentence_Category | Participant_Private_ID), 
#                              data = df_cleaned, 
#                              family = binomial(link = "logit"))

# B. Initial Error Probability Model (Intercept-Only Random Effects due to convergence issues)
model_error_initial <- glmer(Error_Flag ~ Sentence_Category + (1 +Sentence_Category | Participant_Private_ID), 
                             data = df_cleaned, 
                             family = binomial(link = "logit"))

cat("\n=== INITIAL ERROR PROBABILITY MODEL SUMMARY ===\n")
print(summary(model_error_initial))

# ==============================================================================
# 5. DATA PREPARATION FOR FINAL ANALYSIS (TRIMMING & STEM IDs)
# ==============================================================================

# Calculate global mean and standard deviation for Reaction Time
group_mean_rt <- mean(df_cleaned$Reaction_Time, na.rm = TRUE)
group_sd_rt <- sd(df_cleaned$Reaction_Time, na.rm = TRUE)

# Define upper threshold (Mean + 2.5 Standard Deviations)
upper_limit <- group_mean_rt + (3 * group_sd_rt)

# Create the final trimmed dataset and extract Stem IDs in one clean pipeline
df_trimmed <- df_cleaned %>%
  filter(
    Reaction_Time >= 200,          # Excludes impossible anticipatory speeds (< 200ms)
    Reaction_Time < upper_limit    # Excludes extreme attentional lapses (>= 2.5 SDs)
  ) %>%
  mutate(
    # Create a unique ID (1 to 22) for the sentence stems based on Sentence_ID
    Stem_ID = as.factor(ceiling(Sentence_ID / 3))
  )

cat("\n--- OUTLIER EXCLUSION REPORT ---\n")
cat("Trials excluded due to extreme RT: ", nrow(df_cleaned) - nrow(df_trimmed), "\n\n")

# ==============================================================================
# 6. FINAL MODELS (SUBJECT + ITEM CROSSED RANDOM EFFECTS)
# ==============================================================================

# ---------------------------------------------------------
# A. Final Reaction Time Model 
# ---------------------------------------------------------
# Adds (1 | Stem_ID) to calculate a unique baseline speed for each of the 22 stems
model_rt_final <- glmer(RT_sec ~ Sentence_Category + 
                          (1 + Sentence_Category | Participant_Private_ID) + 
                          (1 | Stem_ID), 
                        data = df_trimmed, 
                        family = Gamma(link = "log"))

cat("\n=== FINAL CROSSED RT MODEL SUMMARY ===\n")
print(summary(model_rt_final))

# ---------------------------------------------------------
# B. Final Error Probability Model
# ---------------------------------------------------------
# Adds (1 | Stem_ID) to calculate a unique baseline error risk for each of the 22 stems
model_error_final <- glmer(Error_Flag ~ Sentence_Category + 
                             (1 | Participant_Private_ID) + 
                             (1 | Stem_ID), 
                           data = df_trimmed, 
                           family = binomial(link = "logit"))

cat("\n=== FINAL CROSSED ERROR MODEL SUMMARY ===\n")
print(summary(model_error_final))

# ==============================================================================
# 7. POST-HOC COMPARISONS (ESTIMATED MARGINAL MEANS)
# ==============================================================================

# A. Reaction Time Post-Hocs (Back-transformed to Seconds)
cat("\n=== REACTION TIME: ESTIMATED MARGINAL MEANS (SECONDS) ===\n")
emmeans_rt_final <- emmeans(model_rt_final, specs = ~ Sentence_Category, type = "response")
print(emmeans_rt_final)

cat("\n=== REACTION TIME: PAIRWISE COMPARISONS (TUKEY ADJUSTED) ===\n")
pairs_rt_final <- pairs(emmeans_rt_final)
print(pairs_rt_final)


# B. Error Probability Post-Hocs (Back-transformed to Probabilities)
cat("\n=== ERROR PROBABILITY: ESTIMATED MARGINAL MEANS (PROBABILITY) ===\n")
emmeans_error_final <- emmeans(model_error_final, specs = ~ Sentence_Category, type = "response")
print(emmeans_error_final)

cat("\n=== ERROR PROBABILITY: PAIRWISE COMPARISONS (TUKEY ADJUSTED) ===\n")
pairs_error_final <- pairs(emmeans_error_final)
print(pairs_error_final)

# ==============================================================================
# 8. MODEL DIAGNOSTICS & DISTRIBUTION CHECKS
# ==============================================================================

# ---------------------------------------------------------
# A. Density Plot Comparison (Uncleaned vs. Cleaned)
# ---------------------------------------------------------
plot_density_uncleaned <- ggplot(df_cleaned, aes(x = Reaction_Time, color = Sentence_Category)) +
  geom_density(linewidth = 1) +
  theme_prism() +
  labs(title = "Uncleaned RT Distributions", x = "Reaction Time (ms)", y = "Density")

plot_density_cleaned <- ggplot(df_trimmed, aes(x = Reaction_Time, color = Sentence_Category)) +
  geom_density(linewidth = 1) +
  theme_prism() +
  labs(title = "Cleaned RT Distributions", x = "Reaction Time (ms)", y = "Density")

# Visualize Density Plots
plot_density_uncleaned + plot_density_cleaned

# ---------------------------------------------------------
# B. Q-Q Plots for Gamma Model Residuals (Patchwork)
# ---------------------------------------------------------
# Extract deviance residuals
df_orig_res <- data.frame(Residuals = residuals(model_rt_initial, type = "deviance"))
df_clean_res <- data.frame(Residuals = residuals(model_rt_final, type = "deviance"))

plot_qq_orig <- ggplot(df_orig_res, aes(sample = Residuals)) +
  stat_qq(alpha = 0.5) + stat_qq_line(color = "red", linewidth = 1) +
  theme_classic() + ylim(-3, 6) +
  labs(title = "Original Model", subtitle = "With Extreme Outliers", x = "Theoretical Quantiles", y = "Deviance Residuals")

plot_qq_clean <- ggplot(df_clean_res, aes(sample = Residuals)) +
  stat_qq(alpha = 0.5) + stat_qq_line(color = "red", linewidth = 1) +
  theme_classic() + ylim(-3, 6) +
  labs(title = "Cleaned Model", subtitle = "Outliers Excluded", x = "Theoretical Quantiles", y = "Deviance Residuals")

# Visualize Q-Q plots side-by-side
plot_qq_orig + plot_qq_clean

# ==============================================================================
# 9. FINAL PUBLICATION VISUALIZATIONS
# ==============================================================================

# ---------------------------------------------------------
# Plot A: Reaction Time Violin Plot
# ---------------------------------------------------------
# Prepare EMM data (converting seconds back to ms for plotting)
emm_df_rt_plot <- as.data.frame(emmeans_rt_final) %>%
  mutate(
    Mean_ms = response * 1000,
    SE_ms = SE * 1000
  )

plot_final_rt <- ggplot() +
  # Background Violins (Raw Trimmed Data)
  geom_violin(data = df_trimmed, 
              aes(x = Sentence_Category, y = Reaction_Time, fill = Sentence_Category),
              alpha = 0.3, color = NA, trim = FALSE) +
  # Foreground Model Estimates (+/- 1 SE)
  geom_errorbar(data = emm_df_rt_plot, 
                aes(x = Sentence_Category, ymin = Mean_ms - SE_ms, ymax = Mean_ms + SE_ms), 
                width = 0.1, linewidth = 1, color = "black") +
  geom_point(data = emm_df_rt_plot, 
             aes(x = Sentence_Category, y = Mean_ms), 
             size = 3, color = "black") +
  # Explicit Axis Ordering
  scale_x_discrete(limits = c("AI", "Human", "Tool")) +
  # Formatting
  theme_prism() +
  labs(y = "Reaction Time (ms)", x = "Sentence Category") +
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  coord_cartesian(ylim = c(0, 4000))
plot_final_rt


# ---------------------------------------------------------
# Plot B: Error Probability Bar Plot
# ---------------------------------------------------------
# Prepare EMM data (converting logit probabilities to readable percentages)
emm_df_error_plot <- as.data.frame(emmeans_error_final) %>%
  mutate(
    Prob_pct = prob * 100,
    SE_pct = SE * 100
  )

plot_final_error <- ggplot(data = emm_df_error_plot, aes(x = Sentence_Category, y = Prob_pct)) +
  # Background Bars
  geom_col(aes(fill = Sentence_Category), alpha = 0.3, color = NA, width = 0.6) +
  # Foreground Model Estimates (95% Confidence Intervals)
  geom_errorbar(aes(ymin = asymp.LCL * 100, ymax = asymp.UCL * 100), 
                width = 0.1, linewidth = 1, color = "black") +
  # Explicit Axis Ordering
  scale_x_discrete(limits = c("AI", "Human", "Tool")) +
  # Formatting
  theme_prism() +
  labs(y = "Error Rate (%)", x = "Sentence Category") +
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  coord_cartesian(ylim = c(0, 12))

# Visualize Final Publication Plots side-by-side
plot_final_rt + plot_final_error


# ==============================================================================
# 10. SUBJECT-LEVEL RT DIFFERENCES & VISUALIZATION
# ==============================================================================

# ---------------------------------------------------------
# A. Calculate Subject-Level Means and Differences
# ---------------------------------------------------------
# 1. Calculate Mean RT per subject per category
df_subj_means <- df_trimmed %>%
  group_by(Participant_Private_ID, Sentence_Category) %>%
  summarize(Mean_RT = mean(Reaction_Time, na.rm = TRUE), .groups = "drop")

# 2. Pivot wider to calculate differences from the Human baseline
df_subj_wide <- df_subj_means %>%
  pivot_wider(names_from = Sentence_Category, values_from = Mean_RT) %>%
  mutate(
    Diff_AI_Human = AI - Human,
    Diff_Tool_Human = Tool - Human
  )

# ---------------------------------------------------------
# B. Statistical Tests on Differences
# ---------------------------------------------------------
cat("\n=== ONE-SAMPLE T-TEST: IS AI - HUMAN DIFFERENT FROM ZERO? ===\n")
test_ai_human <- t.test(df_subj_wide$Diff_AI_Human, mu = 0)
print(test_ai_human)

cat("\n=== ONE-SAMPLE T-TEST: IS TOOL - HUMAN DIFFERENT FROM ZERO? ===\n")
test_tool_human <- t.test(df_subj_wide$Diff_Tool_Human, mu = 0)
print(test_tool_human)

cat("\n=== PAIRED T-TEST: IS (AI - HUMAN) DIFFERENT FROM (TOOL - HUMAN)? ===\n")
test_diffs <- t.test(df_subj_wide$Diff_AI_Human, df_subj_wide$Diff_Tool_Human, paired = TRUE)
print(test_diffs)
# ---------------------------------------------------------
# C. Visualization of Differences (Violin + Connected Dots)
# ---------------------------------------------------------
# Pivot the differences back to long format for ggplot
df_diff_long <- df_subj_wide %>%
  select(Participant_Private_ID, Diff_AI_Human, Diff_Tool_Human) %>%
  pivot_longer(
    cols = c(Diff_AI_Human, Diff_Tool_Human),
    names_to = "Comparison",
    values_to = "RT_Difference"
  ) %>%
  mutate(
    # Clean up names for the plot axis
    Comparison = recode(Comparison, 
                        "Diff_AI_Human" = "AI vs. Human Baseline", 
                        "Diff_Tool_Human" = "Tool vs. Human Baseline"),
    # Set explicitly ordered levels
    Comparison = factor(Comparison, levels = c("AI vs. Human Baseline", "Tool vs. Human Baseline"))
  )

# Define a reproducible, locked jitter position so lines and dots move exactly together
# width controls horizontal spread; height = 0 ensures we don't distort actual RT values
locked_jitter <- position_jitter(width = 0.15, height = 0, seed = 42)

plot_subj_diffs <- ggplot(df_diff_long, aes(x = Comparison, y = RT_Difference)) +
  # MIDGROUND: Horizontal line at 0 (representing the Human Baseline)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  # FOREGROUND: Subject-level dots and connecting lines with SYNCHRONIZED JITTER
  geom_line(aes(group = Participant_Private_ID), color = "black", alpha = 0.15, position = locked_jitter) +
  geom_point(color = "black", alpha = 0.3, size = 1.5, position = locked_jitter) +
  
  # BACKGROUND: Semi-transparent Violins
  geom_violin(fill = "gray50", alpha = 0.3, color = NA, trim = FALSE) +
  

  # CLEAN FORMATTING: Prism Theme
  theme_prism() +
  labs(
    y = "Reaction Time Difference (ms)",
    x = "Comparison"
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# Display the plot
plot_subj_diffs




# ==============================================================================
# 11. COVARIATE ANALYSIS (DEMOGRAPHICS & AI EXPERIENCE)
# ==============================================================================

# ---------------------------------------------------------
# A. Preprocessing Covariates
# ---------------------------------------------------------
# Scaling continuous predictors (Age, Knowledge, Familiarity) is mathematically 
# required in GLMMs to prevent convergence failures and ensure stable estimates.
df_cov <- df_trimmed %>%
  mutate(
    Gender = as.factor(Gender),
    Age_scaled = scale(Age)[,1],
    AI_Knowledge_scaled = scale(AI_Knowledge_Score)[,1],
    AI_Familiarity_scaled = scale(AI_Familiarity_Score)[,1]
  )

# ---------------------------------------------------------
# B. Reaction Time Model with Covariates
# ---------------------------------------------------------
# Uses the full random effects structure: (1 + Category | Subject) + (1 | Stem)
model_rt_cov <- glmer(RT_sec ~ Sentence_Category + Age_scaled + Gender + 
                        AI_Knowledge_scaled + AI_Familiarity_scaled + 
                        (1 + Sentence_Category | Participant_Private_ID) + 
                        (1 | Stem_ID), 
                      data = df_cov, 
                      family = Gamma(link = "log"))

cat("\n=== CROSSED RT MODEL WITH COVARIATES ===\n")
print(summary(model_rt_cov))

# ---------------------------------------------------------
# C. Error Probability Model with Covariates
# ---------------------------------------------------------
# Uses the simplified random effects structure: (1 | Subject) + (1 | Stem)
model_error_cov <- glmer(Error_Flag ~ Sentence_Category + Age_scaled + Gender + 
                           AI_Knowledge_scaled + AI_Familiarity_scaled + 
                           (1 | Participant_Private_ID) + 
                           (1 | Stem_ID), 
                         data = df_cov, 
                         family = binomial(link = "logit"))

cat("\n=== CROSSED ERROR MODEL WITH COVARIATES ===\n")
print(summary(model_error_cov))
