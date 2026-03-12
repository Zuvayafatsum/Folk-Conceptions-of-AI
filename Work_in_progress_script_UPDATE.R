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
file_path <- "//nas.ads.mwn.de/ra38lap/MWN-PC/Desktop/Folk_Conceptions_of_AI/Data_First_Experiment_Preprocessed/Folk_Conception_of_AI_Master_Task_Data_All_Included.csv"
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

# Create a unique ID (1 to 22) for the sentence stems based on Sentence_ID
df_cleaned$Stem_ID = as.factor(ceiling(df_cleaned$Sentence_ID / 3))
  
  
nrow_after <- nrow(df_cleaned)
rows_deleted <- nrow_before - nrow_after
participants_excluded <- rows_deleted / 66

# Print exact exclusion statistics to the console for the manuscript
cat("\n--- EXCLUSION REPORT (PRACTICE FAILURES) ---\n")
cat("Total rows deleted: ", rows_deleted, "\n")
cat("Total participants excluded: ", participants_excluded, "\n\n")

# ==============================================================================
# 4. PREREGISTERED MODELS (STRICTLY AS THEY WERE PREREGISTERED)
# ==============================================================================

# exclude Human trials for these models

df_preregistered_stat <- df_cleaned %>%
  filter(Sentence_Category %in% c("AI", "Tool"))
  

# A. Initial Reaction Time Model (Gamma distribution, log link)
model_rt_preregistered <- glmer(RT_sec ~ Sentence_Category + (1 + Sentence_Category | Participant_Private_ID), 
                          data = df_preregistered_stat, 
                          family = Gamma(link = "log"))

cat("\n=== INITIAL REACTION TIME MODEL SUMMARY ===\n")
print(summary(model_rt_preregistered))


# B. Initial Error Probability Model 
model_error_preregistered <- glmer(Error_Flag ~ Sentence_Category + (1 + Sentence_Category | Participant_Private_ID), 
                             data = df_preregistered_stat, 
                             family = binomial(link = "logit"))

cat("\n=== INITIAL ERROR PROBABILITY MODEL SUMMARY ===\n")
print(summary(model_error_preregistered))


# ==============================================================================
# 4B. EFFECT SIZES AND 95% CIs FOR REPORTING
# Add this right after the two model objects are created
# ==============================================================================

# Wald 95% CIs for fixed effects
rt_ci  <- confint(model_rt_preregistered, parm = "beta_", method = "Wald")
err_ci <- confint(model_error_preregistered, parm = "beta_", method = "Wald")

# Exponentiated coefficients and CIs
exp(cbind(Estimate = fixef(model_rt_preregistered), rt_ci))
exp(cbind(Estimate = fixef(model_error_preregistered), err_ci))

# Optional: model-based estimated means/probabilities on response scale
emmeans(model_rt_preregistered, ~ Sentence_Category, type = "response")
emmeans(model_error_preregistered, ~ Sentence_Category, type = "response")


# ==============================================================================
# 5. EXPLORATORY COVARIATE-ADJUSTED MODELS (FULL DATASET: AI + TOOL + HUMAN)
# ==============================================================================
# These are covariate-adjusted main-effects models.
# No interactions with Sentence_Category are included.
# Human is set as the reference level for Sentence_Category.

# ---------------------------------------------------------
# A. Preprocessing Covariates
# ---------------------------------------------------------
# Scaling continuous covariates is strongly recommended for numerical stability
# and easier interpretation in GLMMs.
df_cov <- df_cleaned %>%
  mutate(
    Participant_Private_ID = factor(Participant_Private_ID),
    Stem_ID = factor(Stem_ID),
    Sentence_Category = factor(Sentence_Category, levels = c("Human", "AI", "Tool")),
    Gender = factor(Gender),
    Age_scaled = as.numeric(scale(Age)),
    AI_Knowledge_scaled = as.numeric(scale(AI_Knowledge_Score)),
    AI_Familiarity_scaled = as.numeric(scale(AI_Familiarity_Score))
  ) %>%
  drop_na(
    Sentence_Category, RT_sec, Error_Flag, Age, Gender,
    AI_Knowledge_Score, AI_Familiarity_Score,
    Participant_Private_ID, Stem_ID
  )

cat("\n--- EXPLORATORY COVARIATE DATASET ---\n")
cat("Rows retained: ", nrow(df_cov), "\n")
cat("Participants retained: ", n_distinct(df_cov$Participant_Private_ID), "\n")
cat("Stems retained: ", n_distinct(df_cov$Stem_ID), "\n\n")

# ---------------------------------------------------------
# B. Reaction Time Model with Covariates
# ---------------------------------------------------------
model_rt_cov <- glmer(
  RT_sec ~ Sentence_Category + Age_scaled + Gender +
    AI_Knowledge_scaled + AI_Familiarity_scaled +
    (1 + Sentence_Category | Participant_Private_ID) +
    (1 | Stem_ID),
  data = df_cov,
  family = Gamma(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\n=== COVARIATE-ADJUSTED RT MODEL ===\n")
print(summary(model_rt_cov))
cat("\nRT model singular fit? ", isSingular(model_rt_cov, tol = 1e-4), "\n")

# ---------------------------------------------------------
# C. Error Probability Model with Covariates
# ---------------------------------------------------------
model_error_cov <- glmer(
  Error_Flag ~ Sentence_Category + Age_scaled + Gender +
    AI_Knowledge_scaled + AI_Familiarity_scaled +
    (1 + Sentence_Category | Participant_Private_ID) +
    (1 | Stem_ID),
  data = df_cov,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\n=== COVARIATE-ADJUSTED ERROR PROBABILITY MODEL ===\n")
print(summary(model_error_cov))
cat("\nError model singular fit? ", isSingular(model_error_cov, tol = 1e-4), "\n")

# ---------------------------------------------------------
# D. Wald 95% CIs for Fixed Effects
# ---------------------------------------------------------
rt_cov_ci  <- confint(model_rt_cov, parm = "beta_", method = "Wald")
err_cov_ci <- confint(model_error_cov, parm = "beta_", method = "Wald")

cat("\n=== RT MODEL: EXPONENTIATED FIXED EFFECTS + 95% WALD CIs ===\n")
print(exp(cbind(Estimate = fixef(model_rt_cov), rt_cov_ci)))

cat("\n=== ERROR MODEL: EXPONENTIATED FIXED EFFECTS + 95% WALD CIs ===\n")
print(exp(cbind(Estimate = fixef(model_error_cov), err_cov_ci)))

# ---------------------------------------------------------
# E. Estimated Marginal Means on the Response Scale
# ---------------------------------------------------------
# These are covariate-adjusted marginal means/probabilities.
# Continuous covariates are evaluated at their mean (0 after scaling).
# Gender is averaged over its observed proportions.
emm_rt_cov <- emmeans(model_rt_cov, ~ Sentence_Category, weights = "proportional")
emm_err_cov <- emmeans(model_error_cov, ~ Sentence_Category, weights = "proportional")

cat("\n=== RT MODEL: EMMs ON RESPONSE SCALE ===\n")
print(summary(emm_rt_cov, type = "response", infer = c(TRUE, TRUE)))

cat("\n=== ERROR MODEL: EMMs ON RESPONSE SCALE ===\n")
print(summary(emm_err_cov, type = "response", infer = c(TRUE, TRUE)))

# ---------------------------------------------------------
# F. Focused Post Hoc Contrast: AI vs Tool
# ---------------------------------------------------------
# Because Human is the reference level in the model, AI vs Tool is obtained here
# via emmeans rather than directly from the fixed-effect table.

rt_ai_vs_tool <- contrast(
  emm_rt_cov,
  method = list("AI vs Tool" = c(0, 1, -1))
)

err_ai_vs_tool <- contrast(
  emm_err_cov,
  method = list("AI vs Tool" = c(0, 1, -1))
)

cat("\n=== RT MODEL: AI vs TOOL CONTRAST ===\n")
print(summary(rt_ai_vs_tool, type = "response", infer = c(TRUE, TRUE)))

cat("\n=== ERROR MODEL: AI vs TOOL CONTRAST ===\n")
print(summary(err_ai_vs_tool, type = "response", infer = c(TRUE, TRUE)))


# ==============================================================================
# H1. CLEAN FIXED-EFFECT TABLES FOR MANUSCRIPT
# ==============================================================================

library(dplyr)
library(tibble)

# ---------------------------------------------------------
# Build custom fixed-effect table from current models
# ---------------------------------------------------------

# RT model
rt_sum <- coef(summary(model_rt_cov))
rt_ci  <- confint(model_rt_cov, parm = "beta_", method = "Wald")

rt_table_custom <- data.frame(
  term = rownames(rt_sum),
  B = rt_sum[, "Estimate"],
  SE = rt_sum[, "Std. Error"],
  Statistic = rt_sum[, "t value"],
  p = rt_sum[, "Pr(>|z|)"],
  Effect_Size = exp(rt_sum[, "Estimate"]),
  CI_low = exp(rt_ci[, 1]),
  CI_high = exp(rt_ci[, 2]),
  stringsAsFactors = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Model = "Reaction Time (Gamma)",
    Predictor = recode(
      term,
      "Sentence_CategoryAI" = "Sentence Category (AI vs Human)",
      "Sentence_CategoryTool" = "Sentence Category (Tool vs Human)",
      "Age_scaled" = "Age",
      "GenderMale" = "Gender (Male)",
      "AI_Knowledge_scaled" = "AI Knowledge",
      "AI_Familiarity_scaled" = "AI Familiarity"
    ),
    `Estimate (B)` = round(B, 2),
    SE = round(SE, 2),
    Statistic = round(Statistic, 2),
    `p-value` = ifelse(p < .001, "< .001", sprintf("%.3f", p)),
    `Effect Size` = paste0(round(Effect_Size, 2), " (Ratio)"),
    `95% CI (Effect Size)` = paste0("[", round(CI_low, 2), ", ", round(CI_high, 2), "]")
  ) %>%
  select(Model, Predictor, `Estimate (B)`, SE, Statistic, `p-value`, `Effect Size`, `95% CI (Effect Size)`)

# Error model
err_sum <- coef(summary(model_error_cov))
err_ci  <- confint(model_error_cov, parm = "beta_", method = "Wald")

err_table_custom <- data.frame(
  term = rownames(err_sum),
  B = err_sum[, "Estimate"],
  SE = err_sum[, "Std. Error"],
  Statistic = err_sum[, "z value"],
  p = err_sum[, "Pr(>|z|)"],
  Effect_Size = exp(err_sum[, "Estimate"]),
  CI_low = exp(err_ci[, 1]),
  CI_high = exp(err_ci[, 2]),
  stringsAsFactors = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Model = "Error Probability (Binomial)",
    Predictor = recode(
      term,
      "Sentence_CategoryAI" = "Sentence Category (AI vs Human)",
      "Sentence_CategoryTool" = "Sentence Category (Tool vs Human)",
      "Age_scaled" = "Age",
      "GenderMale" = "Gender (Male)",
      "AI_Knowledge_scaled" = "AI Knowledge",
      "AI_Familiarity_scaled" = "AI Familiarity"
    ),
    `Estimate (B)` = round(B, 2),
    SE = round(SE, 2),
    Statistic = round(Statistic, 2),
    `p-value` = ifelse(p < .001, "< .001", sprintf("%.3f", p)),
    `Effect Size` = paste0(round(Effect_Size, 2), " (OR)"),
    `95% CI (Effect Size)` = paste0("[", round(CI_low, 2), ", ", round(CI_high, 2), "]")
  ) %>%
  select(Model, Predictor, `Estimate (B)`, SE, Statistic, `p-value`, `Effect Size`, `95% CI (Effect Size)`)


print(rt_table_custom)

print(err_table_custom)







# ==============================================================================
# 6. EXPLORATORY RT MODEL INCLUDING RESPONSE ACCURACY
# ==============================================================================

# Create an explicit accuracy factor for readability
df_cov_acc <- df_cov %>%
  mutate(
    Response_Accuracy = factor(Error_Flag,
                               levels = c(0, 1),
                               labels = c("Correct", "Error"))
  )

# Interaction model: category x accuracy
model_rt_acc_int <- glmer(
  RT_sec ~ Sentence_Category * Response_Accuracy +
    Age_scaled + Gender + AI_Knowledge_scaled + AI_Familiarity_scaled +
    (1 + Sentence_Category | Participant_Private_ID) +
    (1 | Stem_ID),
  data = df_cov_acc,
  family = Gamma(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\n=== RT MODEL WITH CATEGORY x ACCURACY INTERACTION ===\n")
print(summary(model_rt_acc_int))
cat("\nRT interaction model singular fit? ", isSingular(model_rt_acc_int, tol = 1e-4), "\n")

# Wald CIs
rt_acc_int_ci <- confint(model_rt_acc_int, parm = "beta_", method = "Wald")
cat("\n=== RT INTERACTION MODEL: EXPONENTIATED FIXED EFFECTS + 95% WALD CIs ===\n")
print(exp(cbind(Estimate = fixef(model_rt_acc_int), rt_acc_int_ci)))

# Cell means: category within accuracy
emm_rt_cat_by_acc <- emmeans(
  model_rt_acc_int,
  ~ Sentence_Category | Response_Accuracy,
  weights = "proportional"
)

cat("\n=== CATEGORY EFFECTS WITHIN EACH ACCURACY LEVEL ===\n")
print(summary(emm_rt_cat_by_acc, type = "response", infer = c(TRUE, TRUE)))

# Pairwise category contrasts within each accuracy level
pairs_rt_cat_by_acc <- pairs(emm_rt_cat_by_acc)
cat("\n=== PAIRWISE CATEGORY CONTRASTS WITHIN EACH ACCURACY LEVEL ===\n")
print(summary(pairs_rt_cat_by_acc, type = "response", infer = c(TRUE, TRUE)))

# Accuracy effect within each category
emm_rt_acc_by_cat <- emmeans(
  model_rt_acc_int,
  ~ Response_Accuracy | Sentence_Category,
  weights = "proportional"
)

cat("\n=== ACCURACY EFFECT WITHIN EACH CATEGORY ===\n")
print(summary(emm_rt_acc_by_cat, type = "response", infer = c(TRUE, TRUE)))

pairs_rt_acc_by_cat <- pairs(emm_rt_acc_by_cat)
cat("\n=== CORRECT vs ERROR WITHIN EACH CATEGORY ===\n")
print(summary(pairs_rt_acc_by_cat, type = "response", infer = c(TRUE, TRUE)))
