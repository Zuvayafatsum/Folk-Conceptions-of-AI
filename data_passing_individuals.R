# ============================================================
# Load and combine RSVP plausibility data across participants
# Exclude participants who fail >= 1 catch trial
# ============================================================

library(readr)
library(dplyr)
library(purrr)
library(stringr)

# ------------------------------------------------------------
# 1. Define folder path
# ------------------------------------------------------------

data_dir <- "\\\\nas.ads.mwn.de\\ra38lap\\MWN-PC\\Desktop\\EEG_Study\\Pilot"

csv_files <- list.files(
  path = data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

# ------------------------------------------------------------
# 2. Define required columns
# ------------------------------------------------------------

required_cols <- c(
  "item_id",
  "condition",
  "sentence_text",
  "trials.thisN",
  "reaction_time",
  "is_correct",
  "trial_type",
  "Participant_ID",
  "Age",
  "Gender"
)

# ------------------------------------------------------------
# 3. Create containers
# ------------------------------------------------------------

all_data <- list()
excluded_subjects <- tibble(
  file_name = character(),
  participant_id = character(),
  reason = character(),
  n_catch_trials = integer(),
  n_failed_catch_trials = integer()
)

# ------------------------------------------------------------
# 4. Loop through files one by one
# ------------------------------------------------------------

for (file in csv_files) {
  
  message("Processing: ", basename(file))
  
  dat <- read_csv(file, show_col_types = FALSE)
  
  # ----------------------------------------------------------
  # Check whether required columns exist
  # ----------------------------------------------------------
  
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    
    excluded_subjects <- excluded_subjects %>%
      add_row(
        file_name = basename(file),
        participant_id = NA_character_,
        reason = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
        n_catch_trials = NA_integer_,
        n_failed_catch_trials = NA_integer_
      )
    
    next
  }
  
  # ----------------------------------------------------------
  # Extract participant ID
  # ----------------------------------------------------------
  
  participant_id <- unique(dat$Participant_ID)
  participant_id <- participant_id[!is.na(participant_id)]
  
  if (length(participant_id) == 0) {
    participant_id <- NA_character_
  } else {
    participant_id <- as.character(participant_id[1])
  }
  
  # ----------------------------------------------------------
  # Identify catch trials
  # In your example file, catch trials have trial_type == "catch"
  # and condition == "catch". This line allows either.
  # ----------------------------------------------------------
  
  catch_trials <- dat %>%
    filter(
      str_to_lower(trial_type) == "catch" |
        str_to_lower(condition) == "catch"
    )
  
  n_catch <- nrow(catch_trials)
  
  failed_catch_trials <- catch_trials %>%
    filter(is.na(is_correct) | is_correct != 1)
  
  n_failed_catch <- nrow(failed_catch_trials)
  
  # ----------------------------------------------------------
  # Exclude participant if not exactly 3 catch trials
  # or if any catch trial was answered incorrectly
  # ----------------------------------------------------------
  
  if (n_catch != 3 | n_failed_catch > 0) {
    
    exclusion_reason <- case_when(
      n_catch != 3 & n_failed_catch > 0 ~ "Wrong number of catch trials and at least one failed catch trial",
      n_catch != 3 ~ "Wrong number of catch trials",
      n_failed_catch > 0 ~ "At least one failed catch trial",
      TRUE ~ "Unknown exclusion reason"
    )
    
    excluded_subjects <- excluded_subjects %>%
      add_row(
        file_name = basename(file),
        participant_id = participant_id,
        reason = exclusion_reason,
        n_catch_trials = n_catch,
        n_failed_catch_trials = n_failed_catch
      )
    
    next
  }
  
  # ----------------------------------------------------------
  # Keep only main task trials
  # ----------------------------------------------------------
  
  subject_main_data <- dat %>%
    filter(
      str_to_lower(trial_type) != "catch",
      str_to_lower(condition) != "catch"
    ) %>%
    mutate(
      source_file = basename(file),
      participant_id = as.character(Participant_ID),
      age = Age,
      gender = Gender,
      
      # PsychoPy trial numbers are typically zero-based.
      # This gives a human-readable 1-based trial order.
      trial_order = `trials.thisN` + 1,
      
      # Keep original PsychoPy trial index too, for traceability.
      trial_order_psychopy = `trials.thisN`
    ) %>%
    select(
      source_file,
      participant_id,
      age,
      gender,
      trial_order,
      trial_order_psychopy,
      item_id,
      condition,
      sentence = sentence_text,
      is_correct,
      reaction_time
    )
  
  # Optional safety check: should be 66 main trials
  if (nrow(subject_main_data) != 66) {
    
    excluded_subjects <- excluded_subjects %>%
      add_row(
        file_name = basename(file),
        participant_id = participant_id,
        reason = paste0("Unexpected number of main trials: ", nrow(subject_main_data)),
        n_catch_trials = n_catch,
        n_failed_catch_trials = n_failed_catch
      )
    
    next
  }
  
  all_data[[basename(file)]] <- subject_main_data
}

# ------------------------------------------------------------
# 5. Bind all included subjects into one dataframe
# ------------------------------------------------------------

combined_df <- bind_rows(all_data)

# ------------------------------------------------------------
# 6. Print summary
# ------------------------------------------------------------

cat("\n============================\n")
cat("Processing summary\n")
cat("============================\n")

cat("Number of CSV files found: ", length(csv_files), "\n")
cat("Number of included subjects: ", n_distinct(combined_df$participant_id), "\n")
cat("Number of excluded subjects: ", nrow(excluded_subjects), "\n")
cat("Number of rows in combined_df: ", nrow(combined_df), "\n")

cat("\nRows per condition:\n")
print(table(combined_df$condition))

cat("\nRows per participant:\n")
print(table(combined_df$participant_id))

cat("\nExcluded subjects:\n")
print(excluded_subjects)

# ------------------------------------------------------------
# 7. Save output files
# ------------------------------------------------------------

write_csv(
  combined_df,
  file.path(data_dir, "combined_main_task_data.csv")
)

write_csv(
  excluded_subjects,
  file.path(data_dir, "excluded_subjects_catch_trial_check.csv")
)
