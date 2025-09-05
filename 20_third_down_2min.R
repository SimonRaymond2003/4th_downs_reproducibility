## 1. Setup
# ==============================================================================
# Load required libraries
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(mfx)) install.packages("mfx"); library(mfx)
if (!require(car)) install.packages("car"); library(car) # For joint hypothesis tests

# Create the output directory
if (!dir.exists("robustness_checks_2min")) {
  dir.create("robustness_checks_2min")
}

cat("--- Libraries Loaded and 'robustness_checks_2min' directory created. ---\n\n")


## 2. Helper Functions
# ==============================================================================

# Function to reorder columns
reorder_columns <- function(dt) {
  all_names <- names(dt)
  pattern_cols <- grepl("^(missing_information|coach_)|_PCA$", all_names)
  other_cols <- all_names[!pattern_cols]
  special_cols <- all_names[pattern_cols]
  new_order <- c(other_cols, special_cols)
  return(dt[, ..new_order])
}

# Function to scale non-binary numeric columns
scale_non_binary <- function(df) {
  cols_to_exclude <- c("my_id")
  cols_to_check <- setdiff(names(df), cols_to_exclude)
  
  for(col in cols_to_check) {
    if (is.numeric(df[[col]])) {
      unique_vals <- unique(na.omit(df[[col]]))
      is_binary <- length(unique_vals) <= 2 && all(unique_vals %in% c(0, 1))
      if(!is_binary) {
        df[[col]] <- scale(df[[col]])[,1]
      }
    }
  }
  return(df)
}

# Function to create interaction and polynomial terms
create_interaction_terms <- function(dt) {
  # Create polynomial terms
  dt[, ydstogo_sq := ydstogo^2]
  dt[, yardline_100_sq := yardline_100^2]
  dt[, score_diff_sq := score_diff^2]
  dt[, seconds_remaining_in_half_sq := seconds_remaining_in_half^2]
  
  # Create interaction terms
  dt[, temp_X_wind := temp * wind]
  dt[, is_winning_X_score_diff := is_winning * score_diff]
  dt[, is_winning_X_score_diff_sq := is_winning * score_diff_sq]
  
  # Seconds remaining interactions
  dt[, seconds_remaining_X_score_diff := seconds_remaining_in_half * score_diff]
  dt[, seconds_remaining_X_is_winning := seconds_remaining_in_half * is_winning]
  dt[, seconds_remaining_sq_X_score_diff := seconds_remaining_in_half_sq * score_diff]
  dt[, seconds_remaining_sq_X_is_winning := seconds_remaining_in_half_sq * is_winning]
  
  # Three-way interactions
  dt[, seconds_remaining_X_is_winning_X_score_diff := seconds_remaining_in_half * is_winning * score_diff]
  dt[, seconds_remaining_sq_X_is_winning_X_score_diff := seconds_remaining_in_half_sq * is_winning * score_diff]
  
  return(dt)
}

# Function to extract model results with heteroskedasticity-adjusted SEs
get_model_results <- function(model, model_type, data) {
  if (is.null(model)) return(NULL)
  tryCatch({
    if (model_type == "LPM") {
      coefs <- coef(model)
      robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
      tvals <- coefs / robust_se
      pvals <- 2 * pt(-abs(tvals), df = model$df.residual)
      return(list(
        coef = coefs,
        se = robust_se,
        tval = tvals,
        pval = pvals
      ))
    }
    if (model_type %in% c("Probit", "Logit")) {
      mfx_func <- if (model_type == "Probit") probitmfx else logitmfx
      mfx_result <- mfx_func(formula = formula(model), data = data, robust = TRUE)
      if (!is.null(mfx_result)) {
        mfx_mat <- mfx_result$mfxest
        return(list(
          mfx = mfx_mat[, "dF/dx"],
          se = mfx_mat[, "Std. Err."],
          tval = mfx_mat[, "z"],
          pval = mfx_mat[, "P>|z|"]
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    cat("Error extracting results for", model_type, ":", e$message, "\n")
    return(NULL)
  })
}

# Function to perform joint significance tests for seconds remaining variables
perform_joint_significance_test <- function(model, model_type) {
  tryCatch({
    coef_names <- names(coef(model))
    # Now look for our explicitly created seconds variables
    seconds_vars_patterns <- c("seconds_remaining_in_half", "seconds_remaining_in_half_sq",
                               "seconds_remaining_X_", "seconds_remaining_sq_X_")
    seconds_vars_in_model <- c()
    for (pattern in seconds_vars_patterns) {
      seconds_vars_in_model <- c(seconds_vars_in_model, 
                                 grep(pattern, coef_names, value = TRUE))
    }
    seconds_vars_in_model <- unique(seconds_vars_in_model)
    
    if (length(seconds_vars_in_model) == 0) {
      return(data.frame(Test_Statistic = NA, P_Value = NA, DF = NA, Num_Restrictions = 0))
    }
    
    # For LPM models, use linearHypothesis with robust standard errors
    if (model_type == "LPM") {
      wald_test <- linearHypothesis(model, 
                                    seconds_vars_in_model, 
                                    vcov = vcovHC(model, type = "HC1"))
      return(data.frame(
        Test_Statistic = wald_test$F[2],
        P_Value = wald_test$`Pr(>F)`[2],
        DF = paste(wald_test$Df[2], wald_test$Res.Df[1], sep = ","),
        Num_Restrictions = length(seconds_vars_in_model)
      ))
    }
    
    # For GLM models (Probit/Logit), use Wald test
    if (model_type %in% c("Probit", "Logit")) {
      wald_test <- linearHypothesis(model, seconds_vars_in_model)
      return(data.frame(
        Test_Statistic = wald_test$Chisq[2],
        P_Value = wald_test$`Pr(>Chisq)`[2],
        DF = wald_test$Df[2],
        Num_Restrictions = length(seconds_vars_in_model)
      ))
    }
    
  }, error = function(e) {
    cat("Error in joint significance test for", model_type, ":", e$message, "\n")
    return(data.frame(Test_Statistic = NA, P_Value = NA, DF = e$message, Num_Restrictions = NA))
  })
}


# ==============================================================================
#
# --- PART 1: THIRD DOWN CONVERSION ANALYSIS ---
#
# ==============================================================================
cat("--- Starting Part 1: Third Down Conversion Analysis ---\n")

## 3. Load and Prepare Third Down Data
# ==============================================================================
third_down_data <- fread("~/Dropbox/NFL_Papers/Player_Data/third_down_offense_defense_specific_positions_PCA.csv")

# ROBUSTNESS CHECK: Filter for plays with more than 120 seconds remaining
third_down_data <- third_down_data[seconds_remaining_in_half > 120]
cat("Third down data filtered for > 120 seconds remaining.\n")

third_down_data[, is_winning := ifelse(score_diff > 0, 1, 0)]
third_down_data[, my_id := NULL]
cols_to_remove <- c("coach_Giff_Smith", "year_2023", "roof_open", "coach_Zac_Taylor")
if(any(cols_to_remove %in% names(third_down_data))) third_down_data[, (cols_to_remove) := NULL]

# Create interaction and polynomial terms BEFORE scaling
third_down_data <- create_interaction_terms(third_down_data)

third_down_data <- reorder_columns(third_down_data)
third_down_data <- scale_non_binary(third_down_data)
third_down_data[, converted := as.numeric(as.character(converted))]

cat("Third down data prepared with explicit interaction terms.\n")

## 4. Specify and Run Third Down Models
# ==============================================================================
coach_vars_3rd <- grep("^coach_", names(third_down_data), value = TRUE)
pca_vars_3rd <- grep("_PCA$", names(third_down_data), value = TRUE)

# Now formula uses explicit column names without : or I()
formula_str_3rd <- "converted ~ week + year_2017 + year_2018 + year_2019 + year_2020 + year_2021 + year_2022 + ydstogo + ydstogo_sq + yardline_100 + yardline_100_sq + posteam_timeouts_remaining + defteam_timeouts_remaining + temp + wind + temp_X_wind + vegas_wp + spread_line + total_line + prep_days + home_attendance + team_win_pct + kicker_id_field_goals_grades_grades_fgep_kicker_12w + punter_id_punting_grades_grades_punter_12w + rush_attempt +
      formation_SHOTGUN + 
      formation_SINGLEBACK + 
      formation_EMPTY + 
      formation_I_FORM + 
      formation_WILDCAT + 
      formation_PISTOL + 
      formation_JUMBO + 
      defense_personnel_DL + 
      defense_personnel_LB + 
      defense_personnel_DB + 
      defense_personnel_other_unknown +
      score_diff + score_diff_sq + is_winning + is_winning_X_score_diff + is_winning_X_score_diff_sq + is_home_team + is_first_half + seconds_remaining_in_half + seconds_remaining_in_half_sq + seconds_remaining_X_score_diff + seconds_remaining_X_is_winning + seconds_remaining_sq_X_score_diff + seconds_remaining_sq_X_is_winning + seconds_remaining_X_is_winning_X_score_diff + seconds_remaining_sq_X_is_winning_X_score_diff"

if(length(coach_vars_3rd) > 0) formula_str_3rd <- paste(formula_str_3rd, "+", paste(coach_vars_3rd, collapse = " + "))
if(length(pca_vars_3rd) > 0) formula_str_3rd <- paste(formula_str_3rd, "+", paste(pca_vars_3rd, collapse = " + "))

formula_3rd <- as.formula(formula_str_3rd)
vars_to_check_3rd <- all.vars(formula_3rd)
clean_data_3rd <- third_down_data[complete.cases(third_down_data[, ..vars_to_check_3rd])]

lpm_3rd <- lm(formula_3rd, data = clean_data_3rd)
probit_3rd <- glm(formula_3rd, data = clean_data_3rd, family = binomial(link = "probit"))
logit_3rd <- glm(formula_3rd, data = clean_data_3rd, family = binomial(link = "logit"))

lpm_results_3rd <- get_model_results(lpm_3rd, "LPM", clean_data_3rd)
probit_results_3rd <- get_model_results(probit_3rd, "Probit", clean_data_3rd)
logit_results_3rd <- get_model_results(logit_3rd, "Logit", clean_data_3rd)

cat("Third down models fitted.\n")

## 5. Save Third Down Results
# ==============================================================================
# Extract coefficient names from all models
all_coef_names_3rd <- unique(c(
  names(lpm_results_3rd$coef),
  names(probit_results_3rd$mfx),
  names(logit_results_3rd$mfx)
))
all_coef_names_3rd <- all_coef_names_3rd[all_coef_names_3rd != "(Intercept)"]

# Create comprehensive results matrices
tval_matrix_3rd <- matrix(NA, nrow = length(all_coef_names_3rd), ncol = 3, 
                          dimnames = list(all_coef_names_3rd, c("LPM", "Probit", "Logit")))
se_matrix_3rd <- matrix(NA, nrow = length(all_coef_names_3rd), ncol = 3,
                        dimnames = list(all_coef_names_3rd, c("LPM", "Probit", "Logit")))
coef_matrix_3rd <- matrix(NA, nrow = length(all_coef_names_3rd), ncol = 3,
                          dimnames = list(all_coef_names_3rd, c("LPM_Coef", "Probit_ME", "Logit_ME")))
pval_matrix_3rd <- matrix(NA, nrow = length(all_coef_names_3rd), ncol = 3,
                          dimnames = list(all_coef_names_3rd, c("LPM", "Probit", "Logit")))

for (coef_name in all_coef_names_3rd) {
  # LPM results
  if (coef_name %in% names(lpm_results_3rd$tval)) {
    tval_matrix_3rd[coef_name, "LPM"] <- lpm_results_3rd$tval[coef_name]
    se_matrix_3rd[coef_name, "LPM"] <- lpm_results_3rd$se[coef_name]
    coef_matrix_3rd[coef_name, "LPM_Coef"] <- lpm_results_3rd$coef[coef_name]
    pval_matrix_3rd[coef_name, "LPM"] <- lpm_results_3rd$pval[coef_name]
  }
  # Probit marginal effects
  if (coef_name %in% names(probit_results_3rd$tval)) {
    tval_matrix_3rd[coef_name, "Probit"] <- probit_results_3rd$tval[coef_name]
    se_matrix_3rd[coef_name, "Probit"] <- probit_results_3rd$se[coef_name]
    coef_matrix_3rd[coef_name, "Probit_ME"] <- probit_results_3rd$mfx[coef_name]
    pval_matrix_3rd[coef_name, "Probit"] <- probit_results_3rd$pval[coef_name]
  }
  # Logit marginal effects
  if (coef_name %in% names(logit_results_3rd$tval)) {
    tval_matrix_3rd[coef_name, "Logit"] <- logit_results_3rd$tval[coef_name]
    se_matrix_3rd[coef_name, "Logit"] <- logit_results_3rd$se[coef_name]
    coef_matrix_3rd[coef_name, "Logit_ME"] <- logit_results_3rd$mfx[coef_name]
    pval_matrix_3rd[coef_name, "Logit"] <- logit_results_3rd$pval[coef_name]
  }
}

# Save all results
write.csv(tval_matrix_3rd, "robustness_checks_2min/third_down_3min_t_values.csv")
write.csv(se_matrix_3rd, "robustness_checks_2min/third_down_3min_standard_errors.csv")
write.csv(coef_matrix_3rd, "robustness_checks_2min/third_down_3min_coefficients_marginal_effects.csv")
write.csv(pval_matrix_3rd, "robustness_checks_2min/third_down_3min_p_values.csv")

cat("--- Third down results saved ---\n")

# Joint Significance Tests
lpm_joint_test_3rd <- perform_joint_significance_test(lpm_3rd, "LPM")
probit_joint_test_3rd <- perform_joint_significance_test(probit_3rd, "Probit")
logit_joint_test_3rd <- perform_joint_significance_test(logit_3rd, "Logit")
joint_test_results_3rd <- rbind(lpm_joint_test_3rd, probit_joint_test_3rd, logit_joint_test_3rd)
rownames(joint_test_results_3rd) <- c("LPM (F-test)", "Probit (Wald test)", "Logit (Wald test)")
write.csv(joint_test_results_3rd, "robustness_checks_2min/third_down_3min_joint_significance.csv")
cat("--- Third down joint significance tests saved ---\n\n")


# ==============================================================================
#
# --- PART 2: FOURTH DOWN ATTEMPT ANALYSIS ---
#
# ==============================================================================
cat("--- Starting Part 2: Fourth Down Attempt Analysis ---\n")

## 6. Load and Prepare Fourth Down Data
# ==============================================================================
attempt_data <- fread("~/Dropbox/NFL_Papers/Player_Data/selection_offense_defense_specific_positions_PCA.csv")

# ROBUSTNESS CHECK: Filter for plays with more than 120 seconds remaining
attempt_data <- attempt_data[seconds_remaining_in_half > 120]
cat("Fourth down data filtered for > 120 seconds remaining.\n")

attempt_data[, is_winning := ifelse(score_diff > 0, 1, 0)]
if(any(cols_to_remove %in% names(attempt_data))) attempt_data[, (cols_to_remove) := NULL]

# Create interaction and polynomial terms BEFORE scaling
attempt_data <- create_interaction_terms(attempt_data)

attempt_data <- reorder_columns(attempt_data)
attempt_data <- scale_non_binary(attempt_data)

cat("Fourth down data prepared with explicit interaction terms.\n")

## 7. Specify and Run Fourth Down Models
# ==============================================================================
coach_vars_4th <- grep("^coach_", names(attempt_data), value = TRUE)
pca_vars_4th <- grep("_PCA$", names(attempt_data), value = TRUE)

# Now formula uses explicit column names without : or I()
formula_str_4th <- "attempt ~ week + year_2017 + year_2018 + year_2019 + year_2020 + year_2021 + year_2022 + ydstogo + ydstogo_sq + yardline_100 + yardline_100_sq + posteam_timeouts_remaining + defteam_timeouts_remaining + temp + wind + temp_X_wind + vegas_wp + spread_line + total_line + prep_days + home_attendance + team_win_pct + kicker_id_field_goals_grades_grades_fgep_kicker_12w + punter_id_punting_grades_grades_punter_12w + score_diff + score_diff_sq + is_winning + is_winning_X_score_diff + is_winning_X_score_diff_sq + is_home_team + is_first_half + seconds_remaining_in_half + seconds_remaining_in_half_sq + seconds_remaining_X_score_diff + seconds_remaining_X_is_winning + seconds_remaining_sq_X_score_diff + seconds_remaining_sq_X_is_winning + seconds_remaining_X_is_winning_X_score_diff + seconds_remaining_sq_X_is_winning_X_score_diff"

if(length(coach_vars_4th) > 0) formula_str_4th <- paste(formula_str_4th, "+", paste(coach_vars_4th, collapse = " + "))
if(length(pca_vars_4th) > 0) formula_str_4th <- paste(formula_str_4th, "+", paste(pca_vars_4th, collapse = " + "))

formula_4th <- as.formula(formula_str_4th)
vars_to_check_4th <- all.vars(formula_4th)
clean_data_4th <- attempt_data[complete.cases(attempt_data[, ..vars_to_check_4th])]

lpm_4th <- lm(formula_4th, data = clean_data_4th)
probit_4th <- glm(formula_4th, data = clean_data_4th, family = binomial(link = "probit"))
logit_4th <- glm(formula_4th, data = clean_data_4th, family = binomial(link = "logit"))

lpm_results_4th <- get_model_results(lpm_4th, "LPM", clean_data_4th)
probit_results_4th <- get_model_results(probit_4th, "Probit", clean_data_4th)
logit_results_4th <- get_model_results(logit_4th, "Logit", clean_data_4th)

cat("Fourth down models fitted.\n")

## 8. Save Fourth Down Results
# ==============================================================================
# Extract coefficient names from all models
all_coef_names_4th <- unique(c(
  names(lpm_results_4th$coef),
  names(probit_results_4th$mfx),
  names(logit_results_4th$mfx)
))
all_coef_names_4th <- all_coef_names_4th[all_coef_names_4th != "(Intercept)"]

# Create comprehensive results matrices
tval_matrix_4th <- matrix(NA, nrow = length(all_coef_names_4th), ncol = 3,
                          dimnames = list(all_coef_names_4th, c("LPM", "Probit", "Logit")))
se_matrix_4th <- matrix(NA, nrow = length(all_coef_names_4th), ncol = 3,
                        dimnames = list(all_coef_names_4th, c("LPM", "Probit", "Logit")))
coef_matrix_4th <- matrix(NA, nrow = length(all_coef_names_4th), ncol = 3,
                          dimnames = list(all_coef_names_4th, c("LPM_Coef", "Probit_ME", "Logit_ME")))
pval_matrix_4th <- matrix(NA, nrow = length(all_coef_names_4th), ncol = 3,
                          dimnames = list(all_coef_names_4th, c("LPM", "Probit", "Logit")))

for (coef_name in all_coef_names_4th) {
  # LPM results
  if (coef_name %in% names(lpm_results_4th$tval)) {
    tval_matrix_4th[coef_name, "LPM"] <- lpm_results_4th$tval[coef_name]
    se_matrix_4th[coef_name, "LPM"] <- lpm_results_4th$se[coef_name]
    coef_matrix_4th[coef_name, "LPM_Coef"] <- lpm_results_4th$coef[coef_name]
    pval_matrix_4th[coef_name, "LPM"] <- lpm_results_4th$pval[coef_name]
  }
  # Probit marginal effects
  if (coef_name %in% names(probit_results_4th$tval)) {
    tval_matrix_4th[coef_name, "Probit"] <- probit_results_4th$tval[coef_name]
    se_matrix_4th[coef_name, "Probit"] <- probit_results_4th$se[coef_name]
    coef_matrix_4th[coef_name, "Probit_ME"] <- probit_results_4th$mfx[coef_name]
    pval_matrix_4th[coef_name, "Probit"] <- probit_results_4th$pval[coef_name]
  }
  # Logit marginal effects
  if (coef_name %in% names(logit_results_4th$tval)) {
    tval_matrix_4th[coef_name, "Logit"] <- logit_results_4th$tval[coef_name]
    se_matrix_4th[coef_name, "Logit"] <- logit_results_4th$se[coef_name]
    coef_matrix_4th[coef_name, "Logit_ME"] <- logit_results_4th$mfx[coef_name]
    pval_matrix_4th[coef_name, "Logit"] <- logit_results_4th$pval[coef_name]
  }
}

# Save all results
write.csv(tval_matrix_4th, "robustness_checks_2min/fourth_down_3min_attempt_t_values.csv")
write.csv(se_matrix_4th, "robustness_checks_2min/fourth_down_3min_attempt_standard_errors.csv")
write.csv(coef_matrix_4th, "robustness_checks_2min/fourth_down_3min_attempt_coefficients_marginal_effects.csv")
write.csv(pval_matrix_4th, "robustness_checks_2min/fourth_down_3min_attempt_p_values.csv")

cat("--- Fourth down results saved ---\n")

# Joint Significance Tests
lpm_joint_test_4th <- perform_joint_significance_test(lpm_4th, "LPM")
probit_joint_test_4th <- perform_joint_significance_test(probit_4th, "Probit")
logit_joint_test_4th <- perform_joint_significance_test(logit_4th, "Logit")
joint_test_results_4th <- rbind(lpm_joint_test_4th, probit_joint_test_4th, logit_joint_test_4th)
rownames(joint_test_results_4th) <- c("LPM (F-test)", "Probit (Wald test)", "Logit (Wald test)")
write.csv(joint_test_results_4th, "robustness_checks_2min/fourth_down_3min_joint_significance.csv")
cat("--- Fourth down joint significance tests saved ---\n\n")

# Summary of results
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("The following files have been saved in 'robustness_checks_2min':\n")
cat("\nThird Down Analysis:\n")
cat("  - third_down_3min_t_values.csv\n")
cat("  - third_down_3min_standard_errors.csv\n")
cat("  - third_down_3min_coefficients_marginal_effects.csv\n")
cat("  - third_down_3min_p_values.csv\n")
cat("  - third_down_3min_joint_significance.csv\n")
cat("\nFourth Down Analysis:\n")
cat("  - fourth_down_3min_attempt_t_values.csv\n")
cat("  - fourth_down_3min_attempt_standard_errors.csv\n")
cat("  - fourth_down_3min_attempt_coefficients_marginal_effects.csv\n")
cat("  - fourth_down_3min_attempt_p_values.csv\n")
cat("  - fourth_down_3min_attempt_joint_significance.csv\n")
cat("\n--- Script execution completed successfully. ---\n")