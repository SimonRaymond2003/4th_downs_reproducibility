## 1. Setup and Hyperparameter Grid
# ==============================================================================
# HYPERPARAMETER GRID - Edit these values to tune different parameters
# ==============================================================================

# Number of bootstrap samples for FINAL validation (after winner is selected)
n_bootstrap <- 100

# Tuning strategy parameters
max_loops <- 10          # Maximum number of loops before stopping
stable_winner_loops <- 10    # Stop if same combo is #1 for this many consecutive loops
drop_percentage <- 0.0     # Drop bottom X% each loop (0.10 = drop bottom 10%, 0 = don't drop any) 



# Define the comprehensive hyperparameter grid
# All parameters are included - edit ranges as needed for tuning
hyper_grid <- expand.grid(
  # Boosting parameters
  nrounds = seq(1, 25001, by = 1000),  
  eta = c(0.01),         
  
  # Tree structure parameters
  max_depth = seq(4, 4, by = 1),                      # Maximum tree depth
  min_child_weight = c(1),                     # Minimum sum of instance weight in child
  
  # Stochastic parameters
  subsample = seq(1, 1, by = 0.1),            # Subsample ratio of training instances
  colsample_bytree = seq(1, 1, by = 0.1),     # Subsample ratio of columns when constructing each tree
  
  # Regularization parameters
  gamma = c(0),                                # Minimum loss reduction for split (regularization)
  lambda = c(1),                               # L2 regularization term on weights
  alpha = c(0)                                 # L1 regularization term on weights
)



# Fixed parameters (not in grid search)
# These will be used for all models but can be easily modified
fixed_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  nthread = -1,                                # Use all available CPU cores (-1 = all cores)
  colsample_bylevel = 1,                       # Subsample ratio of columns for each level
  colsample_bynode = 1,                        # Subsample ratio of columns for each split
  max_delta_step = 0,                          # Maximum delta step for each tree's weight estimation
  scale_pos_weight = 1                         # Balancing of positive and negative weights (handled via weights)
)

cat(sprintf("Hyperparameter grid created with %d total combinations\n", nrow(hyper_grid)))
cat("Grid search will test:\n")
cat(sprintf("  - nrounds: %d values from %d to %d\n", 
            length(unique(hyper_grid$nrounds)), 
            min(hyper_grid$nrounds), 
            max(hyper_grid$nrounds)))
cat(sprintf("  - eta: %d values\n", length(unique(hyper_grid$eta))))
cat(sprintf("  - max_depth: %d values\n", length(unique(hyper_grid$max_depth))))
cat(sprintf("  - min_child_weight: %d values\n", length(unique(hyper_grid$min_child_weight))))
cat(sprintf("  - subsample: %d values\n", length(unique(hyper_grid$subsample))))
cat(sprintf("  - colsample_bytree: %d values\n", length(unique(hyper_grid$colsample_bytree))))
cat(sprintf("  - gamma: %d values\n", length(unique(hyper_grid$gamma))))
cat(sprintf("  - lambda: %d values\n", length(unique(hyper_grid$lambda))))
cat(sprintf("  - alpha: %d values\n", length(unique(hyper_grid$alpha))))
cat(sprintf("\nTotal combinations: %d\n", nrow(hyper_grid)))
if(drop_percentage > 0) {
  cat(sprintf("\nElimination strategy: Drop bottom %.1f%% each loop\n", drop_percentage * 100))
} else {
  cat("\nNo elimination - all combinations tested each loop\n")
}
cat(sprintf("  - Each loop: 1 bootstrap per combination\n"))
cat(sprintf("  - Ranking based on cumulative average AUC across all loops\n"))
cat(sprintf("  - Stop conditions:\n"))
cat(sprintf("    * Reach %d loops\n", max_loops))
cat(sprintf("    * Only 1 combination remains (if dropping enabled)\n"))
cat(sprintf("    * Same combination ranked #1 for %d consecutive loops\n\n", stable_winner_loops))

# make max print 10000
options(max.print = 10000)




# ==============================================================================
# Set a seed for reproducibility
# Create a directory to store all output files
if (!dir.exists("script_outputs_players")) { dir.create("script_outputs_players") }
# Load required libraries
library(data.table)
library(xgboost)
library(ROCR)
library(ggplot2)
library(sandwich)
library(lmtest)
library(mfx)

cat("--- Libraries Loaded ---\n")


## 2. Load Data
# ==============================================================================
attempt_data <- fread("C:/Users/simon/Dropbox/NFL_Papers/Player_Data/selection_offense_defense.csv")
convert_data <- fread("C:/Users/simon/Dropbox/NFL_Papers/Player_Data/outcome_offense_defense.csv")
# # # #
# attempt_data <- fread("~/Dropbox/NFL_Papers/Player_Data/selection_offense_defense.csv")
# convert_data <- fread("~/Dropbox/NFL_Papers/Player_Data/outcome_offense_defense.csv")

# kill be backup qb QB_2 and RB_3 and te 3
attempt_data <- attempt_data[, !grepl("starter_offense_QB_2", names(attempt_data)), with = FALSE]
convert_data <- convert_data[, !grepl("starter_offense_QB_2", names(convert_data)), with = FALSE]

# count how many variable start with starter_
starter_vars_count <- sum(grepl("^starter_", names(attempt_data)))
cat(sprintf("Number of variables starting with 'starter_': %d\n", starter_vars_count))
# count how many variable start with coach_
coach_vars_count <- sum(grepl("^coach_", names(attempt_data)))
cat(sprintf("Number of variables starting with 'coach_': %d\n", coach_vars_count))

#kill any zero var cols
attempt_data <- attempt_data[, which(sapply(attempt_data, function(col) length(unique(col))) > 1), with = FALSE]
convert_data <- convert_data[, which(sapply(convert_data, function(col) length(unique(col))) > 1), with = FALSE]

## 3. Data Preparation
# ==============================================================================
# Filter data based on time remaining
attempt_data <- attempt_data[seconds_remaining_in_half > 120]
convert_data <- convert_data[seconds_remaining_in_half > 120]

# Create a binary indicator for winning
attempt_data[, is_winning := ifelse(score_diff > 0, 1, 0)]
convert_data[, is_winning := ifelse(score_diff > 0, 1, 0)]

# Function to reorder columns
reorder_columns <- function(dt) {
  all_names <- names(dt)
  pattern_cols <- grepl("^(missing_information|coach_|starter_)", all_names)
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

# Apply data preparation
attempt_data <- reorder_columns(attempt_data)
convert_data <- reorder_columns(convert_data)

# Remove problematic columns
cols_to_remove <- c("coach_Giff_Smith", "year_2023", "roof_open", "coach_Zac_Taylor", 
                    # "missing_information_starter_offense_WR_1", "missing_information_starter_offense_WR_2", 
                    # "missing_information_starter_defense_CB_1", "missing_information_starter_defense_CB_2",
                    # "missing_information_starter_offense_RB_1", "missing_information_starter_offense_T_1",
                    "missing_information_starter_defense_NT_1_defensive_grades_grades_pass_rush_defense_12w",
                    "missing_information_starter_defense_DE_1_defensive_grades_grades_pass_rush_defense_12w",
                    "missing_information_starter_offense_QB_1_passing_depth_medium_grades_pass_12w",
                    "missing_information_starter_defense_CB_1_coverage_schemes_man_grades_coverage_defense_12w"
)
if(any(cols_to_remove %in% names(attempt_data))) attempt_data[, (cols_to_remove) := NULL]
if(any(cols_to_remove %in% names(convert_data))) convert_data[, (cols_to_remove) := NULL]

# Scale the data
attempt_data <- scale_non_binary(attempt_data)
convert_data <- scale_non_binary(convert_data)

cat("--- Data Preparation Complete ---\n")


## 4. Variable Selection
# ==============================================================================
# Identify coach, player, and missing information variables
coach_vars <- grep("^coach_", names(attempt_data), value = TRUE)
player_vars <- grep("^starter_", names(attempt_data), value = TRUE)
missing_vars <- grep("^missing_information", names(attempt_data), value = TRUE)

# Define base variables
keep_vars_base <- c(
  "my_id", "week", "year_2017", "year_2018", "year_2019", "year_2020", "year_2021",
  "year_2022", "ydstogo", "yardline_100", "posteam_timeouts_remaining",
  "defteam_timeouts_remaining", "temp", "wind", "vegas_wp", "spread_line",
  "total_line", "prep_days", "score_diff", "home_attendance",
  "overall_win_pct", "team_win_pct",
  "kicker_id_field_goals_grades_grades_fgep_kicker_12w",
  "punter_id_punting_grades_grades_punter_12w", "is_home_team",
  "is_first_half", "seconds_remaining_in_half", "is_winning"
)

# Define full variable lists
keep_vars_attempt <- c("attempt", keep_vars_base, coach_vars, player_vars, missing_vars)

defense_personnel_vars <- c("defense_personnel_DL", 
                            "defense_personnel_LB", 
                            "defense_personnel_DB", 
                            "defense_personnel_other_unknown")

keep_vars_convert <- c("first_down", "rush_attempt", "formation_SHOTGUN", "formation_SINGLEBACK", 
                       "formation_EMPTY", "formation_I_FORM", "formation_WILDCAT", "formation_JUMBO", "formation_PISTOL", 
                       defense_personnel_vars, keep_vars_base, coach_vars, player_vars, missing_vars)

# Apply subsetting
attempt_data <- attempt_data[, ..keep_vars_attempt]
convert_data <- convert_data[, ..keep_vars_convert]

# Ensure outcome is numeric for later models
convert_data[, first_down := as.numeric(as.character(first_down))]

cat("--- Variable Selection Complete ---\n")


## 5. First Stage: XGBoost for Propensity Scores
# ==============================================================================
# Prepare selection data
selection_data <- copy(attempt_data)

cat("\n--- MODIFICATION: Excluding starter_ and missing_information_ variables from the first-stage XGBoost model (keeping coach variables). ---\n")
#player_vars_to_remove_xgb <- grep("^starter_", names(selection_data), value = TRUE)
missing_vars_to_remove_xgb <- grep("^missing_information", names(selection_data), value = TRUE)
xgb_vars_to_exclude <- c("attempt", "my_id", 
                         #player_vars_to_remove_xgb, 
                         missing_vars_to_remove_xgb
)
predictor_vars <- setdiff(names(selection_data), xgb_vars_to_exclude)
cat(sprintf("Coach variables retained in XGBoost model predictors.\n"))
cat(sprintf("%d Missing information variables removed from XGBoost model predictors.\n", length(missing_vars_to_remove_xgb)))

# Impute NAs with 0
for(col in predictor_vars) {
  if(any(is.na(selection_data[[col]]))) {
    set(selection_data, which(is.na(selection_data[[col]])), col, 0)
  }
}

# Define response and predictors for XGBoost
y_var <- "attempt"
x_vars <- predictor_vars

# --- XGBoost Data Preparation ---
selection_matrix <- as.matrix(selection_data[, ..x_vars])
selection_label <- selection_data[[y_var]]

# --- Class Balancing ---
cat("\n--- Calculating Weights for Class Balancing ---\n")
n_total <- nrow(selection_data)
n_pos <- sum(selection_label == 1)
n_neg <- n_total - n_pos
model_weights <- ifelse(selection_label == 1, (n_total / (2 * n_pos)), (n_total / (2 * n_neg)))

# Create the special xgb.DMatrix object
dtrain <- xgb.DMatrix(data = selection_matrix, label = selection_label, weight = model_weights)

# --- NEW: Quartile Elimination with Cumulative Average Ranking ---
cat("\n--- Tuning XGBoost: Elimination with Cumulative Average Ranking ---\n")

# Add combination ID to grid for tracking (FIX: convert to data.table first)
hyper_grid <- as.data.table(hyper_grid)
hyper_grid[, combo_id := 1:.N]

# Initialize tracking structures
current_grid <- copy(hyper_grid)
all_results <- data.table()  # Store ALL results from ALL loops
ranking_history <- data.table()  # Track #1 ranked combo each loop
loop_number <- 1
consecutive_winner_count <- 0
current_winner_id <- NA

cat(sprintf("Starting with %d hyperparameter combinations\n", nrow(current_grid)))

while(TRUE) {
  
  cat(sprintf("\n========================================\n"))
  cat(sprintf("LOOP %d: Testing %d combinations (1 bootstrap each)\n", 
              loop_number, nrow(current_grid)))
  cat(sprintf("========================================\n"))
  
  # Run 1 bootstrap for each combination in current grid
  loop_results <- data.table()
  
  for (i in 1:nrow(current_grid)) {
    current_params <- as.list(current_grid[i, ])
    
    cat(sprintf("  Testing combination %d/%d (ID: %d): nrounds=%d, eta=%.3f, max_depth=%d\n",
                i, nrow(current_grid), current_params$combo_id,
                current_params$nrounds, current_params$eta, 
                current_params$max_depth))
    
    # Create full parameter list
    xgb_params <- c(
      fixed_params,
      list(
        eta = current_params$eta,
        max_depth = current_params$max_depth,
        min_child_weight = current_params$min_child_weight,
        subsample = current_params$subsample,
        colsample_bytree = current_params$colsample_bytree,
        gamma = current_params$gamma,
        lambda = current_params$lambda,
        alpha = current_params$alpha
      )
    )
    
    # Create bootstrap sample with reproducible seed
    train_idx <- sample(1:n_total, size = n_total, replace = TRUE)
    train_idx <- unique(train_idx)
    test_idx <- setdiff(1:n_total, train_idx)
    
    if(length(test_idx) == 0) {
      # Rare case - skip
      next
    }
    
    # Create train and test DMatrix objects
    dtrain_boot <- xgb.DMatrix(data = selection_matrix[train_idx,], 
                               label = selection_label[train_idx],
                               weight = model_weights[train_idx])
    dtest_boot <- xgb.DMatrix(data = selection_matrix[test_idx,], 
                              label = selection_label[test_idx])
    
    # Train model
    xgb_boot_model <- xgb.train(
      params = xgb_params,
      data = dtrain_boot,
      nrounds = current_params$nrounds,
      verbose = 0
    )
    
    # Evaluate
    preds <- predict(xgb_boot_model, newdata = dtest_boot)
    pred_obj <- prediction(preds, getinfo(dtest_boot, "label"))
    auc_val <- performance(pred_obj, "auc")@y.values[[1]]
    
    if(is.na(auc_val)) next
    
    # Store this loop's result
    result_row <- data.table(
      loop = loop_number,
      combo_id = current_params$combo_id,
      nrounds = current_params$nrounds,
      eta = current_params$eta,
      max_depth = current_params$max_depth,
      min_child_weight = current_params$min_child_weight,
      subsample = current_params$subsample,
      colsample_bytree = current_params$colsample_bytree,
      gamma = current_params$gamma,
      lambda = current_params$lambda,
      alpha = current_params$alpha,
      auc = auc_val
    )
    loop_results <- rbind(loop_results, result_row)
  }
  
  # Add this loop's results to all_results
  all_results <- rbind(all_results, loop_results)
  
  # Calculate cumulative average AUC for ONLY the combinations tested so far
  cumulative_avg <- all_results[, .(
    mean_auc = mean(auc),
    n_loops_completed = .N
  ), by = combo_id]
  
  # IMPORTANT: Only keep combinations that are in current_grid
  cumulative_avg <- cumulative_avg[combo_id %in% current_grid$combo_id]
  
  # Merge with hyperparameters for display
  cumulative_avg <- merge(cumulative_avg, 
                          hyper_grid[, .(combo_id, nrounds, eta, max_depth, min_child_weight,
                                         subsample, colsample_bytree, gamma, lambda, alpha)],
                          by = "combo_id")
  
  # Rank by cumulative average AUC
  cumulative_avg <- cumulative_avg[order(-mean_auc)]
  cumulative_avg[, rank := 1:.N]
  
  # Track the #1 combo
  top_combo <- cumulative_avg[1]
  ranking_history <- rbind(ranking_history, data.table(
    loop = loop_number,
    top_combo_id = top_combo$combo_id,
    top_mean_auc = top_combo$mean_auc,
    top_n_loops = top_combo$n_loops_completed,
    n_combos_remaining = nrow(cumulative_avg)
  ))
  
  # Check if same winner as last loop
  if(!is.na(current_winner_id) && current_winner_id == top_combo$combo_id) {
    consecutive_winner_count <- consecutive_winner_count + 1
  } else {
    current_winner_id <- top_combo$combo_id
    consecutive_winner_count <- 1
  }
  
  # Print loop summary
  cat(sprintf("\nLoop %d Complete:\n", loop_number))
  cat(sprintf("  #1 Combo ID: %d (Mean AUC: %.5f across %d loops)\n", 
              top_combo$combo_id, top_combo$mean_auc, top_combo$n_loops_completed))
  cat(sprintf("  #1 params: nrounds=%d, eta=%.3f, max_depth=%d, subsample=%.2f, colsample=%.2f\n",
              top_combo$nrounds, top_combo$eta, top_combo$max_depth, 
              top_combo$subsample, top_combo$colsample_bytree))
  cat(sprintf("  Consecutive loops at #1: %d\n", consecutive_winner_count))
  cat(sprintf("  Combinations remaining: %d\n", nrow(cumulative_avg)))
  
  # Check stopping conditions
  if(loop_number >= max_loops) {
    cat(sprintf("\n*** STOPPING: Reached maximum loops (%d) ***\n", max_loops))
    break
  }
  
  if(drop_percentage > 0 && nrow(cumulative_avg) == 1) {
    cat(sprintf("\n*** STOPPING: Only 1 combination remains ***\n"))
    break
  }
  
  if(consecutive_winner_count >= stable_winner_loops) {
    cat(sprintf("\n*** STOPPING: Same combination ranked #1 for %d consecutive loops ***\n", 
                stable_winner_loops))
    break
  }
  
  # Drop bottom X% based on cumulative average (if drop_percentage > 0)
  if(drop_percentage > 0) {
    # Calculate number to drop and round UP
    n_to_drop <- ceiling(nrow(cumulative_avg) * drop_percentage)
    
    # Make sure we drop at least 1 combo (unless only 1 remains OR we're at/below 10)
    if(n_to_drop < 1 && nrow(cumulative_avg) > 10 && nrow(cumulative_avg) > 1) {
      n_to_drop <- 1
    }
    
    # Don't drop if we're at 10 or below
    if(nrow(cumulative_avg) <= 10) {
      n_to_drop <- 0
    }
    
    # Calculate how many to keep
    n_to_keep <- nrow(cumulative_avg) - n_to_drop
    
    # Make sure we keep at least 1
    if(n_to_keep < 1) n_to_keep <- 1
    
    kept_combos <- cumulative_avg[1:n_to_keep]
    
    if(n_to_drop > 0) {
      cat(sprintf("  Dropping bottom %.1f%% - keeping %d combinations (dropped %d)\n", 
                  drop_percentage * 100, n_to_keep, n_to_drop))
    } else {
      cat(sprintf("  At %d combinations - no more dropping\n", nrow(cumulative_avg)))
    }
    
    # Update current_grid to only include kept combinations
    current_grid <- hyper_grid[combo_id %in% kept_combos$combo_id]
  } else {
    cat(sprintf("  No elimination - all %d combinations continue to next loop\n", nrow(cumulative_avg)))
  }
  
  loop_number <- loop_number + 1
}

# Extract best parameters from final ranking
best_combo <- cumulative_avg[1]
best_nrounds <- best_combo$nrounds
best_eta <- best_combo$eta
best_max_depth <- best_combo$max_depth
best_min_child_weight <- best_combo$min_child_weight
best_subsample <- best_combo$subsample
best_colsample_bytree <- best_combo$colsample_bytree
best_gamma <- best_combo$gamma
best_lambda <- best_combo$lambda
best_alpha <- best_combo$alpha

cat(sprintf("\n\n=== Elimination Complete ===\n"))
cat(sprintf("Total loops completed: %d\n", loop_number))
cat(sprintf("Total bootstrap samples collected: %d\n", nrow(all_results)))
cat(sprintf("Winning combination ID: %d\n", best_combo$combo_id))
cat(sprintf("Winning combination tested in %d loops\n", best_combo$n_loops_completed))
cat(sprintf("\nOptimal Hyperparameters:\n"))
cat(sprintf("  nrounds: %d\n", best_nrounds))
cat(sprintf("  eta: %.3f\n", best_eta))
cat(sprintf("  max_depth: %d\n", best_max_depth))
cat(sprintf("  min_child_weight: %d\n", best_min_child_weight))
cat(sprintf("  subsample: %.2f\n", best_subsample))
cat(sprintf("  colsample_bytree: %.2f\n", best_colsample_bytree))
cat(sprintf("  gamma: %.2f\n", best_gamma))
cat(sprintf("  lambda: %d\n", best_lambda))
cat(sprintf("  alpha: %d\n", best_alpha))
cat(sprintf("  Cumulative Mean AUC: %.5f\n", best_combo$mean_auc))

# Save results
fwrite(all_results, "script_outputs_players/xgboost_all_loop_results_player.csv")
fwrite(cumulative_avg, "script_outputs_players/xgboost_final_rankings_player.csv")
fwrite(ranking_history, "script_outputs_players/xgboost_ranking_history_player.csv")

# ==============================================================================
# Plot: AUC vs nrounds with color showing loop survival
# ==============================================================================
cat("\n--- Creating AUC vs nrounds plot colored by loop survival ---\n")

# Calculate how many loops each combo survived (i.e., max loop number it appears in)
combo_survival <- all_results[, .(max_loop = max(loop)), by = combo_id]

# Calculate mean AUC across all loops for each combo
combo_mean_auc <- all_results[, .(mean_auc = mean(auc)), by = .(combo_id, nrounds)]

# Merge survival info with mean AUC
plot_data <- merge(combo_mean_auc, combo_survival, by = "combo_id")

# Create the plot with just dots
p_nrounds <- ggplot(plot_data, aes(x = nrounds, y = mean_auc, color = max_loop)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_viridis_c(name = "Loops Survived", option = "plasma") +
  labs(
    title = "XGBoost Tuning Performance: AUC vs nrounds",
    subtitle = "Color indicates how many loops each hyperparameter combination survived",
    x = "Number of Rounds (nrounds)",
    y = "Mean AUC Across Loops"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

print(p_nrounds)

# Save the plot
ggsave("script_outputs_players/xgboost_auc_vs_nrounds_survival.png", 
       plot = p_nrounds, width = 12, height = 7, dpi = 300)

cat("--- Plot saved to script_outputs_players/xgboost_auc_vs_nrounds_survival.png ---\n")
# Save best hyperparameters
best_hyperparameters <- data.table(
  parameter = c("combo_id", "nrounds", "eta", "max_depth", "min_child_weight", 
                "subsample", "colsample_bytree", "gamma", "lambda", "alpha",
                "objective", "eval_metric",
                "cumulative_mean_auc", "n_loops_tested", "total_loops_run",
                "final_consecutive_#1_count", "drop_percentage"),
  value = c(best_combo$combo_id, best_nrounds, best_eta, best_max_depth, best_min_child_weight,
            best_subsample, best_colsample_bytree, best_gamma, best_lambda, best_alpha,
            fixed_params$objective, fixed_params$eval_metric,
            best_combo$mean_auc, best_combo$n_loops_completed, loop_number,
            consecutive_winner_count, drop_percentage)
)
fwrite(best_hyperparameters, "script_outputs_players/xgboost_best_hyperparameters_player.csv")

# Create final parameter list
final_xgb_params <- c(
  fixed_params,
  list(
    eta = best_eta,
    max_depth = best_max_depth,
    min_child_weight = best_min_child_weight,
    subsample = best_subsample,
    colsample_bytree = best_colsample_bytree,
    gamma = best_gamma,
    lambda = best_lambda,
    alpha = best_alpha
  )
)

# --- Performance Assessment of Best Hyperparameters ---
cat("\n--- Assessing Performance on Fresh Bootstrapped Samples ---\n")
bootstrap_aucs <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  cat(sprintf("\rBootstrap Sample %d of %d...", i, n_bootstrap))
  
  train_idx <- sample(1:n_total, size = n_total, replace = TRUE)
  train_idx <- unique(train_idx)
  test_idx <- setdiff(1:n_total, train_idx)
  
  if(length(test_idx) == 0) next
  
  dtrain_boot <- xgb.DMatrix(data = selection_matrix[train_idx,], label = selection_label[train_idx])
  dtest_boot <- xgb.DMatrix(data = selection_matrix[test_idx,], label = selection_label[test_idx])
  
  boot_weights <- model_weights[train_idx]
  setinfo(dtrain_boot, "weight", boot_weights)
  
  xgb_boot_model <- xgb.train(
    params = final_xgb_params,
    data = dtrain_boot,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  p_t <- predict(xgb_boot_model, newdata = dtest_boot)
  pred_t <- prediction(p_t, getinfo(dtest_boot, "label"))
  auc_val <- performance(pred_t, "auc")@y.values[[1]]
  if (!is.na(auc_val)) {
    bootstrap_aucs[i] <- auc_val
  }
}
bootstrap_aucs <- bootstrap_aucs[!is.na(bootstrap_aucs) & bootstrap_aucs > 0]
cat(sprintf("\n\nFinal Assessment - Mean AUC over %d bootstraps: %.4f (SD: %.4f)\n", 
            length(bootstrap_aucs), mean(bootstrap_aucs), sd(bootstrap_aucs)))

# write the bootstrap_aucs
fwrite(data.table(bootstrap_aucs), "script_outputs_players/xgboost_bootstrap_aucs_player.csv")

# --- Train Final Model and Get Propensity Scores ---
cat("\n--- Training Final XGBoost Model on Full Data to Get Propensity Scores ---\n")
final_xgb_model <- xgb.train(
  params = final_xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = TRUE,
  print_every_n = 50
)

# Predict probabilities (propensity scores) on the entire dataset
dselection_full <- xgb.DMatrix(data = selection_matrix, label = selection_label)
xgb_probs <- predict(final_xgb_model, newdata = dselection_full)

attempt_data[, xgboost_prob := xgb_probs]
convert_data <- merge(convert_data, attempt_data[, .(my_id, xgboost_prob)], by = "my_id", all.x = TRUE)
cat("--- Propensity scores calculated and merged ---\n")

#write convert data it out
fwrite(convert_data, "script_outputs_players/convert_data_with_xgboost_prob_player.csv")
fwrite(attempt_data, "script_outputs_players/attempt_data_with_xgboost_prob_player.csv")

convert_data <- fread("script_outputs_players/convert_data_with_xgboost_prob_player.csv")
attempt_data <- fread("script_outputs_players/attempt_data_with_xgboost_prob_player.csv")






## 6. Propensity Score Plot
# ==============================================================================
p <- ggplot(convert_data, aes(x = xgboost_prob)) +
  geom_histogram(bins = 100, fill = "seagreen", alpha = 0.7) +
  labs(title = "Distribution of XGBoost Propensity Scores (100 Bins)",
       x = "Propensity Score",
       y = "Frequency") +
  theme_minimal()

print(p)
ggsave("script_outputs_players/xgboost_propensity_score_distribution_player.png", plot = p, width = 10, height = 6)
cat("\n--- Propensity score distribution plot saved to script_outputs_players/xgboost_propensity_score_distribution_player.png ---\n")



## 7. Prepare Second Stage Models
# ==============================================================================
# Create polynomial and interaction columns directly in the data
cat("\n--- Creating polynomial, interaction, and offensive player interaction columns ---\n")

# Create a copy of convert_data to work with
convert_data_with_features <- copy(convert_data)

# Create polynomial columns for conversion/outcome models
convert_data_with_features[, ydstogo_squared := ydstogo^2]
convert_data_with_features[, yardline_100_squared := yardline_100^2]
convert_data_with_features[, score_diff_squared := score_diff^2]
convert_data_with_features[, ydstogo_X_yardline_100 := ydstogo * yardline_100]
convert_data_with_features[, temp_X_wind := temp * wind]
convert_data_with_features[, is_winning_X_score_diff := is_winning * score_diff]
convert_data_with_features[, is_winning_X_score_diff_squared := is_winning * score_diff_squared]

# MODIFIED: Create ALL offensive player interaction columns with rush_attempt
# This includes QB, RB, WR, TE, T, G, C positions
# Excludes defensive players (DE, DT, NT, ILB, OLB, CB, FS, SS) and missing_information variables

# Get all offensive player variables (excluding defense and missing_information)
offensive_player_vars <- grep("^starter_offense_.*grades", player_vars, value = TRUE)
all_offensive_vars <- grep("^starter_offense_", player_vars, value = TRUE)

# Get all defensive player variables  
defensive_player_vars <- grep("^starter_defense_.*grades", player_vars, value = TRUE)
all_defensive_vars <- grep("^starter_defense_", player_vars, value = TRUE)

# Use the union to ensure we get all offensive variables
offensive_vars_to_interact <- unique(c(offensive_player_vars, all_offensive_vars))
defensive_vars_to_interact <- unique(c(defensive_player_vars, all_defensive_vars))

# Create interaction columns for ALL offensive players
offensive_interaction_col_names <- c()
for(offensive_var in offensive_vars_to_interact) {
  interaction_name <- paste0(offensive_var, "_X_rush_attempt")
  convert_data_with_features[, (interaction_name) := get(offensive_var) * rush_attempt]
  offensive_interaction_col_names <- c(offensive_interaction_col_names, interaction_name)
}


# Create interaction columns for ALL defensive players
defensive_interaction_col_names <- c()
for(defensive_var in defensive_vars_to_interact) {
  interaction_name <- paste0(defensive_var, "_X_rush_attempt")
  convert_data_with_features[, (interaction_name) := get(defensive_var) * rush_attempt]
  defensive_interaction_col_names <- c(defensive_interaction_col_names, interaction_name)
}

cat(sprintf("Created %d defensive player interaction columns with rush_attempt\n", length(defensive_interaction_col_names)))
cat("Defensive positions included: DE, DT, NT, ILB, OLB, CB, FS, SS\n")

# Then MODIFY the console output line to show both:
cat(sprintf("Total player x rush_attempt interactions created: %d\n", 
            length(offensive_interaction_col_names) + length(defensive_interaction_col_names)))
# Define base formula string with all created columns
base_formula_str <- "first_down ~ week + 
      year_2017 + year_2018 + year_2019 + year_2020 + year_2021 + year_2022 + 
      ydstogo + ydstogo_squared + yardline_100 + yardline_100_squared + ydstogo_X_yardline_100 +
      rush_attempt + posteam_timeouts_remaining + defteam_timeouts_remaining +
      temp + wind + temp_X_wind + vegas_wp + spread_line + total_line + prep_days +
      home_attendance + team_win_pct + 
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
      kicker_id_field_goals_grades_grades_fgep_kicker_12w +
      punter_id_punting_grades_grades_punter_12w + score_diff +
      score_diff_squared + is_winning + is_winning_X_score_diff + is_winning_X_score_diff_squared + is_home_team"

# Add coach, player, and missing information variables to formula string
if(length(coach_vars) > 0) {
  base_formula_str <- paste(base_formula_str, "+", paste(coach_vars, collapse = " + "))
}
if(length(player_vars) > 0) {
  base_formula_str <- paste(base_formula_str, "+", paste(player_vars, collapse = " + "))
}
if(length(missing_vars) > 0) {
  base_formula_str <- paste(base_formula_str, "+", paste(missing_vars, collapse = " + "))
}

# Add ALL the offensive player interaction columns
if(length(offensive_interaction_col_names) > 0) {
  base_formula_str <- paste(base_formula_str, "+", paste(offensive_interaction_col_names, collapse = " + "))
}
# Add ALL the defensive player interaction columns
if(length(defensive_interaction_col_names) > 0) {
  base_formula_str <- paste(base_formula_str, "+", paste(defensive_interaction_col_names, collapse = " + "))
}
# Clean data by removing rows with NAs in any required variable
all_vars_in_formula <- all.vars(as.formula(base_formula_str))
all_needed_vars <- unique(c(all_vars_in_formula, "xgboost_prob"))
clean_convert_data <- convert_data_with_features[complete.cases(convert_data_with_features[, ..all_needed_vars])]

cat("--- Second stage data prepared with all feature columns including offensive player interactions ---\n")

## 8. Apply Polynomial IMR Correction Terms
# ==============================================================================
# clean_convert_data[, correction_term_linear := {
#   dnorm(qnorm(xgboost_prob)) / pmax(xgboost_prob, .Machine$double.eps)
# }]
clean_convert_data[, correction_term_linear := {
  phi <- dnorm(qnorm(xgboost_prob))
  Phi <- xgboost_prob
  phi / Phi
}]

clean_convert_data[, correction_term_poly2 := correction_term_linear^2]
clean_convert_data[, correction_term_poly3 := correction_term_linear^3]

# Handle any infinite values produced by transformations
clean_convert_data[is.infinite(correction_term_linear), correction_term_linear := NA]
clean_convert_data[is.infinite(correction_term_poly2), correction_term_poly2 := NA]
clean_convert_data[is.infinite(correction_term_poly3), correction_term_poly3 := NA]

# Remove any rows that have NA after correction
clean_convert_data <- na.omit(clean_convert_data)

cat("--- Polynomial IMR correction terms calculated ---\n")


## 9. Helper Functions for Model Results Extraction
# ==============================================================================
# Function to extract model results with heteroskedasticity-adjusted SEs
get_model_results <- function(model, model_type) {
  
  if (is.null(model)) {
    return(NULL)
  }
  
  tryCatch({
    # For OLS models (LPM) - use robust standard errors
    if (model_type == "LPM") {
      coefs <- coef(model)
      # Get heteroskedasticity-robust standard errors
      robust_vcov <- vcovHC(model, type = "HC1")
      robust_se <- sqrt(diag(robust_vcov))
      tvals <- coefs / robust_se
      pvals <- 2 * pt(abs(tvals), df = model$df.residual, lower.tail = FALSE)
      
      return(list(
        ME = coefs,
        SE = robust_se,
        tval = tvals,
        pval = pvals
      ))
    }
    
    # For Probit models - use mfx for main effects, coefficients for interactions
    if (model_type == "Probit") {
      mfx_result <- probitmfx(formula = formula(model), 
                              data = model.frame(model), 
                              robust = TRUE)
      
      if (!is.null(mfx_result)) {
        mfx_mat <- mfx_result$mfxest
        
        # Get the mfx results for variables that mfx could compute
        me_vec <- mfx_mat[, "dF/dx"]
        se_vec <- mfx_mat[, "Std. Err."]
        tval_vec <- mfx_mat[, "z"]
        pval_vec <- mfx_mat[, "P>|z|"]
        
        # Now add any missing variables from the GLM model
        all_coefs <- coef(model)
        mfx_vars <- rownames(mfx_mat)
        missing_vars <- setdiff(names(all_coefs), c("(Intercept)", mfx_vars))
        
        if(length(missing_vars) > 0) {
          # Get robust standard errors for the full model
          robust_vcov <- vcovHC(model, type = "HC1")
          robust_se_full <- sqrt(diag(robust_vcov))
          
          for(var in missing_vars) {
            me_vec[var] <- all_coefs[var]
            se_vec[var] <- robust_se_full[var]
            tval_vec[var] <- all_coefs[var] / robust_se_full[var]
            pval_vec[var] <- 2 * pnorm(abs(tval_vec[var]), lower.tail = FALSE)
          }
        }
        
        return(list(
          ME = me_vec,
          SE = se_vec,
          tval = tval_vec,
          pval = pval_vec
        ))
      }
    }
    
    # For Logit models - use mfx for main effects, coefficients for interactions
    if (model_type == "Logit") {
      mfx_result <- logitmfx(formula = formula(model), 
                             data = model.frame(model), 
                             robust = TRUE)
      
      if (!is.null(mfx_result)) {
        mfx_mat <- mfx_result$mfxest
        
        # Get the mfx results for variables that mfx could compute
        me_vec <- mfx_mat[, "dF/dx"]
        se_vec <- mfx_mat[, "Std. Err."]
        tval_vec <- mfx_mat[, "z"]
        pval_vec <- mfx_mat[, "P>|z|"]
        
        # Now add any missing variables from the GLM model
        all_coefs <- coef(model)
        mfx_vars <- rownames(mfx_mat)
        missing_vars <- setdiff(names(all_coefs), c("(Intercept)", mfx_vars))
        
        if(length(missing_vars) > 0) {
          # Get robust standard errors for the full model
          robust_vcov <- vcovHC(model, type = "HC1")
          robust_se_full <- sqrt(diag(robust_vcov))
          
          for(var in missing_vars) {
            me_vec[var] <- all_coefs[var]
            se_vec[var] <- robust_se_full[var]
            tval_vec[var] <- all_coefs[var] / robust_se_full[var]
            pval_vec[var] <- 2 * pnorm(abs(tval_vec[var]), lower.tail = FALSE)
          }
        }
        
        return(list(
          ME = me_vec,
          SE = se_vec,
          tval = tval_vec,
          pval = pval_vec
        ))
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    cat("Error extracting results for", model_type, ":", e$message, "\n")
    return(NULL)
  })
}

## 10. Fit Second Stage Models with Polynomial IMR Terms
# ==============================================================================
cat("\n--- Fitting second stage models with polynomial IMR terms... ---\n")

# Create formula with polynomial IMR terms
poly_formula <- as.formula(paste(base_formula_str,
                                 "+ correction_term_linear + correction_term_poly2 + correction_term_poly3"))
# Fit models
lpm_poly <- lm(poly_formula, data = clean_convert_data)
probit_poly <- glm(poly_formula, data = clean_convert_data, family = binomial(link = "probit"))
logit_poly <- glm(poly_formula, data = clean_convert_data, family = binomial(link = "logit"))

# Extract results
lpm_poly_results <- get_model_results(lpm_poly, "LPM")
probit_poly_results <- get_model_results(probit_poly, "Probit")
logit_poly_results <- get_model_results(logit_poly, "Logit")

cat("--- Model fitting complete ---\n")

## 11. Create and Save Result Matrices
# ==============================================================================
cat("\n--- Creating result matrices ---\n")

# Get all variables from the polynomial formula
all_vars <- all.vars(poly_formula)
all_vars <- all_vars[all_vars != "first_down"] # Remove dependent variable

# Create storage matrices
n_vars <- length(all_vars)
me_matrix <- matrix(NA, nrow = n_vars, ncol = 3)
se_matrix <- matrix(NA, nrow = n_vars, ncol = 3)
tval_matrix <- matrix(NA, nrow = n_vars, ncol = 3)
pval_matrix <- matrix(NA, nrow = n_vars, ncol = 3)

rownames(me_matrix) <- rownames(se_matrix) <- rownames(tval_matrix) <- rownames(pval_matrix) <- all_vars
colnames(me_matrix) <- colnames(se_matrix) <- colnames(tval_matrix) <- colnames(pval_matrix) <- c("LPM", "Probit", "Logit")

# Fill matrices from polynomial model results
for (var in all_vars) {
  # LPM results
  if (!is.null(lpm_poly_results) && var %in% names(lpm_poly_results$ME)) {
    me_matrix[var, "LPM"] <- lpm_poly_results$ME[var]
    se_matrix[var, "LPM"] <- lpm_poly_results$SE[var]
    tval_matrix[var, "LPM"] <- lpm_poly_results$tval[var]
    pval_matrix[var, "LPM"] <- lpm_poly_results$pval[var]
  }
  
  # Probit results
  if (!is.null(probit_poly_results) && var %in% names(probit_poly_results$ME)) {
    me_matrix[var, "Probit"] <- probit_poly_results$ME[var]
    se_matrix[var, "Probit"] <- probit_poly_results$SE[var]
    tval_matrix[var, "Probit"] <- probit_poly_results$tval[var]
    pval_matrix[var, "Probit"] <- probit_poly_results$pval[var]
  }
  
  # Logit results
  if (!is.null(logit_poly_results) && var %in% names(logit_poly_results$ME)) {
    me_matrix[var, "Logit"] <- logit_poly_results$ME[var]
    se_matrix[var, "Logit"] <- logit_poly_results$SE[var]
    tval_matrix[var, "Logit"] <- logit_poly_results$tval[var]
    pval_matrix[var, "Logit"] <- logit_poly_results$pval[var]
  }
}

# Save all matrices
write.csv(me_matrix, "script_outputs_players/player_marginal_effects_matrix.csv")
write.csv(se_matrix, "script_outputs_players/player_standard_errors_matrix.csv")
write.csv(tval_matrix, "script_outputs_players/player_t_values_matrix.csv")
write.csv(pval_matrix, "script_outputs_players/player_p_values_matrix.csv")

cat("--- All matrices saved to 'script_outputs_players' directory ---\n")

## 12. Create Summary Table for Polynomial IMR Correction Terms
# ==============================================================================
# Initialize results table for correction terms only
correction_results <- data.table(
  Correction_Term = character(),
  Model = character(),
  Coefficient = numeric(),
  Std_Error = numeric(),
  T_value = numeric(),
  P_value = numeric()
)

# Extract results for polynomial IMR terms
poly_vars <- c("correction_term_linear", "correction_term_poly2", "correction_term_poly3")
poly_names <- c("IMR Linear", "IMR Polynomial 2", "IMR Polynomial 3")

for(i in 1:3) {
  for(model_type in c("LPM", "Probit", "Logit")) {
    if (model_type == "LPM" && !is.null(lpm_poly_results) && poly_vars[i] %in% names(lpm_poly_results$ME)) {
      results <- lpm_poly_results
    } else if (model_type == "Probit" && !is.null(probit_poly_results) && poly_vars[i] %in% names(probit_poly_results$ME)) {
      results <- probit_poly_results
    } else if (model_type == "Logit" && !is.null(logit_poly_results) && poly_vars[i] %in% names(logit_poly_results$ME)) {
      results <- logit_poly_results
    } else {
      next
    }
    
    correction_results <- rbind(correction_results, data.table(
      Correction_Term = poly_names[i],
      Model = model_type,
      Coefficient = results$ME[poly_vars[i]],
      Std_Error = results$SE[poly_vars[i]],
      T_value = results$tval[poly_vars[i]],
      P_value = results$pval[poly_vars[i]]
    ))
  }
}

# Create wide format table with Correction_Term as rows and Model as columns (T-values only)
t_values_wide <- dcast(correction_results, Correction_Term ~ Model, value.var = "T_value")

# Define the desired row order
row_order <- c("IMR Linear", "IMR Polynomial 2", "IMR Polynomial 3")
t_values_wide <- t_values_wide[match(row_order, Correction_Term)]

# Reorder columns
col_order <- c("Correction_Term", "LPM", "Probit", "Logit")
t_values_wide <- t_values_wide[, ..col_order]

# Round values
numeric_cols <- c("LPM", "Probit", "Logit")
t_values_wide[, (numeric_cols) := lapply(.SD, round, 3), .SDcols = numeric_cols]

# Save correction term t-values
fwrite(t_values_wide, "script_outputs_players/xgboost_polynomial_imr_tvalues_player.csv")

# Also save the full correction results table
fwrite(correction_results, "script_outputs_players/polynomial_imr_full_results_player.csv")

cat("\n--- Final polynomial IMR t-values table (using XGBoost): ---\n")
print(t_values_wide)

## 13. Run Attempt Models on Selection Data and Extract Full Results
# ==============================================================================
cat("\n--- Running Attempt Models (LPM, Probit, Logit) with Full Results Extraction ---\n")

# Prepare selection data with polynomial and interaction columns
selection_data_with_features <- copy(selection_data)

# Create polynomial and interaction columns for attempt models
selection_data_with_features[, ydstogo_squared := ydstogo^2]
selection_data_with_features[, yardline_100_squared := yardline_100^2]
selection_data_with_features[, score_diff_squared := score_diff^2]
selection_data_with_features[, temp_X_wind := temp * wind]
selection_data_with_features[, is_winning_X_score_diff := is_winning * score_diff]
selection_data_with_features[, is_winning_X_score_diff_squared := is_winning * score_diff_squared]
selection_data_with_features[, seconds_remaining_in_half_squared := seconds_remaining_in_half^2]
selection_data_with_features[, seconds_remaining_in_half_X_score_diff := seconds_remaining_in_half * score_diff]
selection_data_with_features[, seconds_remaining_in_half_X_is_winning := seconds_remaining_in_half * is_winning]
selection_data_with_features[, seconds_remaining_in_half_squared_X_score_diff := seconds_remaining_in_half_squared * score_diff]
selection_data_with_features[, seconds_remaining_in_half_squared_X_is_winning := seconds_remaining_in_half_squared * is_winning]
selection_data_with_features[, seconds_remaining_in_half_X_is_winning_X_score_diff := seconds_remaining_in_half * is_winning * score_diff]
selection_data_with_features[, seconds_remaining_in_half_squared_X_is_winning_X_score_diff := seconds_remaining_in_half_squared * is_winning * score_diff]

# Get coach, player, and missing information variables dynamically
coach_vars_attempt <- grep("^coach_", names(selection_data_with_features), value = TRUE)
coach_vars_attempt <- setdiff(coach_vars_attempt, c("coach_experience", "coach_tenure", "coach_age"))
player_vars_attempt <- grep("^starter_", names(selection_data_with_features), value = TRUE)
missing_vars_attempt <- grep("^missing_information", names(selection_data_with_features), value = TRUE)

# Create formula string for attempt models using column names
attempt_formula_str <- "attempt ~ week + 
                        year_2017 +
                        year_2018 + 
                        year_2019 + 
                        year_2020 + 
                        year_2021 + 
                        year_2022 + 
                        ydstogo + 
                        ydstogo_squared +
                        yardline_100 +
                        yardline_100_squared +
                        posteam_timeouts_remaining + 
                        defteam_timeouts_remaining + 
                        temp +
                        wind + 
                        temp_X_wind +
                        vegas_wp + 
                        spread_line + 
                        total_line + 
                        prep_days + 
                        home_attendance + 
                        team_win_pct + 
                        kicker_id_field_goals_grades_grades_fgep_kicker_12w + 
                        punter_id_punting_grades_grades_punter_12w + 
                        score_diff + 
                        score_diff_squared +
                        is_winning +
                        is_winning_X_score_diff +
                        is_winning_X_score_diff_squared +
                        is_home_team + 
                        is_first_half + 
                        seconds_remaining_in_half +
                        seconds_remaining_in_half_squared +
                        seconds_remaining_in_half_X_score_diff +
                        seconds_remaining_in_half_X_is_winning +
                        seconds_remaining_in_half_squared_X_score_diff +
                        seconds_remaining_in_half_squared_X_is_winning +
                        seconds_remaining_in_half_X_is_winning_X_score_diff +
                        seconds_remaining_in_half_squared_X_is_winning_X_score_diff"

# Add coach, player, and missing information variables to formula
if(length(coach_vars_attempt) > 0) {
  attempt_formula_str <- paste(attempt_formula_str, "+", paste(coach_vars_attempt, collapse = " + "))
}
if(length(player_vars_attempt) > 0) {
  attempt_formula_str <- paste(attempt_formula_str, "+", paste(player_vars_attempt, collapse = " + "))
}
if(length(missing_vars_attempt) > 0) {
  attempt_formula_str <- paste(attempt_formula_str, "+", paste(missing_vars_attempt, collapse = " + "))
}

# Convert to formula object
attempt_formula <- as.formula(attempt_formula_str)

# Get all variables needed for the models
all_vars_attempt <- all.vars(attempt_formula)
clean_selection_data <- selection_data_with_features[complete.cases(selection_data_with_features[, ..all_vars_attempt])]

# Fit the three models
lpm_attempt <- lm(attempt_formula, data = clean_selection_data)
probit_attempt <- glm(attempt_formula, data = clean_selection_data, family = binomial(link = "probit"))
logit_attempt <- glm(attempt_formula, data = clean_selection_data, family = binomial(link = "logit"))

# Extract results for attempt models
lpm_attempt_results <- get_model_results(lpm_attempt, "LPM")
probit_attempt_results <- get_model_results(probit_attempt, "Probit")
logit_attempt_results <- get_model_results(logit_attempt, "Logit")

# Create matrices for attempt models
attempt_vars <- all.vars(attempt_formula)
attempt_vars <- attempt_vars[attempt_vars != "attempt"]

n_vars_attempt <- length(attempt_vars)
me_matrix_attempt <- matrix(NA, nrow = n_vars_attempt, ncol = 3)
se_matrix_attempt <- matrix(NA, nrow = n_vars_attempt, ncol = 3)
tval_matrix_attempt <- matrix(NA, nrow = n_vars_attempt, ncol = 3)
pval_matrix_attempt <- matrix(NA, nrow = n_vars_attempt, ncol = 3)

rownames(me_matrix_attempt) <- rownames(se_matrix_attempt) <- 
  rownames(tval_matrix_attempt) <- rownames(pval_matrix_attempt) <- attempt_vars
colnames(me_matrix_attempt) <- colnames(se_matrix_attempt) <- 
  colnames(tval_matrix_attempt) <- colnames(pval_matrix_attempt) <- c("LPM", "Probit", "Logit")

# Fill matrices for attempt models
for (var in attempt_vars) {
  # LPM results
  if (!is.null(lpm_attempt_results) && var %in% names(lpm_attempt_results$ME)) {
    me_matrix_attempt[var, "LPM"] <- lpm_attempt_results$ME[var]
    se_matrix_attempt[var, "LPM"] <- lpm_attempt_results$SE[var]
    tval_matrix_attempt[var, "LPM"] <- lpm_attempt_results$tval[var]
    pval_matrix_attempt[var, "LPM"] <- lpm_attempt_results$pval[var]
  }
  
  # Probit results
  if (!is.null(probit_attempt_results) && var %in% names(probit_attempt_results$ME)) {
    me_matrix_attempt[var, "Probit"] <- probit_attempt_results$ME[var]
    se_matrix_attempt[var, "Probit"] <- probit_attempt_results$SE[var]
    tval_matrix_attempt[var, "Probit"] <- probit_attempt_results$tval[var]
    pval_matrix_attempt[var, "Probit"] <- probit_attempt_results$pval[var]
  }
  
  # Logit results
  if (!is.null(logit_attempt_results) && var %in% names(logit_attempt_results$ME)) {
    me_matrix_attempt[var, "Logit"] <- logit_attempt_results$ME[var]
    se_matrix_attempt[var, "Logit"] <- logit_attempt_results$SE[var]
    tval_matrix_attempt[var, "Logit"] <- logit_attempt_results$tval[var]
    pval_matrix_attempt[var, "Logit"] <- logit_attempt_results$pval[var]
  }
}

# Save attempt model matrices
write.csv(me_matrix_attempt, "script_outputs_players/player_attempt_marginal_effects_matrix.csv")
write.csv(se_matrix_attempt, "script_outputs_players/player_attempt_standard_errors_matrix.csv")
write.csv(tval_matrix_attempt, "script_outputs_players/player_attempt_t_values_matrix.csv")
write.csv(pval_matrix_attempt, "script_outputs_players/player_attempt_p_values_matrix.csv")

cat("--- Analysis Complete ---\n")
cat("\n--- Key Modification: Bootstrap validation used instead of internal CV ---\n")
cat(sprintf("- Best nrounds: %d with mean bootstrap AUC: %.5f\n", best_nrounds, best_combo$mean_auc))
cat("- Results saved with error bars showing bootstrap variability\n")
cat("\n--- Using player and missing information variables instead of PCA ---\n")

# ==============================================================================
# 14. Joint Significance Tests for Player Position Groups (Raw P-Value Table)
# ==============================================================================
cat("\n--- Performing Joint Significance Tests for Player Position Groups ---\n")

# The 'lmtest' library is required.

# Modified helper function to include interactions for Outcome models
# ALSO excludes missing_information variables
get_player_group_vars <- function(group_pattern, all_vars, include_interactions = TRUE) {
  vars <- grep(group_pattern, all_vars, value = TRUE)
  # ALWAYS exclude missing_information variables
  vars <- vars[!grepl("^missing_information", vars)]
  if (!include_interactions) {
    # For Attempt models, also exclude interaction terms
    vars <- vars[!grepl(":", vars)]
  }
  # For Outcome models, keep all variables including interactions (except missing_information)
  return(vars)
}

# Modified helper function for CASE 3 that INCLUDES interaction terms for Outcome models
# ALSO excludes missing_information variables
get_grade_type_vars <- function(group_patterns, all_vars, include_interactions = TRUE) {
  vars <- c()
  for(pattern in group_patterns) {
    matching_vars <- grep(pattern, all_vars, value = TRUE)
    # ALWAYS exclude missing_information variables
    matching_vars <- matching_vars[!grepl("^missing_information", matching_vars)]
    if (!include_interactions) {
      # For Attempt models, exclude interaction terms
      matching_vars <- matching_vars[!grepl(":", matching_vars)]
    }
    vars <- c(vars, matching_vars)
  }
  return(unique(vars))
}

# CASE 1: Original grouped positions (offense skill, OL, defense groups, etc.)
player_groups_case1 <- list(
  "Offense_QBs" = "^starter_offense_QB",
  "Offense_Skill" = "^starter_offense_(RB|WR|TE)",
  "Offense_OL" = "^starter_offense_(T|G|C)",
  "Defense_DL" = "^starter_defense_(DE|DT|NT)",
  "Defense_LBs" = "^starter_defense_(ILB|OLB)",
  "Defense_DBs" = "^starter_defense_(CB|FS|SS)"
)

# CASE 2: Individual position groups (each position type separately)
player_groups_case2 <- list(
  "Offense_QB" = "^starter_offense_QB_",
  "Offense_RB" = "^starter_offense_RB_",
  "Offense_WR" = "^starter_offense_WR_",
  "Offense_TE" = "^starter_offense_TE_",
  "Offense_T" = "^starter_offense_T_",
  "Offense_G" = "^starter_offense_G_",
  "Offense_C" = "^starter_offense_C_",
  "Defense_DE" = "^starter_defense_DE_",
  "Defense_DT" = "^starter_defense_DT_",
  "Defense_NT" = "^starter_defense_NT_",
  "Defense_ILB" = "^starter_defense_ILB_",
  "Defense_OLB" = "^starter_defense_OLB_",
  "Defense_CB" = "^starter_defense_CB_",
  "Defense_FS" = "^starter_defense_FS_",
  "Defense_SS" = "^starter_defense_SS_"
)

# CASE 3: Grade Types Across Positions
# CORRECTED: All patterns now start with ^ to ensure they don't match missing_information variables
grade_type_groups <- list(
  "Pass_Blocking" = c(
    "^starter_offense_RB_[0-9]+_receiving_grades_grades_pass_block_12w",
    "^starter_offense_TE_[0-9]+_blocking_grades_grades_pass_block_12w",
    "^starter_offense_T_[0-9]+_blocking_grades_grades_pass_block_12w",
    "^starter_offense_G_[0-9]+_blocking_grades_grades_pass_block_12w",
    "^starter_offense_C_[0-9]+_blocking_grades_grades_pass_block_12w"
  ),
  
  "Run_Blocking" = c(
    "^starter_offense_WR_[0-9]+_blocking_grades_grades_run_block_12w",
    "^starter_offense_TE_[0-9]+_blocking_grades_grades_run_block_12w",
    "^starter_offense_T_[0-9]+_blocking_grades_grades_run_block_12w",
    "^starter_offense_G_[0-9]+_blocking_grades_grades_run_block_12w",
    "^starter_offense_C_[0-9]+_blocking_grades_grades_run_block_12w"
  ),
  
  "Rushing" = c(
    "^starter_offense_RB_[0-9]+_rushing_grades_grades_run_12w"
  ),
  
  "Passing" = c(
    "^starter_offense_QB_[0-9]+_passing_.*_grades_pass_12w"
  ),
  
  "Receiving_Pass_Routes" = c(
    "^starter_offense_RB_[0-9]+_receiving_.*_grades_pass_route_12w",
    "^starter_offense_WR_[0-9]+_receiving_.*_grades_pass_route_12w",
    "^starter_offense_TE_[0-9]+_receiving_.*_grades_pass_route_12w"
  ),
  
  "Run_Defense" = c(
    "^starter_defense_DE_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_DT_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_NT_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_ILB_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_OLB_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_CB_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_FS_[0-9]+_defensive_grades_grades_run_defense_12w",
    "^starter_defense_SS_[0-9]+_defensive_grades_grades_run_defense_12w"
  ),
  
  "Pass_Rush" = c(
    "^starter_defense_DE_[0-9]+_defensive_grades_grades_pass_rush_defense_12w",
    "^starter_defense_DT_[0-9]+_defensive_grades_grades_pass_rush_defense_12w",
    "^starter_defense_NT_[0-9]+_defensive_grades_grades_pass_rush_defense_12w"
  ),
  
  "Man_Coverage" = c(
    "^starter_defense_ILB_[0-9]+_coverage_schemes_man_grades_coverage_defense_12w",
    "^starter_defense_OLB_[0-9]+_coverage_schemes_man_grades_coverage_defense_12w",
    "^starter_defense_CB_[0-9]+_coverage_schemes_man_grades_coverage_defense_12w",
    "^starter_defense_FS_[0-9]+_coverage_schemes_man_grades_coverage_defense_12w",
    "^starter_defense_SS_[0-9]+_coverage_schemes_man_grades_coverage_defense_12w"
  ),
  
  "Zone_Coverage" = c(
    "^starter_defense_ILB_[0-9]+_coverage_schemes_zone_grades_coverage_defense_12w",
    "^starter_defense_OLB_[0-9]+_coverage_schemes_zone_grades_coverage_defense_12w",
    "^starter_defense_CB_[0-9]+_coverage_schemes_zone_grades_coverage_defense_12w",
    "^starter_defense_FS_[0-9]+_coverage_schemes_zone_grades_coverage_defense_12w",
    "^starter_defense_SS_[0-9]+_coverage_schemes_zone_grades_coverage_defense_12w"
  )
)

# Function to perform test and return the raw p-value
perform_joint_test_p_value <- function(full_model, group_vars_to_test, model_type) {
  # Return NA if the group has no variables in the model to test
  if (length(group_vars_to_test) == 0) return(NA)
  
  # Create the formula for the restricted model
  restricted_formula <- as.formula(paste(". ~ . -", paste(paste0("`", group_vars_to_test, "`"), collapse = " - ")))
  updated_restricted_formula <- update.formula(formula(full_model), restricted_formula)
  
  # Perform the appropriate test (Wald for LPM, LR for Probit/Logit)
  if (model_type == "LPM") {
    restricted_model <- lm(updated_restricted_formula, data = full_model$model)
    wald_test_robust <- waldtest(full_model, restricted_model, vcov = vcovHC(full_model, type = "HC1"))
    return(wald_test_robust$`Pr(>F)`[2])
  } else { 
    restricted_model <- glm(updated_restricted_formula, data = full_model$model, family = family(full_model))
    lr_test_result <- lrtest(full_model, restricted_model)
    return(lr_test_result$`Pr(>Chisq)`[2])
  }
}

# Initialize results tables for each case
case1_results_table <- data.table()
case2_results_table <- data.table()
case3_results_table <- data.table()

# Define the models for both stages
model_stages <- list(
  Outcome = list(LPM = lpm_poly, Probit = probit_poly, Logit = logit_poly),
  Attempt = list(LPM = lpm_attempt, Probit = probit_attempt, Logit = logit_attempt)
)

# --- CASE 1: Original grouped positions ---
cat("\n--- CASE 1: Joint Significance for Grouped Positions (with interactions for Outcome, excluding missing_information) ---\n")
for (stage_name in names(model_stages)) {
  for (group_name in names(player_groups_case1)) {
    
    p_values <- list()
    p_values$Test_Group <- paste(stage_name, ":", group_name)
    
    current_stage_models <- model_stages[[stage_name]]
    
    for (model_type in names(current_stage_models)) {
      current_model <- current_stage_models[[model_type]]
      all_model_vars <- names(coef(current_model))
      
      # Include interactions for Outcome models, exclude for Attempt models
      include_interactions <- (stage_name == "Outcome")
      current_group_vars <- get_player_group_vars(player_groups_case1[[group_name]], 
                                                  all_model_vars, 
                                                  include_interactions)
      
      p_val <- perform_joint_test_p_value(current_model, current_group_vars, model_type)
      p_values[[model_type]] <- p_val
    }
    case1_results_table <- rbind(case1_results_table, as.data.table(p_values))
  }
}

# --- CASE 2: Individual position groups ---
cat("\n--- CASE 2: Joint Significance for Individual Position Groups (with interactions for Outcome, excluding missing_information) ---\n")
for (stage_name in names(model_stages)) {
  for (group_name in names(player_groups_case2)) {
    
    p_values <- list()
    p_values$Test_Group <- paste(stage_name, ":", group_name)
    
    current_stage_models <- model_stages[[stage_name]]
    
    for (model_type in names(current_stage_models)) {
      current_model <- current_stage_models[[model_type]]
      all_model_vars <- names(coef(current_model))
      
      # Include interactions for Outcome models, exclude for Attempt models
      include_interactions <- (stage_name == "Outcome")
      current_group_vars <- get_player_group_vars(player_groups_case2[[group_name]], 
                                                  all_model_vars, 
                                                  include_interactions)
      
      # Only perform test if there are variables for this position
      if(length(current_group_vars) > 0) {
        p_val <- perform_joint_test_p_value(current_model, current_group_vars, model_type)
      } else {
        p_val <- NA
      }
      p_values[[model_type]] <- p_val
    }
    case2_results_table <- rbind(case2_results_table, as.data.table(p_values))
  }
}

# --- CASE 3: Grade Types Across Positions ---
cat("\n--- CASE 3: Joint Significance for Grade Types Across Positions (with interactions for Outcome, excluding missing_information) ---\n")

# Run on BOTH Attempt and Outcome models
for (stage_name in names(model_stages)) {
  for (group_name in names(grade_type_groups)) {
    
    p_values <- list()
    p_values$Test_Group <- paste(stage_name, ":", group_name)
    
    current_stage_models <- model_stages[[stage_name]]
    
    for (model_type in names(current_stage_models)) {
      current_model <- current_stage_models[[model_type]]
      all_model_vars <- names(coef(current_model))
      
      # For Outcome models, include interaction terms; for Attempt models, exclude them
      include_interactions <- (stage_name == "Outcome")
      current_group_vars <- get_grade_type_vars(grade_type_groups[[group_name]], 
                                                all_model_vars, 
                                                include_interactions)
      
      # Only perform test if there are variables for this grade type
      if(length(current_group_vars) > 0) {
        p_val <- perform_joint_test_p_value(current_model, current_group_vars, model_type)
      } else {
        p_val <- NA
      }
      p_values[[model_type]] <- p_val
    }
    case3_results_table <- rbind(case3_results_table, as.data.table(p_values))
  }
}

# --- Round P-Values for Cleaner Output ---
p_value_cols <- c("LPM", "Probit", "Logit")
case1_results_table[, (p_value_cols) := lapply(.SD, round, 6), .SDcols = p_value_cols]
case2_results_table[, (p_value_cols) := lapply(.SD, round, 6), .SDcols = p_value_cols]
case3_results_table[, (p_value_cols) := lapply(.SD, round, 6), .SDcols = p_value_cols]

# --- Print and Save all three tables ---
cat("\n\n--- CASE 1: Joint Significance P-Values of Grouped Player Positions (excluding missing_information) ---\n")
print(case1_results_table)

cat("\n\n--- CASE 2: Joint Significance P-Values of Individual Position Groups (excluding missing_information) ---\n")
print(case2_results_table)

cat("\n\n--- CASE 3: Joint Significance P-Values of Grade Types Across Positions (excluding missing_information) ---\n")
print(case3_results_table)

# Save the three tables to CSV files
fwrite(case1_results_table, "script_outputs_players/player_group_joint_significance_CASE1_grouped_positions.csv")
fwrite(case2_results_table, "script_outputs_players/player_group_joint_significance_CASE2_individual_positions.csv")
fwrite(case3_results_table, "script_outputs_players/player_group_joint_significance_CASE3_grade_types.csv")

cat("\n--- Joint significance tests complete (missing_information variables excluded from all tests) ---\n")

##########################################################################

# ==============================================================================
# 15. Extract and Save R-squared Values for All Models
# ==============================================================================
cat("\n--- Extracting R-squared and Pseudo R-squared Values ---\n")

# Function to calculate pseudo R-squared for logit/probit models
calculate_pseudo_r2 <- function(model) {
  # McFadden's R-squared
  mcfadden_r2 <- 1 - (logLik(model) / logLik(update(model, . ~ 1)))
  
  # Cox & Snell R-squared
  n <- nobs(model)
  cox_snell_r2 <- 1 - exp((logLik(update(model, . ~ 1)) - logLik(model)) * 2/n)
  
  # Nagelkerke R-squared (normalized Cox & Snell)
  nagelkerke_r2 <- cox_snell_r2 / (1 - exp(logLik(update(model, . ~ 1)) * 2/n))
  
  return(list(
    McFadden = as.numeric(mcfadden_r2),
    CoxSnell = as.numeric(cox_snell_r2),
    Nagelkerke = as.numeric(nagelkerke_r2)
  ))
}

# Initialize results table
r_squared_results <- data.table(
  Model_Stage = character(),
  Model_Type = character(),
  R_Squared = numeric(),
  Adjusted_R_Squared = numeric(),
  McFadden_R2 = numeric(),
  CoxSnell_R2 = numeric(),
  Nagelkerke_R2 = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Log_Likelihood = numeric(),
  N_Observations = integer()
)

# --- OUTCOME MODELS (Second Stage with IMR correction) ---
# LPM Outcome Model
if (!is.null(lpm_poly_results)) {
  r2_adj <- summary(lpm_poly)$adj.r.squared
  r2 <- summary(lpm_poly)$r.squared
  
  r_squared_results <- rbind(r_squared_results, data.table(
    Model_Stage = "Outcome",
    Model_Type = "LPM",
    R_Squared = r2,
    Adjusted_R_Squared = r2_adj,
    McFadden_R2 = NA,
    CoxSnell_R2 = NA,
    Nagelkerke_R2 = NA,
    AIC = AIC(lpm_poly),
    BIC = BIC(lpm_poly),
    Log_Likelihood = as.numeric(logLik(lpm_poly)),
    N_Observations = nobs(lpm_poly)
  ))
}

# Probit Outcome Model
if (!is.null(probit_poly_results)) {
  pseudo_r2 <- calculate_pseudo_r2(probit_poly)
  
  r_squared_results <- rbind(r_squared_results, data.table(
    Model_Stage = "Outcome",
    Model_Type = "Probit",
    R_Squared = NA,
    Adjusted_R_Squared = NA,
    McFadden_R2 = pseudo_r2$McFadden,
    CoxSnell_R2 = pseudo_r2$CoxSnell,
    Nagelkerke_R2 = pseudo_r2$Nagelkerke,
    AIC = AIC(probit_poly),
    BIC = BIC(probit_poly),
    Log_Likelihood = as.numeric(logLik(probit_poly)),
    N_Observations = nobs(probit_poly)
  ))
}

# Logit Outcome Model
if (!is.null(logit_poly_results)) {
  pseudo_r2 <- calculate_pseudo_r2(logit_poly)
  
  r_squared_results <- rbind(r_squared_results, data.table(
    Model_Stage = "Outcome",
    Model_Type = "Logit",
    R_Squared = NA,
    Adjusted_R_Squared = NA,
    McFadden_R2 = pseudo_r2$McFadden,
    CoxSnell_R2 = pseudo_r2$CoxSnell,
    Nagelkerke_R2 = pseudo_r2$Nagelkerke,
    AIC = AIC(logit_poly),
    BIC = BIC(logit_poly),
    Log_Likelihood = as.numeric(logLik(logit_poly)),
    N_Observations = nobs(logit_poly)
  ))
}

# --- ATTEMPT MODELS (First Stage/Selection) ---
# LPM Attempt Model
if (!is.null(lpm_attempt_results)) {
  r2_adj <- summary(lpm_attempt)$adj.r.squared
  r2 <- summary(lpm_attempt)$r.squared
  
  r_squared_results <- rbind(r_squared_results, data.table(
    Model_Stage = "Attempt",
    Model_Type = "LPM",
    R_Squared = r2,
    Adjusted_R_Squared = r2_adj,
    McFadden_R2 = NA,
    CoxSnell_R2 = NA,
    Nagelkerke_R2 = NA,
    AIC = AIC(lpm_attempt),
    BIC = BIC(lpm_attempt),
    Log_Likelihood = as.numeric(logLik(lpm_attempt)),
    N_Observations = nobs(lpm_attempt)
  ))
}

# Probit Attempt Model
if (!is.null(probit_attempt_results)) {
  pseudo_r2 <- calculate_pseudo_r2(probit_attempt)
  
  r_squared_results <- rbind(r_squared_results, data.table(
    Model_Stage = "Attempt",
    Model_Type = "Probit",
    R_Squared = NA,
    Adjusted_R_Squared = NA,
    McFadden_R2 = pseudo_r2$McFadden,
    CoxSnell_R2 = pseudo_r2$CoxSnell,
    Nagelkerke_R2 = pseudo_r2$Nagelkerke,
    AIC = AIC(probit_attempt),
    BIC = BIC(probit_attempt),
    Log_Likelihood = as.numeric(logLik(probit_attempt)),
    N_Observations = nobs(probit_attempt)
  ))
}

# Logit Attempt Model
if (!is.null(logit_attempt_results)) {
  pseudo_r2 <- calculate_pseudo_r2(logit_attempt)
  
  r_squared_results <- rbind(r_squared_results, data.table(
    Model_Stage = "Attempt",
    Model_Type = "Logit",
    R_Squared = NA,
    Adjusted_R_Squared = NA,
    McFadden_R2 = pseudo_r2$McFadden,
    CoxSnell_R2 = pseudo_r2$CoxSnell,
    Nagelkerke_R2 = pseudo_r2$Nagelkerke,
    AIC = AIC(logit_attempt),
    BIC = BIC(logit_attempt),
    Log_Likelihood = as.numeric(logLik(logit_attempt)),
    N_Observations = nobs(logit_attempt)
  ))
}

# Round the results for cleaner output
numeric_cols <- c("R_Squared", "Adjusted_R_Squared", "McFadden_R2", "CoxSnell_R2", 
                  "Nagelkerke_R2", "AIC", "BIC", "Log_Likelihood")
r_squared_results[, (numeric_cols) := lapply(.SD, round, 6), .SDcols = numeric_cols]

# Print the results
cat("\n--- Model Fit Statistics ---\n")
print(r_squared_results)

# Save to CSV
fwrite(r_squared_results, "script_outputs_players/model_fit_statistics_r_squared.csv")


# Also save XGBoost model performance metrics if available
if (exists("bootstrap_aucs") && length(bootstrap_aucs) > 0) {
  xgb_performance <- data.table(
    Model = "XGBoost_First_Stage",
    Mean_Bootstrap_AUC = round(mean(bootstrap_aucs), 6),
    SD_Bootstrap_AUC = round(sd(bootstrap_aucs), 6),
    Min_AUC = round(min(bootstrap_aucs), 6),
    Max_AUC = round(max(bootstrap_aucs), 6),
    N_Bootstrap_Samples = length(bootstrap_aucs),
    Optimal_nrounds = best_nrounds
  )
  
  cat("\n--- XGBoost First Stage Performance ---\n")
  print(xgb_performance)
  
  fwrite(xgb_performance, "script_outputs_players/xgboost_performance_summary.csv")
}

cat("\n--- R-squared extraction complete ---\n")
cat("Files saved:\n")
cat("- script_outputs_players/model_fit_statistics_r_squared.csv (detailed)\n")
cat("- script_outputs_players/r_squared_summary_table.csv (summary)\n")
if (exists("xgb_performance")) {
  cat("- script_outputs_players/xgboost_performance_summary.csv (XGBoost AUC)\n")
}