# set wd to C:\Users\simon\Dropbox\4th_downs_reproducibility\Actual_File_Order
setwd("C:/Users/simon/Dropbox/4th_downs_reproducibility/Actual_File_Order")


# Load required libraries
library(dplyr)
library(nflverse)
library(tidyr)
library(purrr)
library(zoo)
library(car)
library(progress)
library(readr)
library(rlang)
library(lubridate)

# Load position data from CSV
position_data <- read.csv("avg_positions_per_team.csv")

# Create individual position mappings (each position maps to itself)
POSITION_MAPPINGS <- setNames(as.list(position_data$position), position_data$position)
POSITION_QUANTITIES <- setNames(as.list(position_data$avg_per_team), position_data$position)

# Define which positions belong to offense vs defense
OFFENSE_POSITIONS <- c("QB", "RB", "FB", "WR", "TE", "T", "G", "C")
DEFENSE_POSITIONS <- c("DE", "DT", "NT", "ILB", "OLB", "MLB", "LB", "CB", "FS", "SS", "DB")
SPECIAL_POSITIONS <- c("K", "P")

# Validation function
validate_data <- function(df) {
  dupes <- duplicated(names(df))
  if(any(dupes)) {
    warning("Duplicate columns found: ", paste(names(df)[dupes], collapse=", "))
  }
  
  req_cols <- c("play_id", "game_id", "posteam")
  missing <- req_cols[!req_cols %in% names(df)]
  if(length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse=", "))
  }
  return(df)
}

# Get specialists function
get_specialists <- function(year) {
  pbp <- bind_rows(
    load_pbp(year),
    load_pbp(year - 1)
  ) %>%
    arrange(game_date, desc(play_id))
  
  kickers <- pbp %>%
    filter(field_goal_attempt == 1 | extra_point_attempt == 1) %>%
    select(team = posteam, week, season, kicker_player_id, kicker_player_name) %>%
    distinct(team, week, season, .keep_all = TRUE)
  
  punters <- pbp %>%
    filter(punt_attempt == 1) %>%
    select(team = posteam, week, season, punter_player_id, punter_player_name) %>%
    distinct(team, week, season, .keep_all = TRUE)
  
  list(kickers = kickers, punters = punters)
}
add_player_info <- function(df, players_data, player_col, prefix) {
  if (!player_col %in% names(df)) {
    return(df)
  }
  
  new_cols <- c("full_name", "birth_date", "rookie_year", "draft_number", "jersey_number", "pff_id", 
                "weight", "height", "position", "depth_chart_position")
  
  for(col in new_cols) {
    new_col_name <- paste0(prefix, "_", col)
    df[[new_col_name]] <- NA
  }
  
  df[[paste0(prefix, "_days_since_injury")]] <- NA
  df[[paste0(prefix, "_last_injury_type")]] <- NA
  df[[paste0(prefix, "_injury_count_1yr")]] <- NA
  
  df[[paste0(prefix, "_week_questionable")]] <- 0
  df[[paste0(prefix, "_week_doubtful")]] <- 0
  df[[paste0(prefix, "_week_out")]] <- 0
  df[[paste0(prefix, "_week_limited_practice")]] <- 0
  df[[paste0(prefix, "_week_dnp")]] <- 0
  df[[paste0(prefix, "_current_status")]] <- NA
  df[[paste0(prefix, "_current_practice")]] <- NA
  
  valid_players <- unique(df[[player_col]][!is.na(df[[player_col]])])
  
  if(length(valid_players) > 0) {
    players_info <- players_data %>%
      filter(gsis_id %in% valid_players) %>%
      group_by(gsis_id) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(gsis_id, all_of(new_cols))
    
    for(i in 1:nrow(df)) {
      if(!is.na(df[[player_col]][i])) {
        player_id <- df[[player_col]][i]
        current_date <- as.Date(df$game_date[i])
        player_data <- players_info %>% filter(gsis_id == player_id)
        
        if(nrow(player_data) == 1) {
          for(col in new_cols) {
            if(!is.null(player_data[[col]])) {
              df[i, paste0(prefix, "_", col)] <- player_data[[col]][1]
            }
          }
        }
        
        player_injuries <- injuries_data %>%
          filter(gsis_id == player_id,
                 injury_date < current_date,
                 !is.na(injury_date)) %>%
          arrange(desc(injury_date))
        
        if(nrow(player_injuries) > 0) {
          df[i, paste0(prefix, "_days_since_injury")] <- 
            as.numeric(difftime(current_date, player_injuries$injury_date[1], units = "days"))
          
          df[i, paste0(prefix, "_last_injury_type")] <- 
            player_injuries$practice_primary_injury[1]
          
          one_year_ago <- current_date - 365
          df[i, paste0(prefix, "_injury_count_1yr")] <- 
            sum(player_injuries$injury_date >= one_year_ago)
          
          week_ago <- current_date - 7
          recent_injuries <- injuries_data %>%
            filter(gsis_id == player_id,
                   injury_date >= week_ago,
                   injury_date < current_date)
          
          if(nrow(recent_injuries) > 0) {
            df[i, paste0(prefix, "_week_questionable")] <- 
              sum(recent_injuries$report_status == "Questionable", na.rm = TRUE)
            df[i, paste0(prefix, "_week_doubtful")] <- 
              sum(recent_injuries$report_status == "Doubtful", na.rm = TRUE)
            df[i, paste0(prefix, "_week_out")] <- 
              sum(recent_injuries$report_status == "Out", na.rm = TRUE)
            df[i, paste0(prefix, "_week_limited_practice")] <- 
              sum(recent_injuries$practice_status == "Limited Participation in Practice", na.rm = TRUE)
            df[i, paste0(prefix, "_week_dnp")] <- 
              sum(recent_injuries$practice_status == "Did Not Participate In Practice", na.rm = TRUE)
            
            df[i, paste0(prefix, "_current_status")] <- recent_injuries$report_status[1]
            df[i, paste0(prefix, "_current_practice")] <- recent_injuries$practice_status[1]
          }
        }
      }
    }
  }
  return(df)
}




process_year <- function(year) {
  message(paste("\nProcessing year:", year))
  
  message("Step 1/7: Loading data...")
  # Add global assignment
  injuries_data <<- load_injuries((year-2):year) %>%
    mutate(injury_date = as.Date(date_modified))
  
  top_injury_types <- injuries_data %>%
    filter(!is.na(practice_primary_injury)) %>%
    count(practice_primary_injury, sort = TRUE) %>%
    slice_head(n = 9) %>%
    pull(practice_primary_injury)
  
  get_team_injury_counts <- function(team, game_date) {
    game_date <- as.Date(game_date)
    
    team_injuries <- injuries_data %>%
      filter(team == !!team) %>%
      mutate(
        injury_date = as.Date(date_modified),
        standard_injury = case_when(
          practice_primary_injury %in% top_injury_types ~ practice_primary_injury,
          TRUE ~ "Other"
        )
      )
    
    year_ago <- game_date - 365
    year_injuries <- team_injuries %>%
      filter(injury_date >= year_ago,
             injury_date < game_date) %>%
      count(standard_injury) %>%
      pivot_wider(
        names_from = standard_injury,
        names_prefix = "injury_count_year_",
        values_from = n,
        values_fill = 0
      )
    
    week_ago <- game_date - 7
    week_injuries <- team_injuries %>%
      filter(injury_date >= week_ago,
             injury_date < game_date) %>%
      count(standard_injury) %>%
      pivot_wider(
        names_from = standard_injury,
        names_prefix = "injury_count_week_",
        values_from = n,
        values_fill = 0
      )
    
    week_status <- team_injuries %>%
      filter(injury_date >= week_ago,
             injury_date < game_date) %>%
      summarise(
        week_questionable = sum(report_status == "Questionable", na.rm = TRUE),
        week_doubtful = sum(report_status == "Doubtful", na.rm = TRUE),
        week_out = sum(report_status == "Out", na.rm = TRUE),
        week_limited_practice = sum(practice_status == "Limited Participation in Practice", na.rm = TRUE),
        week_dnp = sum(practice_status == "Did Not Participate In Practice", na.rm = TRUE)
      )
    
    bind_cols(year_injuries, week_injuries, week_status)
  }
  
  play_year <- load_pbp(year)
  participation_data <- load_participation(year) %>%
    select(
      nflverse_game_id,
      play_id,
      offense_formation,
      offense_personnel,
      defense_personnel,
      defenders_in_box,
      number_of_pass_rushers,
      defense_man_zone_type,
      defense_coverage_type
    )
  
  roster_data <- load_rosters(year)
  
  message("Step 2/7: Processing weekly stats...")
  weekly_run_stats <- play_year %>%
    # Change this line to include down 4:
    filter(play_type %in% c("pass", "run"), down %in% 1:4) %>%   # Changed from 1:3 to 1:4
    group_by(posteam, week, down) %>%
    summarize(
      weekly_run_plays = sum(play_type == "run", na.rm = TRUE),
      weekly_total_plays = n(),
      .groups = 'drop'
    ) %>%
    arrange(posteam, week, down) %>%
    group_by(posteam, down) %>%
    mutate(
      cum_run_plays = cumsum(weekly_run_plays),
      cum_total_plays = cumsum(weekly_total_plays)
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c(posteam, week),
      names_from = down,
      names_glue = "down{down}_{.value}",
      values_from = c(cum_run_plays, cum_total_plays)
    ) %>%
    arrange(posteam, week)
  
  run_pcts <- weekly_run_stats %>%
    mutate(
      down1_pct = round(100 * down1_cum_run_plays / down1_cum_total_plays, 1),
      down2_pct = round(100 * down2_cum_run_plays / down2_cum_total_plays, 1),
      down3_pct = round(100 * down3_cum_run_plays / down3_cum_total_plays, 1),
      down4_pct = round(100 * down4_cum_run_plays / down4_cum_total_plays, 1)  # Added this line
    ) %>%
    select(posteam, week, down1_pct, down2_pct, down3_pct, down4_pct)  # Added down4_pct here
  
  prep_time_df <- play_year %>%
    select(game_id, game_date, posteam = home_team) %>%
    distinct() %>%
    bind_rows(
      play_year %>%
        select(game_id, game_date, posteam = away_team) %>%
        distinct()
    ) %>%
    arrange(posteam, game_date) %>%
    group_by(posteam) %>%
    mutate(
      last_game_date = lag(game_date),
      prep_days = as.numeric(difftime(game_date, last_game_date, units = "days"))
    ) %>%
    ungroup()
  
  # Add after run_pcts but before prep_time_df in Step 2/7:
  message("Step 2.5/7: Processing coach history...")
  
  # Get coach records across all games
  coach_records <- play_year %>%
    select(game_id, game_date, home_team, away_team, home_coach, away_coach,
           home_score, away_score) %>%
    distinct() %>%
    # Get home coach records
    mutate(
      home_win = home_score > away_score,
      home_loss = home_score < away_score
    ) %>%
    group_by(home_coach) %>%
    summarize(
      overall_wins = sum(home_win, na.rm = TRUE),
      overall_losses = sum(home_loss, na.rm = TRUE),
      overall_win_pct = round(sum(home_win, na.rm = TRUE) / n(), 3)
    ) %>%
    # Combine with away coach records
    bind_rows(
      play_year %>%
        select(game_id, away_coach, home_score, away_score) %>%
        distinct() %>%
        group_by(away_coach) %>%
        summarize(
          overall_wins = sum(away_score > home_score, na.rm = TRUE),
          overall_losses = sum(away_score < home_score, na.rm = TRUE),
          overall_win_pct = round(sum(away_score > home_score, na.rm = TRUE) / n(), 3)
        )
    ) %>%
    group_by(home_coach) %>%
    summarize(
      overall_wins = sum(overall_wins),
      overall_losses = sum(overall_losses),
      overall_win_pct = round(mean(overall_win_pct), 3)
    ) %>%
    rename(coach = home_coach)
  
  # Get coach records with current team only
  current_team_records <- play_year %>%
    select(game_id, game_date, posteam, home_team, away_team, home_coach, away_coach,
           posteam_score, defteam_score) %>%
    distinct() %>%
    mutate(
      coach = ifelse(posteam == home_team, home_coach, away_coach)
    ) %>%
    group_by(coach, posteam) %>%
    summarize(
      team_wins = sum(posteam_score > defteam_score, na.rm = TRUE),
      team_losses = sum(posteam_score < defteam_score, na.rm = TRUE),
      team_win_pct = round(mean(posteam_score > defteam_score, na.rm = TRUE), 3),
      .groups = 'drop'
    )
  
  # Add after coach records calculation in Step 2.5:
  message("Diagnosing coach records duplicates...")
  coach_dupes <- coach_records %>%
    group_by(coach) %>%
    filter(n() > 1)
  
  print("Number of duplicate coach record rows:")
  print(nrow(coach_dupes))
  if(nrow(coach_dupes) > 0) {
    print("Sample of duplicates:")
    print(head(coach_dupes))
  }
  
  message("Step 2.6/7: Processing coach tenure...")
  # Calculate coach tenure - modified to include 5-year lookback
  coach_tenure <- play_year %>%
    select(game_date, home_team, away_team, home_coach, away_coach) %>%
    # Get unique games first
    distinct() %>%
    pivot_longer(
      cols = c(home_coach, away_coach),
      names_to = "role",
      values_to = "coach"
    ) %>%
    mutate(
      team = if_else(role == "home_coach", home_team, away_team),
      # Create lookback date 5 years prior
      lookback_date = as.Date(game_date) - years(5)
    ) %>%
    # Get first appearance for each coach-team combo within 5-year window
    group_by(coach, team) %>%
    summarize(
      first_appearance = max(min(game_date), lookback_date[1]),
      .groups = 'drop'
    )
  
  # Calculate tenure for each game - modified section
  game_tenure <- play_year %>%
    select(game_id, game_date, posteam, home_team, away_team, home_coach, away_coach) %>%
    # Get unique game-team combinations
    distinct() %>%
    mutate(
      coach = ifelse(posteam == home_team, home_coach, away_coach)
    ) %>%
    left_join(coach_tenure, by = c("coach", "posteam" = "team")) %>%
    mutate(
      # Cap tenure at 5 years (1825 days)
      tenure_days = pmin(
        as.numeric(as.Date(game_date) - as.Date(first_appearance)),
        1825  # 5 years * 365 days
      )
    )
  
  # Add after game tenure calculation in Step 2.6:
  message("Diagnosing game tenure duplicates...")
  tenure_dupes <- game_tenure %>%
    group_by(game_id, posteam, coach) %>%
    filter(n() > 1)
  
  print("Number of duplicate tenure rows:")
  print(nrow(tenure_dupes))
  if(nrow(tenure_dupes) > 0) {
    print("Sample of duplicates:")
    print(head(tenure_dupes))
  }
  
  # Add after the game tenure diagnostic check:
  if(nrow(tenure_dupes) > 0) {
    print("Detailed look at tenure duplicates:")
    print(
      tenure_dupes %>%
        arrange(game_id, posteam, coach) %>%
        select(game_id, game_date, posteam, coach, first_appearance, tenure_days) %>%
        head(10)
    )
    
    # Count frequency of duplications
    print("Number of duplicates per game/team/coach combination:")
    print(
      tenure_dupes %>%
        group_by(game_id, posteam, coach) %>%
        summarise(n = n(), .groups = 'drop') %>%
        count(n) %>%
        arrange(desc(n))
    )
  }
  
  message("Step 2.6 completed")
  
  message("Step 2.7/7: Processing formation, defensive tendencies, and coverage rates...")
  
  # Formation tendencies by down with rolling calculations
  message("  Processing formation tendencies...")
  formation_tendencies <- play_year %>%
    left_join(participation_data, by = c("game_id" = "nflverse_game_id", "play_id")) %>%
    filter(play_type %in% c("pass", "run")) %>%
    arrange(posteam, week) %>%
    group_by(posteam, week, down) %>%
    summarize(
      weekly_formation_plays = sum(!is.na(offense_formation)),
      weekly_shotgun = sum(offense_formation == "SHOTGUN", na.rm = TRUE),
      weekly_singleback = sum(offense_formation == "SINGLEBACK", na.rm = TRUE),
      weekly_empty = sum(offense_formation == "EMPTY", na.rm = TRUE),
      weekly_iform = sum(offense_formation == "I_FORM", na.rm = TRUE),
      weekly_pistol = sum(offense_formation == "PISTOL", na.rm = TRUE),
      weekly_wildcat = sum(offense_formation == "WILDCAT", na.rm = TRUE),
      weekly_jumbo = sum(offense_formation == "JUMBO", na.rm = TRUE),
      weekly_shotgun_success = sum(first_down[offense_formation == "SHOTGUN"], na.rm = TRUE),
      weekly_singleback_success = sum(first_down[offense_formation == "SINGLEBACK"], na.rm = TRUE),
      weekly_empty_success = sum(first_down[offense_formation == "EMPTY"], na.rm = TRUE),
      weekly_iform_success = sum(first_down[offense_formation == "I_FORM"], na.rm = TRUE),
      weekly_pistol_success = sum(first_down[offense_formation == "PISTOL"], na.rm = TRUE),
      weekly_wildcat_success = sum(first_down[offense_formation == "WILDCAT"], na.rm = TRUE),
      weekly_jumbo_success = sum(first_down[offense_formation == "JUMBO"], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(posteam, down) %>%
    arrange(week) %>%
    mutate(across(starts_with("weekly_"), ~replace_na(., 0))) %>%
    mutate(
      # Calculate rates using only previous weeks' data
      shotgun_rate = lag(cumsum(weekly_shotgun)) / lag(cumsum(weekly_formation_plays)),
      singleback_rate = lag(cumsum(weekly_singleback)) / lag(cumsum(weekly_formation_plays)),
      empty_rate = lag(cumsum(weekly_empty)) / lag(cumsum(weekly_formation_plays)),
      iform_rate = lag(cumsum(weekly_iform)) / lag(cumsum(weekly_formation_plays)),
      pistol_rate = lag(cumsum(weekly_pistol)) / lag(cumsum(weekly_formation_plays)),
      wildcat_rate = lag(cumsum(weekly_wildcat)) / lag(cumsum(weekly_formation_plays)),
      jumbo_rate = lag(cumsum(weekly_jumbo)) / lag(cumsum(weekly_formation_plays)),
      
      # Success rates using only previous weeks' data
      shotgun_success = lag(cumsum(weekly_shotgun_success)) / lag(cumsum(weekly_shotgun)),
      singleback_success = lag(cumsum(weekly_singleback_success)) / lag(cumsum(weekly_singleback)),
      empty_success = lag(cumsum(weekly_empty_success)) / lag(cumsum(weekly_empty)),
      iform_success = lag(cumsum(weekly_iform_success)) / lag(cumsum(weekly_iform)),
      pistol_success = lag(cumsum(weekly_pistol_success)) / lag(cumsum(weekly_pistol)),
      wildcat_success = lag(cumsum(weekly_wildcat_success)) / lag(cumsum(weekly_wildcat)),
      jumbo_success = lag(cumsum(weekly_jumbo_success)) / lag(cumsum(weekly_jumbo))
    ) %>%
    select(posteam, week, down, ends_with("_rate"), ends_with("_success")) %>%
    ungroup()
  
  # Defense tendencies with down-specific stats
  message("  Processing defensive tendencies and stop rates...")
  defense_tendencies <- play_year %>%
    left_join(participation_data, by = c("game_id" = "nflverse_game_id", "play_id")) %>%
    filter(play_type %in% c("pass", "run")) %>%
    arrange(defteam, week) %>%
    group_by(defteam, week) %>%
    summarize(
      weekly_plays = n(),
      weekly_run_plays = sum(play_type == "run", na.rm = TRUE),
      weekly_pass_plays = sum(play_type == "pass", na.rm = TRUE),
      weekly_run_stops = sum(play_type == "run" & !first_down, na.rm = TRUE),
      weekly_pass_stops = sum(play_type == "pass" & !first_down, na.rm = TRUE),
      weekly_total_stops = sum(!first_down, na.rm = TRUE),
      weekly_1st_run_plays = sum(play_type == "run" & down == 1, na.rm = TRUE),
      weekly_1st_pass_plays = sum(play_type == "pass" & down == 1, na.rm = TRUE),
      weekly_1st_run_stops = sum(play_type == "run" & down == 1 & !first_down, na.rm = TRUE),
      weekly_1st_pass_stops = sum(play_type == "pass" & down == 1 & !first_down, na.rm = TRUE),
      weekly_2nd_run_plays = sum(play_type == "run" & down == 2, na.rm = TRUE),
      weekly_2nd_pass_plays = sum(play_type == "pass" & down == 2, na.rm = TRUE),
      weekly_2nd_run_stops = sum(play_type == "run" & down == 2 & !first_down, na.rm = TRUE),
      weekly_2nd_pass_stops = sum(play_type == "pass" & down == 2 & !first_down, na.rm = TRUE),
      weekly_3rd_run_plays = sum(play_type == "run" & down == 3, na.rm = TRUE),
      weekly_3rd_pass_plays = sum(play_type == "pass" & down == 3, na.rm = TRUE),
      weekly_3rd_run_stops = sum(play_type == "run" & down == 3 & !first_down, na.rm = TRUE),
      weekly_3rd_pass_stops = sum(play_type == "pass" & down == 3 & !first_down, na.rm = TRUE),
      weekly_4th_run_plays = sum(play_type == "run" & down == 4, na.rm = TRUE),
      weekly_4th_pass_plays = sum(play_type == "pass" & down == 4, na.rm = TRUE),
      weekly_4th_run_stops = sum(play_type == "run" & down == 4 & !first_down, na.rm = TRUE),
      weekly_4th_pass_stops = sum(play_type == "pass" & down == 4 & !first_down, na.rm = TRUE),
      weekly_pass_rushers = mean(number_of_pass_rushers, na.rm = TRUE),
      weekly_box_defenders = mean(defenders_in_box, na.rm = TRUE),
      weekly_cover0 = sum(defense_coverage_type == "COVER_0", na.rm = TRUE),
      weekly_cover1 = sum(defense_coverage_type == "COVER_1", na.rm = TRUE),
      weekly_cover2 = sum(defense_coverage_type == "COVER_2", na.rm = TRUE),
      weekly_cover3 = sum(defense_coverage_type == "COVER_3", na.rm = TRUE),
      weekly_cover4 = sum(defense_coverage_type == "COVER_4", na.rm = TRUE),
      weekly_cover6 = sum(defense_coverage_type == "COVER_6", na.rm = TRUE),
      weekly_two_man = sum(defense_coverage_type == "2_MAN", na.rm = TRUE),
      weekly_prevent = sum(defense_coverage_type == "PREVENT", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(defteam) %>%
    arrange(week) %>%
    mutate(
      # Calculate stop rates using only previous weeks' data
      def_stop_rate_run = if_else(lag(cumsum(weekly_run_plays)) >= 5, 
                                  lag(cumsum(weekly_run_stops)) / lag(cumsum(weekly_run_plays)), 
                                  NA_real_),
      def_stop_rate_pass = if_else(lag(cumsum(weekly_pass_plays)) >= 5, 
                                   lag(cumsum(weekly_pass_stops)) / lag(cumsum(weekly_pass_plays)), 
                                   NA_real_),
      def_stop_rate_total = if_else(lag(cumsum(weekly_plays)) >= 10, 
                                    lag(cumsum(weekly_total_stops)) / lag(cumsum(weekly_plays)), 
                                    NA_real_),
      
      # Down-specific stop rates using only previous weeks' data
      def_stop_rate_1st_run = if_else(lag(cumsum(weekly_1st_run_plays)) >= 5,
                                      lag(cumsum(weekly_1st_run_stops)) / lag(cumsum(weekly_1st_run_plays)),
                                      NA_real_),
      def_stop_rate_1st_pass = if_else(lag(cumsum(weekly_1st_pass_plays)) >= 5,
                                       lag(cumsum(weekly_1st_pass_stops)) / lag(cumsum(weekly_1st_pass_plays)),
                                       NA_real_),
      def_stop_rate_2nd_run = if_else(lag(cumsum(weekly_2nd_run_plays)) >= 5,
                                      lag(cumsum(weekly_2nd_run_stops)) / lag(cumsum(weekly_2nd_run_plays)),
                                      NA_real_),
      def_stop_rate_2nd_pass = if_else(lag(cumsum(weekly_2nd_pass_plays)) >= 5,
                                       lag(cumsum(weekly_2nd_pass_stops)) / lag(cumsum(weekly_2nd_pass_plays)),
                                       NA_real_),
      def_stop_rate_3rd_run = if_else(lag(cumsum(weekly_3rd_run_plays)) >= 5,
                                      lag(cumsum(weekly_3rd_run_stops)) / lag(cumsum(weekly_3rd_run_plays)),
                                      NA_real_),
      def_stop_rate_3rd_pass = if_else(lag(cumsum(weekly_3rd_pass_plays)) >= 5,
                                       lag(cumsum(weekly_3rd_pass_stops)) / lag(cumsum(weekly_3rd_pass_plays)),
                                       NA_real_),
      def_stop_rate_4th_run = if_else(lag(cumsum(weekly_4th_run_plays)) >= 5,
                                      lag(cumsum(weekly_4th_run_stops)) / lag(cumsum(weekly_4th_run_plays)),
                                      NA_real_),
      def_stop_rate_4th_pass = if_else(lag(cumsum(weekly_4th_pass_plays)) >= 5,
                                       lag(cumsum(weekly_4th_pass_stops)) / lag(cumsum(weekly_4th_pass_plays)),
                                       NA_real_),
      
      # Rolling averages using only previous weeks' data
      avg_pass_rushers = lag(cummean(weekly_pass_rushers)),
      avg_box_defenders = lag(cummean(weekly_box_defenders)),
      
      # Coverage rates using only previous weeks' data
      cover0_rate = lag(cumsum(weekly_cover0)) / lag(cumsum(weekly_plays)),
      cover1_rate = lag(cumsum(weekly_cover1)) / lag(cumsum(weekly_plays)),
      cover2_rate = lag(cumsum(weekly_cover2)) / lag(cumsum(weekly_plays)),
      cover3_rate = lag(cumsum(weekly_cover3)) / lag(cumsum(weekly_plays)),
      cover4_rate = lag(cumsum(weekly_cover4)) / lag(cumsum(weekly_plays)),
      cover6_rate = lag(cumsum(weekly_cover6)) / lag(cumsum(weekly_plays)),
      two_man_rate = lag(cumsum(weekly_two_man)) / lag(cumsum(weekly_plays)),
      prevent_rate = lag(cumsum(weekly_prevent)) / lag(cumsum(weekly_plays))
    ) %>%
    select(defteam, week, starts_with("def_stop_rate"), starts_with("avg_"), ends_with("_rate")) %>%
    ungroup()
  
  # Coverage-specific stop rates
  message("  Processing coverage-specific stop rates...")
  coverage_tendencies <- play_year %>%
    left_join(participation_data, by = c("game_id" = "nflverse_game_id", "play_id")) %>%
    filter(play_type %in% c("pass", "run")) %>%
    arrange(defteam, week) %>%
    group_by(defteam, week, defense_coverage_type, down) %>%
    summarize(
      weekly_plays = n(),
      weekly_stops = sum(!first_down, na.rm = TRUE),
      weekly_run_plays = sum(play_type == "run", na.rm = TRUE),
      weekly_pass_plays = sum(play_type == "pass", na.rm = TRUE),
      weekly_run_stops = sum(play_type == "run" & !first_down, na.rm = TRUE),
      weekly_pass_stops = sum(play_type == "pass" & !first_down, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(defteam, defense_coverage_type, down) %>%
    arrange(week) %>%
    mutate(
      # Calculate stop rates using only previous weeks' data
      coverage_stop_rate = if_else(lag(cumsum(weekly_plays)) >= 10,
                                   lag(cumsum(weekly_stops)) / lag(cumsum(weekly_plays)),
                                   NA_real_),
      coverage_stop_rate_run = if_else(lag(cumsum(weekly_run_plays)) >= 5,
                                       lag(cumsum(weekly_run_stops)) / lag(cumsum(weekly_run_plays)),
                                       NA_real_),
      coverage_stop_rate_pass = if_else(lag(cumsum(weekly_pass_plays)) >= 5,
                                        lag(cumsum(weekly_pass_stops)) / lag(cumsum(weekly_pass_plays)),
                                        NA_real_)
    ) %>%
    select(defteam, week, defense_coverage_type, down, 
           coverage_stop_rate, coverage_stop_rate_run, coverage_stop_rate_pass) %>%
    ungroup()
  
  # Print diagnostic info for all tendencies
  message("\nFormation tendencies summary:")
  print(summary(formation_tendencies))
  message(paste("Number of NA values in formation rates:", 
                sum(is.na(formation_tendencies$shotgun_rate))))
  
  message("\nDefense tendencies summary:")
  print(summary(defense_tendencies))
  message(paste("Number of NA values in stop rates:",
                sum(is.na(defense_tendencies$def_stop_rate_total))))
  
  message("\nCoverage tendencies summary:")
  print(summary(coverage_tendencies))
  message(paste("Number of NA values in coverage stop rates:",
                sum(is.na(coverage_tendencies$coverage_stop_rate))))
  
  message("Step 2.7 completed successfully")
  
  
  message("Step 3/7: Processing third and fourth downs...")
  third_downs <- play_year %>%
    filter(down == 3) %>%
    select(
      play_id,
      game_id,
      game_date,
      posteam,
      defteam,
      desc,
      home_team,
      away_team,
      ydstogo,
      yardline_100,
      game_seconds_remaining,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      posteam_score,
      defteam_score,
      play_type,
      yards_gained,
      first_down,
      rush_attempt,
      pass_attempt,
      field_goal_attempt,
      punt_attempt,
      home_coach,
      away_coach,
      week,
      roof,
      temp, 
      wind,
      drive,
      drive_start_transition,
      posteam_type,
      vegas_wp,
      vegas_home_wp,
      total_line,
      spread_line
    ) %>%
    mutate(
      score_diff = posteam_score - defteam_score,
      posteam_coach = ifelse(posteam == home_team, home_coach, away_coach),
      season = year,
      # Adjust spread_line based on posteam
      spread_line = case_when(
        posteam == home_team ~ spread_line,
        posteam == away_team ~ -1 * spread_line,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!field_goal_attempt, !punt_attempt, play_type %in% c("pass", "run")) %>%
    mutate(converted = first_down) %>%
    select(-first_down) %>%
    left_join(participation_data, 
              by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))
  
  fourth_downs <- play_year %>%
    filter(down == 4) %>%
    select(
      play_id,
      game_id,
      game_date,
      posteam,
      defteam,
      desc,
      home_team,
      away_team,
      ydstogo,
      yardline_100,
      game_seconds_remaining,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      posteam_score,
      defteam_score,
      play_type,
      yards_gained,
      first_down,
      rush_attempt,
      pass_attempt,
      field_goal_attempt,
      punt_attempt,
      home_coach,
      away_coach,
      week,
      roof,
      temp, 
      wind,
      drive,
      drive_start_transition,
      posteam_type,
      vegas_wp,
      vegas_home_wp,
      total_line,
      spread_line
    ) %>%
    mutate(
      score_diff = posteam_score - defteam_score,
      posteam_coach = ifelse(posteam == home_team, home_coach, away_coach),
      attempt = if_else(play_type %in% c("punt", "field_goal"), 0, 1),
      season = year,
      # Adjust spread_line based on posteam
      spread_line = case_when(
        posteam == home_team ~ spread_line,
        posteam == away_team ~ -1 * spread_line,
        TRUE ~ NA_real_
      )
    ) %>%
    left_join(participation_data, 
              by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))
  
  message("Step 3.1/7: Processing Vegas win probabilities for possession team...")
  
  # Convert vegas_home_wp to possession team win probability
  third_downs <- third_downs %>%
    mutate(
      vegas_wp_posteam = case_when(
        posteam == home_team ~ vegas_home_wp,  # Changed from vegas_wp
        posteam == away_team ~ 1 - vegas_home_wp,  # Changed from vegas_wp
        TRUE ~ NA_real_
      )
    )
  
  fourth_downs <- fourth_downs %>%
    mutate(
      vegas_wp_posteam = case_when(
        posteam == home_team ~ vegas_home_wp,  # Changed from vegas_wp
        posteam == away_team ~ 1 - vegas_home_wp,  # Changed from vegas_wp
        TRUE ~ NA_real_
      )
    )
  
  # Validation
  message("\nValidation:")
  message(sprintf("Third downs - NA rate in vegas_wp_posteam: %.2f%%", 
                  100 * sum(is.na(third_downs$vegas_wp_posteam)) / nrow(third_downs)))
  message(sprintf("Fourth downs - NA rate in vegas_wp_posteam: %.2f%%", 
                  100 * sum(is.na(fourth_downs$vegas_wp_posteam)) / nrow(fourth_downs)))
  
  message("Step 3.3/7: Processing previous season statistics...")
  
  prev_year <- year - 1
  message(sprintf("\nLoading data for previous year: %d", prev_year))
  
  # Load previous year data with error handling
  prev_play_year <- tryCatch({
    data <- load_pbp(prev_year)
    message(sprintf("Successfully loaded %d plays from previous year", nrow(data)))
    data
  }, error = function(e) {
    stop(sprintf("Error loading previous year data: %s", e$message))
  })
  
  prev_participation <- tryCatch({
    data <- load_participation(prev_year)
    message(sprintf("Successfully loaded %d participation records", nrow(data)))
    data
  }, error = function(e) {
    stop(sprintf("Error loading previous year participation data: %s", e$message))
  })
  
  # Initial data validation
  message("\nDEBUG: Initial data validation")
  message(sprintf("Number of unique games in prev_play_year: %d", 
                  length(unique(prev_play_year$game_id))))
  message(sprintf("Number of weeks in prev_play_year: %d", 
                  length(unique(prev_play_year$week))))
  message("Unique teams: ", 
          paste(sort(unique(prev_play_year$posteam[!is.na(prev_play_year$posteam)])), 
                collapse=", "))
  
  # Formation tendencies
  message("\nCalculating formation tendencies...")
  formation_stats <- prev_play_year %>%
    left_join(prev_participation, 
              by = c("game_id" = "nflverse_game_id", "play_id")) %>%
    filter(play_type %in% c("pass", "run")) %>%
    group_by(posteam) %>%
    summarize(
      prev_total_plays = n(),
      prev_shotgun_rate = mean(offense_formation == "SHOTGUN", na.rm = TRUE),
      prev_singleback_rate = mean(offense_formation == "SINGLEBACK", na.rm = TRUE),
      prev_empty_rate = mean(offense_formation == "EMPTY", na.rm = TRUE),
      prev_iform_rate = mean(offense_formation == "I_FORM", na.rm = TRUE),
      prev_pistol_rate = mean(offense_formation == "PISTOL", na.rm = TRUE),
      prev_wildcat_rate = mean(offense_formation == "WILDCAT", na.rm = TRUE),
      prev_jumbo_rate = mean(offense_formation == "JUMBO", na.rm = TRUE),
      
      prev_shotgun_success = mean(first_down[offense_formation == "SHOTGUN"], na.rm = TRUE),
      prev_singleback_success = mean(first_down[offense_formation == "SINGLEBACK"], na.rm = TRUE),
      prev_empty_success = mean(first_down[offense_formation == "EMPTY"], na.rm = TRUE),
      prev_iform_success = mean(first_down[offense_formation == "I_FORM"], na.rm = TRUE),
      prev_pistol_success = mean(first_down[offense_formation == "PISTOL"], na.rm = TRUE),
      prev_wildcat_success = mean(first_down[offense_formation == "WILDCAT"], na.rm = TRUE),
      prev_jumbo_success = mean(first_down[offense_formation == "JUMBO"], na.rm = TRUE)
    )
  
  message("\nDEBUG: Formation stats summary")
  print(summary(formation_stats))
  
  # Defensive statistics
  message("\nCalculating defensive statistics...")
  defensive_stats <- prev_play_year %>%
    filter(!is.na(defteam), play_type %in% c("pass", "run")) %>%
    group_by(defteam) %>%
    summarize(
      prev_stop_rate_overall = mean(!first_down, na.rm = TRUE),
      prev_stop_rate_run = mean(!first_down[play_type == "run"], na.rm = TRUE),
      prev_stop_rate_pass = mean(!first_down[play_type == "pass"], na.rm = TRUE),
      prev_stop_rate_1st = mean(!first_down[down == 1], na.rm = TRUE),
      prev_stop_rate_2nd = mean(!first_down[down == 2], na.rm = TRUE),
      prev_stop_rate_3rd = mean(!first_down[down == 3], na.rm = TRUE),
      prev_stop_rate_4th = mean(!first_down[down == 4], na.rm = TRUE)
    )
  
  message("\nDEBUG: Defensive stats summary")
  print(summary(defensive_stats))
  
  # Coverage statistics
  message("\nCalculating coverage statistics...")
  coverage_data <- prev_play_year %>%
    left_join(prev_participation, 
              by = c("game_id" = "nflverse_game_id", "play_id")) %>%
    filter(!is.na(defteam), play_type %in% c("pass", "run")) %>%
    mutate(
      defense_coverage_type = ifelse(is.na(defense_coverage_type), "NO_COVERAGE_DATA", defense_coverage_type)
    )
  
  message("\nDEBUG: Coverage type distribution:")
  print(table(coverage_data$defense_coverage_type, useNA = "ifany"))
  
  # Calculate stats for each coverage type
  coverage_types <- c(
    "COVER_0", "COVER_1", "COVER_2", "COVER_3", "COVER_4", "COVER_6",
    "2_MAN", "PREVENT", "NO_COVERAGE_DATA"
  )
  
  coverage_stats_list <- list()
  
  for(cov_type in coverage_types) {
    clean_name <- tolower(gsub("_", "", sub("COVER_", "cover", cov_type)))
    
    coverage_stats_list[[clean_name]] <- coverage_data %>%
      filter(defense_coverage_type == cov_type) %>%
      group_by(defteam) %>%
      summarize(
        !!sym(paste0("prev_", clean_name, "_plays")) := n(),
        !!sym(paste0("prev_", clean_name, "_stop_rate")) := mean(!first_down, na.rm = TRUE),
        !!sym(paste0("prev_", clean_name, "_stop_rate_run")) := mean(!first_down[play_type == "run"], na.rm = TRUE),
        !!sym(paste0("prev_", clean_name, "_stop_rate_pass")) := mean(!first_down[play_type == "pass"], na.rm = TRUE)
      )
  }
  
  # Combine all coverage stats
  coverage_stats <- Reduce(function(x, y) full_join(x, y, by = "defteam"), coverage_stats_list)
  
  message("\nDEBUG: Coverage stats summary by type:")
  for(type in names(coverage_stats_list)) {
    plays_col <- paste0("prev_", type, "_plays")
    message(sprintf("\n%s stats:", toupper(type)))
    print(summary(coverage_stats[[plays_col]]))
  }
  
  # Run/Pass tendencies
  message("\nCalculating run/pass tendencies...")
  run_pass_tendencies <- prev_play_year %>%
    filter(play_type %in% c("pass", "run")) %>%
    group_by(posteam, down) %>%
    summarize(
      prev_down_plays = n(),
      prev_down_run_pct = mean(play_type == "run", na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Fourth down specific tendencies
  fourth_down_tendencies <- prev_play_year %>%
    filter(down == 4, play_type %in% c("pass", "run")) %>%
    group_by(posteam) %>%
    summarize(
      prev_fourth_down_attempts = n(),
      prev_fourth_down_run_pct = mean(play_type == "run", na.rm = TRUE)
    )
  
  message("\nDEBUG: Run/Pass tendencies by down")
  print(table(prev_play_year$play_type, prev_play_year$down, useNA = "ifany"))
  
  # Team performance statistics
  message("\nCalculating team performance statistics...")
  team_games <- prev_play_year %>%
    select(game_id, game_date, week, home_team, away_team, home_score, away_score) %>%
    distinct() %>%
    mutate(
      home_win = case_when(
        home_score > away_score ~ 1,
        home_score < away_score ~ 0,
        home_score == away_score ~ 0.5
      )
    ) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = "team_type",
      values_to = "team"
    ) %>%
    mutate(
      win = if_else(
        team_type == "home_team",
        home_win,
        1 - home_win
      )
    )
  
  # Verify game counts
  game_counts <- team_games %>%
    group_by(team) %>%
    summarize(
      games = n(),
      min_week = min(week),
      max_week = max(week)
    )
  
  message("\nDEBUG: Game counts per team:")
  print(game_counts)
  
  # Calculate team stats
  team_stats <- team_games %>%
    group_by(team) %>%
    summarize(
      prev_total_games = n(),
      prev_total_wins = sum(win),
      prev_win_pct = mean(win),
      prev_home_win_pct = mean(win[team_type == "home_team"]),
      prev_away_win_pct = mean(win[team_type == "away_team"])
    ) %>%
    rename(posteam = team)
  
  # Verify no team has more games than possible
  max_possible_games <- if(prev_year >= 2021) 17 else 16
  teams_over_max <- team_stats %>% 
    filter(prev_total_games > max_possible_games)
  
  if(nrow(teams_over_max) > 0) {
    message("\nWARNING: Teams with more than maximum possible games:")
    print(teams_over_max)
  }
  
  message("\nDEBUG: Team stats summary")
  print(summary(team_stats))
  
  # Final validation
  message("\nFinal validation checks...")
  validation_results <- list(
    formation_teams = length(unique(formation_stats$posteam)),
    defensive_teams = length(unique(defensive_stats$defteam)),
    coverage_teams = length(unique(coverage_stats$defteam)),
    tendency_teams = length(unique(run_pass_tendencies$posteam)),
    team_stats_teams = length(unique(team_stats$posteam))
  )
  
  message("\nTeam counts in each dataset:")
  print(validation_results)
  
  if(length(unique(c(validation_results))) > 1) {
    warning("Inconsistent team counts across datasets!")
  }
  
  message("Step 3.3 completed with full validation")
  
 

  #################################################
  
  message("Step 3.6/7: Adding starters based on individual position depth chart...")
  
  # First load required libraries with error handling
  required_packages <- c("nflreadr", "dplyr", "purrr", "tidyr", "lubridate")
  for(pkg in required_packages) {
    if(!require(pkg, character.only = TRUE)) {
      message(sprintf("Installing %s...", pkg))
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Ensure depth charts are loaded with explicit seasons range
  message("Loading depth charts data...")
  depth_charts <- tryCatch({
    # Load charts for current year and adjacent years for better coverage
    charts <- load_depth_charts(seasons = c(year-1, year, year+1))
    message(sprintf("Successfully loaded %d depth chart records", nrow(charts)))
    charts
  }, error = function(e) {
    message(sprintf("Error loading depth charts: %s", e$message))
    return(data.frame()) # Return empty dataframe on error
  })
  
  # Display available team codes in depth charts
  if(nrow(depth_charts) > 0) {
    message("Available team codes in depth charts:")
    team_codes <- unique(depth_charts$club_code)
    message(paste(sort(team_codes), collapse=", "))
  }
  
  # Individual position-based starter selection function - FIXED NAMING
  get_individual_starters <- function(target_team, target_week, target_year) {
    # Handle team code standardization
    standard_team_code <- case_when(
      target_team == "JAC" ~ "JAX",
      target_team == "LAR" ~ "LA",
      target_team == "WSH" ~ "WAS",
      target_team == "HST" ~ "HOU",
      target_team == "CLV" ~ "CLE", 
      target_team == "BLT" ~ "BAL",
      target_team == "ARZ" ~ "ARI",
      target_team == "LV" ~ "LV",
      TRUE ~ target_team
    )
    
    # Get depth chart data with fallback logic
    team_depth_chart <- depth_charts %>%
      filter(
        season == target_year,
        week == target_week,
        club_code == standard_team_code
      )
    
    # Fallback to previous week if no data
    if(nrow(team_depth_chart) == 0 && target_week > 1) {
      team_depth_chart <- depth_charts %>%
        filter(
          season == target_year,
          week == target_week - 1,
          club_code == standard_team_code
        )
    }
    
    # Final fallback to week 1
    if(nrow(team_depth_chart) == 0) {
      team_depth_chart <- depth_charts %>%
        filter(
          season == target_year,
          week == 1,
          club_code == standard_team_code
        )
    }
    
    if(nrow(team_depth_chart) == 0) {
      return(data.frame())
    }
    
    # Process and categorize positions with proper filtering
    team_depth_chart <- team_depth_chart %>%
      mutate(
        depth_team = as.numeric(ifelse(is.na(depth_team), "99", depth_team)),
        side = case_when(
          formation == "Offense" ~ "offense",
          formation == "Defense" ~ "defense",
          TRUE ~ "special"
        )
      ) %>%
      # Filter to only valid side-position combinations
      filter(
        (side == "offense" & position %in% OFFENSE_POSITIONS) |
          (side == "defense" & position %in% DEFENSE_POSITIONS)
      ) %>%
      # Also ensure the position exists in our CSV
      filter(position %in% names(POSITION_MAPPINGS))
    
    # Get starters for each individual position
    position_starters <- team_depth_chart %>%
      group_by(side, position) %>%
      arrange(depth_team) %>%
      slice_head(n = max(unlist(POSITION_QUANTITIES))) %>%
      mutate(position_rank = row_number()) %>%
      ungroup()
    
    # Filter to only the quantities we want for each position
    final_starters <- position_starters %>%
      rowwise() %>%
      filter(position_rank <= as.numeric(POSITION_QUANTITIES[[position]])) %>%
      ungroup() %>%
      mutate(
        # FIXED: Create column names like starter_offense_QB_1 (the correct pattern)
        column_name = paste0("starter_", side, "_", position, "_", position_rank)
      ) %>%
      select(column_name, gsis_id)
    
    # Convert to wide format
    if(nrow(final_starters) > 0) {
      result <- final_starters %>%
        pivot_wider(
          names_from = column_name,
          values_from = gsis_id,
          values_fill = NA_character_
        )
      
      if(nrow(result) == 0) {
        result <- data.frame(dummy = NA_character_)
        result$dummy <- NULL
      }
    } else {
      result <- data.frame()
    }
    
    return(result)
  }
  
  # Function to add individual position-based starters to datasets - FIXED JOINING
  add_individual_starters <- function(df) {
    message("Adding individual position-based starters...")
    
    # Process each unique team-week-season combination
    unique_combinations <- df %>%
      select(posteam, defteam, week, season) %>%
      distinct() %>%
      # Get unique teams (both offense and defense)
      gather(team_type, team, posteam, defteam) %>%
      select(team, week, season) %>%
      distinct()
    
    # Get starters for each combination
    all_starters <- unique_combinations %>%
      group_by(team, week, season) %>%
      group_modify(~{
        tryCatch({
          get_individual_starters(.y$team[1], .y$week[1], .y$season[1])
        }, error = function(e) {
          message(sprintf("Error getting starters for %s, week %d, season %d: %s", 
                          .y$team[1], .y$week[1], .y$season[1], e$message))
          data.frame()
        })
      }) %>%
      ungroup()
    
    # Look for columns that start with starter_offense_ and starter_defense_
    offense_cols <- grep("^starter_offense_", names(all_starters), value = TRUE)
    defense_cols <- grep("^starter_defense_", names(all_starters), value = TRUE)
    
    message(sprintf("Found %d offense starter columns and %d defense starter columns", 
                    length(offense_cols), length(defense_cols)))
    
    # Start with original dataframe
    result_df <- df
    
    # Join offensive starters
    if(length(offense_cols) > 0) {
      offense_starters <- all_starters %>%
        select(team, week, season, all_of(offense_cols))
      
      result_df <- result_df %>%
        left_join(offense_starters, by = c("posteam" = "team", "week", "season"))
      
      message("Successfully joined offense starters")
    } else {
      message("No offense starter columns found")
    }
    
    # Join defensive starters
    if(length(defense_cols) > 0) {
      defense_starters <- all_starters %>%
        select(team, week, season, all_of(defense_cols))
      
      result_df <- result_df %>%
        left_join(defense_starters, by = c("defteam" = "team", "week", "season"))
      
      message("Successfully joined defense starters")
    } else {
      message("No defense starter columns found")
    }
    
    return(result_df)
  }
  
  # Process third downs with individual position-based starters
  message("\nProcessing third downs starters...")
  third_downs_with_starters <- add_individual_starters(third_downs)
  
  # Process fourth downs with individual position-based starters
  message("\nProcessing fourth downs starters...")
  fourth_downs_with_starters <- add_individual_starters(fourth_downs)
  
  # Validate results
  message("\nValidating Third Downs:")
  message(sprintf("Original rows: %d, With starters: %d", 
                  nrow(third_downs), nrow(third_downs_with_starters)))
  
  message("\nValidating Fourth Downs:")
  message(sprintf("Original rows: %d, With starters: %d", 
                  nrow(fourth_downs), nrow(fourth_downs_with_starters)))
  
  # Calculate percentage of starter fields that are filled - CORRECTED PATTERN
  calculate_starter_coverage <- function(df) {
    offense_cols <- grep("^starter_offense_", names(df), value = TRUE)
    defense_cols <- grep("^starter_defense_", names(df), value = TRUE)
    
    message(sprintf("Found %d offense columns and %d defense columns for coverage check", 
                    length(offense_cols), length(defense_cols)))
    
    offense_coverage <- if(length(offense_cols) > 0) {
      mean(!is.na(df[, offense_cols])) * 100
    } else {
      0
    }
    
    defense_coverage <- if(length(defense_cols) > 0) {
      mean(!is.na(df[, defense_cols])) * 100
    } else {
      0
    }
    
    message(sprintf("Offense starter fields filled: %.2f%%", offense_coverage))
    message(sprintf("Defense starter fields filled: %.2f%%", defense_coverage))
  }
  
  message("\nThird downs starter coverage:")
  calculate_starter_coverage(third_downs_with_starters)
  
  message("\nFourth downs starter coverage:")
  calculate_starter_coverage(fourth_downs_with_starters)
  
  # Debug: Show sample of column names created
  message("\nSample of starter columns created:")
  starter_cols <- grep("^starter_", names(third_downs_with_starters), value = TRUE)
  if(length(starter_cols) > 0) {
    message("Sample starter columns:", paste(head(starter_cols, 10), collapse=", "))
  } else {
    message("No starter columns found in final dataset")
  }
  
  # Replace original dataframes with processed versions
  third_downs <- third_downs_with_starters
  fourth_downs <- fourth_downs_with_starters
  
  message("Step 3.6 completed")
  
 
  message("Step 4/7: Adding player information...")
  message("Step 4/7: Adding player information...")
  
  # Process individual position-based starter players - FIXED WITH DEBUGGING
  process_individual_player_info <- function(df, roster_data) {
    message("DEBUG: Checking column names in dataframe...")
    all_cols <- names(df)
    
    # Debug: Show all column names that contain "starter"
    starter_related_cols <- grep("starter", all_cols, value = TRUE)
    message(sprintf("DEBUG: Found %d columns containing 'starter':", length(starter_related_cols)))
    if(length(starter_related_cols) > 0) {
      message("DEBUG: First 10 starter-related columns:", paste(head(starter_related_cols, 10), collapse=", "))
    }
    
    # CORRECTED: Look for the actual pattern that was created in Step 3.6
    # The pattern should be: starter_offense_QB_1, starter_defense_DE_1, etc.
    starter_cols <- grep("^starter_(offense|defense)_[A-Z]+_[0-9]+$", names(df), value = TRUE)
    
    message(sprintf("DEBUG: Found %d starter columns with correct pattern", length(starter_cols)))
    if(length(starter_cols) > 0) {
      message("DEBUG: First 10 starter columns:", paste(head(starter_cols, 10), collapse=", "))
    } else {
      message("DEBUG: No columns found with pattern ^starter_(offense|defense)_[A-Z]+_[0-9]+$")
      message("DEBUG: Let's try a broader search...")
      
      # Try broader patterns to see what we actually have
      broader_starter_cols <- grep("^starter_", names(df), value = TRUE)
      message(sprintf("DEBUG: Found %d columns starting with 'starter_':", length(broader_starter_cols)))
      if(length(broader_starter_cols) > 0) {
        message("DEBUG: All starter columns:", paste(broader_starter_cols, collapse=", "))
      }
    }
    
    message(sprintf("Found %d starter columns to process", length(starter_cols)))
    
    # Process each starter column
    for(i in seq_along(starter_cols)) {
      col_name <- starter_cols[i]
      cat("\rProcessing", col_name, "(", i, "of", length(starter_cols), ")")
      
      prefix <- col_name
      df <- add_player_info(df, roster_data, col_name, prefix)
    }
    
    cat("\n")
    return(df)
  }
  
  # Process third downs
  message("\nProcessing third downs...")
  message("DEBUG: Column names before processing (first 20):")
  message(paste(head(names(third_downs), 20), collapse=", "))
  
  third_downs <- process_individual_player_info(third_downs, roster_data)
  
  message("DEBUG: Column names after processing third downs (showing new player info columns):")
  player_info_cols <- grep("_(full_name|birth_date|rookie_year|injury|position)", names(third_downs), value = TRUE)
  message(sprintf("DEBUG: Found %d player info columns", length(player_info_cols)))
  if(length(player_info_cols) > 0) {
    message("DEBUG: First 10 player info columns:", paste(head(player_info_cols, 10), collapse=", "))
  }
  
  # Process fourth downs
  message("\nProcessing fourth downs...")
  message("DEBUG: Column names before processing (first 20):")
  message(paste(head(names(fourth_downs), 20), collapse=", "))
  
  fourth_downs <- process_individual_player_info(fourth_downs, roster_data)
  
  message("DEBUG: Column names after processing fourth downs (showing new player info columns):")
  player_info_cols <- grep("_(full_name|birth_date|rookie_year|injury|position)", names(fourth_downs), value = TRUE)
  message(sprintf("DEBUG: Found %d player info columns", length(player_info_cols)))
  if(length(player_info_cols) > 0) {
    message("DEBUG: First 10 player info columns:", paste(head(player_info_cols, 10), collapse=", "))
  }
  
  # Print diagnostic info - UPDATED FOR CORRECT PATTERN
  diagnostic_summary <- function(df, name) {
    message("\nDiagnostic summary for ", name)
    
    # Count starter columns by type using the correct pattern
    offense_starter_cols <- sum(grepl("^starter_offense_.*_(full_name|birth_date|rookie_year|injury)", names(df)))
    defense_starter_cols <- sum(grepl("^starter_defense_.*_(full_name|birth_date|rookie_year|injury)", names(df)))
    
    message(sprintf("Offense starter columns: %d", offense_starter_cols))
    message(sprintf("Defense starter columns: %d", defense_starter_cols))
    
    # Check for missing data
    check_missing <- function(pattern) {
      cols <- grep(pattern, names(df), value = TRUE)
      if(length(cols) > 0) {
        missing_rate <- mean(is.na(df[cols]))
        return(missing_rate)
      }
      return(0)
    }
    
    message("\nMissing data rates:")
    message(sprintf("Offense starters: %.2f%%", check_missing("^starter_offense_") * 100))
    message(sprintf("Defense starters: %.2f%%", check_missing("^starter_defense_") * 100))
  }
  
  # Run diagnostics
  diagnostic_summary(third_downs, "Third downs")
  diagnostic_summary(fourth_downs, "Fourth downs")
  
  message("Step 4/7 completed successfully")

# Run diagnostics
diagnostic_summary(third_downs, "Third downs")
diagnostic_summary(fourth_downs, "Fourth downs")

message("Step 4/7 completed successfully")
  
  message("Step 5/7: Processing team injury data...")
  
  # Process third downs team-level injuries
  third_downs <- third_downs %>%
    mutate(defteam = ifelse(posteam == home_team, away_team, home_team)) %>%
    left_join(run_pcts %>% 
                mutate(join_week = week + 1) %>% 
                select(-week), 
              by = c("posteam", "week" = "join_week")) %>%
    left_join(prep_time_df %>% 
                select(posteam, game_date, prep_days),
              by = c("posteam", "game_date"))
  
  fourth_downs <- fourth_downs %>%
    mutate(defteam = ifelse(posteam == home_team, away_team, home_team)) %>%
    left_join(run_pcts %>% 
                mutate(join_week = week + 1) %>% 
                select(-week), 
              by = c("posteam", "week" = "join_week")) %>%
    left_join(prep_time_df %>% 
                select(posteam, game_date, prep_days),
              by = c("posteam", "game_date"))
  
  # Function to get individual position-specific injury aggregations
  get_individual_position_injuries <- function(df, side = "offense") {
    # Get all individual positions from our CSV that belong to this side
    if(side == "offense") {
      available_positions <- intersect(names(POSITION_MAPPINGS), OFFENSE_POSITIONS)
    } else {
      available_positions <- intersect(names(POSITION_MAPPINGS), DEFENSE_POSITIONS)
    }
    
    # For each position, find matching columns and calculate average injuries
    for(pos in available_positions) {
      # Find columns matching this individual position
      pattern <- paste0("^", side, "_starter_", side, "_", pos, "_\\d+_injury_count_1yr$")
      matching_cols <- grep(pattern, names(df), value = TRUE)
      
      # Calculate average if we found matching columns
      if(length(matching_cols) > 0) {
        col_name <- paste0(side, "_", pos, "_injuries")
        df[[col_name]] <- rowMeans(df[matching_cols], na.rm = TRUE)
        # Set to 0 if all values were NA
        df[[col_name]][is.nan(df[[col_name]])] <- 0
      } else {
        # Create the column with 0s if no matching columns found
        col_name <- paste0(side, "_", pos, "_injuries")
        df[[col_name]] <- 0
      }
    }
    
    return(df)
  }
  
  
  # Process individual position-specific injury aggregations for third downs
  # Process individual position-specific injury aggregations for third downs
  third_downs <- third_downs %>%
    group_by(posteam, game_date) %>%
    group_modify(~get_individual_position_injuries(.x, "offense")) %>%
    ungroup() %>%
    group_by(defteam, game_date) %>%
    group_modify(~get_individual_position_injuries(.x, "defense")) %>%
    ungroup()
  
  # Process individual position-specific injury aggregations for fourth downs
  fourth_downs <- fourth_downs %>%
    group_by(posteam, game_date) %>%
    group_modify(~get_individual_position_injuries(.x, "offense")) %>%
    ungroup() %>%
    group_by(defteam, game_date) %>%
    group_modify(~get_individual_position_injuries(.x, "defense")) %>%
    ungroup()
  
  get_team_injury_counts <- function(team, game_date) {
    game_date <- as.Date(game_date)
    
    team_injuries <- injuries_data %>%
      filter(team == !!team) %>%
      mutate(
        injury_date = as.Date(date_modified),
        standard_injury = case_when(
          practice_primary_injury %in% top_injury_types ~ practice_primary_injury,
          TRUE ~ "Other"
        )
      )
    
    year_ago <- game_date - 365
    year_injuries <- team_injuries %>%
      filter(injury_date >= year_ago,
             injury_date < game_date) %>%
      count(standard_injury) %>%
      pivot_wider(
        names_from = standard_injury,
        names_prefix = "injury_count_year_",
        values_from = n,
        values_fill = 0
      )
    
    week_ago <- game_date - 7
    week_injuries <- team_injuries %>%
      filter(injury_date >= week_ago,
             injury_date < game_date) %>%
      count(standard_injury) %>%
      pivot_wider(
        names_from = standard_injury,
        names_prefix = "injury_count_week_",
        values_from = n,
        values_fill = 0
      )
    
    week_status <- team_injuries %>%
      filter(injury_date >= week_ago,
             injury_date < game_date) %>%
      summarise(
        week_questionable = sum(report_status == "Questionable", na.rm = TRUE),
        week_doubtful = sum(report_status == "Doubtful", na.rm = TRUE),
        week_out = sum(report_status == "Out", na.rm = TRUE),
        week_limited_practice = sum(practice_status == "Limited Participation in Practice", na.rm = TRUE),
        week_dnp = sum(practice_status == "Did Not Participate In Practice", na.rm = TRUE)
      )
    
    bind_cols(year_injuries, week_injuries, week_status)
  }
  
  # Apply team injury counts
  third_downs <- third_downs %>%
    rowwise() %>%
    mutate(
      offense_injuries = list(get_team_injury_counts(posteam, game_date)),
      defense_injuries = list(get_team_injury_counts(defteam, game_date))
    ) %>%
    ungroup() %>%
    unnest_wider(c(offense_injuries, defense_injuries), 
                 names_sep = "_")
  
  fourth_downs <- fourth_downs %>%
    rowwise() %>%
    mutate(
      offense_injuries = list(get_team_injury_counts(posteam, game_date)),
      defense_injuries = list(get_team_injury_counts(defteam, game_date))
    ) %>%
    ungroup() %>%
    unnest_wider(c(offense_injuries, defense_injuries), 
                 names_sep = "_")
  
  # Add fixed down values since all third downs have down=3
  third_downs$down <- 3
  fourth_downs$down <- 4

  message("Step 6: Checking key data structures and creating final models...")
  
  # Debug data structures
  print_data_summary <- function(df, name) {
    message(sprintf("\nSummary for %s:", name))
    message(sprintf("Dimensions: %d rows × %d columns", nrow(df), ncol(df)))
    message("Sample of columns:", paste(head(names(df)), collapse=", "))
    message(sprintf("Number of unique games: %d", length(unique(df$game_id))))
    message(sprintf("Number of unique teams: %d", length(unique(df$posteam))))
  }
  
  print_data_summary(third_downs, "third_downs")
  print_data_summary(fourth_downs, "fourth_downs")
  
  # Validate input structure
  validate_input_structure <- function(df, name) {
    message(sprintf("\n%s structure:", name))
    
    # Check key required columns
    req_cols <- c("play_id", "game_id", "posteam", "defteam", "season", "week")
    missing <- req_cols[!req_cols %in% names(df)]
    if(length(missing) > 0) {
      warning(sprintf("Missing required columns in %s: %s", name, paste(missing, collapse=", ")))
    }
  }
  
  # Create base models with tracking
  message("\nCreating base models...")
  
  create_base_model <- function(df, model_name) {
    message(sprintf("\nProcessing %s...", model_name))
    
    result <- df %>%
      mutate(
        play_type_clean = case_when(
          pass_attempt == 1 ~ "pass",
          rush_attempt == 1 ~ "rush",
          TRUE ~ "other"
        )
      ) %>%
      group_by(posteam) %>%
      arrange(week) %>%
      mutate(
        down1_pct = zoo::na.locf(down1_pct, na.rm = FALSE),
        down2_pct = zoo::na.locf(down2_pct, na.rm = FALSE),
        down3_pct = zoo::na.locf(down3_pct, na.rm = FALSE),
        down4_pct = zoo::na.locf(down4_pct, na.rm = FALSE)
      ) %>%
      ungroup()
    
    message(sprintf("%s base processing complete. Rows: %d", model_name, nrow(result)))
    return(result)
  }
  
  model_3rd <- create_base_model(third_downs, "model_3rd")
  model_4th <- create_base_model(fourth_downs, "model_4th")
  
  # Process joins with validation
  process_joins <- function(model_df, model_name) {
    message(sprintf("\nProcessing joins for %s...", model_name))
    
    tryCatch({
      # Coach and team record joins
      result <- model_df %>%
        left_join(coach_records, by = c("posteam_coach" = "coach")) %>%
        left_join(current_team_records, by = c("posteam_coach" = "coach", "posteam")) %>%
        left_join(game_tenure %>% 
                    select(game_id, posteam, coach, tenure_days), 
                  by = c("game_id", "posteam", "posteam_coach" = "coach"))
      
      # Previous season stats joins
      result <- result %>%
        left_join(formation_stats, by = "posteam") %>%
        left_join(defensive_stats, by = "defteam") %>%
        left_join(coverage_stats, by = "defteam") %>%
        left_join(run_pass_tendencies, by = c("posteam", "down")) %>%
        left_join(fourth_down_tendencies, by = "posteam") %>%
        left_join(team_stats, by = "posteam")
      
      # Current season tendencies
      result <- result %>%
        left_join(formation_tendencies %>% 
                    filter(down == model_df$down[1]), 
                  by = c("posteam", "down", "week")) %>%
        left_join(defense_tendencies, by = c("defteam", "week"))
      
      return(result)
    }, error = function(e) {
      message(sprintf("Error in %s joins: %s", model_name, e$message))
      return(model_df)
    })
  }
  
  # Process both models
  model_3rd <- process_joins(model_3rd, "model_3rd")
  model_4th <- process_joins(model_4th, "model_4th")
  
  # Check duplicates
  check_duplicates <- function(df, model_name) {
    dupes <- df %>%
      group_by(play_id, game_id) %>%
      filter(n() > 1) %>%
      ungroup()
    
    n_dupes <- nrow(dupes)
    message(sprintf("%s duplicates: %d", model_name, n_dupes))
    
    if(n_dupes > 0) {
      message("\nSample of duplicates:")
      print(dupes %>% 
              select(play_id, game_id, posteam, defteam, week) %>%
              head())
    }
    
    return(n_dupes)
  }
  
  third_dupes <- check_duplicates(model_3rd, "model_3rd")
  fourth_dupes <- check_duplicates(model_4th, "model_4th")
  
  # Final validation - Updated for individual position-based approach
  validate_final_model <- function(df, model_name) {
    message(sprintf("\nValidating %s:", model_name))
    
    # Check dimensions and key columns
    message(sprintf("Dimensions: %d rows × %d columns", nrow(df), ncol(df)))
    
    # Position-specific validations for individual position-based system
    validate_individual_position_coverage <- function(df) {
      message("\nIndividual position coverage:")
      
      # Check coverage for each individual position
      for(side in c("offense", "defense")) {
        message(sprintf("\n%s positions:", toupper(side)))
        
        # Get position names based on side
        if(side == "offense") {
          positions <- intersect(names(POSITION_MAPPINGS), OFFENSE_POSITIONS)
        } else {
          positions <- intersect(names(POSITION_MAPPINGS), DEFENSE_POSITIONS)
        }
        
        for(pos in positions) {
          pattern <- paste0("^", side, "_starter_", side, "_", pos, "_\\d+$")
          pos_cols <- grep(pattern, names(df), value = TRUE)
          
          if(length(pos_cols) > 0) {
            filled_count <- rowSums(!is.na(df[pos_cols]))
            avg_filled <- mean(filled_count)
            max_possible <- length(pos_cols)
            coverage_pct <- mean(!is.na(df[pos_cols])) * 100
            
            message(sprintf("  %s: %.1f avg players filled (max: %d, %.1f%% coverage)", 
                            pos, avg_filled, max_possible, coverage_pct))
          } else {
            message(sprintf("  %s: No columns found", pos))
          }
        }
      }
    }
    
    validate_individual_position_coverage(df)
    
    # Check NA rates
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    na_rates <- sapply(df[numeric_cols], function(x) mean(is.na(x))) * 100
    high_na_cols <- names(na_rates)[na_rates > 20]
    
    if(length(high_na_cols) > 0) {
      message(sprintf("\nColumns with high NA rates (>20%%): %d columns", length(high_na_cols)))
      # Show just the first few to avoid overwhelming output
      message("Sample high-NA columns:", paste(head(high_na_cols, 10), collapse=", "))
    }
    
    return(list(
      dims = dim(df),
      high_na_cols = high_na_cols
    ))
  }
  
  model_3rd_validation <- validate_final_model(model_3rd, "model_3rd")
  model_4th_validation <- validate_final_model(model_4th, "model_4th")
  
  # Return final models
  return(list(
    third_downs = model_3rd,
    fourth_downs = model_4th
  ))
  
}


# Main processing
years <- 2017:2023
all_data <- list()

for(year in years) {
  all_data[[as.character(year)]] <- process_year(year)
}

combined_third_downs <- bind_rows(
  lapply(all_data, function(x) validate_data(x$third_downs))
)

combined_fourth_downs <- bind_rows(
  lapply(all_data, function(x) validate_data(x$fourth_downs))
)



# After the bind_rows, add specialists data
message("Adding specialists data...")

# Load depth charts for all years (add this section)
message("Loading depth charts for specialists fallback...")
depth_charts <- tryCatch({
  charts <- load_depth_charts(seasons = 2016:2024)  # Load wider range for coverage
  message(sprintf("Successfully loaded %d depth chart records for specialists", nrow(charts)))
  charts
}, error = function(e) {
  message(sprintf("Error loading depth charts for specialists: %s", e$message))
  return(data.frame()) # Return empty dataframe on error
})



# Function to get specialists for a year with depth chart fallback
get_specialists <- function(year) {
  pbp <- bind_rows(
    load_pbp(year),
    load_pbp(year - 1)
  ) %>%
    arrange(game_date, desc(play_id))
  
  # Get kickers from PBP, including the game date
  kickers_pbp <- pbp %>%
    filter(field_goal_attempt == 1 | extra_point_attempt == 1) %>%
    select(team = posteam, week, season, game_date, 
           kicker_player_id, kicker_player_name) %>%
    distinct(team, week, season, .keep_all = TRUE)
  
  # Get punters from PBP, including the game date
  punters_pbp <- pbp %>%
    filter(punt_attempt == 1) %>%
    select(team = posteam, week, season, game_date,
           punter_player_id, punter_player_name) %>%
    distinct(team, week, season, .keep_all = TRUE)
  
  # Get depth chart data for fallback
  depth_charts_year <- depth_charts %>%
    filter(season == year) %>%
    mutate(
      club_code = case_when(
        club_code == "JAX" ~ "JAC",
        club_code == "LA" ~ "LAR", 
        club_code == "WAS" ~ "WSH",
        club_code == "HOU" ~ "HST",
        club_code == "CLE" ~ "CLV",
        club_code == "BAL" ~ "BLT",
        club_code == "ARI" ~ "ARZ",
        TRUE ~ club_code
      )
    )
  
  # Get kickers from depth charts
  kickers_depth <- depth_charts_year %>%
    filter(position == "K") %>%
    group_by(club_code, week) %>%
    arrange(depth_team) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(team = club_code, week, season,
           kicker_player_id = gsis_id, 
           kicker_player_name = full_name)
  
  # Get punters from depth charts
  punters_depth <- depth_charts_year %>%
    filter(position == "P") %>%
    group_by(club_code, week) %>%
    arrange(depth_team) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(team = club_code, week, season,
           punter_player_id = gsis_id,
           punter_player_name = full_name)
  
  # Combine PBP data with depth chart fallback for kickers
  kickers_combined <- kickers_pbp %>%
    full_join(kickers_depth, by = c("team", "week", "season")) %>%
    mutate(
      kicker_player_id = coalesce(kicker_player_id.x, kicker_player_id.y),
      kicker_player_name = coalesce(kicker_player_name.x, kicker_player_name.y)
    ) %>%
    select(team, week, season, game_date, kicker_player_id, kicker_player_name)
  
  # Combine PBP data with depth chart fallback for punters
  punters_combined <- punters_pbp %>%
    full_join(punters_depth, by = c("team", "week", "season")) %>%
    mutate(
      punter_player_id = coalesce(punter_player_id.x, punter_player_id.y),
      punter_player_name = coalesce(punter_player_name.x, punter_player_name.y)
    ) %>%
    select(team, week, season, game_date, punter_player_id, punter_player_name)
  
  list(kickers = kickers_combined, punters = punters_combined)
}

# Get specialists for all years
years <- 2017:2023
all_specialists <- lapply(years, function(year) {
  message(sprintf("Getting specialists for year %d...", year))
  get_specialists(year)
})

# Combine specialists data across all years 
all_kickers <- bind_rows(lapply(all_specialists, function(x) x$kickers))
all_punters <- bind_rows(lapply(all_specialists, function(x) x$punters))

# Process kickers data
processed_kickers <- all_kickers %>%
  mutate(game_date = as.Date(game_date)) %>%  # Convert to Date first
  arrange(desc(coalesce(game_date, as.Date("1900-01-01")))) %>%
  group_by(team, season, week) %>%
  slice(1) %>%  # Take most recent kicker for each team-week
  ungroup() %>%
  select(team, season, week, 
         kicker_id = kicker_player_id, 
         kicker_name = kicker_player_name)

# Process punters data
processed_punters <- all_punters %>%
  mutate(game_date = as.Date(game_date)) %>%  # Convert to Date first
  arrange(desc(coalesce(game_date, as.Date("1900-01-01")))) %>%
  group_by(team, season, week) %>%
  slice(1) %>%  # Take most recent punter for each team-week
  ungroup() %>%
  select(team, season, week,
         punter_id = punter_player_id,
         punter_name = punter_player_name)

# Add specialists to combined datasets
combined_third_downs <- combined_third_downs %>%
  left_join(processed_kickers, 
            by = c("posteam" = "team", "season", "week")) %>%
  left_join(processed_punters,
            by = c("posteam" = "team", "season", "week"))

combined_fourth_downs <- combined_fourth_downs %>%
  left_join(processed_kickers,
            by = c("posteam" = "team", "season", "week")) %>%
  left_join(processed_punters,
            by = c("posteam" = "team", "season", "week"))

# Validate the joins
message("\nValidating specialist data:")
message(sprintf("Third downs - Kickers: %d unique IDs, %d NAs", 
                length(unique(na.omit(combined_third_downs$kicker_id))),
                sum(is.na(combined_third_downs$kicker_id))))
message(sprintf("Third downs - Punters: %d unique IDs, %d NAs", 
                length(unique(na.omit(combined_third_downs$punter_id))),
                sum(is.na(combined_third_downs$punter_id))))
message(sprintf("Fourth downs - Kickers: %d unique IDs, %d NAs", 
                length(unique(na.omit(combined_fourth_downs$kicker_id))),
                sum(is.na(combined_fourth_downs$kicker_id))))
message(sprintf("Fourth downs - Punters: %d unique IDs, %d NAs", 
                length(unique(na.omit(combined_fourth_downs$punter_id))),
                sum(is.na(combined_fourth_downs$punter_id))))




# Filter out playoff games from combined datasets
combined_third_downs <- combined_third_downs %>%
  filter(
    (season < 2021 & week <= 17) |  # Pre-2021 seasons
      (season >= 2021 & week <= 18)    # 2021 and later seasons
  )

combined_fourth_downs <- combined_fourth_downs %>%
  filter(
    (season < 2021 & week <= 17) |  # Pre-2021 seasons
      (season >= 2021 & week <= 18)    # 2021 and later seasons
  )

# Read the attendance data
attendance_data <- read.csv("nfl_attendance_2016_2023.csv")
# Revised add_attendance function
add_attendance <- function(pbp_df, attendance_df) {
  pbp_df %>%
    left_join(
      # Reshape attendance data to long format for joining
      attendance_df %>%
        pivot_longer(
          cols = starts_with("Week."),  # Match Week. columns
          names_to = "week_label",
          values_to = "attendance"
        ) %>%
        # Extract week number, accounting for the dot
        mutate(
          week = as.numeric(gsub("Week\\.", "", week_label)),
          home_attendance = attendance
        ) %>%
        select(Team, Year, week, home_attendance),
      # Join conditions
      by = c(
        "home_team" = "Team",
        "season" = "Year",
        "week" = "week"
      )
    )
}

# Apply to your pbp datasets
combined_third_downs <- add_attendance(combined_third_downs, attendance_data)
combined_fourth_downs <- add_attendance(combined_fourth_downs, attendance_data)

# Verify the join worked
print("Number of NA attendance values in third downs:")
print(sum(is.na(combined_third_downs$home_attendance)))
print("\nNumber of NA attendance values in fourth downs:")
print(sum(is.na(combined_fourth_downs$home_attendance)))




library(nflreadr)
library(tidyverse)
library(slider)

# Create season-aware team mapping function
team_name_mapping <- function(team, season) {
  case_when(
    # Chargers: SD -> LAC in 2017
    team == "LAC" & season < 2017 ~ "SD",
    team == "SD" & season >= 2017 ~ "LAC",
    # Rams: STL/SL -> LA in 2016
    team %in% c("LA", "SL") & season < 2016 ~ "STL",
    team == "STL" & season >= 2016 ~ "LA",
    # Raiders: OAK -> LV in 2020
    team == "LV" & season < 2020 ~ "OAK",
    team == "OAK" & season >= 2020 ~ "LV",
    # Other historical fixes
    team == "ARZ" ~ "ARI",
    team == "BLT" ~ "BAL",
    team == "CLV" ~ "CLE",
    team == "HST" ~ "HOU",
    # If no mapping needed, return original
    TRUE ~ team
  )
}

# Process roster data for team changes
player_team_changes <- load_rosters_weekly(seasons = 2015:2023) %>%
  filter(
    (season < 2021 & week <= 17) |  
      (season >= 2021 & week <= 18)    
  ) %>%
  mutate(
    team = map2_chr(team, season, team_name_mapping)
  ) %>%
  select(player_id = gsis_id, team, season, week) %>%
  group_by(player_id, season, week) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(player_id, season, week) %>%
  group_by(player_id) %>%
  mutate(
    prev_team = lag(team),
    prev_season = lag(season),
    prev_week = lag(week)
  ) %>%
  filter(team != prev_team, !is.na(prev_team)) %>%
  # Remove relocations
  filter(!(
    # Rams relocation
    (prev_team == "STL" & team == "LA" & prev_season == 2015 & season == 2016) |
      # Chargers relocation
      (prev_team == "SD" & team == "LAC" & prev_season == 2016 & season == 2017) |
      # Raiders relocation
      (prev_team == "OAK" & team == "LV" & prev_season == 2019 & season == 2020)
  )) %>%
  select(
    player_id,
    season_from = prev_season,
    week_from = prev_week,
    season_to = season,
    week_to = week,
    old_team = prev_team,
    new_team = team
  ) %>%
  ungroup()

# Load and process games data
games_data <- load_schedules(seasons = 2013:2023) %>%
  filter(game_type == "REG") %>%
  mutate(
    home_team = map2_chr(home_team, season, team_name_mapping),
    away_team = map2_chr(away_team, season, team_name_mapping)
  )

# Create team-level data with wins
team_games <- bind_rows(
  # Home games
  games_data %>%
    select(season, week, team = home_team, opponent = away_team, 
           team_score = home_score, opp_score = away_score) %>%
    mutate(win = case_when(
      team_score > opp_score ~ 1,
      team_score < opp_score ~ 0,
      TRUE ~ 0.5  # ties count as half win
    )),
  # Away games  
  games_data %>%
    select(season, week, team = away_team, opponent = home_team, 
           team_score = away_score, opp_score = home_score) %>%
    mutate(win = case_when(
      team_score > opp_score ~ 1,
      team_score < opp_score ~ 0,
      TRUE ~ 0.5
    ))
) %>%
  arrange(season, week, team)

# Calculate rolling wins for each team
team_rolling_wins <- team_games %>%
  group_by(team) %>%
  arrange(season, week) %>%
  mutate(
    wins_last_18 = slide_sum(win, before = 17, after = 0)
  ) %>%
  ungroup()

# Final join
player_team_changes_with_wins <- player_team_changes %>%
  left_join(
    team_rolling_wins %>% 
      select(season, week, team, wins_last_18),
    by = c(
      "season_from" = "season",
      "week_from" = "week",
      "old_team" = "team"
    )
  ) %>%
  # Optionally rename the wins column to be more descriptive
  rename(old_team_wins_last_18 = wins_last_18)



library(zoo)
# Create a complete season-week grid for each player
player_weeks <- load_rosters_weekly(seasons = 2015:2023) %>%
  select(player_id = gsis_id, season) %>%
  distinct() %>%
  # Create all possible weeks for each player-season
  crossing(
    week = 1:max(
      if_else(season < 2021, 17, 18)
    )
  ) %>%
  arrange(player_id, season, week)

# Join with roster data to get team information
player_status <- load_rosters_weekly(seasons = 2015:2023) %>%
  filter(
    (season < 2021 & week <= 17) |  
      (season >= 2021 & week <= 18)    
  ) %>%
  mutate(
    team = map2_chr(team, season, team_name_mapping)
  ) %>%
  select(player_id = gsis_id, team, season, week) %>%
  # Get latest team status for each week
  group_by(player_id, season, week) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  right_join(player_weeks, by = c("player_id", "season", "week")) %>%
  arrange(player_id, season, week) %>%
  group_by(player_id) %>%
  # Fill in team status for weeks between changes
  fill(team, .direction = "down") %>%
  # Create sequential week number starting from 2015
  mutate(
    sequential_week = case_when(
      season == 2015 ~ week,
      season == 2016 ~ week + 17,
      season == 2017 ~ week + 34,
      season == 2018 ~ week + 51,
      season == 2019 ~ week + 68,
      season == 2020 ~ week + 85,
      season == 2021 ~ week + 102,
      season == 2022 ~ week + 120,
      season == 2023 ~ week + 138
    ),
    prev_team = lag(team),
    team_switch = team != prev_team & !is.na(prev_team),
    # Find the most recent switch week for each row
    last_switch_week = lag(if_else(team_switch, sequential_week, NA_real_)),
    last_switch_week = na.locf(last_switch_week, na.rm = FALSE),
    # Calculate weeks since most recent switch
    weeks_since_switch = if_else(
      is.na(last_switch_week),
      0,  # For players with no prior switches
      sequential_week - last_switch_week
    )
  ) %>%
  ungroup()

final_player_status <- player_status %>%
  # First join for current team wins
  left_join(
    team_rolling_wins %>% 
      select(season, week, team, wins_last_18),
    by = c("season", "week", "team")
  ) %>%
  group_by(player_id) %>%
  mutate(
    prev_team = lag(team),
    team_switch = team != prev_team & !is.na(prev_team),
    temp_old_team = if_else(team_switch, prev_team, NA_character_),
    old_team = na.locf(temp_old_team, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  # Second join to get old team's wins
  left_join(
    team_rolling_wins %>% 
      select(season, week, team, wins_last_18),
    by = c("season" = "season", 
           "week" = "week",
           "old_team" = "team"),
    suffix = c("", "_old")
  ) %>%
  select(
    player_id,
    season,
    week,
    current_team = team,
    old_team,
    team_switch,
    weeks_since_switch,
    current_team_wins = wins_last_18,
    old_team_wins = wins_last_18_old
  )

# Updated add_switch_info function for individual positions
add_switch_info_individual <- function(pbp_data, switch_data) {
  result <- pbp_data
  
  # Get all individual positions from our CSV
  offense_positions <- intersect(names(POSITION_MAPPINGS), OFFENSE_POSITIONS)
  defense_positions <- intersect(names(POSITION_MAPPINGS), DEFENSE_POSITIONS)
  
  # Process offense positions
  for(pos in offense_positions) {
    max_quantity <- POSITION_QUANTITIES[[pos]]
    
    for(i in 1:max_quantity) {
      player_col <- paste0("starter_offense_", pos, "_", i)
      
      if(player_col %in% names(result)) {
        join_by <- setNames(c("player_id", "season", "week"), 
                            c(player_col, "season", "week"))
        result <- result %>%
          left_join(
            switch_data %>% 
              select(player_id, season, week, weeks_since_switch, old_team, old_team_wins),
            by = join_by,
            suffix = c("", paste0("_offense_", pos, "_", i))
          )
      }
    }
  }
  
  # Process defense positions
  for(pos in defense_positions) {
    max_quantity <- POSITION_QUANTITIES[[pos]]
    
    for(i in 1:max_quantity) {
      player_col <- paste0("starter_defense_", pos, "_", i)
      
      if(player_col %in% names(result)) {
        join_by <- setNames(c("player_id", "season", "week"), 
                            c(player_col, "season", "week"))
        result <- result %>%
          left_join(
            switch_data %>% 
              select(player_id, season, week, weeks_since_switch, old_team, old_team_wins),
            by = join_by,
            suffix = c("", paste0("_defense_", pos, "_", i))
          )
      }
    }
  }
  
  return(result)
}



# Apply to both datasets using individual positions
combined_third_downs <- add_switch_info_individual(combined_third_downs, final_player_status)
combined_fourth_downs <- add_switch_info_individual(combined_fourth_downs, final_player_status)


#kill yards_gained
combined_third_downs <- combined_third_downs %>%
  select(-yards_gained)
combined_fourth_downs <- combined_fourth_downs %>%
  select(-yards_gained)



# Function to add years since rookie for all individual position players
add_yrs_since_rookie_individual <- function(df) {
  # Get all individual positions
  offense_positions <- intersect(names(POSITION_MAPPINGS), OFFENSE_POSITIONS)
  defense_positions <- intersect(names(POSITION_MAPPINGS), DEFENSE_POSITIONS)
  
  # Process offense positions
  for(pos in offense_positions) {
    max_quantity <- POSITION_QUANTITIES[[pos]]
    
    for(i in 1:max_quantity) {
      rookie_col <- paste0("starter_offense_", pos, "_", i, "_rookie_year")
      yrs_since_col <- paste0("starter_offense_", pos, "_", i, "_yrs_since_rookie")
      
      if(rookie_col %in% names(df)) {
        df[[yrs_since_col]] <- df$season - df[[rookie_col]]
      }
    }
  }
  
  # Process defense positions
  for(pos in defense_positions) {
    max_quantity <- POSITION_QUANTITIES[[pos]]
    
    for(i in 1:max_quantity) {
      rookie_col <- paste0("starter_defense_", pos, "_", i, "_rookie_year")
      yrs_since_col <- paste0("starter_defense_", pos, "_", i, "_yrs_since_rookie")
      
      if(rookie_col %in% names(df)) {
        df[[yrs_since_col]] <- df$season - df[[rookie_col]]
      }
    }
  }
  
  return(df)
}

# Apply to both datasets
combined_third_downs <- add_yrs_since_rookie_individual(combined_third_downs)
combined_fourth_downs <- add_yrs_since_rookie_individual(combined_fourth_downs)


# Save results
write.csv(combined_third_downs, "ctd.csv", row.names = FALSE)
write.csv(combined_fourth_downs, "cfd.csv", row.names = FALSE)


combined_third_downs <- read.csv("ctd.csv")
combined_fourth_downs <- read.csv("cfd.csv")

# Filtering for only 4th down attempts that are a rush_attempt = 1 or pass_attempt = 1
attempts_4th <- combined_fourth_downs %>%
  filter(rush_attempt == 1 | pass_attempt == 1)

# Define defensive positions
defensive_positions <- c("K", "P", "LS", "DB", "CB", "S", "FS", "SS", "LB", "ILB", "OLB", "MLB", "DL", "DE", "DT", "NT", "EDGE")

# Function to check if any offensive starter has a defensive position (individual version)
check_defensive_positions_individual <- function(row) {
  positions <- c()
  
  # Get all offensive starter position columns using individual naming pattern
  offense_starter_cols <- grep("^starter_offense_.+_\\d+$", names(row), value = TRUE)
  
  # For each offensive starter column, get the corresponding position column
  for(col in offense_starter_cols) {
    pos_col <- paste0(col, "_position")
    if(pos_col %in% names(row)) {
      positions <- c(positions, row[[pos_col]])
    }
  }
  
  defensive_positions <- c("K", "P", "LS", "DB", "CB", "S", "FS", "SS", 
                           "LB", "ILB", "OLB", "MLB", "DL", "DE", "DT", "NT", "EDGE")
  
  !any(positions %in% defensive_positions, na.rm = TRUE)
}

# Apply the filter with individual position checking
clean_attempts_4th <- attempts_4th %>%
  # First filter for actual attempts
  filter(rush_attempt == 1 | pass_attempt == 1) %>%
  # Filter defensive positions based on individual position-based starters
  filter(mapply(check_defensive_positions_individual, split(., 1:nrow(.)))) %>%
  # Filter out special teams plays by description
  filter(!grepl("punt", desc, ignore.case = TRUE) & 
           !grepl("field goal", desc, ignore.case = TRUE)) 
#%>% filter(has_adequate_individual_starters(.))

# Print the number of rows removed
cat("Original rows:", nrow(attempts_4th), "\n")
cat("Rows after filtering:", nrow(clean_attempts_4th), "\n")
cat("Rows removed:", nrow(attempts_4th) - nrow(clean_attempts_4th), "\n")

# Optional: Examine some examples of removed plays
removed_plays <- attempts_4th %>%
  filter(!mapply(check_defensive_positions_individual, split(., 1:nrow(.)))) %>%
  select(play_id, game_id, desc, matches("starter_offense_.+_\\d+_position"))


write.csv(clean_attempts_4th, "cafd.csv", row.names = FALSE)

########


# Simple NA percentage check for player ID columns
check_na_simple <- function(df, dataset_name) {
  # Get all starter ID columns
  id_cols <- grep("^starter_(offense|defense)_.+_\\d+$", names(df), value = TRUE)
  id_cols <- id_cols[!grepl("_(full_name|birth_date|rookie_year|injury|position|height|weight|pff_id|jersey_number|depth_chart_position|days_since_injury|last_injury_type|injury_count_1yr|week_questionable|week_doubtful|week_out|week_limited_practice|week_dnp|current_status|current_practice|yrs_since_rookie)", id_cols)]
  
  cat("\n", dataset_name, "- Player ID NA Percentages:\n")
  for(col in sort(id_cols)) {
    na_pct <- round(mean(is.na(df[[col]])) * 100, 2)
    cat(col, ":", na_pct, "%\n")
  }
  
  # Dataset summary
  total_na_pct <- round(mean(is.na(df[id_cols])) * 100, 2)
  cat("\nOverall", dataset_name, "NA percentage:", total_na_pct, "%\n")
}

# Read and check datasets
ctd <- read.csv("ctd.csv")
cfd <- read.csv("cfd.csv") 
cafd <- read.csv("cafd.csv")

check_na_simple(ctd, "CTD")
check_na_simple(cfd, "CFD")
check_na_simple(cafd, "CAFD")

# Function to remove position groups where base ID has 50%+ NA
remove_high_na_positions <- function(df, na_threshold = 0.5) {
  # Get all base player ID columns (the actual starter columns without suffixes)
  base_id_cols <- grep("^starter_(offense|defense)_.+_\\d+$", names(df), value = TRUE)
  base_id_cols <- base_id_cols[!grepl("_(full_name|birth_date|rookie_year|injury|position|height|weight|pff_id|jersey_number|depth_chart_position|days_since_injury|last_injury_type|injury_count_1yr|week_questionable|week_doubtful|week_out|week_limited_practice|week_dnp|current_status|current_practice|yrs_since_rookie)", base_id_cols)]
  
  # Check NA rate for each base ID column
  cols_to_remove <- c()
  
  for(base_col in base_id_cols) {
    na_rate <- mean(is.na(df[[base_col]]))
    
    if(na_rate >= na_threshold) {
      # Find all related columns for this position
      # Extract the position pattern (e.g., "starter_offense_QB_1")
      position_pattern <- base_col
      
      # Find all columns that start with this pattern
      related_cols <- grep(paste0("^", gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\])", "\\\\\\1", position_pattern)), names(df), value = TRUE)
      
      cols_to_remove <- c(cols_to_remove, related_cols)
      
      cat("Removing position group:", position_pattern, "- NA rate:", round(na_rate * 100, 2), "%\n")
      cat("  Related columns removed:", length(related_cols), "\n")
    }
  }
  
  # Remove the identified columns
  if(length(cols_to_remove) > 0) {
    df_filtered <- df %>% select(-all_of(cols_to_remove))
    
    cat("\nSummary:\n")
    cat("Original columns:", ncol(df), "\n")
    cat("Columns removed:", length(cols_to_remove), "\n")
    cat("Remaining columns:", ncol(df_filtered), "\n")
    
    return(df_filtered)
  } else {
    cat("No position groups found with NA rate >= ", na_threshold * 100, "%\n")
    return(df)
  }
}


# Apply position removal right after defining the function
cat("Applying remove_high_na_positions to datasets...\n")
ctd <- remove_high_na_positions(ctd)
cfd <- remove_high_na_positions(cfd)
cafd <- remove_high_na_positions(cafd)

# Function to filter out rows with too many NA player IDs
filter_na_players <- function(df, max_na_rate_offense = 0.5, max_na_rate_defense = 0.5) {
  # Only get the actual player ID columns (not the additional info columns)
  offense_cols <- grep("^starter_offense_.+_\\d+$", names(df), value = TRUE)
  offense_cols <- offense_cols[!grepl("_(full_name|birth_date|rookie_year|injury|position|height|weight|pff_id|jersey_number|depth_chart_position|days_since_injury|last_injury_type|injury_count_1yr|week_questionable|week_doubtful|week_out|week_limited_practice|week_dnp|current_status|current_practice|yrs_since_rookie)", offense_cols)]
  
  defense_cols <- grep("^starter_defense_.+_\\d+$", names(df), value = TRUE)
  defense_cols <- defense_cols[!grepl("_(full_name|birth_date|rookie_year|injury|position|height|weight|pff_id|jersey_number|depth_chart_position|days_since_injury|last_injury_type|injury_count_1yr|week_questionable|week_doubtful|week_out|week_limited_practice|week_dnp|current_status|current_practice|yrs_since_rookie)", defense_cols)]
  
  # Only count one column per position (take _1 for each position)
  offense_cols <- offense_cols[grepl("_1$", offense_cols)]
  defense_cols <- defense_cols[grepl("_1$", defense_cols)]
  
  offense_na_rates <- rowMeans(is.na(df[offense_cols]))
  defense_na_rates <- rowMeans(is.na(df[defense_cols]))
  
  df %>%
    filter((offense_na_rates <= max_na_rate_offense) & (defense_na_rates <= max_na_rate_defense))
}

# Apply the filtering function to each dataset
ctd <- filter_na_players(ctd)
cfd <- filter_na_players(cfd)
cafd <- filter_na_players(cafd)
# Save filtered results
write.csv(ctd, "ctd.csv", row.names = FALSE)
write.csv(cfd, "cfd.csv", row.names = FALSE)
write.csv(cafd, "cafd.csv", row.names = FALSE)
