# Carolina Youth Coalition - Automated ETL Pipeline
# Integrates five disparate programmatic datasets with data validation and quality checks
# Eliminates manual processing and enables real-time analysis
# Author: Sofia Blankenship

# =============================================================================
# SETUP & CONFIGURATION
# =============================================================================

library(tidyverse)
library(lubridate)
library(glue)

# ETL Configuration
ETL_CONFIG <- list(
  data_path = "data/raw",
  output_path = "data/processed",
  logs_path = "logs",
  quality_threshold = 0.97,  # 97% accuracy benchmark
  max_records_per_batch = 50000,
  current_run_id = paste0("ETL_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)

# Create output directories
if (!dir.exists(ETL_CONFIG$output_path)) dir.create(ETL_CONFIG$output_path, recursive = TRUE)
if (!dir.exists(ETL_CONFIG$logs_path)) dir.create(ETL_CONFIG$logs_path, recursive = TRUE)

# =============================================================================
# LOGGING & ERROR HANDLING FRAMEWORK
# =============================================================================

log_message <- function(level, message, run_id = ETL_CONFIG$current_run_id) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- glue("[{timestamp}] [{level}] [{run_id}] {message}")
  
  # Write to log file
  log_file <- file.path(ETL_CONFIG$logs_path, paste0("etl_log_", Sys.Date(), ".log"))
  write(log_entry, file = log_file, append = TRUE)
  
  # Also print to console
  cat(log_entry, "\n")
}

log_info <- function(message) log_message("INFO", message)
log_warning <- function(message) log_message("WARNING", message)
log_error <- function(message) log_message("ERROR", message)

# Error handling wrapper
safe_execute <- function(expr, operation_name) {
  tryCatch({
    result <- expr
    log_info(glue("{operation_name} completed successfully"))
    return(result)
  }, error = function(e) {
    log_error(glue("{operation_name} failed: {e$message}"))
    stop(e)
  })
}

# =============================================================================
# DATA VALIDATION & QUALITY CHECKS
# =============================================================================

validate_dataset <- function(data, dataset_name, required_columns = NULL) {
  log_info(glue("Validating {dataset_name}..."))
  
  validation_results <- list(
    dataset = dataset_name,
    total_records = nrow(data),
    total_columns = ncol(data),
    missing_data_pct = round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 2),
    duplicate_records = sum(duplicated(data)),
    quality_score = 0
  )
  
  # Check required columns
  if (!is.null(required_columns)) {
    missing_cols <- setdiff(required_columns, colnames(data))
    if (length(missing_cols) > 0) {
      log_warning(glue("Missing required columns in {dataset_name}: {paste(missing_cols, collapse = ', ')}"))
      validation_results$missing_columns <- missing_cols
    }
  }
  
  # Data quality scoring
  quality_deductions <- 0
  if (validation_results$missing_data_pct > 5) quality_deductions <- quality_deductions + 0.1
  if (validation_results$duplicate_records > 0) quality_deductions <- quality_deductions + 0.05
  if (validation_results$total_records == 0) quality_deductions <- quality_deductions + 1
  
  validation_results$quality_score <- max(0, 1 - quality_deductions)
  
  # Log validation results
  log_info(glue("{dataset_name}: {validation_results$total_records} records, {validation_results$missing_data_pct}% missing, Quality: {round(validation_results$quality_score * 100, 1)}%"))
  
  if (validation_results$quality_score < ETL_CONFIG$quality_threshold) {
    log_warning(glue("{dataset_name} quality score ({round(validation_results$quality_score * 100, 1)}%) below threshold ({ETL_CONFIG$quality_threshold * 100}%)"))
  }
  
  return(validation_results)
}

# Anomaly detection for numeric columns
detect_numeric_anomalies <- function(data, column_name, z_threshold = 3) {
  if (!column_name %in% names(data) || !is.numeric(data[[column_name]])) return(NULL)
  
  values <- data[[column_name]][!is.na(data[[column_name]])]
  if (length(values) < 10) return(NULL)  # Need sufficient data
  
  z_scores <- abs(scale(values))
  anomalies <- which(z_scores > z_threshold)
  
  if (length(anomalies) > 0) {
    log_warning(glue("{length(anomalies)} anomalies detected in {column_name} (Z > {z_threshold})"))
    
    # Return anomalies with participant_id if it exists
    if ("participant_id" %in% names(data)) {
      return(data[anomalies, c("participant_id", column_name)])
    } else {
      return(data[anomalies, column_name, drop = FALSE])
    }
  }
  
  return(NULL)
}

# =============================================================================
# DATA EXTRACTION FUNCTIONS
# =============================================================================

extract_enrollment_data <- function() {
  log_info("Extracting enrollment data...")
  
  # Load master enrollment file
  enrollment <- safe_execute(
    read_csv(file.path(ETL_CONFIG$data_path, "master_enrollment_2019_2023.csv"), 
             show_col_types = FALSE),
    "Enrollment data extraction"
  )
  
  # Validate
  validation <- validate_dataset(enrollment, "Enrollment", 
                                 c("participant_id", "cohort_year", "enrollment_date"))
  
  return(list(data = enrollment, validation = validation))
}

extract_survey_data <- function() {
  log_info("Extracting survey data...")
  
  # Load pre and post survey data
  pre_surveys <- safe_execute(
    read_csv(file.path(ETL_CONFIG$data_path, "master_pre_surveys_2019_2023.csv"), 
             show_col_types = FALSE),
    "Pre-survey data extraction"
  )
  
  post_surveys <- safe_execute(
    read_csv(file.path(ETL_CONFIG$data_path, "master_post_surveys_2019_2023.csv"), 
             show_col_types = FALSE),
    "Post-survey data extraction"
  )
  
  # Validate both datasets
  pre_validation <- validate_dataset(pre_surveys, "Pre-Surveys", 
                                     c("participant_id", "act_composite_pre", "sat_total_pre"))
  post_validation <- validate_dataset(post_surveys, "Post-Surveys", 
                                      c("participant_id", "act_composite_post", "sat_total_post"))
  
  return(list(
    pre_data = pre_surveys, 
    post_data = post_surveys,
    pre_validation = pre_validation,
    post_validation = post_validation
  ))
}

extract_participation_data <- function() {
  log_info("Extracting participation tracking data...")
  
  participation <- safe_execute(
    read_csv(file.path(ETL_CONFIG$data_path, "master_participation_2019_2023.csv"), 
             show_col_types = FALSE),
    "Participation data extraction"
  )
  
  # Validate
  validation <- validate_dataset(participation, "Participation", 
                                 c("participant_id", "session_date", "attended"))
  
  return(list(data = participation, validation = validation))
}

extract_funding_data <- function() {
  log_info("Extracting funding data...")
  
  # Load grant and individual funding data
  grants <- safe_execute(
    read_csv(file.path(ETL_CONFIG$data_path, "grant_funding_2019_2023.csv"), 
             show_col_types = FALSE),
    "Grant funding data extraction"
  )
  
  individual_funding <- safe_execute(
    read_csv(file.path(ETL_CONFIG$data_path, "participant_funding_2019_2023.csv"), 
             show_col_types = FALSE),
    "Individual funding data extraction"
  )
  
  # Validate both datasets
  grant_validation <- validate_dataset(grants, "Grant Funding", 
                                       c("grant_id", "funding_source", "grant_amount"))
  funding_validation <- validate_dataset(individual_funding, "Individual Funding", 
                                         c("participant_id", "program_cost_per_participant"))
  
  return(list(
    grants = grants,
    individual = individual_funding,
    grant_validation = grant_validation,
    funding_validation = funding_validation
  ))
}

# =============================================================================
# DATA TRANSFORMATION FUNCTIONS
# =============================================================================

transform_participant_master <- function(enrollment, pre_surveys, post_surveys) {
  log_info("Creating integrated participant master dataset...")
  
  # Start with enrollment as base
  participant_master <- enrollment %>%
    left_join(pre_surveys, by = "participant_id", suffix = c("", "_pre")) %>%
    left_join(post_surveys, by = "participant_id", suffix = c("", "_post"))
  
  # Get actual column names to avoid referencing non-existent columns
  available_cols <- names(participant_master)
  
  # Data normalization and standardization
  participant_master <- participant_master %>%
    mutate(
      # Standardize date formats
      enrollment_date = as.Date(enrollment_date)
    )
  
  # Add conditional transformations based on available columns
  if ("survey_date" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(survey_date = as.Date(survey_date))
  }
  
  if ("survey_date_post" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(survey_date_post = as.Date(survey_date_post))
  }
  
  # Calculate program duration if both dates exist
  if (all(c("survey_date", "survey_date_post") %in% available_cols)) {
    participant_master <- participant_master %>%
      mutate(program_duration_days = as.numeric(survey_date_post - survey_date))
  }
  
  # Standardize demographic categories if race_ethnicity exists
  if ("race_ethnicity" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(
        race_ethnicity_std = case_when(
          race_ethnicity %in% c("Black or African American") ~ "Black/African American",
          race_ethnicity %in% c("Hispanic/Latino") ~ "Hispanic/Latino",
          race_ethnicity %in% c("White") ~ "White",
          race_ethnicity %in% c("Asian") ~ "Asian",
          TRUE ~ "Other/Multiracial"
        )
      )
  }
  
  # Income level standardization if family_income_range exists
  if ("family_income_range" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(
        income_level = case_when(
          family_income_range %in% c("Under $25,000", "$25,000-$39,999") ~ "Low Income",
          family_income_range %in% c("$40,000-$59,999", "$60,000-$79,999") ~ "Middle Income",
          family_income_range %in% c("$80,000-$99,999", "$100,000+") ~ "Higher Income",
          TRUE ~ "Unknown"
        )
      )
  }
  
  # Program completion status
  if ("act_composite_post" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(
        completion_status = case_when(
          !is.na(act_composite_post) ~ "Completed",
          TRUE ~ "Did Not Complete"
        )
      )
  } else {
    participant_master <- participant_master %>%
      mutate(completion_status = "Unknown")
  }
  
  # Academic achievement levels
  if (all(c("act_composite_pre", "sat_total_pre") %in% available_cols)) {
    participant_master <- participant_master %>%
      mutate(
        academic_level_pre = case_when(
          act_composite_pre >= 25 | sat_total_pre >= 1200 ~ "High Achieving",
          act_composite_pre >= 20 | sat_total_pre >= 1000 ~ "Proficient",
          TRUE ~ "Developing"
        )
      )
  }
  
  # Calculate key performance indicators if improvement columns exist
  if ("act_improvement" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(act_met_improvement_goal = ifelse(!is.na(act_improvement), act_improvement >= 2, FALSE))
  }
  
  if ("sat_improvement" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(sat_met_improvement_goal = ifelse(!is.na(sat_improvement), sat_improvement >= 70, FALSE))
  }
  
  if ("gpa_improvement" %in% available_cols) {
    participant_master <- participant_master %>%
      mutate(gpa_met_improvement_goal = ifelse(!is.na(gpa_improvement), gpa_improvement >= 0.25, FALSE))
  }
  
  # Clean up redundant columns (only if they exist)
  cols_to_remove <- intersect(names(participant_master), c("survey_date_post"))
  if (length(cols_to_remove) > 0) {
    participant_master <- participant_master %>%
      select(-all_of(cols_to_remove))
  }
  
  # Reorder columns logically (keep only existing columns)
  priority_cols <- c("participant_id", "cohort_year", "enrollment_date", "survey_date", "completion_status")
  priority_cols <- intersect(priority_cols, names(participant_master))
  other_cols <- setdiff(names(participant_master), priority_cols)
  
  participant_master <- participant_master %>%
    select(all_of(priority_cols), all_of(other_cols))
  
  log_info(glue("Participant master created: {nrow(participant_master)} records"))
  return(participant_master)
}

transform_participation_summary <- function(participation_data) {
  log_info("Creating participation summary metrics...")
  
  # Check which columns are available
  available_cols <- names(participation_data)
  
  participation_summary <- participation_data %>%
    group_by(participant_id, cohort_year) %>%
    summarise(
      total_sessions_offered = n(),
      .groups = "drop"
    )
  
  # Add attendance calculations if attended column exists
  if ("attended" %in% available_cols) {
    participation_summary <- participation_data %>%
      group_by(participant_id, cohort_year) %>%
      summarise(
        total_sessions_offered = n(),
        sessions_attended = sum(attended == "Present", na.rm = TRUE),
        sessions_excused = sum(attended == "Excused", na.rm = TRUE),
        sessions_absent = sum(attended == "Absent", na.rm = TRUE),
        attendance_rate = round(sessions_attended / total_sessions_offered * 100, 1),
        .groups = "drop"
      )
  }
  
  # Add hours if available
  if ("hours_participated" %in% available_cols) {
    participation_summary <- participation_summary %>%
      left_join(
        participation_data %>%
          group_by(participant_id, cohort_year) %>%
          summarise(
            total_hours = sum(hours_participated, na.rm = TRUE),
            avg_session_length = round(mean(hours_participated[hours_participated > 0], na.rm = TRUE), 1),
            .groups = "drop"
          ),
        by = c("participant_id", "cohort_year")
      )
  }
  
  # Add session type analysis if available
  if ("session_type" %in% available_cols && "attended" %in% available_cols) {
    session_type_summary <- participation_data %>%
      group_by(participant_id, cohort_year) %>%
      summarise(
        test_prep_sessions = sum(session_type == "Test Prep" & attended == "Present", na.rm = TRUE),
        college_planning_sessions = sum(session_type == "College Planning" & attended == "Present", na.rm = TRUE),
        tutoring_sessions = sum(session_type == "Tutoring" & attended == "Present", na.rm = TRUE),
        .groups = "drop"
      )
    
    participation_summary <- participation_summary %>%
      left_join(session_type_summary, by = c("participant_id", "cohort_year"))
  }
  
  # Add date range if session_date exists
  if ("session_date" %in% available_cols) {
    date_summary <- participation_data %>%
      group_by(participant_id, cohort_year) %>%
      summarise(
        program_start_date = min(as.Date(session_date), na.rm = TRUE),
        program_end_date = max(as.Date(session_date), na.rm = TRUE),
        .groups = "drop"
      )
    
    participation_summary <- participation_summary %>%
      left_join(date_summary, by = c("participant_id", "cohort_year"))
  }
  
  # Add engagement metrics if we have the necessary columns
  if (all(c("attendance_rate", "total_hours") %in% names(participation_summary))) {
    participation_summary <- participation_summary %>%
      mutate(
        consistent_attendance = attendance_rate >= 75,
        high_engagement = total_hours >= 60,
        engagement_level = case_when(
          attendance_rate >= 85 & total_hours >= 75 ~ "High Engagement",
          attendance_rate >= 70 & total_hours >= 50 ~ "Moderate Engagement",
          attendance_rate >= 50 ~ "Low Engagement",
          TRUE ~ "Minimal Engagement"
        )
      )
  } else if ("attendance_rate" %in% names(participation_summary)) {
    participation_summary <- participation_summary %>%
      mutate(
        consistent_attendance = attendance_rate >= 75,
        engagement_level = case_when(
          attendance_rate >= 85 ~ "High Engagement",
          attendance_rate >= 70 ~ "Moderate Engagement",
          attendance_rate >= 50 ~ "Low Engagement",
          TRUE ~ "Minimal Engagement"
        )
      )
  }
  
  log_info(glue("Participation summary created: {nrow(participation_summary)} participants"))
  return(participation_summary)
}

transform_financial_summary <- function(grants, individual_funding) {
  log_info("Creating financial summary and cost analysis...")
  
  # Grant summary by year and focus area (check available columns)
  grant_cols <- names(grants)
  
  if (all(c("award_year", "focus_area", "grant_amount", "participants_served") %in% grant_cols)) {
    grant_summary <- grants %>%
      group_by(award_year, focus_area) %>%
      summarise(
        total_grants = n(),
        total_funding = sum(grant_amount, na.rm = TRUE),
        avg_grant_size = round(mean(grant_amount, na.rm = TRUE)),
        participants_reached = sum(participants_served, na.rm = TRUE),
        cost_per_participant = round(total_funding / pmax(participants_reached, 1)),
        .groups = "drop"
      )
  } else {
    # Basic summary if columns are missing
    grant_summary <- grants %>%
      summarise(
        total_grants = n(),
        .groups = "drop"
      )
  }
  
  # Individual cost analysis
  funding_cols <- names(individual_funding)
  
  if ("cohort_year" %in% funding_cols && "program_cost_per_participant" %in% funding_cols) {
    cost_analysis <- individual_funding %>%
      group_by(cohort_year) %>%
      summarise(
        total_participants = n(),
        avg_program_cost = round(mean(program_cost_per_participant, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Add additional columns if they exist
    if ("transportation_stipend" %in% funding_cols) {
      cost_analysis <- cost_analysis %>%
        left_join(
          individual_funding %>%
            group_by(cohort_year) %>%
            summarise(
              total_transportation_stipends = sum(transportation_stipend, na.rm = TRUE),
              .groups = "drop"
            ),
          by = "cohort_year"
        )
    }
    
    if ("laptop_loaned" %in% funding_cols) {
      cost_analysis <- cost_analysis %>%
        left_join(
          individual_funding %>%
            group_by(cohort_year) %>%
            summarise(
              laptops_provided = sum(laptop_loaned == "Yes", na.rm = TRUE),
              .groups = "drop"
            ),
          by = "cohort_year"
        )
    }
    
    # Calculate average cost per participant if transportation_stipend exists
    if ("transportation_stipend" %in% funding_cols) {
      cost_analysis <- cost_analysis %>%
        left_join(
          individual_funding %>%
            group_by(cohort_year) %>%
            summarise(
              avg_cost_per_participant = round(mean(program_cost_per_participant + 
                                                   ifelse(is.na(transportation_stipend), 0, transportation_stipend), na.rm = TRUE)),
              .groups = "drop"
            ),
          by = "cohort_year"
        )
    }
  } else {
    # Basic cost analysis if columns are missing
    cost_analysis <- individual_funding %>%
      summarise(
        total_participants = n(),
        .groups = "drop"
      )
  }
  
  log_info("Financial summaries created")
  return(list(grant_summary = grant_summary, cost_analysis = cost_analysis))
}

# =============================================================================
# DATA QUALITY & PROFILING FUNCTIONS
# =============================================================================

run_data_profiling <- function(dataset, dataset_name) {
  log_info(glue("Running data profiling on {dataset_name}..."))
  
  profile_results <- list(
    dataset_name = dataset_name,
    total_records = nrow(dataset),
    total_columns = ncol(dataset),
    column_profiles = map_dfr(names(dataset), function(col_name) {
      col_data <- dataset[[col_name]]
      
      tibble(
        column_name = col_name,
        data_type = class(col_data)[1],
        missing_count = sum(is.na(col_data)),
        missing_pct = round(sum(is.na(col_data)) / length(col_data) * 100, 2),
        unique_values = length(unique(col_data[!is.na(col_data)])),
        completeness = round((1 - sum(is.na(col_data)) / length(col_data)) * 100, 1)
      )
    })
  )
  
  # Flag columns with high missing rates
  high_missing <- profile_results$column_profiles %>%
    filter(missing_pct > 10) %>%
    pull(column_name)
  
  if (length(high_missing) > 0) {
    log_warning(glue("High missing data in {dataset_name}: {paste(high_missing, collapse = ', ')}"))
  }
  
  return(profile_results)
}

run_anomaly_detection <- function(participant_master) {
  log_info("Running anomaly detection on integrated dataset...")
  
  anomalies_detected <- list()
  
  # Check numeric columns for outliers - only if they exist
  potential_numeric_cols <- c("act_composite_pre", "act_composite_post", "sat_total_pre", "sat_total_post", 
                             "gpa_pre", "gpa_post", "study_hours_per_week", "household_size")
  
  available_numeric_cols <- intersect(potential_numeric_cols, names(participant_master))
  
  for (col in available_numeric_cols) {
    anomalies <- detect_numeric_anomalies(participant_master, col)
    if (!is.null(anomalies)) {
      anomalies_detected[[col]] <- anomalies
    }
  }
  
  # Check for logical inconsistencies - only with available columns
  logical_conditions <- list()
  
  if (all(c("act_composite_post", "act_composite_pre") %in% names(participant_master))) {
    logical_conditions <- c(logical_conditions, 
                           expression(act_composite_post < act_composite_pre - 10))
  }
  
  if (all(c("gpa_post", "gpa_pre") %in% names(participant_master))) {
    logical_conditions <- c(logical_conditions,
                           expression(gpa_post < gpa_pre - 1.0))
  }
  
  if ("study_hours_per_week" %in% names(participant_master)) {
    logical_conditions <- c(logical_conditions,
                           expression(study_hours_per_week > 60))
  }
  
  # Apply logical checks if any conditions exist
  if (length(logical_conditions) > 0) {
    # Build the filter expression
    filter_expr <- logical_conditions[[1]]
    if (length(logical_conditions) > 1) {
      for (i in 2:length(logical_conditions)) {
        filter_expr <- call("|", filter_expr, logical_conditions[[i]])
      }
    }
    
    logical_issues <- participant_master %>%
      filter(!!filter_expr)
    
    if (nrow(logical_issues) > 0) {
      log_warning(glue("{nrow(logical_issues)} records with logical inconsistencies detected"))
      anomalies_detected[["logical_issues"]] <- logical_issues
    }
  }
  
  return(anomalies_detected)
}

# =============================================================================
# MAIN ETL PIPELINE
# =============================================================================

run_etl_pipeline <- function() {
  log_info("Starting ETL Pipeline Execution")
  log_info(glue("Run ID: {ETL_CONFIG$current_run_id}"))
  
  pipeline_start_time <- Sys.time()
  
  # ===================
  # EXTRACT PHASE
  # ===================
  log_info("=== EXTRACT PHASE ===")
  
  enrollment_extract <- extract_enrollment_data()
  survey_extract <- extract_survey_data()
  participation_extract <- extract_participation_data()
  funding_extract <- extract_funding_data()
  
  # ===================
  # TRANSFORM PHASE
  # ===================
  log_info("=== TRANSFORM PHASE ===")
  
  # Create integrated participant master
  participant_master <- safe_execute(
    transform_participant_master(
      enrollment_extract$data, 
      survey_extract$pre_data, 
      survey_extract$post_data
    ),
    "Participant master transformation"
  )
  
  # Create participation summary
  participation_summary <- safe_execute(
    transform_participation_summary(participation_extract$data),
    "Participation summary transformation"
  )
  
  # Create financial summaries
  financial_summaries <- safe_execute(
    transform_financial_summary(funding_extract$grants, funding_extract$individual),
    "Financial summary transformation"
  )
  
  # ===================
  # QUALITY ASSURANCE
  # ===================
  log_info("=== QUALITY ASSURANCE PHASE ===")
  
  # Run data profiling
  master_profile <- run_data_profiling(participant_master, "Participant Master")
  participation_profile <- run_data_profiling(participation_summary, "Participation Summary")
  
  # Run anomaly detection
  anomalies <- run_anomaly_detection(participant_master)
  
  # ===================
  # LOAD PHASE
  # ===================
  log_info("=== LOAD PHASE ===")
  
  # Save transformed datasets
  safe_execute({
    write_csv(participant_master, file.path(ETL_CONFIG$output_path, "participant_master_integrated.csv"))
  }, "Saving participant master dataset")
  
  safe_execute({
    write_csv(participation_summary, file.path(ETL_CONFIG$output_path, "participation_summary.csv"))
  }, "Saving participation summary")
  
  safe_execute({
    write_csv(financial_summaries$grant_summary, file.path(ETL_CONFIG$output_path, "grant_summary_by_year.csv"))
  }, "Saving grant summary")
  
  safe_execute({
    write_csv(financial_summaries$cost_analysis, file.path(ETL_CONFIG$output_path, "cost_analysis_by_year.csv"))
  }, "Saving cost analysis")
  
  # Save data quality reports
  safe_execute({
    write_csv(master_profile$column_profiles, file.path(ETL_CONFIG$output_path, "data_quality_profile.csv"))
  }, "Saving data quality profile")
  
  # ===================
  # PIPELINE SUMMARY
  # ===================
  pipeline_end_time <- Sys.time()
  execution_time <- round(as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins")), 2)
  
  # Calculate overall quality score
  all_validations <- list(
    enrollment_extract$validation, survey_extract$pre_validation, survey_extract$post_validation,
    participation_extract$validation, funding_extract$grant_validation, funding_extract$funding_validation
  )
  
  overall_quality <- mean(map_dbl(all_validations, ~ .x$quality_score))
  
  pipeline_summary <- tibble(
    run_id = ETL_CONFIG$current_run_id,
    execution_date = as.character(Sys.Date()),
    execution_time_minutes = execution_time,
    total_participants_processed = nrow(participant_master),
    datasets_integrated = 5,
    overall_quality_score = round(overall_quality * 100, 1),
    quality_meets_benchmark = overall_quality >= ETL_CONFIG$quality_threshold,
    anomalies_detected = length(anomalies),
    files_created = 6
  )
  
  # Save pipeline summary
  write_csv(pipeline_summary, file.path(ETL_CONFIG$output_path, "etl_pipeline_summary.csv"))
  
  # Final logging
  log_info("=== ETL PIPELINE COMPLETED ===")
  log_info(glue("Execution time: {execution_time} minutes"))
  log_info(glue("Participants processed: {nrow(participant_master)}"))
  log_info(glue("Quality score: {round(overall_quality * 100, 1)}% (Benchmark: {ETL_CONFIG$quality_threshold * 100}%)"))
  log_info(glue("Output files saved to: {ETL_CONFIG$output_path}"))
  
  if (overall_quality >= ETL_CONFIG$quality_threshold) {
    log_info("Quality benchmark achieved!")
  } else {
    log_warning("Quality benchmark not met - review data quality issues")
  }
  
  return(list(
    participant_master = participant_master,
    participation_summary = participation_summary,
    financial_summaries = financial_summaries,
    pipeline_summary = pipeline_summary,
    quality_score = overall_quality
  ))
}

# =============================================================================
# EXECUTE ETL PIPELINE
# =============================================================================

cat("CAROLINA YOUTH COALITION - AUTOMATED ETL PIPELINE\n")
cat("=====================================================\n\n")

# Run the complete ETL pipeline
etl_results <- run_etl_pipeline()

cat("\nPIPELINE RESULTS SUMMARY:\n")
cat("============================\n")
cat("Datasets integrated: 5\n")
cat(glue("Total participants: {nrow(etl_results$participant_master)}"), "\n")
cat(glue("Quality score: {round(etl_results$quality_score * 100, 1)}%"), "\n")
cat(glue("Files created in: {ETL_CONFIG$output_path}"), "\n")
cat(glue("Logs saved to: {ETL_CONFIG$logs_path}"), "\n")

cat("\nReady for KPI dashboard development and real-time analysis\n")
