# Carolina Youth Coalition - Synthetic Survey Data Generator
# Creates realistic pre/post academic survey data for portfolio demonstration
# Mirrors 5-year longitudinal analysis of ACT/SAT outcomes and demographics
# Author: Sofia Blankenship

# =============================================================================
# PACKAGE INSTALLATION & LOADING
# =============================================================================

#Install required packages
required_packages <- c("tidyverse", "lubridate")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages)) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

#Load required libraries
library(tidyverse)
library(lubridate)

#Set seed for reproducible synthetic data
set.seed(42)

# =============================================================================
# CONFIGURATION & PARAMETERS
# =============================================================================

#Charlotte Metro High Schools 
CHARLOTTE_SCHOOLS <- c(
  "Myers Park High School", "Charlotte Catholic High School", "Providence High School",
  "Independence High School", "West Charlotte High School", "Olympic High School",
  "Mallard Creek High School", "North Mecklenburg High School", "South Mecklenburg High School",
  "East Mecklenburg High School", "Garinger High School", "Harding University High School",
  "Hopewell High School", "Julius Chambers High School", "Vance High School",
  "Ardrey Kell High School", "Butler High School", "Rocky River High School"
)

#Years of data collection
DATA_YEARS <- 2019:2023
N_PARTICIPANTS_PER_YEAR <- 180:220  # Varying enrollment each year

# =============================================================================
# HELPER FUNCTIONS FOR REALISTIC DATA
# =============================================================================

generate_realistic_act_scores <- function(n, pre_post = "pre", baseline_score = NULL) {
  if(pre_post == "pre") {
    #Pre-scores: typically lower, more variation
    pmax(10, pmin(36, round(rnorm(n, mean = 18.5, sd = 4.2))))
  } else {
    #Post-scores: improvement but realistic gains
    improvement <- rnorm(n, mean = 2.8, sd = 1.5)  #Average 2.8 point improvement
    pmax(10, pmin(36, round(baseline_score + improvement)))
  }
}

generate_realistic_sat_scores <- function(n, pre_post = "pre", baseline_score = NULL) {
  if(pre_post == "pre") {
    #Pre-scores: SAT scale 400-1600
    pmax(400, pmin(1600, round(rnorm(n, mean = 980, sd = 180), -1)))  #Round to nearest 10
  } else {
    #Post-scores: realistic improvement
    improvement <- rnorm(n, mean = 85, sd = 45)  #Average 85 point improvement
    pmax(400, pmin(1600, round(baseline_score + improvement, -1)))
  }
}

# =============================================================================
# DATASET 1: PARTICIPANT DEMOGRAPHICS & ENROLLMENT (Multi-year CSV files)
# =============================================================================

create_annual_enrollment_data <- function(year) {
  n_participants <- sample(N_PARTICIPANTS_PER_YEAR, 1)
  
  enrollment_data <- tibble(
    participant_id = paste0("CYC", year, str_pad(1:n_participants, 3, pad = "0")),
    cohort_year = year,
    enrollment_date = sample(seq(as.Date(paste0(year, "-08-15")), 
                                 as.Date(paste0(year, "-10-15")), by = "day"), n_participants, replace = TRUE),
    
    #Demographics
    age_at_enrollment = sample(14:18, n_participants, replace = TRUE, prob = c(0.05, 0.20, 0.25, 0.30, 0.20)),
    grade_level = case_when(
      age_at_enrollment == 14 ~ "9th",
      age_at_enrollment == 15 ~ ifelse(runif(n_participants) < 0.3, "9th", "10th"),
      age_at_enrollment == 16 ~ ifelse(runif(n_participants) < 0.2, "10th", "11th"),
      age_at_enrollment == 17 ~ ifelse(runif(n_participants) < 0.3, "11th", "12th"),
      age_at_enrollment == 18 ~ "12th",
      TRUE ~ "Unknown"
    ),
    
    race_ethnicity = sample(c("Black or African American", "Hispanic/Latino", "White", 
                              "Asian", "Multiracial", "Native American", "Other"), 
                            n_participants, replace = TRUE, 
                            prob = c(0.38, 0.25, 0.20, 0.08, 0.06, 0.02, 0.01)),
    
    gender = sample(c("Female", "Male", "Non-binary"), n_participants, replace = TRUE, 
                    prob = c(0.54, 0.44, 0.02)),
    
    high_school = sample(CHARLOTTE_SCHOOLS, n_participants, replace = TRUE),
    
    title_1_school = sample(c("Yes", "No"), n_participants, replace = TRUE, prob = c(0.65, 0.35)),
    
    #Socioeconomic factors
    family_income_range = sample(c("Under $25,000", "$25,000-$39,999", "$40,000-$59,999", 
                                   "$60,000-$79,999", "$80,000-$99,999", "$100,000+"), 
                                 n_participants, replace = TRUE, 
                                 prob = c(0.35, 0.25, 0.20, 0.12, 0.05, 0.03)),
    
    household_size = sample(1:8, n_participants, replace = TRUE, prob = c(0.08, 0.15, 0.25, 0.25, 0.15, 0.08, 0.03, 0.01)),
    
    first_generation_college = sample(c("Yes", "No", "Unknown"), n_participants, replace = TRUE, 
                                      prob = c(0.72, 0.25, 0.03)),
    
    #Program participation
    program_track = sample(c("College Prep", "Career Readiness", "Dual Track", "Intensive Support"), 
                           n_participants, replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10)),
    
    transportation_barrier = sample(c("Yes", "No"), n_participants, replace = TRUE, prob = c(0.35, 0.65)),
    childcare_needs = sample(c("Yes", "No", "N/A"), n_participants, replace = TRUE, prob = c(0.15, 0.70, 0.15))
  )
  
  return(enrollment_data)
}

# =============================================================================
# DATASET 2: PRE-SURVEY DATA (Academic baseline)
# =============================================================================

create_pre_survey_data <- function(enrollment_data) {
  pre_survey <- enrollment_data %>%
    mutate(
      survey_date = enrollment_date + sample(0:14, nrow(enrollment_data), replace = TRUE),
      
      #Academic scores (pre-intervention)
      act_composite_pre = generate_realistic_act_scores(nrow(enrollment_data), "pre"),
      act_english_pre = pmax(1, pmin(36, act_composite_pre + sample(-3:3, nrow(enrollment_data), replace = TRUE))),
      act_math_pre = pmax(1, pmin(36, act_composite_pre + sample(-4:2, nrow(enrollment_data), replace = TRUE))),
      act_reading_pre = pmax(1, pmin(36, act_composite_pre + sample(-2:4, nrow(enrollment_data), replace = TRUE))),
      act_science_pre = pmax(1, pmin(36, act_composite_pre + sample(-3:3, nrow(enrollment_data), replace = TRUE))),
      
      sat_total_pre = generate_realistic_sat_scores(nrow(enrollment_data), "pre"),
      sat_math_pre = round(sat_total_pre * runif(nrow(enrollment_data), 0.45, 0.55), -1),
      sat_verbal_pre = sat_total_pre - sat_math_pre,
      
      #Academic indicators
      gpa_pre = pmax(1.0, pmin(4.0, round(rnorm(nrow(enrollment_data), mean = 2.4, sd = 0.8), 2))),
      
      #Self-reported measures
      college_readiness_self_rating = sample(1:10, nrow(enrollment_data), replace = TRUE, prob = rep(0.1, 10)),
      study_hours_per_week = pmax(0, round(rnorm(nrow(enrollment_data), mean = 8, sd = 5))),
      career_confidence_rating = sample(1:10, nrow(enrollment_data), replace = TRUE, prob = c(0.15, 0.15, 0.15, 0.15, 0.10, 0.10, 0.08, 0.05, 0.04, 0.03)),
      
      #Barriers and challenges
      academic_support_at_home = sample(c("None", "Limited", "Moderate", "Strong"), 
                                        nrow(enrollment_data), replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
      technology_access = sample(c("No reliable access", "Limited access", "Good access"), 
                                 nrow(enrollment_data), replace = TRUE, prob = c(0.20, 0.45, 0.35)),
      
      #Goals and aspirations
      post_secondary_plan = sample(c("4-year college", "Community college", "Trade school", 
                                     "Military", "Work", "Undecided"), 
                                   nrow(enrollment_data), replace = TRUE, 
                                   prob = c(0.35, 0.25, 0.15, 0.08, 0.12, 0.05))
    ) %>%
    select(participant_id, cohort_year, survey_date, everything(), -enrollment_date)
  
  return(pre_survey)
}

# =============================================================================
# DATASET 3: POST-SURVEY DATA (Academic outcomes)
# =============================================================================

create_post_survey_data <- function(enrollment_data, pre_survey_data) {
  #Only create post-surveys for participants who would have completed (not all)
  n_completed <- max(1, round(nrow(enrollment_data) * 0.73))  #Ensure at least 1
  n_completed <- min(n_completed, nrow(enrollment_data))      #Don't exceed total
  completed_participants <- sample(enrollment_data$participant_id, n_completed)
  
  post_survey <- pre_survey_data %>%
    filter(participant_id %in% completed_participants) %>%
    mutate(
      survey_date = enrollment_data$enrollment_date[match(participant_id, enrollment_data$participant_id)] + 
        sample(300:400, length(completed_participants), replace = TRUE),  # ~1 year later
      
      #Academic improvements (realistic gains)
      act_composite_post = generate_realistic_act_scores(length(completed_participants), "post", act_composite_pre),
      act_english_post = pmax(1, pmin(36, act_composite_post + sample(-2:2, length(completed_participants), replace = TRUE))),
      act_math_post = pmax(1, pmin(36, act_composite_post + sample(-3:2, length(completed_participants), replace = TRUE))),
      act_reading_post = pmax(1, pmin(36, act_composite_post + sample(-2:3, length(completed_participants), replace = TRUE))),
      act_science_post = pmax(1, pmin(36, act_composite_post + sample(-2:2, length(completed_participants), replace = TRUE))),
      
      sat_total_post = generate_realistic_sat_scores(length(completed_participants), "post", sat_total_pre),
      sat_math_post = round(sat_total_post * runif(length(completed_participants), 0.45, 0.55), -1),
      sat_verbal_post = sat_total_post - sat_math_post,
      
      gpa_post = pmax(1.0, pmin(4.0, gpa_pre + rnorm(length(completed_participants), mean = 0.35, sd = 0.25))),
      
      #Improved self-measures
      college_readiness_self_rating_post = pmax(1, pmin(10, college_readiness_self_rating + 
                                                          sample(-1:4, length(completed_participants), replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.2, 0.1)))),
      study_hours_per_week_post = pmax(0, study_hours_per_week + rnorm(length(completed_participants), mean = 3, sd = 2)),
      career_confidence_rating_post = pmax(1, pmin(10, career_confidence_rating + 
                                                     sample(0:4, length(completed_participants), replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.10)))),
      
      #Updated status
      post_secondary_enrollment = sample(c("Enrolled 4-year", "Enrolled 2-year", "Trade program", 
                                           "Gap year", "Working", "Military", "Other"), 
                                         length(completed_participants), replace = TRUE,
                                         prob = c(0.42, 0.28, 0.12, 0.08, 0.06, 0.03, 0.01)),
      
      scholarship_received = sample(c("Yes", "No"), length(completed_participants), replace = TRUE, prob = c(0.35, 0.65)),
      scholarship_amount = ifelse(scholarship_received == "Yes", 
                                  round(runif(length(completed_participants), 1000, 15000), -2), NA),
      
      program_satisfaction = sample(1:5, length(completed_participants), replace = TRUE, 
                                    prob = c(0.02, 0.05, 0.15, 0.35, 0.43)),  # Most satisfied
      
      would_recommend = sample(c("Definitely", "Probably", "Maybe", "Probably Not", "Definitely Not"), 
                               length(completed_participants), replace = TRUE,
                               prob = c(0.60, 0.25, 0.10, 0.03, 0.02))
    )
  
  #Calculate improvement metrics
  post_survey <- post_survey %>%
    mutate(
      act_improvement = act_composite_post - act_composite_pre,
      sat_improvement = sat_total_post - sat_total_pre,
      gpa_improvement = gpa_post - gpa_pre,
      readiness_improvement = college_readiness_self_rating_post - college_readiness_self_rating,
      career_confidence_improvement = career_confidence_rating_post - career_confidence_rating
    )
  
  return(post_survey)
}

# =============================================================================
# DATASET 4: PROGRAM PARTICIPATION TRACKING
# =============================================================================

create_participation_data <- function(enrollment_data) {
  #Generate session attendance for each participant
  participation <- map_dfr(enrollment_data$participant_id, function(pid) {
    participant_info <- enrollment_data %>% filter(participant_id == pid)
    start_date <- participant_info$enrollment_date
    
    #Program typically runs for academic year
    program_weeks <- sample(28:36, 1)  #Varies by when they enrolled
    
    #Generate weekly sessions
    weekly_sessions <- tibble(
      participant_id = pid,
      cohort_year = participant_info$cohort_year,
      session_week = 1:program_weeks,
      session_date = start_date + weeks(0:(program_weeks-1)),
      session_type = sample(c("Test Prep", "College Planning", "Study Skills", "Career Exploration", 
                              "Financial Literacy", "Leadership", "Tutoring"), 
                            program_weeks, replace = TRUE),
      attended = sample(c("Present", "Absent", "Excused"), program_weeks, replace = TRUE,
                        prob = c(0.75, 0.20, 0.05)),
      hours_participated = ifelse(attended == "Present", 
                                  sample(c(1.5, 2.0, 2.5, 3.0), program_weeks, replace = TRUE,
                                         prob = c(0.20, 0.40, 0.30, 0.10)), 0),
      tutor_assigned = sample(paste0("TUTOR", str_pad(1:12, 2, pad = "0")), program_weeks, replace = TRUE)
    )
    
    return(weekly_sessions)
  })
  
  return(participation)
}

# =============================================================================
# DATASET 5: FINANCIAL AID & FUNDING TRACKING
# =============================================================================

create_funding_data <- function() {
  #Grant sources over 5 years
  grants <- tibble(
    grant_id = paste0("GRANT", str_pad(1:15, 3, pad = "0")),
    funding_source = c("Federal TRIO", "NC Education Lottery", "United Way", "Wells Fargo Foundation",
                       "Foundation For The Carolinas", "Duke Energy Foundation", "Bank of America",
                       "Federal GEAR UP", "NC Dept of Public Instruction", "Charlotte Mecklenburg Foundation",
                       "Private Donor - Anonymous", "Corporate Sponsor A", "Corporate Sponsor B",
                       "State Academic Enhancement", "Federal Title I Supplement"),
    award_year = sample(DATA_YEARS, 15, replace = TRUE),
    grant_amount = c(145000, 95000, 45000, 35000, 55000, 25000, 85000, 125000, 65000, 
                     40000, 15000, 20000, 30000, 75000, 95000),
    focus_area = sample(c("Test Prep", "College Access", "Career Readiness", "Academic Support", 
                          "Leadership Development"), 15, replace = TRUE),
    participants_served = sample(150:250, 15, replace = TRUE)
  )
  
  #Individual participant funding
  funding_individual <- map_dfr(DATA_YEARS, function(year) {
    n_year_participants <- sample(180:220, 1)  # Generate consistent participant count
    year_participants <- paste0("CYC", year, str_pad(1:n_year_participants, 3, pad = "0"))
    
    tibble(
      participant_id = year_participants,
      cohort_year = year,
      program_cost_per_participant = sample(c(850, 950, 1100, 1250), length(year_participants), replace = TRUE),
      transportation_stipend = sample(c(0, 25, 50, 75), length(year_participants), replace = TRUE,
                                      prob = c(0.40, 0.25, 0.25, 0.10)),
      materials_provided = sample(c("Basic", "Standard", "Premium"), length(year_participants), replace = TRUE,
                                  prob = c(0.20, 0.60, 0.20)),
      laptop_loaned = sample(c("Yes", "No"), length(year_participants), replace = TRUE, prob = c(0.65, 0.35))
    )
  })
  
  list(grants = grants, individual_funding = funding_individual)
}

# =============================================================================
# GENERATE 5 YEARS OF LONGITUDINAL DATA
# =============================================================================

cat("Generating 5 years of synthetic Carolina Youth Coalition survey data...\n\n")

#Create directory structure
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/raw")) dir.create("data/raw")
if (!dir.exists("data/raw/annual_cohorts")) dir.create("data/raw/annual_cohorts")

#Generate data for each year
all_enrollment <- tibble()
all_pre_surveys <- tibble()
all_post_surveys <- tibble()
all_participation <- tibble()

for(year in DATA_YEARS) {
  cat("Generating data for cohort year:", year, "\n")
  
  #Create annual cohort
  cat("Creating enrollment data...\n")
  annual_enrollment <- create_annual_enrollment_data(year)
  cat("Participants enrolled:", nrow(annual_enrollment), "\n")
  
  cat("Creating pre-survey data...\n")
  annual_pre_survey <- create_pre_survey_data(annual_enrollment)
  
  cat("Creating post-survey data...\n")
  annual_post_survey <- create_post_survey_data(annual_enrollment, annual_pre_survey)
  
  cat("Creating participation data...\n")
  annual_participation <- create_participation_data(annual_enrollment)
  
  #Save individual year files
  write_csv(annual_enrollment, paste0("data/raw/annual_cohorts/enrollment_", year, ".csv"))
  write_csv(annual_pre_survey, paste0("data/raw/annual_cohorts/pre_survey_", year, ".csv"))
  write_csv(annual_post_survey, paste0("data/raw/annual_cohorts/post_survey_", year, ".csv"))
  write_csv(annual_participation, paste0("data/raw/annual_cohorts/participation_", year, ".csv"))
  
  #Combine for master datasets
  all_enrollment <- bind_rows(all_enrollment, annual_enrollment)
  all_pre_surveys <- bind_rows(all_pre_surveys, annual_pre_survey)
  all_post_surveys <- bind_rows(all_post_surveys, annual_post_survey)
  all_participation <- bind_rows(all_participation, annual_participation)
}

#Create master combined files
write_csv(all_enrollment, "data/raw/master_enrollment_2019_2023.csv")
write_csv(all_pre_surveys, "data/raw/master_pre_surveys_2019_2023.csv")
write_csv(all_post_surveys, "data/raw/master_post_surveys_2019_2023.csv")
write_csv(all_participation, "data/raw/master_participation_2019_2023.csv")

#Generate funding data
funding_data <- create_funding_data()
write_csv(funding_data$grants, "data/raw/grant_funding_2019_2023.csv")
write_csv(funding_data$individual_funding, "data/raw/participant_funding_2019_2023.csv")

# =============================================================================
# GENERATE SUMMARY STATISTICS
# =============================================================================

#Create data summary for documentation
data_summary <- list(
  generation_info = list(
    created_date = as.character(Sys.Date()),
    years_covered = paste(min(DATA_YEARS), "to", max(DATA_YEARS)),
    total_participants = nrow(all_enrollment),
    completed_participants = nrow(all_post_surveys),
    completion_rate = round(nrow(all_post_surveys) / nrow(all_enrollment) * 100, 1)
  ),
  
  demographic_breakdown = all_enrollment %>%
    count(race_ethnicity, sort = TRUE) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)),
  
  academic_improvements = all_post_surveys %>%
    summarise(
      avg_act_improvement = round(mean(act_improvement, na.rm = TRUE), 2),
      avg_sat_improvement = round(mean(sat_improvement, na.rm = TRUE)),
      avg_gpa_improvement = round(mean(gpa_improvement, na.rm = TRUE), 3),
      pct_act_improved = round(sum(act_improvement > 0, na.rm = TRUE) / n() * 100, 1),
      pct_sat_improved = round(sum(sat_improvement > 0, na.rm = TRUE) / n() * 100, 1)
    ),
  
  file_inventory = tibble(
    dataset = c("Master Enrollment", "Master Pre-Surveys", "Master Post-Surveys", 
                "Master Participation", "Grant Funding", "Individual Funding"),
    records = c(nrow(all_enrollment), nrow(all_pre_surveys), nrow(all_post_surveys),
                nrow(all_participation), nrow(funding_data$grants), nrow(funding_data$individual_funding)),
    date_range = rep(paste(min(DATA_YEARS), "to", max(DATA_YEARS)), 6)
  )
)

#Write summary as CSV 
write_csv(data_summary$file_inventory, "data/file_inventory.csv")
write_csv(data_summary$demographic_breakdown, "data/demographic_summary.csv")
write_csv(data_summary$academic_improvements, "data/academic_improvements_summary.csv")

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("\n 5-Year Longitudinal Data Generation Complete!\n")
cat("Total participants across all cohorts:", nrow(all_enrollment), "\n")
cat("Pre-surveys collected:", nrow(all_pre_surveys), "\n")
cat("Post-surveys collected:", nrow(all_post_surveys), "\n")
cat("Participation records:", nrow(all_participation), "\n")
cat("Overall completion rate:", round(nrow(all_post_surveys) / nrow(all_enrollment) * 100, 1), "%\n")

cat("\n Files created:\n")
cat("data/raw/annual_cohorts/ (individual year files)\n")
cat("data/raw/master_enrollment_2019_2023.csv\n")
cat("data/raw/master_pre_surveys_2019_2023.csv\n") 
cat("data/raw/master_post_surveys_2019_2023.csv\n")
cat("data/raw/master_participation_2019_2023.csv\n")
cat("data/raw/grant_funding_2019_2023.csv\n")
cat("data/raw/participant_funding_2019_2023.csv\n")

cat("\n Sample improvements achieved:\n")
sample_improvements <- all_post_surveys %>%
  summarise(
    avg_act_gain = round(mean(act_improvement, na.rm = TRUE), 1),
    avg_sat_gain = round(mean(sat_improvement, na.rm = TRUE)),
    avg_gpa_gain = round(mean(gpa_improvement, na.rm = TRUE), 2)
  )
cat("Average ACT improvement:", sample_improvements$avg_act_gain, "points\n")
cat("Average SAT improvement:", sample_improvements$avg_sat_gain, "points\n")
cat("Average GPA improvement:", sample_improvements$avg_gpa_gain, "points\n")

cat("\n Ready for ETL pipeline development!\n")
cat("Next: Create automated scripts to process and analyze this longitudinal data\n")
