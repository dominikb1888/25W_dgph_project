pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  skimr
)

# IMPORT ---------------------------------------
linelist_raw <- import("linelist_raw.xlsx")
#skimr::skim(linelist_raw)


# CLEANING -----------------------------------
linelist <- linelist_raw %>% 

  ## Clean Column Names ----------------------
  janitor::clean_names() %>%

  ## Rename Column Names ---------------------
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)  %>%
  
  
  
  ## Deduplicate Data
  distinct() %>%

  ## Remove unnecessary columns
  select(-c(date_onset, fever:vomit)) %>%
  
  ## Compute new columns 
  mutate(
    bmi = wt_kg / (ht_cm/100)^2
    )
