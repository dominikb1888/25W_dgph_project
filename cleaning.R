pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  parsedate,  # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
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
  # select(-c(fever:vomit)) %>%
  
  ## Compute new columns 
  mutate(
    bmi = wt_kg / (ht_cm/100)^2,
    new_var_paste = stringr::str_glue("This Patient came to {hospital} on ({date_hospitalisation})"),
  ) %>%  
  

  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  #separate(new_var_paste, into = c("split_hospital", "split_date_hospitalization"), sep = "on", extra = "drop") %>%
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>%
  
  ## Mutate hospital name values
  mutate(
    hospital = recode(hospital,
                      # for reference: OLD = NEW
                      "Mitylira Hopital"  = "Military Hospital",
                      "Mitylira Hospital" = "Military Hospital",
                      "Military Hopital"  = "Military Hospital",
                      "Port Hopital"      = "Port Hospital",
                      "Central Hopital"   = "Central Hospital",
                      "other"             = "Other",
                      "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
    )
  ) %>%
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  mutate(hospital_factor = fct_other(                      # adjust levels
         linelist$hospital,
         keep = c("Port Hospital", "Central Hospital", "Missing"),  # keep these separate
         other_level = "Other Hospital")) %>%
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age)) %>% 
  
  mutate(
    # age categories: custom
    age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70)),
    
    # age categories: 0 to 85 by 5s
    age_cat5 = epikit::age_categories(age_years, breakers = seq(0, 85, 5)))
