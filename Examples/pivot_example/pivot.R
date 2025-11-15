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
linelist_raw <- readRDS("pivot_example/malaria_facility_count_data.rds")

df_long <- malaria_facility_count_data %>% 
      pivot_longer(
        cols = c(`malaria_rdt_0-4`, `malaria_rdt_5-14`, `malaria_rdt_15`)
      ) %>%
      select(-c(malaria_tot))

ggplot(data = df_long) +
     geom_col(
        mapping = aes(x = data_date, y = value, fill = name),
        width = 1
     )
