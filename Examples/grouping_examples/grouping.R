pacman::p_load(
  rio,       # to import data
  here,      # to locate files
  tidyverse, # to clean, handle, and plot the data (includes dplyr)
  janitor,
  kableExtra
  )   # adding total rows and columns

linelist <- import("grouping_examples/linelist_cleaned.rds")

ll_by_outcome <- linelist %>% 
  group_by(outcome, gender) %>%
  tally() %>%
  pivot_wider(
    id_cols = gender,
    names_from = outcome,
    values_from = n
  )

ll_by_outcome %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>%
  knitr::kable(align = "lrrrr") %>% 
  kableExtra::row_spec(row = 4, bold = TRUE) %>% 
  kableExtra::column_spec(column = 5, bold = TRUE) %>%
  kable_styling()


daily_counts <- linelist %>% 
  drop_na(date_onset) %>%        # remove that were missing date_onset
  count(date_onset) %>%            # count number of rows per unique date
  complete(                               # ensure all days appear even if no cases
    date_onset = seq.Date(                # re-define date colume as daily sequence of dates
      from = min(date_onset, na.rm=T), 
      to = max(date_onset, na.rm=T),
      by = "day"),
    fill = list(n = 0)
  )   

weekly_counts <- linelist %>% 
  drop_na(date_onset) %>%                 # remove cases missing date_onset
  mutate(week = lubridate::floor_date(date_onset, unit = "week")) %>%  # new column of week of onset
  count(week) %>%                         # group data by week and count rows per group
  complete(                               # ensure all days appear even if no cases
    week = seq.Date(                      # re-define date colume as daily sequence of dates
      from = min(week, na.rm=T), 
      to = max(week, na.rm=T),
      by = "week"),
    fill = list(n = 0))                   # set new filled-in rows to display 0

#plot(weekly_counts)

linelist_vis <- linelist %>%
  mutate(
    hospital_ord = fct_infreq(hospital)
    ) %>%
  filter(! hospital_ord %in% c("Missing","Other"))

ggplot(data = linelist_vis, mapping = aes(x = age_cat5, 
                                          y = wt_kg
                                          ))+ 
  geom_boxplot()+ 
  scale_color_brewer(palette="OrRd")
  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#000999","#F00F00", "#6BF4E9"))

ggplot(data = linelist, mapping = aes(x = age))+
  geom_histogram(
    binwidth = 2,                # width of bins
    color = "red",               # bin line color
    fill = "blue",               # bin interior color
    alpha = 0.1
  )