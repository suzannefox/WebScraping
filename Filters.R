library(tidyverse)
library(lubridate)

# -------------------------------------------------------------------------

file_planets <- '2025-02-19_exoclock_schedule.csv'

df_planets_orig <- read_csv(file_planets)

df_moonphases <- tibble::tribble(
~date,~phase,
"2025-03-14",100,
"2025-03-15",92.8,
"2025-03-16",85.7,
"2025-03-17",78.5,
"2025-03-18",71.4,
"2025-03-19",64.2,
"2025-03-20",57.1,
"2025-03-21",50,
"2025-03-22",43.75,
"2025-03-23",37.5,
"2025-03-24",31.25,
"2025-03-25",25,
"2025-03-26",18.75,
"2025-03-27",12.5,
"2025-03-28",6.25,
"2025-03-29",0,
"2025-03-30",6.25,
"2025-03-31",12.5,
"2025-04-01",18.75,
"2025-04-02",25,
"2025-04-03",31.25,
"2025-04-04",37.5,
"2025-04-05",43.75,
"2025-04-06",50,
"2025-04-07",57.1,
"2025-04-08",64.2,
"2025-04-09",71.4,
"2025-04-10",78.5,
"2025-04-11",85.77,
"2025-04-12",92.8,
"2025-04-13",100,
"2025-04-14",92.8,
"2025-04-15",85.7,
"2025-04-16",78.5,
"2025-04-17",71.4,
"2025-04-18",64.2,
"2025-04-19",57.1,
"2025-04-20",50,
"2025-04-21",43.75,
"2025-04-22",37.5,
"2025-04-23",31.25,
"2025-04-24",25,
"2025-04-25",18.75,
"2025-04-26",12.5,
"2025-04-27",6.25,
"2025-04-28",0,
"2025-04-29",7.1,
"2025-04-30",14.2,
"2025-05-01",21.4,
"2025-05-02",28.5,
"2025-05-03",35.7,
"2025-05-04",42.85,
"2025-05-05",50,
"2025-05-06",57.1,
"2025-05-07",64.2,
"2025-05-08",71.4,
"2025-05-09",78.5,
"2025-05-10",85.7,
"2025-05-11",92.8,
"2025-05-12",100,
"2025-05-13",92.8,
"2025-05-14",85.7) %>% 
  mutate(date = as.Date(date))

# -------------------------------------------------------------------------
df_planets <- df_planets_orig %>% 
  mutate(original_lineno = row_number()) %>% 
  # REJECT if either ingress or egress is in red font
  mutate(reject_redfont = t_before_red | t_after_red) %>% 
  # REJECT if date isn't between mid-March and mid-May
  mutate(reject_baddate = case_when(t_before_date >= '2025-03-14' & t_before_date <= '2025-05-14' ~ FALSE, T ~ TRUE)) %>% 
  # calculate time span between 1 hr before to 1 hr after
  mutate(time_span = difftime(t_after_time, t_before_time, units="hours")) %>% 
  # REJECT if time is more than 6 hours
  mutate(reject_toolong = time_span > 6) %>% 
  # moon illumination 
  mutate(illumination = round(as.numeric(str_remove(moon_illumination, '%')))) %>% 
  mutate(distance = round(as.numeric(str_remove(moon_distance, '°')))) %>% 
  # REJECT if illumination is >= 50 and distance <= 90
  mutate(reject_toobright = case_when(illumination > 50 & distance < 90 ~ TRUE, T ~ FALSE)) %>% 
  # REJECT for any reason
  mutate(reject = reject_redfont | reject_baddate | reject_toolong | reject_toobright) %>% 
  # note says this is optimal
  mutate(optimal = str_detect(note, 'OPTIMAL')) %>% 
  # recent observations 
  mutate(recent_observations = gsub("\\)", "", recent_observations)) %>% 
  mutate(recent_observations = gsub("\\(", "", recent_observations)) %>% 
  mutate(total_obs = as.integer(word(recent_observations, 4, sep = ' '))) %>%
  mutate(recent_obs = as.integer(word(recent_observations, 5, sep = ' '))) %>% 
  identity()

# df_planets %>% 
#   filter(reject == FALSE) %>% 
#   select(t_before_date, moon_illumination, moon_distance) %>%
#   left_join(df_moonphases, by=c('t_before_date' = 'date')) %>% 
#   identity() 

df_planets_potential <- df_planets %>% 
  filter(reject == FALSE) %>% 
  # useful data for choosing
  select(original_lineno, reject, observatory, note, planet, status, optimal, oc,
         total_obs, recent_obs,
         t_before_date, t_before_time, t_after_time, time_span,
         illumination, distance) %>% 
  identity()
  
# -------------------------------------------------------------------------

df_planets %>% 
  filter(reject == FALSE) %>% 
  ggplot(aes(distance, illumination, color = status)) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "Moon Illumination vs Moon Distance",
    x = "Distance",
    y = "Illumination",
    color = "Status"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------

df_planets %>% 
  filter(reject == FALSE) %>% 
  count(optimal, note)

df_planets %>% 
  filter(reject == FALSE) %>% 
  select(oc,  total_obs, recent_obs)

# -------------------------------------------------------------------------

df_planets %>% 
  filter(reject_baddate == FALSE) %>% 
  filter(reject_redfont == FALSE) %>% 
  filter(reject_toolong == FALSE) %>% 
  filter(reject_toobright == FALSE) %>% 
  # tally() %>% 
  identity()

df_planets_potential %>% 
  filter(optimal) %>% 
  select(optimal, original_lineno, observatory, planet, status, oc, total_obs, recent_obs, 
         time_span, t_before_date,
         illumination, distance) %>% 
  arrange(total_obs) %>% 
  View()
