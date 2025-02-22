library(tidyverse)
library(lubridate)
library(glue)

source('C:\\Users\\suzan\\Documents\\R\\burrow\\burrow.R')
source('Filters-supplement.R')
# -------------------------------------------------------------------------

file_transits <- '2025-02-19_exoclock_schedule.csv'
df_transits_orig <- read_csv(file_transits)

# -------------------------------------------------------------------------
df_transits <- df_transits_orig %>% 
  mutate(original_lineno = row_number()) %>% 
  # REJECT if either ingress or egress is in red font
  mutate(reject_redfont = t_before_red | t_after_red) %>% 
  # REJECT if date isn't between mid-March and mid-May
  mutate(reject_baddate = case_when(t_before_date >= '2025-03-14' & t_before_date <= '2025-05-14' ~ FALSE, T ~ TRUE)) %>% 
  # calculate time span between 1 hr before to 1 hr after
  mutate(ingress = as.POSIXct(paste(t_before_date, t_before_time), format = "%Y-%m-%d %H:%M")) %>% 
  mutate(egress = as.POSIXct(paste(t_after_date, t_after_time), format = "%Y-%m-%d %H:%M")) %>% 
  mutate(time_span = difftime(egress, ingress, units="hours")) %>% 
  # REJECT if time is more than 6 hours
  mutate(reject_toolong = time_span > 6) %>% 
  # moon illumination 
  mutate(illumination = round(as.numeric(str_remove(moon_illumination, '%')))) %>% 
  mutate(distance = round(as.numeric(str_remove(moon_distance, '°')))) %>% 
  # REJECT if illumination is >= 50 and distance <= 90
  mutate(reject_toobright = case_when(illumination > 50 & distance < 90 ~ TRUE, T ~ FALSE)) %>% 
  # REJECT if there are multiple entries with very different transit times
  mutate(reject_badtiming = case_when(planet == 'WASP-36b' & time_span < 3 ~ 'should be 3.81',
                                      planet == 'WASP-41b' & time_span < 4 ~ 'should be 4.6',
                                      T ~ 'OK')) %>% 
  # REJECT for any reason
  mutate(reject = reject_redfont | reject_baddate | reject_toolong | reject_toobright | reject_badtiming != 'OK') %>% 
  # note says this is optimal
  mutate(optimal = coalesce(str_detect(note, 'OPTIMAL'), FALSE)) %>% 
  # recent observations 
  mutate(recent_observations = gsub("\\)", "", recent_observations)) %>% 
  mutate(recent_observations = gsub("\\(", "", recent_observations)) %>% 
  mutate(total_obs = as.integer(word(recent_observations, 4, sep = ' '))) %>%
  mutate(recent_obs = as.integer(word(recent_observations, 5, sep = ' '))) %>% 
  mutate(status = factor(status, levels = c('ALERT','HIGH','MEDIUM','LOW','CAMPAIGNLOW','TTVs'))) %>% 
  identity()

print(glue('Start with {df_transits %>% tally() %>% pull()}'))
print(glue('Remove baddates, leaves {df_transits %>% filter(reject_baddate == FALSE) %>% tally() %>% pull()}'))
print(glue('Remove redfont, leaves {df_transits %>% filter(reject_baddate == FALSE & reject_redfont == FALSE) %>% tally() %>% pull()}'))
print(glue('Remove toolong, leaves {df_transits %>% filter(reject_baddate == FALSE & reject_redfont == FALSE & reject_toolong == FALSE) %>% tally() %>% pull()}'))
print(glue('Remove toobright, leaves {df_transits %>% filter(reject_baddate == FALSE & reject_redfont == FALSE & reject_toolong == FALSE & reject_toobright == FALSE) %>% tally() %>% pull()}'))

df_transits_potential <- df_transits %>% 
  filter(reject_baddate == FALSE) %>% 
  filter(reject_redfont == FALSE) %>% 
  filter(reject_toolong == FALSE) %>% 
  filter(reject_toobright == FALSE) %>% 
  filter(reject == FALSE) %>% 
  identity()

# check
df_transits_potential %>% count(reject)
df_transits_potential %>% distinct(planet)

count_optimal <- df_transits %>% 
  filter(reject == FALSE) %>% 
  filter(optimal) %>% 
  tally() %>% 
  pull()

# -------------------------------------------------------------------------
# planets represented by those transits

df_planets <- df_transits_potential %>% 
  count(planet, status, optimal, oc) %>% 
  rename(potential_transits = n) %>% 
  arrange(status) %>% 
  identity()

df_planets_obs <- df_transits_potential %>% 
  distinct(planet, total_obs, recent_obs)

df_transit_dates <- df_transits_potential %>%
  arrange(planet, t_before_date) %>% 
  mutate(where = str_remove(observatory, ' Observatory')) %>% 
  mutate(transit = glue('{where} {t_before_date}')) %>% 
  group_by(planet) %>%
  summarise(dates = paste((transit), collapse = ", "))

df_planets <- df_planets %>% 
  left_join(df_planets_obs, by = 'planet') %>% 
  left_join(df_transit_dates, by = 'planet')

df_planets <- df_planets %>% 
  mutate(discovered = word(planet, 1, sep='-'), .before = 1)

df_planets 
# -------------------------------------------------------------------------

df_transits %>% 
  filter(reject == FALSE) %>% 
  ggplot(aes(distance, illumination, color = status)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Moon Illumination vs Moon Distance",
    x = "Distance",
    y = "Illumination",
    color = "Status"
  ) +
  theme_minimal()

myplanet <- 'HATS-37Ab'
df_choice <- df_transits_potential %>% 
  filter(planet == myplanet) %>% 
  arrange(t_before_date)

for (i in 1:nrow(df_choice)) {

  if (i == 1){
    print(paste(" Planet: ", df_choice$planet[i], " status:", df_choice$status[i]))
    print(glue("observations : total {df_choice$total_obs[i]}, recent {df_choice$recent_obs[i]}"))
    print(glue("{df_choice$oc[i]}"))
    print('')
    } 
  
  observatory <- df_choice$observatory[i]
  date <- df_choice$t_before_date[i]
  time <- df_choice$t_before_time[i]
  
  print(glue('Choice {i}'))
  print(paste("Observatory: ", df_choice$observatory[i]))
  print(glue('date {date}, {time}'))
} 

df_transits %>% 
  filter(reject == FALSE) %>% 
  filter(planet == myplanet) %>% 
  mutate(time_span = round(time_span, 1)) %>% 
  mutate(observatory = str_remove(observatory, ' Observatory')) %>% 
  mutate(t_before_time = substr(t_before_time,1,5)) %>% 
  mutate(point_label = glue('{observatory}\n{planet} {time_span}H\n{t_before_date} {t_before_time}')) %>% 
  ggplot(aes(distance, illumination, color = status)) +
  scale_x_continuous(limits = c(80, NA)) +
  scale_y_continuous(limits = c(NA, 45)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = point_label), vjust = -0.05, hjust = 1.5, size = 3) +  # Add text labels
  labs(
    title = glue("Moon Illumination vs Moon Distance - ({myplanet})"),
    x = "Distance",
    y = "Illumination",
    color = "Status"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------

tab_planets <- df_transits_potential %>% 
  count(planet, oc, status, optimal, total_obs, recent_obs, time_span) %>% 
  pivot_wider(names_from = observatory, values_from = n, values_fill = 0)

tab_planets <- tab_planets %>% 
  left_join(df_transits_potential %>% count(planet) %>% rename(Total = n),
            by = planet)

tab_planets <- tab_planets %>% 
  left_join(
    df_transits_potential %>% 
      count(planet, observatory) %>% 
      pivot_wider(names_from = observatory, values_from = n, values_fill = 0), by = 'planet'
  ) 

tab_planets %>% arrange(status)

df_transits_potential %>% 
  filter(optimal) %>% 
  select(optimal, original_lineno, observatory, planet, status, oc, total_obs, recent_obs, 
         time_span, t_before_date,
         illumination, distance) %>% 
  arrange(total_obs) %>% 
  identity()
