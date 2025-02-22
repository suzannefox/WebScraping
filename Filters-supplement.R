
# these planets have inconsistent transit times
variable_timings <- c('WASP-178b', 'WASP-36b','WASP-41b')

df_planets_potential %>% 
  filter(planet %in% variable_timings) %>% 
  select(planet, reject, time_span, observatory) %>% 
  arrange(planet)

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
