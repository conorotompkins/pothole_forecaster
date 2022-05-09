library(tidyverse)
library(lubridate)
library(vroom)
library(janitor)
library(fable)
library(tsibble)
library(fabletools)
library(feasts)

floor_date(Sys.Date(), "week")

pothole_df <- vroom("inputs/data/76fda9d0-69be-4dd5-8108-0de7907fc5a4.csv") %>% 
  clean_names() %>% 
  filter(request_type == "Potholes",) %>% 
  mutate(report_week = floor_date(created_on, "week") %>% yearweek) %>% 
  select(-c(id, request_id)) %>% 
  filter(report_week < yearweek("2022-05-01") - 1)

glimpse(pothole_df)

pothole_df %>% 
  count(status)

pothole_history <- pothole_df %>% 
  group_by(request_type, report_week) %>% 
  summarize(potholes = n()) %>% 
  #mutate(potholes = log(potholes)) %>% 
  ungroup() %>% 
  mutate(report_week = yearweek(report_week)) %>% 
  as_tsibble(index = report_week)

pothole_history %>% 
  autoplot()

pothole_history_log <- pothole_history %>% 
  mutate(potholes = log(potholes))

pothole_history_log %>% 
  gg_season()

pothole_history_log %>% 
  gg_subseries()

pothole_history_log %>% 
  ACF(potholes) %>% 
  autoplot()

components <- pothole_history_log %>% 
  model(STL(potholes ~ trend(window = 11) +
              season(window = 5))) %>% 
  components()

components %>% 
  autoplot()

components %>% 
  autoplot(season_adjust)

components %>% 
  ggplot(aes(x = report_week)) +
  geom_line(aes(y = potholes), alpha = .3) +
  geom_line(aes(y = season_adjust))

count_model <- pothole_history %>% 
  model(CROSTON(potholes))

count_model %>% 
  forecast(h = 52) %>% 
  autoplot() %>% 
  autolayer(pothole_history)

pothole_history %>% 
  features(potholes, features = feat_stl)

pothole_history %>% 
  features(potholes, feat_acf)



model_df <- pothole_history_log %>% 
  model(naive = NAIVE(potholes),
        snaive = SNAIVE(potholes),
        ets = ETS(potholes),
        tslm = TSLM(potholes),
        drift = NAIVE(potholes ~ drift()),
        arima = ARIMA(potholes))
  
model_df %>% 
  forecast(h = 24) %>% 
  autoplot() +
  autolayer(pothole_history_log %>% 
              slice_tail(n = 52*2)) +
  facet_wrap(~.model, ncol = 1)

model_df %>% 
  accuracy() %>% 
  arrange(RMSSE)
