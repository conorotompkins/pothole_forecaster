library(fpp3)
library(tidyverse)
library(janitor)

pothole_df <- read_csv("inputs/wprdc_311.csv") |> 
  clean_names() |> 
  filter(request_type == "Potholes") |> 
  mutate(created_yearmonth = yearmonth(created_on)) |> 
  group_by(created_yearmonth, request_type) |> 
  summarize(report_count = n()) |> 
  ungroup() |>
  as_tsibble()

glimpse(pothole_df)

pothole_df |> 
  ggplot(aes(created_yearmonth, report_count)) +
  geom_line()

pothole_df |> 
  gg_season()

pothole_df |> 
  gg_subseries()

# pothole_df <- pothole_df |> 
#   stretch_tsibble(.step = 12, .init = 24)

data_test <- pothole_df |> 
  slice_tail(prop = .2)

data_train <- pothole_df |> 
  anti_join(data_test, by = "created_yearmonth")

model_df <- data_train |> 
  model(arima = ARIMA(log(report_count + 1)),
        ets = ETS(log(report_count + 1)),
        ts_lm = TSLM(log(report_count + 1) ~ trend() + season()))

pothole_fc <- model_df |> 
  forecast(data_test)

pothole_fc |> 
  accuracy(pothole_df)

pothole_fc |> 
  autoplot(pothole_df)

#cv
pothole_cv <- stretch_tsibble(pothole_df, .step = 6, .init = 24)

models_cv <- pothole_cv |> 
  model(arima = ARIMA(log(report_count + 1)),
        ets = ETS(log(report_count + 1)),
        ts_lm = TSLM(log(report_count + 1) ~ trend() + season()))


forecast_cv <- models_cv |> 
  forecast(h = 12)

forecast_cv |> 
  accuracy(pothole_df, measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_cprs = skill_score(CRPS))) |> 
  select(.model, .type, MAPE, RMSSE, CRPS, skill_cprs)

forecast_cv |> 
  autoplot(pothole_cv) +
  facet_wrap(vars(.id), ncol = 2, scales = "free_y")

final_model <- pothole_df |> 
  model(ts_lm = TSLM(log(report_count + 1) ~ trend() + season()))

final_model |> 
  forecast(h = 12) |> 
  autoplot(pothole_df)
