library(fpp3)
library(tidyverse)
library(janitor)
library(future)
library(tictoc)
library(hrbrthemes)

theme_set(theme_ipsum())

plan(multisession)

pothole_data <- read_csv("inputs/wprdc_311.csv") |> 
  clean_names() |> 
  filter(request_type == "Potholes") |> 
  mutate(created_yearmonth = yearmonth(created_on))

pothole_df <- pothole_data |> 
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

progressr::with_progress(
  
  model_df <- data_train |> 
    model(arima = ARIMA(log(report_count + 1)),
          ets = ETS(log(report_count + 1)),
          ts_lm = TSLM(log(report_count + 1) ~ trend() + season()))
  
)

pothole_fc <- model_df |> 
  forecast(data_test)

pothole_fc |> 
  accuracy(pothole_df) |> 
  arrange(RMSSE)

pothole_fc |> 
  autoplot(pothole_df)

#cv
pothole_cv <- stretch_tsibble(pothole_df, .step = 1, .init = 24)

pothole_cv |> 
  count(.id)

#132.048 sec elapsed
tic()
progressr::with_progress(
  
  models_cv <- pothole_cv |> 
    model(arima = ARIMA(log(report_count + 1)),
          ets = ETS(log(report_count + 1)),
          ts_lm = TSLM(log(report_count + 1) ~ trend() + season()))
  
)
toc()

#3.271 sec elapsed
tic()
progressr::with_progress(
  
  forecast_cv <- models_cv |> 
    forecast(h = 12)
  
)
toc()

#72.336 sec elapsed
tic()
progressr::with_progress(
  
  cv_acc <- forecast_cv |> 
    accuracy(pothole_df, measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_cprs = skill_score(CRPS))) |> 
    select(.model, .type, MAPE, RMSSE, skill_cprs) |> 
    arrange(desc(skill_cprs))
  
)
toc()

cv_acc |> 
  arrange(desc(skill_cprs))

forecast_cv |> 
  filter(between(.id, 65, 70)) |> 
  autoplot(pothole_cv) +
  facet_wrap(vars(.id), ncol = 2, scales = "free_y")

forecast_cv |> 
  mutate(.id = .id + 1) |> 
  filter(between(.id, 65, 70)) |> 
  autoplot(pothole_cv, alpha = .1) +
  facet_wrap(vars(.id), ncol = 2, scales = "free_y")

#0.322 sec elapsed
tic()
progressr::with_progress(
  
  final_model <- pothole_df |> 
    model(ts_lm = TSLM(log(report_count + 1) ~ trend() + season()))
  
)
toc()

report(final_model)

final_model |> 
  forecast(h = 12) |> 
  autoplot(pothole_df)

#hierarchical reconciled forecast
pothole_data |> 
  glimpse()

pothole_dfh <- pothole_data |> 
  group_by(created_yearmonth, council_district) |> 
  summarize(report_count = n()) |> 
  ungroup() |> 
  drop_na(council_district) |> 
  tsibble(key = council_district, index = created_yearmonth) |> 
  aggregate_key(council_district, report_count = sum(report_count))

scan_gaps(pothole_dfh)

autoplot(pothole_dfh)

gg_subseries(pothole_dfh)

lambda_df <- pothole_dfh |> 
  features(report_count, guerrero)

pothole_dfh <- pothole_dfh |> 
  left_join(lambda_df)

pothole_dfh |> 
  autoplot(box_cox(report_count, lambda_guerrero))

train <- pothole_dfh |> 
  group_by(council_district) |> 
  slice_head(prop = .8) |> 
  ungroup() |> 
  left_join(lambda_df)

ts_length <- pothole_dfh |> 
  count(council_district) |> 
  distinct(n) |> 
  pull()

train_length <- train |> 
  count(council_district) |> 
  ungroup() |> 
  distinct(n) |> 
  pull()

test <- new_data(train, ts_length - train_length) |> 
  left_join(lambda_df)

models <- train |> 
  model(model = ARIMA(fabletools::box_cox(report_count, dplyr::first(lambda_guerrero))))

fc_test <- models |> 
  forecast(test)

fc_test |> 
  accuracy(pothole_df, measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_cprs = skill_score(CRPS))) |> 
  select(.model, .type, MAPE, RMSSE, skill_cprs) |> 
  arrange(desc(skill_cprs))

fc_test|> 
  autoplot(pothole_dfh) +
  facet_wrap(vars(council_district), ncol = 2, scales = "free_y")

reconciled_fc <- models |> 
  reconcile(bu = bottom_up(model)) |> 
  forecast(test)

reconciled_fc |> 
  autoplot(pothole_dfh) +
  facet_wrap(vars(council_district, .model), scales = "free_y", ncol = 2)

reconciled_fc |> 
  accuracy(pothole_dfh) |> 
  select(.model, council_district, RMSSE) |> 
  pivot_wider(names_from = .model, values_from = RMSSE)

#notes
#https://github.com/tidyverts/fabletools/issues/103