library(fpp3)
library(tidyverse)
library(janitor)
library(future)
library(tictoc)
library(hrbrthemes)
library(skimr)

theme_set(theme_ipsum())

plan(multisession)

#read in pothole data

pothole_data <- read_csv("inputs/wprdc_311.csv") |> 
  clean_names() |> 
  filter(request_type == "Potholes") |> 
  mutate(created_yearmonth = yearmonth(created_on))

skim(pothole_data)

#read in weather data
weather_data <- read_csv("inputs/allegheny_county_weather_data.csv")

glimpse(weather_data)
skim(weather_data)

weather_data <- weather_data |> 
  mutate(date_ym = yearmonth(date)) |> 
  group_by(date_ym) |> 
  summarize(min_avg = mean(min),
            temp_avg = mean(temp),
            max_avg = mean(max),
            prcp_sum = sum(prcp)) |> 
  ungroup() |> 
  mutate(temp_diff = max_avg - min_avg) |> 
  mutate(across(c(min_avg, temp_avg, max_avg, temp_diff, prcp_sum), ~lag(.x, 1), .names = "{.col}_lag1")) |> 
  mutate(across(c(min_avg, temp_avg, max_avg, temp_diff, prcp_sum), ~lag(.x, 2), .names = "{.col}_lag2")) |> 
  mutate(across(c(min_avg, temp_avg, max_avg, temp_diff, prcp_sum), ~lag(.x, 3), .names = "{.col}_lag3"))

skim(weather_data)

weather_data |> 
  ggplot(aes(date_ym, temp_avg)) +
  geom_ribbon(aes(ymin = min_avg, ymax = max_avg), alpha = .3) +
  geom_line()

weather_data |> 
  ggplot(aes(date_ym, prcp_sum)) +
  geom_col()

weather_data |> 
  mutate(year = as.factor(year(date_ym))) |> 
  ggplot(aes(min_avg, prcp_sum, color = year)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(year))

#create basic tsibble

pothole_df <- pothole_data |> 
  group_by(created_yearmonth, request_type) |> 
  summarize(report_count = n()) |> 
  ungroup() |>
  as_tsibble()

#join pothole and weather data

pothole_df <- pothole_df |> 
  left_join(weather_data, by = c("created_yearmonth" = "date_ym"))

# lambda <- pothole_df |> 
#   features(report_count, guerrero) |> 
#   pull(lambda_guerrero)
# 
# pothole_df <- pothole_df |> 
#   mutate(lambda_guerrero = lambda)

glimpse(pothole_df)

pothole_df |> 
  ggplot(aes(created_yearmonth, report_count)) +
  geom_line()

pothole_df |> 
  gg_season()

pothole_df |> 
  gg_subseries()

pothole_df |> 
  ggplot(aes(temp_avg, report_count)) +
  geom_point()

pothole_df |> 
  ggplot(aes(prcp_sum, report_count)) +
  geom_point()

pothole_df |> 
  mutate(temp_diff = max_avg - min_avg) |> 
  ggplot(aes(temp_diff, report_count)) +
  geom_point()

pothole_df |> 
  as_tibble() |> 
  select(report_count, contains("temp")) |> 
  pivot_longer(contains("temp")) |> 
  ggplot(aes(value, report_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free")

pothole_df |> 
  as_tibble() |> 
  select(report_count, contains("min")) |> 
  pivot_longer(contains("min")) |> 
  ggplot(aes(value, report_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free")

pothole_df |> 
  as_tibble() |> 
  select(report_count, contains("max")) |> 
  pivot_longer(contains("max")) |> 
  ggplot(aes(value, report_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free")

pothole_df |> 
  as_tibble() |> 
  select(report_count, contains("prcp")) |> 
  pivot_longer(contains("prcp")) |> 
  ggplot(aes(value, report_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free")

pothole_df |> 
  as_tibble() |> 
  select(report_count, contains("diff")) |> 
  pivot_longer(contains("diff")) |> 
  ggplot(aes(value, report_count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free")

#split into train/test and forecast
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

#CV and forecast
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
  filter(between(.id, 65, 70)) |> 
  mutate(.id = .id + 1) |> 
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


#forecast with exogenous variables
#train/test
#296.7 sec elapsed
tic()
progressr::with_progress(
  
model_df_exo <- data_train |> 
  model(ets = ETS(log(report_count + 1)),
        ts_lm = TSLM(log(report_count + 1) ~ trend() + season()),
        ts_lm_exo = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg + min_avg + max_avg + prcp_sum),
        ts_lm_exo_lag1 = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg_lag1 + min_avg_lag1 + max_avg_lag1 + prcp_sum_lag1),
        ts_lm_exo_lag3 = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg_lag3 + min_avg_lag3 + max_avg_lag3 + prcp_sum_lag3),
        arima = ARIMA(log(report_count + 1)),
        arima_exo = ARIMA(log(report_count + 1) ~ temp_avg + min_avg + max_avg + prcp_sum),
        arima_exo_lag1 = ARIMA(log(report_count + 1) ~ temp_avg_lag1 + min_avg_lag1 + max_avg_lag1 + prcp_sum_lag1),
        arima_exo_lag3 = ARIMA(log(report_count + 1) ~ temp_avg_lag3 + min_avg_lag3 + max_avg_lag3 + prcp_sum_lag3))

)
toc()

pothole_fc_exo <- model_df_exo |> 
  forecast(data_test)

fc_exo_acc <- pothole_fc_exo |> 
  accuracy(pothole_df, measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_crps = skill_score(CRPS))) |> 
  select(.model, .type, MAPE, RMSSE, skill_crps) |> 
  arrange(desc(skill_crps))

fc_exo_acc

pothole_fc_exo |> 
  left_join(fc_exo_acc) |> 
  mutate(.model = fct_reorder(.model, skill_crps, .desc = TRUE)) |> 
  autoplot(pothole_df) +
  facet_wrap(vars(.model), ncol = 1)


#cv

#384.208 sec elapsed
tic()
progressr::with_progress(
  
  model_df_exo <- pothole_cv |> 
    model(ets = ETS(log(report_count + 1)),
          ts_lm = TSLM(log(report_count + 1) ~ trend() + season()),
          ts_lm_exo = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg + min_avg + max_avg + prcp_sum),
          ts_lm_exo_lag1 = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg_lag1 + min_avg_lag1 + max_avg_lag1 + prcp_sum_lag1),
          ts_lm_exo_lag2 = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg_lag2 + min_avg_lag2 + max_avg_lag2 + prcp_sum_lag2),
          ts_lm_exo_lag3 = TSLM(log(report_count + 1) ~ trend() + season() + temp_avg_lag3 + min_avg_lag3 + max_avg_lag3 + prcp_sum_lag3),
          arima = ARIMA(log(report_count + 1)),
          arima_exo = ARIMA(log(report_count + 1) ~ temp_avg + min_avg + max_avg + prcp_sum),
          arima_exo_lag1 = ARIMA(log(report_count + 1) ~ temp_avg_lag1 + min_avg_lag1 + max_avg_lag1 + prcp_sum_lag1),
          arima_exo_lag2 = ARIMA(log(report_count + 1) ~ temp_avg_lag2 + min_avg_lag2 + max_avg_lag2 + prcp_sum_lag2),
          arima_exo_lag3 = ARIMA(log(report_count + 1) ~ temp_avg_lag3 + min_avg_lag3 + max_avg_lag3 + prcp_sum_lag3)
          )
  
)
toc()

new_weather_data <- pothole_df |> 
  mutate(month = month(created_yearmonth, label = TRUE)) |> 
  as_tibble() |> 
  select(month, min_avg:prcp_sum_lag1_lag3) |> 
  group_by(month) |> 
  summarize(across(where(is.numeric), mean)) |> 
  ungroup()

horizon_data <- new_data(pothole_cv, 12) |> 
  mutate(month = month(created_yearmonth, label = TRUE)) |> 
  left_join(new_weather_data)
  
pothole_fc_exo <- model_df_exo |> 
  forecast(horizon_data)

tic()
fc_exo_acc <- pothole_fc_exo |> 
  accuracy(pothole_df, measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_crps = skill_score(CRPS))) |> 
  select(.model, .type, MAPE, RMSSE, skill_crps) |> 
  arrange(desc(skill_crps))
toc()

fc_exo_acc

fc_exo_acc |> 
  ggplot(aes(RMSSE, skill_crps, label = .model)) +
  geom_label() +
  scale_x_reverse()

fc_exo_acc |> 
  filter(!.model %in% c("ets", "arima")) |> 
  ggplot(aes(RMSSE, skill_crps, label = .model)) +
  geom_label() +
  scale_x_reverse()

pothole_fc_exo |> 
  filter(.id == max(.id)) |> 
  left_join(fc_exo_acc) |> 
  mutate(.model = fct_reorder(.model, skill_crps, .desc = TRUE)) |> 
  autoplot(pothole_cv) +
  facet_wrap(vars(.model), ncol = 1)

final_exo_model <- pothole_df |> 
  model(arima_exo_lag3 = ARIMA(log(report_count + 1) ~ temp_avg_lag3 + min_avg_lag3 + max_avg_lag3 + prcp_sum_lag3))

horizon_data_final <- new_data(pothole_df, 12) |> 
  mutate(month = month(created_yearmonth, label = TRUE)) |> 
  left_join(new_weather_data)

final_exo_model |> 
  forecast(horizon_data_final) |> 
  autoplot(pothole_df)

plan(sequential)

#notes
#https://github.com/tidyverts/fabletools/issues/103