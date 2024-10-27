library(fpp3)
library(readr)
library(janitor)
library(future)
library(hrbrthemes)

theme_set(theme_ipsum())

plan(multisession)

options(scipen = 999, digits = 4)

set.seed(1234)

#read in pothole data
#https://data.wprdc.org/datastore/dump/29462525-62a6-45bf-9b5e-ad2e1c06348d
report_data <- read_csv("inputs/wprdc_311_2024_10_20.csv") |> 
  clean_names() |>
  mutate(create_date = yearmonth(create_date_et)) |> 
  rename(request_type = request_type_name)

#create basic tsibble
pothole_df <- report_data |> 
  filter(request_type == "Potholes") |> 
  summarize(report_count = n(),
            .by = c(create_date, request_type)) |> 
  ungroup() |>
  filter(year(create_date) >= 2016) |> 
  as_tsibble(key = request_type, index = create_date)

pothole_df

#exploratory data analysis

##basic plots

autoplot(pothole_df)

gg_season(pothole_df)

gg_subseries(pothole_df) +
  facet_wrap(vars(month(create_date, label = TRUE)), ncol = 3)

##decomposition

dcmp <- pothole_df |>
  model(stl = STL(report_count, robust = TRUE))

dcmp_components <- components(dcmp)

dcmp_components

dcmp_components |> 
  autoplot()

##outlier analysis

outliers <- dcmp_components |>
  filter(remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
           remainder > quantile(remainder, 0.75) + 3*IQR(remainder))

outliers |> 
  select(create_date, remainder)

pothole_df |>
  ggplot(aes(create_date, report_count)) +
  geom_line() +
  geom_point(data = outliers, color = "red")

#modeling

##split into train/test and forecast
##use last 20% of observations as test set

data_test <- pothole_df |> 
  slice_tail(prop = .2)

data_train <- pothole_df |> 
  anti_join(data_test, by = "create_date")

##create models

model_df <- data_train |> 
  model(naive = NAIVE(log(report_count + 1)),
        naive_seasonal = SNAIVE(log(report_count + 1)),
        mean = MEAN(log(report_count + 1)),
        mean_moving_6 = MEAN(log(report_count + 1), window = 6),
        mean_moving_12 = MEAN(log(report_count + 1), window = 12),
        lm = TSLM(log(report_count + 1) ~ trend()),
        lm_seasonal = TSLM(log(report_count + 1) ~ trend() + season()),
        arima = ARIMA(log(report_count + 1)),
        ets = ETS(log(report_count + 1)))

model_df

##make test forecast

pothole_fc <- model_df |> 
  forecast(data_test)

pothole_fc

##calculate accuracy

fc_acc <- pothole_fc |> 
  accuracy(pothole_df,
           measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_cprs = skill_score(CRPS))) |> 
  rename(rmse = RMSE) |> 
  select(request_type, .model, .type, skill_cprs, rmse) |> 
  arrange(desc(skill_cprs))

fc_acc

fc_acc |> 
  ggplot(aes(x = skill_cprs, y = rmse, label = .model)) +
  geom_label() +
  scale_x_continuous(expand = expansion(mult = c(.1, .1))) +
  scale_y_reverse()

model_acc <- fc_acc |> 
  pull(.model)

pothole_fc <- pothole_fc |> 
  mutate(.model = factor(.model, levels = model_acc))
  
pothole_fc |>  
  autoplot(data = pothole_df |> filter(year(create_date) >= 2021)) +
  facet_wrap(vars(.model), scales = "free_y", ncol = 2) +
  guides(fill_ramp = "none",
         fill = "none",
         color = "none") +
  labs(title = "Forecasts by model",
       subtitle = "Sorted descending by accuracy")

pothole_fc |> 
  mutate(.model = factor(.model, levels = model_acc)) |> 
  filter(.model %in% model_acc[1:3]) |> 
  autoplot(data = pothole_df |> filter(year(create_date) >= 2021)) +
  facet_wrap(vars(.model), scales = "free_y", ncol = 1) +
  guides(fill_ramp = "none",
         fill = "none",
         color = "none") +
  labs(title = "Top 3 forecasts by model",
       subtitle = "Sorted descending by accuracy")

##inspect model

model_df |> 
  select(lm_seasonal) |> 
  report()

##final forecast

# final_model <- model_df |>
#   select(lm_seasonal) |>
#   refit(pothole_df, reestimate = TRUE)
final_model <- pothole_df |> 
  model(lm_seasonal = TSLM(log(report_count + 1) ~ trend() + season()))

final_model |> 
  forecast(h = 12) |> 
  autoplot(pothole_df) +
  labs(title = "Final 12 month forecast of pothole complaints",
       x = "Report create date",
       y = "Report count")

##extract from forecast distribution
final_model |> 
  forecast(h = 12) |> 
  mutate(fc_hilo = hilo(report_count, level = .95),
         fc_upper = fc_hilo$upper,
         fc_lower = fc_hilo$lower)

## forecast multiple time series
report_data |> 
  count(request_type, sort = TRUE)

report_df <- report_data |> 
  filter(request_type %in% c("Potholes", "Weeds/Debris")) |> 
  summarize(report_count = n(),
            .by = c(create_date, request_type)) |> 
  ungroup() |>
  filter(year(create_date) >= 2016) |> 
  as_tsibble(key = request_type, index = create_date)

autoplot(report_df)

gg_season(report_df)

gg_subseries(report_df)

report_test <- report_df |> 
  group_by(request_type) |> 
  slice_tail(prop = .2) |> 
  ungroup()

report_train <- anti_join(report_df, report_test, by = c("create_date", "request_type"))

report_models <- report_train |> 
  model(naive = NAIVE(log(report_count + 1)),
        naive_seasonal = SNAIVE(log(report_count + 1)),
        mean = MEAN(log(report_count + 1)),
        mean_moving_6 = MEAN(log(report_count + 1), window = 6),
        mean_moving_12 = MEAN(log(report_count + 1), window = 12),
        lm = TSLM(log(report_count + 1) ~ trend()),
        lm_seasonal = TSLM(log(report_count + 1) ~ trend() + season()),
        arima = ARIMA(log(report_count + 1)),
        ets = ETS(log(report_count + 1)))

report_fc <- report_models |> 
  forecast(report_test)

##fc accuracy
fc_acc_report <- report_fc |> 
  accuracy(report_df,
           measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_cprs = skill_score(CRPS))) |> 
  select(request_type, .model, .type, skill_cprs, RMSE) |> 
  rename(rmse = RMSE) |> 
  arrange(request_type, desc(skill_cprs))

fc_acc_report

top_models <- fc_acc_report |> 
  group_by(request_type) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  select(request_type, .model)

top_models

report_models |> 
  select(request_type, lm_seasonal, naive_seasonal) |> 
  refit(report_df, reestimate = TRUE) |> 
  forecast(h = 12) |> 
  semi_join(top_models) |> 
  autoplot(report_df) +
  facet_wrap(vars(request_type), ncol = 1, scales = "free_y")