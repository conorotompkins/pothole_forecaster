library(fpp3)
library(tidyverse)
library(janitor)
library(future)
library(hrbrthemes)

theme_set(theme_ipsum())

plan(multisession)

options(scipen = 999, digits = 4)

set.seed(1234)

#read in pothole data
#https://data.wprdc.org/datastore/dump/29462525-62a6-45bf-9b5e-ad2e1c06348d
pothole_data <- read_csv("inputs/wprdc_311.csv") |> 
  clean_names() |> 
  filter(request_type == "Potholes") |> 
  mutate(created_yearmonth = yearmonth(created_on))

#create basic tsibble
pothole_df <- pothole_data |> 
  group_by(created_yearmonth, request_type) |> 
  summarize(report_count = n()) |> 
  ungroup() |>
  as_tsibble()

pothole_df

#exploratory data analysis

##basic plots

autoplot(pothole_df)

gg_season(pothole_df)

gg_subseries(pothole_df) +
  facet_wrap(vars(month(created_yearmonth, label = TRUE)))

##decomposition

dcmp <- pothole_df |>
  model(stl = STL(report_count, robust = TRUE))

dcmp_components <- components(dcmp)

dcmp_components

dcmp_components |> 
  autoplot()

##outlier analysis

outliers <- dcmp_components |>
  filter(
    remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
      remainder > quantile(remainder, 0.75) + 3*IQR(remainder)
  )

outliers |> 
  select(created_yearmonth, remainder)

pothole_df |>
  ggplot(aes(created_yearmonth, report_count)) +
  geom_line() +
  geom_point(data = outliers, color = "red")

#modeling

##split into train/test and forecast
data_test <- pothole_df |> 
  slice_tail(prop = .2)

data_train <- pothole_df |> 
  anti_join(data_test, by = "created_yearmonth")

##create models

model_df <- data_train |> 
  model(arima = ARIMA(log(report_count + 1)),
        ets = ETS(log(report_count + 1)),
        lm_seasonal = TSLM(log(report_count + 1) ~ trend() + season()))

pothole_fc <- model_df |> 
  forecast(data_test)

pothole_fc

##calculate accuracy

fc_acc <- pothole_fc |> 
  accuracy(pothole_df,
           measures = list(point_accuracy_measures, distribution_accuracy_measures, skill_cprs = skill_score(CRPS))) |> 
  select(.model, .type, skill_cprs, RMSE) |> 
  arrange(desc(skill_cprs))

fc_acc

pothole_fc |> 
  autoplot(pothole_df |> 
             filter(year(created_yearmonth) >= 2021)) +
  facet_wrap(vars(.model), scales = "free_y", ncol = 1)

##inspect model

model_df |> 
  select(lm_seasonal) |> 
  report()

##final forecast

final_model <- model_df |> 
  select(lm_seasonal) |> 
  refit(pothole_df, reestimate = TRUE)

final_model |> 
  forecast(h = 12) |> 
  autoplot(pothole_df)
