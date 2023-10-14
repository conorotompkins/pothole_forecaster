library(tidyverse)
library(GSODR)
library(janitor)

load(system.file("extdata", "isd_history.rda", package = "GSODR"))

isd_history |> 
  count(COUNTRY_NAME, sort = TRUE)

local_stations <- isd_history |> 
  filter(COUNTRY_NAME == "UNITED STATES") |>
  filter(str_detect(NAME, "ALLEGHENY")) |> 
  distinct(NAME, STNID)

local_stations

tbar <- get_GSOD(years = c(2014:2023), station = "725205-14762") |> 
  as_tibble() |> 
  clean_names()

glimpse(tbar)

tbar_subset <- tbar |> 
  select(stnid, name, date = yearmoda, min, temp, max, prcp, sndp) |> 
  mutate(max_min_diff = max - min)

tbar_subset |> 
  ggplot(aes(date, temp)) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = .3) +
  geom_line()

tbar_subset |> 
  ggplot(aes(date, prcp)) +
  geom_line()

tbar_subset |> 
  ggplot(aes(prcp)) +
  geom_histogram()

tbar_subset |> 
  ggplot(aes(date, sndp)) +
  geom_line()

tbar_subset |> 
  count(sndp)

tbar_subset |> 
  ggplot(aes(temp, prcp)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(year(date)))

tbar_subset |> 
  select(-sndp) |> 
  write_csv("inputs/allegheny_county_weather_data.csv")