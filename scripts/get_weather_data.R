library(tidyverse)
library(GSODR)
library(janitor)

isd_history |> 
  count(COUNTRY_NAME, sort = TRUE)

local_stations <- isd_history |> 
  filter(COUNTRY_NAME == "UNITED STATES") |>
  filter(str_detect(NAME, "ALLEGHENY")) |> 
  distinct(NAME, STNID)

local_stations

tbar <- get_GSOD(years = c(2022:2023), station = "725205-14762") |> 
  as_tibble() |> 
  clean_names()

glimpse(tbar)

tbar_subset <- tbar |> 
  select(stnid, name, date = yearmoda, min, temp, max, prcp, sndp)

tbar_subset |> 
  ggplot(aes(YEARMODA, TEMP)) +
  geom_ribbon(aes(ymin = MIN, ymax = MAX), alpha = .3) +
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
  count(SNDP)

tbar_subset |> 
  select(-sndp) |> 
  write_csv("inputs/allegheny_county_weather_data.csv")