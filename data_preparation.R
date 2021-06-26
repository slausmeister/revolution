# library for reading csvs
library(readr)
# library for piping
library(dplyr)
# library for tibbles
library(tibble)
# library for string actions
library(stringr)
# library for cleaning up tibbles
library(tidyr)


### population data: -> maybe move this to incidence.R?
# import the population of 'Landkreis' with the given csv
population_lk_data <- read_csv("csvs/population_lk.csv")

# transform the 'IdLandkreis' column to a numeric
population_lk_data %>% mutate(IdLandkreis=as.numeric(IdLandkreis)) -> population_lk_data

# calculate the total german population
population_lk_data %>% summarise(n=sum(Bevölkerung)) %>%
  `[[`(1) -> total_population_germany

# import the population per age group in 2020
population_age_2020_data <- read_csv("csvs/population_age.csv") %>% filter(Jahr==2020)

### rki covid data:
# import raw rki data
rki_data <- read_csv("csvs/RKI_COVID19.csv")

# the 'Neuer' and 'Datenstand' columns compare this dataset to the one from yesterday,
# which makes it useless for our research
rki_data %>% select(-NeuerFall, -NeuerTodesfall, -NeuGenesen, -Datenstand) ->
  rki_data

# the 'Refdatum' column is not necessary to analyse the fraction of known infection dates,
# we can use 'IstErkrankungsbeginn' for that
rki_data %>% select(-Refdatum) -> rki_data

# change the column type of 'Meldedatum' to date
rki_data %>% mutate(Meldedatum=as.Date(Meldedatum)) -> rki_data

# in most cases, 'Altersgruppe2' is not available
rki_data %>% select(-Altersgruppe2) -> rki_data

# transform the 'IdLandkreis' column to a numeric
rki_data %>% mutate(IdLandkreis=as.numeric(IdLandkreis)) -> rki_data

# we will use the 'Landkreis' column from the other csv, because of normalization
rki_data %>% select(-Landkreis) %>%
  left_join(select(population_lk_data, IdLandkreis, Landkreis), by="IdLandkreis") ->
  rki_data

# 'IdBundesland' is a part of 'IdLandkreis' and we have the 'Bundesland' column
rki_data %>% select(-IdBundesland) -> rki_data

# 'FID' is the case id, which is useless for our research
rki_data %>% select(-FID) -> rki_data



### cases data for germany (data for each day since 01/01/2020):
# join the rki_data with all days since 01/01/2020
days_since_2020 <- seq(as.Date("2020-01-01"), as.Date(Sys.Date()), by="days")

left_join(tibble(date=days_since_2020),
  rki_data, by=c("date"="Meldedatum")) %>%
  group_by(date) %>%
  summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
  # the days we have no infection data for are days with 0 infections
  mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
    recoveries=replace_na(recoveries, 0)) ->
  cases_time_series_germany
