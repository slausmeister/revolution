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

### rki covid data:
# import raw rki data
rki_data <- read_csv("csvs/RKI_COVID19.csv")

# the 'Neuer' and 'Datenstand' columns compare this dataset to the one from yesterday,
# which makes it useless for our research
rki_data %>% select(-NeuerFall, -NeuerTodesfall, -NeuGenesen, -Datenstand) ->
  rki_data

# change the column type of 'Meldedatum' and 'Refdatum' to date
rki_data %>% mutate(Meldedatum=as.Date(Meldedatum), Refdatum=as.Date(Refdatum)) -> rki_data

# in most cases, 'Altersgruppe2' is not available
rki_data %>% select(-Altersgruppe2) -> rki_data

# transform the 'IdLandkreis' column to a numeric
rki_data %>% mutate(IdLandkreis=as.numeric(IdLandkreis)) -> rki_data

# we will use the 'Landkreis' column from the other csv, because of normalization
rki_data %>% select(-Landkreis) %>%
  left_join(select(read_csv("csvs/population_lk.csv"), IdLandkreis, Landkreis), by="IdLandkreis") ->
  rki_data

# 'IdBundesland' is a part of 'IdLandkreis' and we have the 'Bundesland' column
rki_data %>% select(-IdBundesland) -> rki_data

# 'FID' is the case id, which is useless for our research
rki_data %>% select(-FID) -> rki_data

days_since_2020 <- seq(as.Date("2020-01-01"), as.Date(Sys.Date()), by="days")
