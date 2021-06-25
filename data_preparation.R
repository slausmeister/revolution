# library for reading csvs
library(readr)
# library for piping
library(dplyr)
# library for tibbles
library(tibble)
# library for string actions
library(stringr)

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

# rename the 'Landkreis' column to look nicer
rki_data %>% mutate(Landkreis=str_remove(Landkreis, "[:alpha:]+ ")) -> rki_data

# transform the 'IdLandkreis' column to a numeric
rki_data %>% mutate(IdLandkreis=as.numeric(IdLandkreis)) -> rki_data


### population data:
# import the population of 'Landkreis' with the given csv
population_lk_data <- read_csv("csvs/population_lk.csv")

# transform the 'IdLandkreis' column to a numeric
population_lk_data %>% mutate(IdLandkreis=as.numeric(IdLandkreis)) -> population_lk_data

# calculate the total german population
population_lk_data %>% summarise(n=sum(Bevölkerung)) %>%
  `[[`(1) -> total_population_germany
