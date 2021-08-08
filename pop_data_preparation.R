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
read_csv("csvs/population_age.csv") %>% 
  group_by(Altersgruppe,Jahr) %>% summarise(Bevölkerung=sum(Bevölkerung)) ->
  population_age_data
