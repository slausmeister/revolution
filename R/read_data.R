# This is the main data preperation function.
#' @export
read_rki_data <- function(){
# import the population of 'Landkreis' with the given csv
population_lk_data <- readr::read_csv(system.file("extdata", "population_lk.csv", package="revolution"))

# transform the 'IdLandkreis' column to a numeric
population_lk_data %>% dplyr::mutate(IdLandkreis=as.numeric(IdLandkreis)) -> population_lk_data

# calculate the total german population
population_lk_data %>% dplyr::summarise(n=sum(Bevölkerung)) %>%
  `[[`(1) -> total_population_germany

# import the population per age group in 2020
readr::read_csv(system.file("extdata", "population_age.csv", package="revolution")) %>% dplyr::filter(Jahr==2020) %>%
  dplyr::group_by(Altersgruppe) %>% dplyr::summarise(Bevölkerung=sum(Bevölkerung)) ->
  population_age_2020_data

### rki covid data:
# import raw rki data
rki_data <- readr::read_csv(system.file("extdata", "RKI_COVID19.csv", package="revolution"))

# the 'Neuer' and 'Datenstand' columns compare this dataset to the one from yesterday,
# which makes it useless for our research
rki_data %>% dplyr::select(-NeuerFall, -NeuerTodesfall, -NeuGenesen, -Datenstand) ->
  rki_data

# change the column type of 'Meldedatum' and 'Refdatum' to date
rki_data %>% dplyr::mutate(Meldedatum=as.Date(Meldedatum), Refdatum=as.Date(Refdatum)) -> rki_data

# in most cases, 'Altersgruppe2' is not available
rki_data %>% dplyr::select(-Altersgruppe2) -> rki_data

# transform the 'IdLandkreis' column to a numeric
rki_data %>% dplyr::mutate(IdLandkreis=as.numeric(IdLandkreis)) -> rki_data

# we will use the 'Landkreis' column from the other csv, because of normalization
rki_data %>% dplyr::select(-Landkreis) %>%
  dplyr::left_join(dplyr::select(readr::read_csv(system.file("extdata","population_lk.csv", package="revolution")), IdLandkreis, Landkreis), by="IdLandkreis") ->
  rki_data

# 'IdBundesland' is a part of 'IdLandkreis' and we have the 'Bundesland' column
rki_data %>% dplyr::select(-IdBundesland) -> rki_data

# 'FID' is the case id, which is useless for our research
rki_data %>% dplyr::select(-FID) -> rki_data

return(rki_data)
}
