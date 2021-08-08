#' @export
#' @importFrom magrittr %>%
read_rki_data <- function(){
# import the population of 'Landkreis' with the given csv
population_lk_data <- readr::read_csv(system.file("extdata", "population_lk.csv",package="revolution"))

# transform the 'IdLandkreis' column to a numeric
population_lk_data %>% dplyr::mutate(IdLandkreis=as.numeric(IdLandkreis)) -> population_lk_data

# calculate the total german population
population_lk_data %>% dplyr::summarise(n=sum(Bevölkerung)) %>%
  `[[`(1) -> total_population_germany

# import the population per age group in 2020
readr::read_csv("extdata", "population_age.csv") %>% dplyr::filter(Jahr==2020) %>%
  dplyr::group_by(Altersgruppe) %>% dplyr::summarise(Bevölkerung=sum(Bevölkerung)) ->
  population_age_2020_data
}


