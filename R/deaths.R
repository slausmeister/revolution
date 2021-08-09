pop_prep <- function(population_data){
  population_data %>%
    dplyr::pivot_wider(names_from="Altersgruppe",values_from="Bevölkerung") %>%
    dplyr::mutate(`A00-A34`=`A00-A04`+`A05-A14`+`A15-A34`) %>%
    dplyr::select(Jahr,`A00-A34`,`A35-A59`,`A60-A79`,`A80+`) %>%
    dplyr::pivot_longer(c(`A00-A34`,`A35-A59`,`A60-A79`,`A80+`),names_to="Altersgruppe",values_to="population")->
    prepared
  prepared
}

abs_deaths <- function(years=2020){
  daily_deaths <- readxl::read_excel(system.file("extdata", "einkommen.xlsx", package="revolution"))

  for(jahr in years){
    stopifnot("data only available for the years 2000-2020"=jahr %in% daily_deaths$year)
    stopifnot("invalid years"=suppressWarnings(!is.na(as.numeric(jahr))))
  }
  daily_deaths %>% dplyr::filter(year %in% years)->deaths_filtered
  deaths_filtered <- mutate(deaths_filtered,year=as.character(year))
  ggplot(deaths_filtered,aes(x=days,y=deaths,color=year))+geom_line()-> plt
  plt
}
