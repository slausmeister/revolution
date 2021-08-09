pop_prep <- function(population_data){
  population_data %>%
    tidyr::pivot_wider(names_from="Altersgruppe",values_from="Bevölkerung") %>%
    dplyr::mutate(`A00-A34`=`A00-A04`+`A05-A14`+`A15-A34`) %>%
    dplyr::select(Jahr,`A00-A34`,`A35-A59`,`A60-A79`,`A80+`) %>%
    tidyr::pivot_longer(c(`A00-A34`,`A35-A59`,`A60-A79`,`A80+`),names_to="Altersgruppe",values_to="population")->
    prepared
  prepared
}

#' @export
get_abs_deaths <- function(years=2020){
  daily_deaths <- readxl::read_excel(system.file("extdata", "daily_deaths.xlsx", package="revolution"))

  for(jahr in years){
    stopifnot("data only available for the years 2000-2020"=jahr %in% daily_deaths$year)
    stopifnot("invalid years"=suppressWarnings(!is.na(as.numeric(jahr))))
  }
  daily_deaths %>% dplyr::filter(year %in% years) -> deaths_filtered
  deaths_filtered <- dplyr::mutate(deaths_filtered,year=as.character(year))
  return(deaths_filtered)
  ggplot2::ggplot(deaths_filtered,  ggplot2::aes(x=days,y=deaths,color=year))+
      ggplot2::geom_line()-> plt
  plt
}

#' @export
plot_abs_deaths <- function(years=2020){
  get_abs_deaths(years) %>% ggplot2::ggplot(ggplot2::aes(x=days,y=deaths,color=year))+
    ggplot2::geom_line()-> plt
  return(plt)
}

#' @export
get_weekly_absolute_mortality <- function(years=2020,age="A80+"){
  deaths_per_age <- readxl::read_excel(system.file("extdata", "deaths_per_age.xlsx", package="revolution"))
  age_pop <- pop_prep(rev.env$population_age_data)
  for(jahr in years){
    stopifnot("data only available for the years 2000-2020"=jahr %in% age_pop$Jahr)
  }
  for(alter in age){
    stopifnot("data only available for the age groups 'A00-A34','A35-A59','A60-A79','A80+'"=age %in% age_pop$Altersgruppe)
  }
  deaths_per_age %>%
    dplyr::mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
    dplyr::mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) %>%
    dplyr::select(1,17:21) -> deaths_new
  filtered_data <- dplyr::filter(deaths_new,Jahr %in% years)
  temp <- dplyr::select(filtered_data,Jahr,age,week)
  temp %>%
    dplyr::rename_with(~return("mortality"),tidyselect::starts_with("A")) -> temp
  for(i in (1:length(years))){
    div <- as.numeric(dplyr::filter(age_pop,Jahr==years[i],Altersgruppe==age)[1,3])
    bool <- temp["Jahr"]==years[i]
    temp[bool[,1],2] <- temp[bool[,1],2]*(1/div)
  }
  temp <- dplyr::mutate(temp,Jahr=as.character(Jahr))
  return(temp)
  plt <- ggplot2::ggplot(temp,mapping=ggplot2::aes(x=week,y=mortality,color=Jahr))+
    ggplot2::geom_line()
  return(plt)
}

#' @export
plot_weekly_absolute_mortality <- function(years=2020,age="A80+"){
  data <- get_weekly_absolute_mortality(years, age)
  plt <- ggplot2::ggplot(data, mapping=ggplot2::aes(x=week,y=mortality,color=Jahr))+
    ggplot2::geom_line()
  return(plt)
}

#' @export
get_total_mortality <- function(age="A80+"){
  age_pop <- pop_prep(rev.env$population_age_data)
  for(alter in age){
    stopifnot("data only available for the age groups 'A00-A34','A35-A59','A60-A79','A80+'"=age %in% age_pop$Altersgruppe)
  }
  age_pop %>%
    tidyr::pivot_wider(names_from=Altersgruppe,values_from=population)-> pop

  deaths_per_age <- readxl::read_excel(system.file("extdata", "deaths_per_age.xlsx", package="revolution"))

  deaths_per_age %>%
    dplyr::mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
    dplyr::mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) -> deaths_summarized

  deaths_summarized <- deaths_summarized[-(1:16)]
  deaths_summarized %>%
    dplyr::group_by(Jahr) %>%
    dplyr::summarize(`A00-A34`=sum(`A00-A34`),`A35-A59`=sum(`A35-A59`),`A60-A79`=sum(`A60-A79`),`A80+`=sum(`A80+`))-> final_tibble
  deaths <- final_tibble[-nrow(final_tibble),]

  per_age_group <- deaths[2:5]/pop[2:5]
  per_age_group <- dplyr::mutate(per_age_group,Jahr=c(2014,2015,2016,2017,2018,2019,2020),.before=`A00-A34`)

  per_age_group %>%
    tidyr::pivot_longer(age,names_to="ages",values_to="rate") %>%
    return()

}

#' @export
plot_total_mortality <- function(age="A80+"){
  get_total_mortality(age) %>% ggplot2::ggplot(aes(x=Jahr,y=rate,color=ages))+
  ggplot2::geom_line()->plt
  plt
}
