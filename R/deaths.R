pop_prep <- function(population_data){
  population_data %>%
    tidyr::pivot_wider(names_from="Altersgruppe",values_from="Bevölkerung") %>%
    dplyr::mutate(`A00-A34`=`A00-A04`+`A05-A14`+`A15-A34`) %>%
    dplyr::select(Jahr,`A00-A34`,`A35-A59`,`A60-A79`,`A80+`) %>%
    tidyr::pivot_longer(c(`A00-A34`,`A35-A59`,`A60-A79`,`A80+`),names_to="Altersgruppe",values_to="population")->
    prepared
  prepared
}

#'Time series of the total number of daily deaths in Germany
#'
#'\code{get_abs_deaths} is used to create a tibble of the total number of deaths during the time span of a year.
#'It is possible to include more than one year.
#'
#'@param years A vector of years. The years must be represented as integers. Data is available for the years 2000 up to 2020.
#'
#'@return A tibble that contains the daily death data of the years the user specified.
#'
#'@examples get_abs_deaths(c(2001,2003,2006,2016))
#'
#'\dontrun{get_abs_deaths(1999)}
#'#Data is only available for 2000-2020
#'
#'\dontrun{get_abs_deaths(c("2006"))}
#'#The years must be integer values
#'@export
get_abs_deaths <- function(years=2020){
  daily_deaths <- readxl::read_excel(system.file("extdata", "daily_deaths.xlsx", package="revolution"))

  for(jahr in years){
    stopifnot("data only available for the years 2000-2020"=jahr %in% daily_deaths$year)
    stopifnot("invalid years"=suppressWarnings(!is.na(as.numeric(jahr))))
  }
  daily_deaths %>% dplyr::filter(year %in% years) -> deaths_filtered
  deaths_filtered <- dplyr::mutate(deaths_filtered,year=as.character(year))
  return(deaths_filtered)

}

#'Plotting the total number of daily deaths in Germany
#'
#'\code{plot_abs_deaths} is used to create a plot of the total number of deaths during the time span of a year.
#'It is possible to include more than one year.
#'
#'@param years A vector of years. The years must be represented as integers. Data is available for the years 2000 up to 2020.
#'@param smoothing A positive integer that defines the window size of the moving average. Thus, the plot will be smoother
#'the higher 'smoothing' is chosen. The default setting is 'no smoothing'.
#'
#'@return A plot of the daily death data of the years the user specified.
#'
#'@examples get_abs_deaths(c(2001,2003,2006,2016),smoothing=2)
#'
#'\dontrun{get_abs_deaths(1999)}
#'#Data is only available for 2000-2020.
#'
#'\dontrun{get_abs_deaths(c("2006"),smoothing=-2.4)}
#'#Years must be a vector of integer values and smoothing must be a positive integer.
#' @export
plot_abs_deaths <- function(years=2020,smoothing=0){
  get_abs_deaths(years) %>%
    dplyr::mutate(deaths=slider::slide_dbl(deaths,mean,.before=smoothing,.after=smoothing)) %>%
    ggplot2::ggplot(ggplot2::aes(x=days,y=deaths,color=year))+ggplot2::geom_line()-> plt
  return(plt)
}

#'Time series of the total number of weekly deaths in Germany
#'
#'\code{get_weekly_deaths} is used to create a tibble of the total number of deaths during the time span of a year.
#'It is possible to include more than one year. The data can be further specified by several age groups.
#'
#'@param years A vector of years. The years must be represented as integers. Data is available for the years 2014 up to 2020.
#'@param age A string that indicates the desired age group.
#'Possible age groups are "A00-A34", "A35-A59", "A60-A79" and "A80+".
#'@param rate A boolean to decide whether the absolute number of weekly deaths or the weekly mortality should be returned.
#'
#'@section Warning:
#'Population data per age group is only available for the whole year (census is always 31-12). Thus, if \code{rate=T}, the mortality
#'cannot be 100% precise because the weekly number of deaths is always divided by the same factor, even though the
#'age distribution is continuously changing during the year. Due to aging society, the mortality is therefore
#'overestimated during the first weeks of the year.
#'
#'@return A tibble that contains the weekly death data of the years and age groups the user specified.
#'
#'@examples get_weekly_deaths(c(2001,2003,2006,2016), age="A35-A59",rate=T)
#'
#'\dontrun{get_weekly_deaths("1999")}
#'#Data is only available for 2014-2020 and \code{years} must be an integer vector
#'
#'\dontrun{get_weekly_deaths(2015,age=65)}
#'#The age group must be a string. Allowed strings are "A00-A34","A35-A59","A60-79" and "A80+".
#' @export
get_weekly_deaths <- function(years=2020,age="A80+",rate=F){
  deaths_per_age <- readxl::read_excel(system.file("extdata", "deaths_per_age.xlsx", package="revolution"))
  age_pop <- pop_prep(rev.env$population_age_data)
  for(jahr in years){
    stopifnot("data only available for the years 2014-2020"=jahr %in% age_pop$Jahr)
  }
  for(alter in age){
    stopifnot("data only available for the age groups 'A00-A34','A35-A59','A60-A79','A80+'"=age %in% age_pop$Altersgruppe)
  }
  stopifnot("rate must be boolean"=(!is.na(as.logical(rate))))
  deaths_per_age %>%
    dplyr::mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
    dplyr::mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) %>%
    dplyr::select(1,17:21) -> deaths_new
  filtered_data <- dplyr::filter(deaths_new,Jahr %in% years)
  temp <- dplyr::select(filtered_data,Jahr,age,week)
  temp %>%
    dplyr::rename_with(~return("Absolute deaths"),tidyselect::starts_with("A")) -> temp
  for(i in (1:length(years))){
    div <- as.numeric(dplyr::filter(age_pop,Jahr==years[i],Altersgruppe==age)[1,3])
    bool <- temp["Jahr"]==years[i]
    if(rate==T){
      temp[bool[,1],2] <- temp[bool[,1],2]*(1/div)
      temp %>% dplyr::rename_with(~return("Mortality"),tidyselect::starts_with("A")) -> temp
    }
  }
  temp <- dplyr::mutate(temp,Jahr=as.character(Jahr))
  return(temp)
}

#'Plotting the total number of weekly deaths in Germany
#'
#'\code{plot_weekly_deaths} is used to create a plot of the total number of deaths during the time span of a year.
#'It is possible to include more than one year. The plot can be further specified for several age groups.
#'
#'@param years A vector of years. The years must be represented as integers. Data is available for the years 2014 up to 2020.
#'@param age A string that indicates the desired age group.
#'Possible age groups are "A00-A34", "A35-A59", "A60-A79" and "A80+".
#'@param rate A boolean to decide whether the absolute number of weekly deaths or the weekly mortality should be plotted.
#'
#'@section Warning:
#'Population data per age group is only available for the whole year (census is always 31-12). Thus, if \code{rate=T}, the mortality
#'cannot be 100% precise because the weekly number of deaths is always divided by the same factor, even though the
#'age distribution is continuously changing during the year. Due to aging society, the mortality is therefore
#'overestimated during the first weeks of the year.
#'
#'@return A plot of the weekly death data of the years and age groups the user specified.
#'
#'@examples plot_weekly_deaths(c(2014,2016,2016), age="A35-A59",rate=T)
#'
#'\dontrun{plot_weekly_deaths("1999")}
#'#Data is only available for 2014-2020 and \code{years} must be an integer vector
#'
#'\dontrun{plot_weekly_deaths(2015,age=65)}
#'#The age group must be a string. Allowed strings are "A00-A34","A35-A59","A60-79" and "A80+".
#' @export
plot_weekly_deaths <- function(years=2020,age="A80+",rate=F){
  data <- get_weekly_deaths(years, age,rate=rate)
  if(rate==T){
    plt <- ggplot2::ggplot(data, mapping=ggplot2::aes(x=week,y=Mortality,color=Jahr))+
    ggplot2::geom_line()
  }else{
    plt <- ggplot2::ggplot(data, mapping=ggplot2::aes(x=week,y=`Absolute deaths`,color=Jahr))+
      ggplot2::geom_line()
  }
  return(plt)
}


#'Yearly mortality per age group
#'
#'\code{get_total_mortality} is used to create a tibble of the yearly mortality per age group since 2014.
#'
#'@param age A string that indicates the desired age group.
#'Possible age groups are "A00-A34", "A35-A59", "A60-A79" and "A80+".
#'
#'@return A tibble that contains the yearly mortality per age group.
#'
#'@examples get_total_mortality(age="A35-A59")
#'
#'\dontrun{get_total_mortality(age=65)}
#'#The age group must be a string. Allowed strings are "A00-A34","A35-A59","A60-79" and "A80+".
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

#'Plotting the yearly mortality per age group
#'
#'\code{plot_total_mortality} is used to create a plot of the yearly mortality per age group since 2014.
#'
#'@param age A string that indicates the desired age group.
#'Possible age groups are "A00-A34", "A35-A59", "A60-A79" and "A80+".
#'
#'@return A plot that shows the yearly mortality per age group.
#'
#'@examples plot_total_mortality(age="A35-A59")
#'
#'\dontrun{plot_total_mortality(age=65)}
#'#The age group must be a string. Allowed strings are "A00-A34","A35-A59","A60-79" and "A80+".
#' @export
plot_total_mortality <- function(age="A80+"){
  get_total_mortality(age) %>% ggplot2::ggplot(aes(x=Jahr,y=rate,color=ages))+
  ggplot2::geom_line()->plt
  plt
}
#'Plotting of excess mortality
#'
#'\code{plot_excess_mortality} creates a plot of the total number of daily deaths of one year in contrast to the
#'average values of other years. Data is available for the years 2000-2020.
#'
#'@param excess_year An integer that stands for the year of which the total deaths should be plotted against the average.
#'@param average_years A vector of integers that represent the years of which the function should calculate the average deaths.
#'@param smoothing A positive integer that defines the window size of the moving average. Thus, the plot will be smoother
#'the higher 'smoothing' is chosen. The default setting is 'no smoothing'.
#'
#'@section Warning:
#'Due to missing data for February 29th in non-leap years, the smoothing function increases the gap in the plot the higher
#'smoothing factor is chosen.
#'
#'@return A plot that shows the total number of deaths in comparison to a user-defined average.
#'
#'@examples plot_excess_mortality(2020,c(2015,2016,2017,2018,2019),smoothing=1)
#'\dontrun{plot_excess_mortality(excess_year="2020",average_years=c(1999,2017),smoothing=100)}
#'#"excess_year" and "average_years" should be an integer. Only the years 2000-2020 can be considered.
#'#Furthermore, it's not recommended to choose a smoothing factor > 5 due to the gap in the plot described in section "Warning".
#' @export
plot_excess_mortality <- function(excess_year=2020,average_years=c(2016,2017,2018,2019),smoothing=0){
  stopifnot("excess_year needs to be an integer >=2000 and <= 2020"= (is.integer(as.integer(excess_year))&& excess_year>=2000 && excess_year<=2020))
  stopifnot("average_years needs to be an integer vector >=2000 <= 2020"= (is.integer(as.integer(average_years))&& max(average_years)<=2020 && min(average_years)>=2000))
  stopifnot("smoothing must be a positive integer"=(is.integer(as.integer(smoothing)))&&smoothing>=0)
  daily_death <- readxl::read_excel(system.file("extdata", "daily_deaths.xlsx", package="revolution"))
  daily_death %>%
    dplyr::filter(days<=365)->daily_deaths
  daily_deaths %>%
    dplyr::filter(year %in% average_years)->deaths_without
  daily_deaths %>%
    dplyr::filter(year==excess_year) %>%
    dplyr::mutate(deaths=slider::slide_dbl(deaths,mean,.before=smoothing,.after=smoothing)) %>%
    dplyr::mutate(year=as.character(year))-> deaths_excess
  deaths_without %>%
    dplyr::group_by(days) %>%
    dplyr::summarize(deaths=sum(deaths),days=unique(days)) %>%
    dplyr::mutate(deaths=(1/length(average_years))*deaths) %>%
    dplyr::mutate(deaths=slider::slide_dbl(deaths,mean,.before=smoothing,.after=smoothing)) %>%
    dplyr::mutate(year=rep(paste("average of",paste(as.character(average_years),collapse=", ")),365))->deaths_summarized

  deaths_summarized <- deaths_summarized[,c(2,1,3)]
  deaths_excess %>%
    tibble::add_row(deaths_summarized)->deaths

  deaths %>%
    ggplot2::ggplot(aes(x=days,y=deaths,color=year))+geom_line()->plt
  plt
}
