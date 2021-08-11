#'Time series of German COVID-19 vaccination data
#'
#'\code{get_vaccination_data()} is used to create a tibble of vaccination data for regions and a certain
#'age group. The time period can be defined by the user.
#'
#'@param ages A string of the desired age group. Data is available for the groups "12-17","18-59" and "60+".
#'The default value is "all", so that the data is not specified for a special age group.
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's.
#'Warning: states and districts must not be mixed, so e.g. \code{get_vaccination_data(regions=c("Sachsen","Heidelberg"))}
#'is not allowed. The default region is the whole country "Germany".
#'@param from A date that specifies the beginning of the time series.
#'@param to A date that specifies the end of the time series. The default value is today.
#'@param vac_num Either 1 or 2 or "all". Indicates whether the first or second vaccine should be considered.
#'@param cumulate A boolean that indicates whether the time series values should be absolute or cumulative.
#'
#'@return A tibble that contains the public vaccine data prepared in a user-specified way.
#'
#'@examples get_vaccination_data(ages="60+",regions=c("Heidelberg","Sachsen"),from="2021-06-07",vac_num=1,cumulate=T)
#'
#'get_vaccination_data(ages="all",regions=c("Schleswig-Holstein","Niedersachsen"),to="2021-07-08")
#'
#'\dontrun{get_vaccination_data(regions=c("Germany","Heidelberg"))}
#'#don't mix districts, states or "Germany
#'
#'\dontrun{get_vaccination_data(ages=12-17)}
#'#age group must always be a string
#'
#'\dontrun{get_vaccination_data(from="2021-06-07",to="2021-05-06")}
#'#'from' needs to be earlier than 'to'
#'
#'@family vaccination
#'@export
get_vaccination_data <- function(ages="all", regions="Germany", from="2020-12-26",
  to=Sys.Date(), vac_num="all", cumulate=F){
    #TODO: fehlermeldungen
    bundeslander<-c("Schleswig-Holstein","Mecklenburg-Vorpommern","Niedersachsen","Sachsen-Anhalt",
                   "Hamburg","Bremen","Sachsen","Thüringen","Hessen","Nordrhein-Westfalen","Rheinland-Pfalz",
                   "Saarland","Baden-Württemberg","Bayern","Brandenburg","Berlin")

   #stopifnot("No data available for Berlin in total. Only district data availabe"=(!("Berlin" %in% regions)))
   stopifnot("invalid vac_num"=(vac_num=="all" || vac_num%in%c(1,2)))
   stopifnot("'from' must be earlier than 'to'"= as.Date(from)<as.Date(to))
   stopifnot("invalid age, ages must be a string and either '12-17','18-59','60+'"=(ages=="all"||ages=="12-17"||ages=="18-59"||ages=="60+"))
   stopifnot("cumulate must be boolean"=(!is.na(as.logical(cumulate))))
   rev.env$vax_data -> vaccines

   vaccines %>%
     dplyr::filter(suppressWarnings(!is.na(as.numeric(LandkreisId_Impfort)))) %>%
     dplyr::mutate(LandkreisId_Impfort=as.numeric(LandkreisId_Impfort)) %>%
     dplyr::left_join(rev.env$population_lk_data,by=c("LandkreisId_Impfort"="IdLandkreis")) ->
     test
   vaccines <- test[-(8:12)]
   vaccines %>%
     dplyr::filter(Impfdatum >= as.Date(from) & Impfdatum <= as.Date(to)) -> vaccine_days

   if(vac_num=="all"){
     vaccine_days %>%
       dplyr::group_by(Impfdatum,LandkreisId_Impfort,Altersgruppe,Bundesland,Landkreis) %>%
       dplyr::summarize(Anzahl=sum(Anzahl))->
       vaccine_number
   }
   else{
     vaccine_days %>%
       dplyr::filter(Impfschutz %in% vac_num)-> vaccine_number
   }
   if(ages=="all"){
    vaccine_number %>%
      dplyr::group_by(Impfdatum,LandkreisId_Impfort,Bundesland,Landkreis) %>%
      dplyr::summarize(Anzahl=sum(Anzahl))-> vaccine_age
   }
   else{
     vaccine_number %>%
       dplyr::filter(Altersgruppe %in% ages)->vaccine_age
   }
   if(all(regions=="Germany")){
     vaccine_age %>%
       dplyr::group_by(Impfdatum) %>%
       dplyr::summarize(Anzahl=sum(Anzahl))-> vaccine_prepared
     k <- 1
   }
   else if(is.numeric(regions)==T){
     vaccine_age %>%
       dplyr::filter(LandkreisId_Impfort %in% regions)->vaccine_prepared
     k <- 2
   }
   else if(all(regions %in% bundeslander)){
     ids <- c()
     for(region in regions){
       if(region=="Berlin"){
         ids <- c(ids,11000)
       }
       rev.env$population_lk_data %>%
         dplyr::filter(Bundesland==region)->temp
       temp$IdLandkreis->current_ids
       ids <- c(ids,current_ids)
     }
     vaccine_age %>%
       dplyr::filter(LandkreisId_Impfort %in% ids) %>%
       dplyr::group_by(Impfdatum,Bundesland) %>%
       dplyr::summarize(Anzahl=sum(Anzahl)) %>%
       tidyr::replace_na(list(Bundesland="Berlin"))->vaccine_prepared
     k <- 3
   }
   else{
     ids <- c()
     for(region in regions){
       current_ids <- get_lk_id_from_string(region)
       ids <- c(ids,current_ids)
     }
     vaccine_age %>%
        dplyr::filter(LandkreisId_Impfort %in% ids)-> temp
    temp$LandkreisId_Impfort <- as.character(temp$LandkreisId_Impfort)
    vaccine_prepared <- temp
    k <- 2
   }
   if(cumulate==F){
     return(vaccine_prepared)
   }else {
     if(k==1){
       vaccine_prepared %>%
         dplyr::mutate(Anzahl=cumsum(Anzahl)) -> t1
      return(t1)
     }
     if(k==2){
       vaccine_prepared %>%
         dplyr::group_by(Landkreis) %>%
         dplyr::mutate(Anzahl=cumsum(Anzahl)) -> t1
      return(t1)
     }
     if(k==3){
       vaccine_prepared %>%
         dplyr::group_by(Bundesland) %>%
         dplyr::mutate(Anzahl=cumsum(Anzahl)) -> t1
      return(t1)
     }
   }
}
#'Plotting the time series of German COVID-19 vaccination data
#'
#'\code{plot_vaccination_data()} is used to create a plot of vaccination data for regions and a certain
#'age group. The time period can be defined by the user.
#'
#'@param ages A string of the desired age group. Data is available for the groups "12-17","18-59" and "60+".
#'The default value is "all", so that the plot is not specified for a special age group.
#'@param regions A vector that either consists of strings (the names of German districts or the names of German states) or district ID's.
#'Warning: states and districts must not be mixed, so e.g. \code{get_vaccination_data(regions=c("Sachsen","Heidelberg"))}
#'is not allowed. The default region is the whole country "Germany".
#'@param from A date that specifies the beginning of the time series.
#'@param to A date that specifies the end of the time series. The default value is today.
#'@param vac_num Either 1 or 2 or "all". Indicates whether the first or second vaccine should be considered. The default is "all".
#'@param cumulate A boolean that indicates whether the time series values should be absolute or cumulative.
#'@param smoothing A positive integer that defines the window size of the moving average. Thus, the plot will be smoother
#'the higher 'smoothing' is chosen. The default setting is 'no smoothing'.
#'
#'@return A plot of public vaccine data prepared in a user-specified way.
#'
#'@examples plot_vaccination_data(ages="60+",regions=c("Heidelberg","Sachsen"),from="2021-06-07",vac_num=1,cumulate=T)
#'
#'plot_vaccination_data(ages="all",regions=c("Schleswig-Holstein","Niedersachsen"),to="2021-07-08")
#'
#'\dontrun{plot_vaccination_data(regions=c("Germany","Heidelberg"))}
#'#don't mix districts, states or "Germany"
#'
#'\dontrun{plot_vaccination_data(ages=12-17)}
#'#age group must always be a string
#'
#'\dontrun{plot_vaccination_data(from="2021-06-07",to="2021-05-06")}
#'#'from' needs to be earlier than 'to'
#'
#' @family vaccination
#' @export
plot_vaccination_data <- function(ages="all", regions="Germany", from="2020-12-26",
  to=Sys.Date(), vac_num="all", cumulate=F,smoothing=0){
    data <- get_vaccination_data(ages, regions, from, to, vac_num, cumulate)
    stopifnot("smoothing must be a positive integer"=(is.integer(as.integer(smoothing)))&&smoothing>=0)

    if(ncol(data)==2){
      data %>%
        dplyr::mutate(Anzahl=slider::slide_dbl(Anzahl,mean,.before=smoothing,.after=smoothing)) %>%
        ggplot2::ggplot(ggplot2::aes(x=Impfdatum, y=Anzahl)) %>%
        `+`(ggplot2::geom_line()) %>%
        return()
    }
    else if(ncol(data)==3){
      data %>%
        dplyr::group_by(Bundesland) %>%
        dplyr::mutate(Anzahl=slider::slide_dbl(Anzahl,mean,.before=smoothing,.after=smoothing)) %>%
        ggplot2::ggplot(ggplot2::aes(x=Impfdatum, y=Anzahl, color=Bundesland)) %>%
        `+`(ggplot2::geom_line()) %>%
        return()
    }
    else{
      data %>%
        dplyr::group_by(Landkreis) %>%
        dplyr::mutate(Anzahl=slider::slide_dbl(Anzahl,mean,.before=smoothing,.after=smoothing)) %>%
        ggplot2::ggplot(ggplot2::aes(x=Impfdatum, y=Anzahl, color=Landkreis)) %>%
        `+`(ggplot2::geom_line()) %>%
        return()
    }
  }
