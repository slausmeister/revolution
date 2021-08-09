#' @export
get_vaccination_data <- function(ages="all", regions="Germany", from="2020-12-01",
  to=Sys.Date(), vac_num="all", cumulate=F){
    #TODO: fehlermeldungen
    bundeslander<-c("Schleswig-Holstein","Mecklenburg-Vorpommern","Niedersachsen","Sachsen-Anhalt","Berlin",
                   "Hamburg","Bremen","Sachsen","Thüringen","Hessen","Nordrhein-Westfalen","Rheinland-Pfalz",
                   "Saarland","Baden-Württemberg","Bayern","Brandenburg")

   rev.env$vax_data -> vaccines

   vaccines %>%
     dplyr::filter(suppressWarnings(!is.na(as.numeric(LandkreisId_Impfort)))) %>%
     dplyr::mutate(LandkreisId_Impfort=as.numeric(LandkreisId_Impfort)) %>%
     dplyr::left_join(rev.env$population_lk_data,by=c("LandkreisId_Impfort"="IdLandkreis")) ->
     test
   vaccines <- test[-(8:12)]
   vaccines %>%
     dplyr::filter(Impfdatum >= from & Impfdatum <= to) -> vaccine_days

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
       rev.env$population_lk_data %>%
         dplyr::filter(Bundesland==region)->temp
       temp$IdLandkreis->current_ids
       ids <- c(ids,current_ids)
     }
     vaccine_age %>%
       dplyr::filter(LandkreisId_Impfort %in% ids) %>%
       dplyr::group_by(Impfdatum,Bundesland) %>%
       dplyr::summarize(Anzahl=sum(Anzahl))->vaccine_prepared
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
   }else{
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

#' @export
plot_vaccination_data <- function(ages="all", regions="Germany", from="2020-12-01",
  to=Sys.Date(), vac_num="all", cumulate=F){
    # TODO: Etwas robuster machen
    data <- get_vaccination_data(ages, regions, from, to, vac_num, cumulate)
    print(data)
    print(ncol(data))
    if(ncol(data)==2){
      data %>%
        ggplot2::ggplot(ggplot2::aes(x=Impfdatum, y=Anzahl)) %>%
        `+`(ggplot2::geom_line()) %>%
        return()
    }
    else if(ncol(data)==3){
      data %>%
        ggplot2::ggplot(ggplot2::aes(x=Impfdatum, y=Anzahl, color=Bundesland)) %>%
        `+`(ggplot2::geom_line()) %>%
        return()
    }
    else{
      data %>%
        ggplot2::ggplot(ggplot2::aes(x=Impfdatum, y=Anzahl, color=Landkreis)) %>%
        `+`(ggplot2::geom_line()) %>%
        return()
    }
  }
