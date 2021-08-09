source("utilities.R",encoding="UTF-8")

vaccine_time_series <- function(ages="all",regions="Germany",from="2020-12-01",to=Sys.Date(),vac_num="all",cumulate=F){
  
  bundeslander=c("Schleswig-Holstein","Mecklenburg-Vorpommern","Niedersachsen","Sachsen-Anhalt","Berlin",
                 "Hamburg","Bremen","Sachsen","Thüringen","Hessen","Nordrhein-Westfalen","Rheinland-Pfalz",
                 "Saarland","Baden-Württemberg","Bayern","Brandenburg")
  
  vaccines <- read_csv("csvs/vac_COVID19.csv")
  vaccines$LandkreisId_Impfort <- suppressWarnings(as.numeric(vaccines$LandkreisId_Impfort))
  vaccines %>% 
    left_join(population_lk_data,by=c("LandkreisId_Impfort"="IdLandkreis"))->test
  vaccines <- test[-(8:12)]
  
  vaccines %>% 
    filter(Impfdatum >= from & Impfdatum <= to) -> vaccine_days
  
  if(vac_num=="all"){
    vaccine_days %>% 
      group_by(Impfdatum,LandkreisId_Impfort,Altersgruppe,Bundesland,Landkreis) %>% 
      summarize(Anzahl=sum(Anzahl))-> vaccine_number
  }
  else{
    vaccine_days %>% 
      filter(Impfschutz %in% vac_num)-> vaccine_number
  }
  
  if(ages=="all"){ 
    vaccine_number %>% 
    group_by(Impfdatum,LandkreisId_Impfort,Bundesland,Landkreis) %>% 
    summarize(Anzahl=sum(Anzahl))-> vaccine_age  
  } 
  else{
    vaccine_number %>% 
      filter(Altersgruppe %in% ages)->vaccine_age
  }
  if(all(regions=="Germany")){
    vaccine_age %>% 
      group_by(Impfdatum) %>% 
      summarize(Anzahl=sum(Anzahl))-> vaccine_prepared
    k <- 1
  }
  else if(is.numeric(regions)==T){
    vaccine_age %>% 
      filter(LandkreisId_Impfort %in% regions)->vaccine_prepared
    k <- 2
  }
  else if(all(regions %in% bundeslander)){
    ids <- c()
    for(region in regions){
      population_lk_data %>% 
        filter(Bundesland==region)->temp
      temp$IdLandkreis->current_ids
      ids <- c(ids,current_ids)
    }
    vaccine_age %>%
      filter(LandkreisId_Impfort %in% ids) %>% 
      group_by(Impfdatum,Bundesland) %>% 
      summarize(Anzahl=sum(Anzahl))->vaccine_prepared
    k <- 3
  }
  
  else{
    ids <- c()
    for(region in regions){
      current_ids <- get_lk_id_from_string(region)
      ids <- c(ids,current_ids)
    }
    vaccine_age %>% 
      filter(LandkreisId_Impfort %in% ids)-> temp
    temp$LandkreisId_Impfort <- as.character(temp$LandkreisId_Impfort)
    vaccine_prepared <- temp
    k <- 2
  }
  if(cumulate==F){
    if(k==1){
      vaccine_prepared %>% 
        ggplot(aes(x=Impfdatum,y=Anzahl))+geom_line()->plt
      return(plt)}
    if(k==2){
      vaccine_prepared %>% 
        ggplot(aes(x=Impfdatum,y=Anzahl,color=Landkreis))+geom_line()->plt
      return(plt)
    }
    if(k==3){
      vaccine_prepared %>% 
        ggplot(aes(x=Impfdatum,y=Anzahl,color=Bundesland))+geom_line()->plt
      return(plt)
    }
  }
  else{
    if(k==1){
      vaccine_prepared %>% 
        mutate(`Kumulierte Impfungen`=cumsum(Anzahl)) %>% 
        ggplot(aes(x=Impfdatum,y=`Kumulierte Impfungen`))+geom_line()->plt
      return(plt)
    }
    if(k==2){
      vaccine_prepared %>% 
        group_by(Landkreis) %>% 
        mutate(`Kumulierte Impfungen`=cumsum(Anzahl)) %>% 
        ggplot(aes(x=Impfdatum,y=`Kumulierte Impfungen`,color=Landkreis))+geom_line()->plt
      return(plt)
    }
    if(k==3){
      vaccine_prepared %>% 
        group_by(Bundesland) %>% 
        mutate(`Kumulierte Impfungen`=cumsum(Anzahl)) %>% 
        ggplot(aes(x=Impfdatum,y=`Kumulierte Impfungen`,color=Bundesland))+geom_line()->plt
      return(plt)
    }
  }
}


