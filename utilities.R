# ein Skript, das aus allen Kriterien (zB Altersgruppe, Landkreisname, Bundeslandname etc)
# eine Zeitreihe aus Infektionen erstellen kann
# außerdem soll das Skript Benutzereingaben in IDs umwandeln können (zB Schleswig Holstein zu 01)

source("pop_data_preparation.R",encoding="UTF-8")
source("rki_data_preparation.R",encoding="UTF-8")

# HIDDEN FUNKTION
# klar
get_bundesland_id_from_lk_id <- function(lk_id){
  return((lk_id - lk_id %% 1000) / 1000)
}

<<<<<<< HEAD
get_bundesland_from_bundesland_id <- function(a){
  if(a==1){return("Schleswig-Holstein")}; if(a==2){return("Hamburg")}
  if(a==3){return("Niedersachsen")}; if(a==4){return("Bremen")}
  if(a==5){return("Nordrhein-Westfalen")};if(a==6){return("Hessen")}
  if(a==7){return("Rheinland-Pfalz")};if(a==8){return("Baden-Württemberg")}
  if(a==9){return("Bayern")};if(a==10){return("Saarland")}
  if(a==11){return("Berlin")};if(a==12){return("Brandenburg")}
  if(a==13){return("Mecklenburg-Vorpommern")};if(a==14){return("Sachsen")}
  if(a==15){return("Sachsen-Anhalt")};if(a==16){return("Thüringen")}
  else{return("Error 404: Bundesland not found")}
}

# get the LandkreisID from a input string
=======
# HIDDEN FUNKTION
# get the LandkreisID from an input string
>>>>>>> 78379e898dd8dc8794ca800f1e87bbe408f1932b
get_lk_id_from_string <- function(lk_name, print_process=F){
  # TODO: check if valid ID
  if(suppressWarnings(!is.na(as.numeric(lk_name)))) return(lk_name)
  population_lk_data %>% filter(str_detect(Landkreis, regex(lk_name, ignore_case=T))) %>%
     `[[`("Landkreis") -> lks

  population_lk_data %>% filter(str_detect(Landkreis, regex(lk_name, ignore_case=T))) %>%
    `[[`("IdLandkreis") -> lk_ids

  if(length(lks) == 1) return(lk_ids)
  if(length(lks) == 0) {
    cat("No match found for:\n")
    print(lk_name)
    cat("Defaulting to Heidelberg\n")
    return(8221)
  }

  if(print_process){
    cat("For ", lk_name, " the following 'Landkreise' were found\n")
    print(lks)
  }

  for(i in 1:length(lks)) if(tolower(lks[i]) == tolower(lk_name)){
    if(print_process){
      cat("Because of an exact match, the following 'Landkreis' was returned:\n")
      print(lk_name)
      cat("If this is wrong, please type the exact 'Landkreis'\n")
    }

    return(as.numeric(lk_ids[[i]]))
  }

  if(print_process){
    cat("The following 'Landkreis' was returned:\n")
    print(lks[[1]])
    cat("If this is wrong, please type the exact 'Landkreis'\n")
  }

  return(as.numeric(lk_ids[[1]]))
}

# HIDDEN FUNKTION
# get the right age label from number
get_age_label_from_number <- function(age_number){
  if(as.integer(age_number) < 0) return("A00-A04")
  if(as.integer(age_number) < 5) return("A00-A04")
  if(as.integer(age_number) < 15) return("A05-A14")
  if(as.integer(age_number) < 34) return("A15-A34")
  if(as.integer(age_number) < 59) return("A35-A59")
  if(as.integer(age_number) < 79) return("A60-A79")
  return("A80+")
}

# USER FUNKTION (?)
# gibt die ganze rki tabelle nach den Kriterien gefiltert zurück
filter_data_by <- function(ages="all", regions="Germany", from="2020-01-01", to=Sys.Date()){
  # TODO: check if ids valid if ids
  ids <- is.numeric(regions)

  data <- rki_data
  # filter the age groups
  if(!all(ages=="all")){
    for(i in 1:length(ages)){
      ages[i] <- get_age_label_from_number(ages[i])
    }
    data %>% filter(Altersgruppe %in% ages) -> data
  }

  # filter the time span
  data %>% filter(Meldedatum >= from, Meldedatum <= to) -> data

  if(ids){
    data %>% filter(IdLandkreis %in% regions) -> data
  }
  else{
    # filter the regions (not robust at the moment)
    rki_data %>% select(Bundesland) %>% unique() %>%
      filter(!Bundesland %in% c("Berlin", "Bremen", "Hamburg")) %>%
      `[[`("Bundesland") %>% tolower() -> bundesländer

    if(all(tolower(regions) %in% bundesländer)){
      data %>% filter(tolower(Bundesland) %in% tolower(regions)) -> data
    }
    else if(!all(tolower(regions)=="germany")){
      for(i in 1:length(regions)) regions[i] <- get_lk_id_from_string(regions[i], print_process=F)
      data %>% filter(IdLandkreis %in% regions) -> data
    }
  }

  return(data)
}
