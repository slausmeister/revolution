# This header has to be run afer data_preparation!!!
sti_id <- function(id){
    # Getting population
    pop <- as.integer(filter(population_lk_data,IdLandkreis==id)[5])
    
    #Getting each case
    rki_id <- rki_data[which(rki_data$IdLandkreis == id),]
    
    
    left_join(tibble(date=days_since_2020),
        rki_id, by=c("date"="Meldedatum")) %>%
        group_by(date) %>%
        summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall), recoveries=sum(AnzahlGenesen)) %>%
      # the days we have no infection data for are days with 0 infections
        mutate(cases=replace_na(cases, 0), deaths=replace_na(deaths, 0),
        recoveries=replace_na(recoveries, 0)) ->
            id_time_series


    # Calculating the rolling average
    n <- length(id_time_series$date)
    sti <- rep(0, n)
    for(i in 1:n){
        for(j in max(1, i-6):i) sti[i] <- sti[i] + id_time_series$cases[j]
    }
    sti <- sti / pop * 1e5

    return(tibble(id_time_series$date,sti, .name_repair = ~ c("Datum", "STI")))
}
