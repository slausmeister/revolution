source("data_preparation.R")
sti_id <- function(id){
    # Getting population
    pop <- as.integer(filter(population_lk_data,IdLandkreis==id)[5])
    
    #Getting each case
    cases <- select(rki_data,c(IdLandkreis,Meldedatum)) 
    cases <- cases[which(cases$IdLandkreis == id),]

    # Bulding frequency table. We thus have cases per date
    cases <- as.data.frame(table(cases$Meldedatum))

    # Calculating the rowling average
    n <- length(cases$Freq)
    sti <- rep(0, n)
    for(i in 1:n){
        for(j in max(1, i-6):i) sti[i] <- sti[i] + cases$Freq[j]
    }
    sti <- sti / pop * 1e5

    return(tibble(cases$Var1,sti, .name_repair = ~ c("Datum", "STI")))
}
