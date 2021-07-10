source("data_preparation.R")
sti_id <- function(id){
    pop <- as.integer(filter(population_lk_data,IdLandkreis==id)[5])

    cases <- select(rki_data,c(IdLandkreis,Meldedatum)) 
    cases <- cases[which(cases$IdLandkreis == id),]


    cases <- as.data.frame(table(cases$Meldedatum))

    n <- length(cases$Freq)
    sti <- rep(0, n)
    for(i in 1:n){
        for(j in max(1, i-6):i) sti[i] <- sti[i] + cases$Freq[j]
    }
    return(tibble(cases$Var1,sti / pop * 1e5, .name_repair = ~ c("Datum", "STI")))
}
