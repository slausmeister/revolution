rolling_correlation <- function(tablepath){
    library("readxl")
    source("sti_id.R")

# reading data
    LKBudget <- read_excel(tablepath, sheet=11)
    LKBudget <- LKBudget[c("...3","...27" )]

# generating a nice tibble
    LKBudget_tibble <- as_tibble(drop_na(LKBudget))[-1,]
    LKBudget_tibble <- rename(LKBudget_tibble, c(id=...3,Einkommen=...27))
    LKBudget_tibble$id <- as.integer(LKBudget_tibble$id)
    LKBudget_tibble$Einkommen <- as.integer(LKBudget_tibble$Einkommen)

    LKBudget_tibble <- LKBudget_tibble[which(LKBudget_tibble$id>100|LKBudget_tibble$id==2),] #Filtering Bundesländer etc.
    LKBudget_tibble$id[LKBudget_tibble$id==2] <- 2000 #Fixing Hamburg

# Filtering for available indection data
    fehlt <- population_lk_data[!population_lk_data$IdLandkreis %in% LKBudget_tibble$id,]
    vorhanden <- population_lk_data[population_lk_data$IdLandkreis %in% LKBudget_tibble$id,]

# Calculating Average LKBudget_tibble
    AvgLKBudget_tibble <- merge(vorhanden, LKBudget_tibble, by.x="IdLandkreis", by.y="id")
    AvgLKBudget_tibble <- mutate(AvgLKBudget_tibble, AvgEin = Einkommen/Bevölkerung * 10e5)

# Sorting the array for a sanity check
    AvgLKBudget_tibble %>% select(c(IdLandkreis,AvgEin)) %>%
            arrange(IdLandkreis) -> AvgLKBudget_tibble

# Initialising tibble with ids as columns, sti time series as rows
    general_incidence <- tibble(sort(unique(sti_id(1001)$Datum)), .name_repair = ~ c("Datum"))
    for(id in unique(rki_data$IdLandkreis)){
        temp <- sti_id(id)$STI
        general_incidence[as.character(id)] <- temp
        remove(temp)
     }

# setting up empyt vector to be filled with correlation coefficients

    cts <- rep(0,length(unique(sti_id(1001)$Datum))) # CorrelationTimeSeries

    for(i in 1:length(cts)){
        # selecting all LKs with LKBudget_tibble data
        temp <- select(general_incidence,as.character(AvgLKBudget_tibble$IdLandkreis))
        # slicing date
        temp <- as.numeric(temp[i,])
        cts[i] <- cor(AvgLKBudget_tibble$AvgEin, temp)
        remove(temp)
    }

# Building correlation tibble

    return(income_correlation <- tibble(sti_id(1001)$Datum,cts))
}
