source("data_preparation.R")
source("sti_id.R")
library("readxl")

# reading data
Arbeitsnehmerentgeld <- read_excel("xlsx/einkommen.xlsx", sheet=11)
Arbeitsnehmerentgeld <- Arbeitsnehmerentgeld[c("...3","...27" )]

# generating a nice tibble
AEG <- as_tibble(drop_na(Arbeitsnehmerentgeld))[-1,]
AEG <- rename(AEG, c(id=...3,Einkommen=...27))
AEG$id <- as.integer(AEG$id)
AEG$Einkommen <- as.integer(AEG$Einkommen)

AEG <- AEG[which(AEG$id>100|AEG$id==2),] #Filtering Bundesländer etc.
AEG$id[AEG$id==2] <- 2000 #Fixing Hamburg

# Filtering for available indection data
fehlt <- population_lk_data[!population_lk_data$IdLandkreis %in% AEG$id,]
vorhanden <- population_lk_data[population_lk_data$IdLandkreis %in% AEG$id,]

# Calculating Average AEG
AAEG <- merge(vorhanden, AEG, by.x="IdLandkreis", by.y="id")
AAEG <- mutate(AAEG, AvgEin = Einkommen/Bevölkerung * 10e5)

# Sorting the array for a sanity check
AAEG %>% select(c(IdLandkreis,AvgEin)) %>%
        arrange(IdLandkreis) -> AAEG

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
    # selecting all LKs with AEG data
    temp <- select(general_incidence,as.character(AAEG$IdLandkreis))
    # slicing date
    temp <- as.numeric(temp[i,])
    cts[i] <- cor(AAEG$AvgEin, temp)
    remove(temp)
}

# Building correlation tibble

income_correlation <- tibble(sti_id(1001)$Datum,cts)

# Plotting

plot(income_correlation)
