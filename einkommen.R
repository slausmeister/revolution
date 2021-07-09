source("data_preparation.R")
library("readxl")
library("tidyverse")
Arbeitsnehmerentgeld <- read_excel("xlsx/einkommen.xlsx", sheet=11)
Arbeitsnehmerentgeld <- Arbeitsnehmerentgeld[c("...3","...27" )]

AEG <- as_tibble(drop_na(Arbeitsnehmerentgeld))[-1,]
AEG <- rename(AEG, c(id=...3,Einkommen=...27))
AEG$id <- as.integer(AEG$id)
AEG$Einkommen <- as.integer(AEG$Einkommen)

AEG <- AEG[which(AEG$id>100),]

fehlt <- population_lk_data[!population_lk_data$IdLandkreis %in% AEG$id,]
vorhanden <- population_lk_data[population_lk_data$IdLandkreis %in% AEG$id,]
