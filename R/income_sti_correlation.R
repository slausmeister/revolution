#TODO: fehlermeldungen!! (mit stopifnot)

#' @export
einkommen_sti_korrelation <- function(path, sheetnr){

    if(missing(path)&&missing(sheetnr)){
        path <- system.file("extdata", "einkommen.xlsx", package="revolution")
        sheetnr <- 11
        rolling_correlation(path, sheetnr)
    }else if(!missing(path) && !missing(sheetnr)){
        rolling_correlation(path, sheetnr)
    }else{
        print("please provide a table and a sheet number")
    }
}

#' @export
auszahlungen_sti_korrelation <- function(path, sheetnr){

    if(missing(path)){
        path <- system.file("extdata", "auszahlungen.xlsx", package="revolution")
        if (!missing(sheetnr)){
        print("please provide a table")
        }
        else{
            sheetnr <- 11
        }
    }

    rolling_correlation(path, sheetnr)
}


rolling_correlation <- function(tablepath, sheet){
# reading data

    tablepath <- "inst/extdata/einkommen.xlsx"
    sheet <- 11

    LKBudget <- readxl::read_excel(tablepath ,sheet=sheet)
    LKBudget <- LKBudget[c("...3","...27" )]

# generating a nice tibble
    LKBudget_tibble <- tibble::as_tibble(tidyr::drop_na(LKBudget))[-1,]
    LKBudget_tibble <- dplyr::rename(LKBudget_tibble, c(id=...3,Einkommen=...27))
    LKBudget_tibble$id <- as.integer(LKBudget_tibble$id)
    LKBudget_tibble$Einkommen <- as.integer(LKBudget_tibble$Einkommen)

    LKBudget_tibble <- LKBudget_tibble[which(LKBudget_tibble$id>100|LKBudget_tibble$id==2),] #Filtering Bundesländer etc.
    LKBudget_tibble$id[LKBudget_tibble$id==2] <- 2000 #Fixing Hamburg

# Filtering for available indection data
    fehlt <- rev.env$population_lk_data[!rev.env$population_lk_data$IdLandkreis %in% LKBudget_tibble$id,]
    vorhanden <- rev.env$population_lk_data[rev.env$population_lk_data$IdLandkreis %in% LKBudget_tibble$id,]

# Calculating Average LKBudget_tibble
    AvgLKBudget_tibble <- merge(vorhanden, LKBudget_tibble, by.x="IdLandkreis", by.y="id")
    AvgLKBudget_tibble <- dplyr::mutate(AvgLKBudget_tibble, AvgEin = Einkommen/Bevölkerung * 10e5)

# Sorting the array for a sanity check
    AvgLKBudget_tibble %>% dplyr::select(c(IdLandkreis,AvgEin)) %>%
            dplyr::arrange(IdLandkreis) -> AvgLKBudget_tibble

# Initialising tibble with ids as columns, sti time series as rows
    general_incidence <- tibble::tibble(sort(unique(sti_id(1001)$date)), .name_repair = ~ c("Datum"))
    for(id in unique(rki_data$IdLandkreis)){
        temp <- sti_id(id)$sti
        general_incidence[as.character(id)] <- temp
        remove(temp)
     }

# setting up empyt vector to be filled with correlation coefficients

    cts <- rep(0,length(unique(sti_id(1001)$date))) # CorrelationTimeSeries

    for(i in 1:length(cts)){
        # selecting all LKs with LKBudget_tibble data
        temp <- dplyr::select(general_incidence,as.character(AvgLKBudget_tibble$IdLandkreis))
        # slicing date
        temp <- as.numeric(temp[i,])
        cts[i] <- cor(AvgLKBudget_tibble$AvgEin, temp)
        remove(temp)

    }

# Building correlation tibble

    return(income_correlation <- tibble::tibble(sti_id(1001)$date,cts))
}
