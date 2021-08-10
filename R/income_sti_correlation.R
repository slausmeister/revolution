#' Correlation between income and STI 
#'
#' This function calculates the correlation between average income and seven day
#' incidence in all administrative regions of germany on a dayly basis.
#' 
#' The funciton is based on data by Destatis aquired via
#' https://www.statistikportal.de/de/vgrdl/ergebnisse-kreisebene/einkommen-kreise
#' on the 12.8.2021. The user can load his own income data into the function, as
#' described in the parameters.
#' 
#' @param path Specifies the path to a giben excel sheet. If left empty the
#'   function defaults to the data provided by the pachage.
#' @param sheetnr Specifies the sheet of the excel table, where the relevant data can
#'   be found. It only has to be specified, if custom data is used.
#'
#' @return The function returns a tibble carring a "date" column, and a "cor" column.
#'   The "cor" column describes the correlation for a given date.
#'
#' @examples
#' income_sti_correlation()
#' income_sti_correlation("expl/path.xlsx", 11)
#' 
#' \dontrun{income_sti_correlation(sheetnr=3)}
#' \dontrun{income_sti_correlation(path="custom/path/without/sheetnumber.xlsx")}
#' 
#' @export
income_sti_correlation <- function(path, sheetnr){

    if(missing(path)){
        path <- system.file("extdata", "einkommen.xlsx", package="revolution")
        if (!missing(sheetnr)){
        print("please provide a table")
        }
        else{
            sheetnr <- 11
        }
    }

    return(rolling_correlation(path, sheetnr))
}

#' Korrelation zwischen Auszahlungen und STI
#' 
#' Diese Funktion berechnet die Korellation zwischen der STI eines Landkreises
#' und dem durchschnittlichen Arbeiterentgeld (i.e. ausgezahlter Lohn) des Landkreises
#' für jeden Tag aus. Anschließend wird der Korrelationskoeffizient als Zeitreihe ausgegeben.
#' 
#' Die Funktion basiert auf Daten des Destatis, bezogen
#' von https://www.statistikportal.de/de/vgrdl/ergebnisse-kreisebene/einkommen-kreise
#' am 12.8.2021. Man kann auch eine eigene Tabelle einladen. Hierfür muss der entsprechende
#' Pfad, als auch die Sheetnumber des relevanten Datensatz angegeben werden.
#' 
#' @param path Gibt an, wo sich die relevante Tabelle befindet. Wird path nicht
#'   angegeben, so wird die im Package vorhandene Tabelle benutzt.
#' @param sheetnr Gibt an, auf welcher Seite der Excel Tabelle die releanten
#'   Daten zu finden sind. Der Parameter muss nur angegeben werden, falls ein
#'   eigener Datensatz verwendet wird.
#' @return Gibt den täglichen Korrelationskoeffizienten zwischen Lohnauszahlungen
#'   und der STI in allen Landkreisen als Zeitreihe in der Form eines Tibbles
#'   zurück. Die Spalte "date" beschreibt das Datum, die Spalte "cor" den Koeffizienten.
#'
#' @examples
#' auszahlungen_sti_korrelation()
#' auszahlungen_sti_korrelation("expl/path.xlsx", 11)
#' 
#' \dontrun{auszahlungen_sti_korrelation(sheetnr=3)}
#' \dontrun{auszahlungen_sti_korrelation(path="custom/path/without/sheetnumber.xlsx")}
#'
#' @family Wohlstand und Corona
# export
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

    return(rolling_correlation(path, sheetnr))
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

    correlation <- tibble::tibble(sti_id(1001)$date, cts)
    
    correlation %>%
        dplyr::rename( cor = cts) %>%
        dplyr::rename( date = "sti_id(1001)$date") %>%
        return()
}

#' East Germany 
#'
#' Generates an STI time series for the new federal states.
#'
#' @return A tibble with a "date" and a "mean_sti" column.
#' @family East-West
#' @export
east_germany <- function(){
    ids <- rev.env$population_lk_data$IdLandkreis[rev.env$population_lk_data$IdLandkreis>=12000]
    temp <- tibble::tibble(sti_id(1001)$date)
    for(id in ids){
        temp[as.character(id)] <- sti_id(id)$sti
    }
    
    temp %>%
        dplyr::transmute(mean_sti = rowMeans(dplyr::select(., !"sti_id(1001)$date"))) %>%
        dplyr::mutate(date = sti_id(1001)$date, .before = 1) %>%
        return()
}

#' West Germany 
#'
#' Generates an STI time series for the old federal states.
#'
#' @return A tibble with a "date" and a "mean_sti" column.
#' @family East-West
#' @export
west_germany <- function(){
    ids <- rev.env$population_lk_data$IdLandkreis[rev.env$population_lk_data$IdLandkreis<=12000]
    temp <- tibble::tibble(sti_id(1001)$date)
    for(id in ids){
        temp[as.character(id)] <- sti_id(id)$sti
    }
    
    temp %>%
        dplyr::transmute(mean_sti = rowMeans(dplyr::select(., !"sti_id(1001)$date"))) %>%
        dplyr::mutate(date = sti_id(1001)$date, .before = 1) %>%
        return()
}
