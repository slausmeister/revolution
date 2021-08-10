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

#' Correlation between payouts and STI
#'
#' This function calculates the correlation between the STI of a county
#' and the average worker compensation (i.e., wages paid out) of the county
#' for each day. It then outputs the correlation coefficient as a time series.
#'
#' The function is based on data from Destatis, sourced
#' from https://www.statistikportal.de/de/vgrdl/ergebnisse-kreisebene/einkommen-kreise
#' on 12.8.2021. You can also load your own table. For this the corresponding
#' path, as well as the sheetnumber of the relevant dataset.
#'
#' @param path Specifies where the relevant table is located. If path is not
#' specified, the table available in the package will be used.
#' @param sheetnr Specifies on which page of the Excel table the relevant
#' data can be found. The parameter must only be specified if an
#' own dataset is used.
#' @return Gives the daily correlation coefficient between wage
#' payments and the
#' and the STI in all counties as a time series in the form of a tibble
#' The "date" column describes the date, and the "cor" column describes the coefficient.
#'
#' @examples
#' auszahlungen_sti_korrelation()
#' auszahlungen_sti_korrelation("expl/path.xlsx", 11)
#'
#' \dontrun{auszahlungen_sti_korrelation(sheetnr=3)}
#' \dontrun{auszahlungen_sti_korrelation(path="custom/path/without/sheetnumber.xlsx")}
#'
#' @family wealth and corona
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
