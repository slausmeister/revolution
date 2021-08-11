#' Manually load RKI data
#' 
#' This function manually laods an RKI file into the package. The user has to provide a valid 
#' path to a valid RKI file. If a wrong file is loaded, the package cannot build its internal 
#' data. Thus we recommend using the automatic \code{update_rki_data()} function. 
#'
#' Please be aware of the front-slash/back-slash conventions of your operating system.
#' 
#' @param path A path to a file which is to be loaded in. The bath has to be in parentheses.
#' @family update 
#' @examples
#' manual_load_rki_data("~/Downloads/RKI.csv")
#' \dontrun{manual_load_rki_data()}
#' \dontrun{manual_load_rki_data(~/Downloads/RKI.csv)}
#' @export
manual_load_rki_data <- function(path){
if(!hasArg(path)){
    print("please provide a path to an rki file as a function arguement")
    return()
}

# checks if we have an old RKI file
if(""!=system.file("extdata", "RKI_COVID19.csv", package = "revolution")){
# if so, make it a backup in case something went wrong
file.rename(system.file("extdata", "RKI_COVID19.csv", package = "revolution"), file.path(system.file(package="revolution"),"extdata", "RKI_COVID19_old.csv"))
}


# check if the download was successful, if so remove the backup file and rename the new file
if(hasArg(path)){
file.copy(
           from = path,
           to = file.path(system.file(package = "revolution"), "extdata", "RKI_COVID19.csv")
            )
if(""!=system.file("extdata", "RKI_COVID19_old.csv", package = "revolution")){
file.remove(
            system.file("extdata", "RKI_COVID19_old.csv", package = "revolution")
            )}
print("Load successful")
} else{
# if the download failed, restore the old file
print("Something went wrong, restoring the old file.")
file.rename(
            system.file("extdata", "RKI_COVID19_old.csv", package = "revolution"),
            file.path(system.file(package= "revolution"),"extdata", "RKI_COVID19.csv")
            )
}

### rki covid data:
# import raw rki data
rki_data <- readr::read_csv(system.file("extdata", "RKI_COVID19.csv", package="revolution"),show_col_types = FALSE)

# the 'Neuer' and 'Datenstand' columns compare this dataset to the one from yesterday,
# which makes it useless for our research
rki_data %>% dplyr::select(-NeuerFall, -NeuerTodesfall, -NeuGenesen, -Datenstand) ->
  rki_data

# change the column type of 'Meldedatum' and 'Refdatum' to date
rki_data %>% dplyr::mutate(Meldedatum=as.Date(Meldedatum), Refdatum=as.Date(Refdatum)) -> rki_data

# in most cases, 'Altersgruppe2' is not available
rki_data %>% dplyr::select(-Altersgruppe2) -> rki_data

# transform the 'IdLandkreis' column to a numeric
rki_data %>% dplyr::mutate(IdLandkreis=as.numeric(IdLandkreis)) -> rki_data

# we will use the 'Landkreis' column from the other csv, because of normalization
rki_data %>% dplyr::select(-Landkreis) %>%
  dplyr::left_join(dplyr::select(readr::read_csv(system.file("extdata","population_lk.csv", package="revolution"),show_col_types = FALSE), IdLandkreis, Landkreis), by="IdLandkreis") ->
  rki_data

# 'IdBundesland' is a part of 'IdLandkreis' and we have the 'Bundesland' column
rki_data %>% dplyr::select(-IdBundesland) -> rki_data

# 'FID' is the case id, which is useless for our research
rki_data %>% dplyr::select(-FID) ->> rki_data
# assign("rki_data", rki_data1, envir=as.environment("package:revolution"))
}
