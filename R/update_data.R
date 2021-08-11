#' Update RKI data
#' 
#' Loads the newest RKI Covid-19 data via the RKI API, and loads it into the package. 
#' 
#' @param method Specifies via which method the download should happen.
#'  Possible options include "auto", "internal", "libcurl", "wget" and "curl".
#'  If none is chosen, the function defaults to "auto". From experience the
#'  option "auto" works well on Wndows/OSX. For some Linux distributions it's
#'  recommended to choose "wget".
#' 
#' @examples
#' update_rki_data()
#' update_rki_data(method="wget")
#' \dontrun{update_rki_data(method=wget)}
#' 
#' @family update
#' @export
update_rki_data <- function(method){
# checks if we have a backup RKI file
if (""!=system.file("extdata", "RKI_COVID19_old.csv", package = "revolution")) {
# delete file if it exists
file.remove(system.file("extdata", "RKI_COVID19_old.csv", package = "revolution"))
}


# check if we have an old "new" file
if (""!=system.file("extdata", "RKI_COVID19_new.csv", package = "revolution")) {
# delete file if it exists
file.remove(system.file("extdata", "RKI_COVID19_new.csv", package = "revolution"))
}


# download the latest RKI file
download.file(url="https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data",
destfile=file.path(system.file(package="revolution"),"extdata", "RKI_COVID19_new.csv"), method=method)


# checks if we have an old RKI file
if(""!=system.file("extdata", "RKI_COVID19.csv", package = "revolution")){
# if so, make it a backup in case something went wrong
file.rename(system.file("extdata", "RKI_COVID19.csv", package = "revolution"), file.path(system.file(package="revolution"),"extdata", "RKI_COVID19_old.csv"))
}


# check if the download was successful, if so remove the backup file and rename the new file
if(""!=system.file("extdata", "RKI_COVID19_new.csv", package = "revolution")){
file.rename(
            system.file("extdata", "RKI_COVID19_new.csv", package = "revolution"),
            file.path(system.file(package = "revolution"), "extdata", "RKI_COVID19.csv")
            )
if(""!=system.file("extdata", "RKI_COVID19_old.csv", package = "revolution")){
file.remove(
            system.file("extdata", "RKI_COVID19_old.csv", package = "revolution")
            )
}
print("Download successful")
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

#' Update Variants of Concern data
#'
#' This function gets the newest Variants of Concern data via the RKI API
#' and loads it into the package.
#'
#' @param method Specifies via which method the download should happen.
#'  Possible options include "auto", "internal", "libcurl", "wget" and "curl".
#'  If none is chosen, the function defaults to "auto".
#' 
#' @examples
#' update_voc_data()
#' update_voc_data(method="wget")
#' \dontrun{update_voc_data(method=wget)}
#' 
#' @family update
#' @export
update_voc_data <- function(method){

# checks if we have a backup VOC file
if (""!=system.file("extdata", "VOC_VOI_Tabelle_old.xlsx", package = "revolution")) {
# delete file if it exists
file.remove(system.file("extdata", "VOC_VOI_Tabelle_old.xlsx", package = "revolution"))
}


# check if we have an old "new" file
if (""!=system.file("extdata", "VOC_VOI_Tabelle_new.xlsx", package = "revolution")) {
# delete file if it exists
file.remove(system.file("extdata", "VOC_VOI_Tabelle_new.xlsx", package = "revolution"))
}


# download the latest VOC file
download.file(url="https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile",
destfile=file.path(system.file(package="revolution"),"extdata", "VOC_VOI_Tabelle_new.xlsx"), method=method)


# checks if we have an old VOC file
if(""!=system.file("extdata", "VOC_VOI_Tabelle.xlsx", package = "revolution")){
# if so, make it a backup in case something went wrong
file.rename(system.file("extdata", "VOC_VOI_Tabelle.xlsx", package = "revolution"), file.path(system.file(package="revolution"),"extdata", "VOC_VOI_Tabelle_old.xlsx"))
}


# check if the download was successful, if so remove the backup file and rename the new file
if(""!=system.file("extdata", "VOC_VOI_Tabelle_new.xlsx", package = "revolution")){
file.rename(
            system.file("extdata", "VOC_VOI_Tabelle_new.xlsx", package = "revolution"),
            file.path(system.file(package = "revolution"), "extdata", "VOC_VOI_Tabelle.xlsx")
            )
file.remove(
            system.file("extdata", "VOC_VOI_Tabelle_old.xlsx", package = "revolution")
            )
print("Download successful")
} else{
# if the download failed, restore the old file
print("Something went wrong, restoring the old file.")
file.rename(
            system.file("extdata", "VOC_VOI_Tabelle_old.xlsx", package = "revolution"),
            file.path(system.file(package= "revolution"),"extdata", "VOC_VOI_Tabelle.xlsx")
            )
}

}

#' Update Vaccination data
#' 
#' Loads the newest Vaccination Data via the RKI API and loads it into
#' the package.
#'
#' @param method Specifies via which method the download should happen.
#'  Possible options include "auto", "internal", "libcurl", "wget" and "curl".
#'  If none is chosen, the function defaults to "auto".
#' 
#' @examples
#' update_vac_data()
#' update_vac_data(method="wget")
#' \dontrun{update_vac_data(method=wget)}
#' 
#' @family update
#' @export
update_vac_data <- function(method){

# checks if we have a backup file
if (""!=system.file("extdata", "vac_COVID19_old.csv", package = "revolution")) {
# delete file if it exists
file.remove(system.file("extdata", "vac_COVID19_old.csv", package = "revolution"))
}


# check if we have an old "new" file
if (""!=system.file("extdata", "vac_COVID19_new.csv", package = "revolution")) {
# delete file if it exists
file.remove(system.file("extdata", "vac_COVID19_new.csv", package = "revolution"))
}


# download the latest vac file
download.file(url="https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv",
destfile=file.path(system.file(package="revolution"),"extdata", "vac_COVID19_new.csv"), method=method)


# checks if we have an old vac file
if(""!=system.file("extdata", "vac_COVID19.csv", package = "revolution")){
# if so, make it a backup in case something went wrong
file.rename(system.file("extdata", "vac_COVID19.csv", package = "revolution"), file.path(system.file(package="revolution"),"extdata", "vac_COVID19_old.csv"))
}


# check if the download was successful, if so remove the backup file and rename the new file
if(""!=system.file("extdata", "vac_COVID19_new.csv", package = "revolution")){
file.rename(
            system.file("extdata", "vac_COVID19_new.csv", package = "revolution"),
            file.path(system.file(package = "revolution"), "extdata", "vac_COVID19.csv")
            )
file.remove(
            system.file("extdata", "vac_COVID19_old.csv", package = "revolution")
            )
print("Download successful")
} else{
# if the download failed, restore the old file
print("Something went wrong, restoring the old file.")
file.rename(
            system.file("extdata", "vac_COVID19_old.csv", package = "revolution"),
            file.path(system.file(package= "revolution"),"extdata", "vac_COVID19.csv")
            )
}

# import the data for vaccination
rev.env$vax_data <- readr::read_csv(system.file("extdata", "vac_COVID19.csv", package="revolution"),
  show_col_types = FALSE)

}
