#' @export
update_rki_data <- function(method){
# the end user needs to specify a download method, because it depends on the system
# on Manjaro Linux, "wget" seems to work
# see ?download.file for all possible methods

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
file.remove(
            system.file("extdata", "RKI_COVID19_old.csv", package = "revolution")
            )
print("Download successful")
} else{
# if the download failed, restore the old file
print("Something went wrong, restoring the old file.")
file.rename(
            system.file("extdata", "RKI_COVID19_old.csv", package = "revolution"),
            file.path(system.file(package= "revolution"),"extdata", "RKI_COVID19.csv")
            )
}

}
#' @export
update_voc_data <- function(method){
# the end user needs to specify a download method, because it depends on the system
# on Manjaro Linux, "wget" seems to work
# see ?download.file for all possible methods

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

#' @export
update_vac_data <- function(method){
# the end user needs to specify a download method, because it depends on the system
# on Manjaro Linux, "wget" seems to work
# see ?download.file for all possible methods

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

}
