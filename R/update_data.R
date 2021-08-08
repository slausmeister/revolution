#' @export
update_rki_data <- function(method){
  # the end user needs to specify a download method, because it depends on the system
  # on Manjaro Linux, "wget" seems to work
  # see ?download.file for all possible methods

  # checks if we have a backup RKI file
  if (file.exists("inst/extdata/RKI_COVID19_old.csv")) {
    # delete file if it exists
    file.remove("inst/extdata/RKI_COVID19_old.csv")
  }


  # check if we have an old "new" file
  if (file.exists("inst/extdata/RKI_COVID19_new.csv")) {
    # delete file if it exists
    file.remove("inst/extdata/RKI_COVID19_new.csv")
  }


  # download the latest RKI file
  download.file(url="https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data",
   destfile="inst/extdata/RKI_COVID19_new.csv", method=method)


  # checks if we have an old RKI_file
  if(file.exists("inst/extdata/RKI_COVID19.csv")){
    # if so, make it a backup in case something went wrong
    file.rename("inst/extdata/RKI_COVID19.csv", "inst/extdata/RKI_COVID19_old.csv")
  }


  # check if the download was successful, if so remove the backup file and rename the new file
  if(file.exists("inst/extdata/RKI_COVID19_new.csv")){
    file.rename("inst/extdata/RKI_COVID19_new.csv", "inst/extdata/RKI_COVID19.csv")
    file.remove("inst/extdata/RKI_COVID19_old.csv")
    print("Download successful")
  } else{
    # if the download failed, restore the old file
    print("Something went wrong, restoring the old file.")
    file.rename("inst/extdata/RKI_COVID19_old.csv", "inst/extdata/RKI_COVID19.csv")
  }

}

#' @export
update_voc_data <- function(method){
  # the end user needs to specify a download method, because it depends on the system
  # on Manjaro Linux, "wget" seems to work
  # see ?download.file for all possible methods

  # checks if we have a backup VOC file
  if (file.exists("inst/extdata/VOC_VOI_Tabelle_old.xlsx")) {
    # delete file if it exists
    file.remove("inst/extdata/VOC_VOI_Tabelle_old.xlsx")
  }


  # check if we have an old "new" file
  if (file.exists("inst/extdata/VOC_VOI_Tabelle_new.xlsx")) {
    # delete file if it exists
    file.remove("inst/extdata/VOC_VOI_Tabelle_new.xlsx")
  }


  # download the latest VOC file
  download.file(url="https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile",
   destfile="inst/extdata/VOC_VOI_Tabelle_new.xlsx", method=method)


  # checks if we have an old VOC file
  if(file.exists("inst/extdata/VOC_VOI_Tabelle.xlsx")){
    # if so, make it a backup in case something went wrong
    file.rename("inst/extdata/VOC_VOI_Tabelle.xlsx", "inst/extdata/VOC_VOI_Tabelle_old.xlsx")
  }


  # check if the download was successful, if so remove the backup file and rename the new file
  if(file.exists("inst/extdata/VOC_VOI_Tabelle_new.xlsx")){
    file.rename("inst/extdata/VOC_VOI_Tabelle_new.xlsx", "inst/extdata/VOC_VOI_Tabelle.xlsx")
    file.remove("inst/extdata/VOC_VOI_Tabelle_old.xlsx")
    print("Download successful")
  } else{
    # if the download failed, restore the old file
    print("Something went wrong, restoring the old file.")
    file.rename("inst/extdata/VOC_VOI_Tabelle_old.xlsx", "inst/extdata/VOC_VOI_Tabelle.xlsx")
  }

}
