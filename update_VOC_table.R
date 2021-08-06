get_latest_voc_data <- function(method){
  # the end user needs to specify a download method, because it depends on the system
  # on Manjaro Linux, "wget" seems to work
  # see ?download.file for all possible methods

  # checks if we have a backup VOC file
  if (file.exists("xlsx/VOC_VOI_Tabelle_old.xlsx")) {
    # delete file if it exists
    file.remove("xlsx/VOC_VOI_Tabelle_old.xlsx")
  }


  # check if we have an old "new" file
  if (file.exists("xlsx/VOC_VOI_Tabelle_new.xlsx")) {
    # delete file if it exists
    file.remove("xlsx/VOC_VOI_Tabelle_new.xlsx")
  }


  # download the latest VOC file
  download.file(url="https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile",
   destfile="xlsx/VOC_VOI_Tabelle_new.xlsx", method=method)


  # checks if we have an old VOC file
  if(file.exists("xlsx/VOC_VOI_Tabelle.xlsx")){
    # if so, make it a backup in case something went wrong
    file.rename("xlsx/VOC_VOI_Tabelle.xlsx", "xlsx/VOC_VOI_Tabelle_old.xlsx")
  }


  # check if the download was successful, if so remove the backup file and rename the new file
  if(file.exists("xlsx/VOC_VOI_Tabelle_new.xlsx")){
    file.rename("xlsx/VOC_VOI_Tabelle_new.xlsx", "xlsx/VOC_VOI_Tabelle.xlsx")
    file.remove("xlsx/VOC_VOI_Tabelle_old.xlsx")
    print("Download successful")
  } else{
    # if the download failed, restore the old file
    print("Something went wrong, restoring the old file.")
    file.rename("xlsx/VOC_VOI_Tabelle_old.xlsx", "xlsx/VOC_VOI_Tabelle.xlsx")
  }

}
