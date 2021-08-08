get_latest_rki_data <- function(method){
  # the end user needs to specify a download method, because it depends on the system
  # on Manjaro Linux, "wget" seems to work
  # see ?download.file for all possible methods

  # checks if we have a backup RKI file
  if (file.exists("csvs/RKI_COVID19_old.csv")) {
    # delete file if it exists
    file.remove("csvs/RKI_COVID19_old.csv")
  }


  # check if we have an old "new" file
  if (file.exists("csvs/RKI_COVID19_new.csv")) {
    # delete file if it exists
    file.remove("csvs/RKI_COVID19_new.csv")
  }


  # download the latest RKI file
  download.file(url="https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data",
   destfile="csvs/RKI_COVID19_new.csv", method=method)


  # checks if we have an old RKI_file
  if(file.exists("csvs/RKI_COVID19.csv")){
    # if so, make it a backup in case something went wrong
    file.rename("csvs/RKI_COVID19.csv", "csvs/RKI_COVID19_old.csv")
  }


  # check if the download was successful, if so remove the backup file and rename the new file
  if(file.exists("csvs/RKI_COVID19_new.csv")){
    file.rename("csvs/RKI_COVID19_new.csv", "csvs/RKI_COVID19.csv")
    file.remove("csvs/RKI_COVID19_old.csv")
    print("Download successful")
  } else{
    # if the download failed, restore the old file
    print("Something went wrong, restoring the old file.")
    file.rename("csvs/RKI_COVID19_old.csv", "csvs/RKI_COVID19.csv")
  }
}
