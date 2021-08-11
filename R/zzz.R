.onLoad <- function(libname, pkgname){
  # TODO: checken, ob die rki datei schon ex, wenn nicht, runterladen!!!!!!
  # TODO: package dependencies

  unlockBindings(package::revolution)

  assign("rev.env", new.env(parent = emptyenv()), envir=topenv())
  # rev.env <<- new.env(parent = emptyenv())

  # import the population of 'Landkreis' with the given csv
  rev.env$population_lk_data <- readr::read_csv(system.file("extdata", "population_lk.csv", package="revolution"),show_col_types = FALSE)

  # transform the 'IdLandkreis' column to a numeric
  rev.env$population_lk_data %>% dplyr::mutate(IdLandkreis=as.numeric(IdLandkreis)) -> rev.env$population_lk_data

  # calculate the total german population
  rev.env$population_lk_data %>% dplyr::summarise(n=sum(Bevölkerung)) %>%
    `[[`(1) -> total_population_germany

  # import the data for vaccination
  rev.env$vax_data <- readr::read_csv(system.file("extdata", "vac_COVID19.csv", package="revolution"),
    show_col_types = FALSE)

  # import the population per age group in 2020
  readr::read_csv(system.file("extdata", "population_age.csv", package="revolution"),show_col_types = FALSE) %>%
    dplyr::group_by(Altersgruppe, Jahr) %>% dplyr::summarise(Bevölkerung=sum(Bevölkerung)) ->
    rev.env$population_age_data

  ### rki covid data:
  # import raw rki data
  if(system.file("extdata", "RKI_COVID19.csv", package = "revolution")==""){
    print("No RKI data yet, please provide one by calling the function 'update_rki_data()'")
    print("Hint: Check the documentation of that function before calling it, to specify the download method!")
  }
  else{
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
    rki_data %>% dplyr::select(-FID) -> rki_data

    assign("rki_data", rki_data, envir=topenv())

    rev.env$days_since_2020 <- seq(as.Date("2020-01-01"), as.Date(Sys.Date()), by="days")
  }

}
