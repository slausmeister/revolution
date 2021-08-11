#' @details
#' The core functions of the package include \code{get_data_for()},
#' \code{get_sti_series_for()}, \code{get_vaccination_data()}, \code{variant_case_time_series()}
#' and \code{plot_excess_mortality()} with their respective plot functions for
#' visualisation.
#'
#' The functions can roughly be divided into 2 categories: Plot functions, and generators
#' of time series.
#'
#' Plot functions look at a certain aspact of the COVID-19 pandemic, may it be vaccinations, variants,
#' exess deaths, seven day incidences for different regions, and much more.
#'
#' Generator functions generate the underlaying data which is to be plotted. These functions are usefull
#' if the user wants to build tools ontop of the data.
#'
#' The package uses a lot of external data. The main data set is the RKI COVED-19 data API, which 
#' can be found under \url{https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/about}
#' In addition the package draws on vaccination data aquired via \url{https://impfdashboard.de/daten} provided
#' by the Paul Ehrlich Institute, and variants of concern data provided by the RKI. Since all these data sets
#' have to be updated to understand the current stage of the pandemic, the package provides an update function
#' for each of these data sets (\code{update_rki_data()},\code{update_vac_data()}, and \code{update_voc_data()}).
#' Please refere to the documentation of these functions, since especially the RKI data set is rather large.
#' 
#' Other data used in the package includes economic data, as well as mobility data. These are further explained
#' in their respective documentation (of the functions calling on them).
#'
#'
#' @references
#' RKI Data: \url{https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/about}
#'
#' Vaccination Data: \url{https://impfdashboard.de/daten}
#'
#' Variants Data: \url{https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile}
#'
#' Every other dataset in this package is not up to date and comes from the
#' "Statistisches Bundesamt" (\url{https://www.destatis.de/DE/Home/_inhalt.html}),
#' but was modified to fit our purposes.
#'
#'
#'
#'
#' @keywords internal

"_PACKAGE"
