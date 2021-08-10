#'Traffic accidents of 2019 and 2020
#'
#'\code{get_accidents_data()} is used to create a tibble of the monthly number of traffic accidents during 2019-2020.
#'
#'@examples get_accidents_data()
#'@export
get_accidents_data <- function(){
  readr::read_csv(system.file("extdata", "unfaelle_jahre.csv", package="revolution"),
    show_col_types = FALSE) %>%
      dplyr::mutate(Jahr=as.character(Jahr)) %>%
      return()
}

#'Comparison between traffic accidents and 7-day-incidence
#'
#'\code{plot_accidents_with_sti()} is used to create a plot of the monthly number of traffic accidents in comparison with
#'the 7-day-incidence in Germany. Obviously, the number of accidents drastically sank when COVID-19 reached Germany.
#'
#'@examples plot_accidents_with_sti()
#'@export
plot_accidents_with_sti <- function(){
  unfaelle_data <- get_accidents_data()

  ggplot2::ggplot(data=unfaelle_data, ggplot2::aes(x=Monat, y=Unfaelle, fill=factor(Jahr))) +
    ggplot2::geom_col(position=position_dodge()) +
    ggplot2::scale_x_discrete(limits=unfaelle_data$Monat[1:12]) +
    ggplot2::labs(fill = "Jahr") ->
    unfaelle_plt

  ggplot2::ggplot(data=get_sti_series_for(to="2020-12-31"), ggplot2::aes(x=date, y=sti)) +
    ggplot2::geom_line() ->
    plt_germany

  cowplot::plot_grid(plotlist = list(plt_germany, unfaelle_plt), nrow=2) %>%
    suppressMessages() %>%
    return()
}

#'Kilometers per person in traffic
#'
#'\code{get_public_transportation_data} is used to create a tibble that contains quarterly data of kilometers per person of the years 2019 and 2020.
#'
#'@examples get_public_transportation_data
#' @export
get_public_transportation_data <- function(){
  readr::read_csv(system.file("extdata", "oepnv_jahre.csv", package="revolution"),
    show_col_types = FALSE) %>% return()
}

#'Comparison between traffic kilometers per person and 7-day-incidence
#'
#'\code{plot_public_transportation_with_sti} is used to create a plot of the person-kilometers in comparison with
#'the 7-day-incidence in Germany. Obviously, the number of kilometers drastically sank when COVID-19 reached Germany.
#'
#'@examples plot_public_transportation_with_sti
#' @export
plot_public_transportation_with_sti <- function(){
  oepnv_data <- get_public_transportation_data()
  ggplot2::ggplot(data=oepnv_data, ggplot2::aes(x=Quartal, y=Personenkilometer, fill=factor(Jahr))) +
    ggplot2::geom_col(position=position_dodge()) +
    ggplot2::scale_x_discrete(limits=oepnv_data$Quartal[1:4]) +
    ggplot2::labs(fill = "Jahr") ->
    oepnv_plt

  ggplot2::ggplot(data=get_sti_series_for(to="2020-12-31"), ggplot2::aes(x=date, y=sti)) +
    ggplot2::geom_line() ->
    plt_germany

  cowplot::plot_grid(plotlist = list(plt_germany, oepnv_plt), nrow=2) %>%
    suppressMessages() %>%
    return()
}
