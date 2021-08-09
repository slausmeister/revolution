# gibt einen tibble mit unfällen pro monat in 2019 und 2020 aus
#' @export
get_accidents_data <- function(){
  readr::read_csv(system.file("extdata", "unfaelle_jahre.csv", package="revolution"),
    show_col_types = FALSE) %>%
      dplyr::mutate(Jahr=as.character(Jahr)) %>%
      return()
}

# plottet die sti und darunter den vergleich der unfälle in 2019 und 2020
#' @export
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
    return()
}

# returnt die Personenkilometer des öpvs über 2019 und 2020 nach Quartal und Bundesland
#' @export
get_public_transportation_data <- function(){
  readr::read_csv(system.file("extdata", "oepnv_jahre.csv", package="revolution"),
    show_col_types = FALSE) %>% return()
}

# plottet die obigen Daten im Vergleich 2019 2020 und als referenz noch mit sti
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
    return()
}
