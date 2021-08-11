#' Correlation Between Population Density and STI
#'
#' This function can be used to analyze the relation between the population density
#' and average incidence over the pandemic. The user can specify, which districts
#' to analyze.
#' @param regions A vector of district IDs or district names, which will be analyzed.
#' @examples get_pop_density_with_sti(regions=c(8221, "Mannheim"))
#' @return A tibble which contains the average STI over the pandemic with the population density of the district
#' @section Warning:
#' When calling this function with the paramater "all" (the default paramater),
#' the results may take a while, because this is a very expensive computation.
#' 
#' @family population density and STI
#' @export
get_pop_density_with_sti <- function(regions="all"){
  # TODO: Fehlermeldungen
  mean_stis <- c()

  if(all(regions=="all")){
    lk_ids <- rev.env$population_lk_data[["IdLandkreis"]]
  }
  else{
    lk_ids <- c()
    for(lk in regions){
      lk_ids <- c(lk_ids, get_lk_id_from_string(lk, T))
    }
  }

  n <- length(lk_ids)
  for(i in 1:n){
    lk_id <- lk_ids[i]
    print(paste("Progress: ", 100*i/n, "%"))
    mean_stis <- c(mean_stis, mean(get_sti_series_simple(lk_id)))
  }

  tibble::tibble(IdLandkreis=lk_ids, mean_sti=mean_stis) %>%
    dplyr::left_join(rev.env$population_lk_data, by="IdLandkreis") %>%
    dplyr::transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>%
    return()
}

#' Plot Correlation Between Population Density and STI
#'
#' Utilizes a scatter plot to display the data provided by \code{\link{get_pop_density_with_sti}}.
#' The plot has the population density on the x axis and the average STI on the y axis.
#' Additionally, a linear model is fit to the data, to visualize a possible correlation.
#' @examples get_pop_density_with_sti(regions=c(8221, "Mannheim"))
#' @return A plot which contains the average STI over the pandemic with the population density of the district
#' @section Warning:
#' When calling this function with the paramater "all" (the default paramater),
#' the results may take a while, because this is a very expensive computation.
#' 
#' @family population density and STI
#' @export
plot_pop_density_with_linear_model <- function(regions="all"){
  sti_density <- get_pop_density_with_sti(regions)
  stats::lm(mean_sti~pop_density, data=sti_density) -> model

  intercept <- model$coefficients[[1]]
  weight <- model$coefficients[[2]]

  ggplot2::ggplot(NULL, ggplot2::aes(x, y)) +
    ggplot2::geom_point(data=sti_density, ggplot2::aes(x=pop_density, y=mean_sti)) +
    ggplot2::geom_line(ggplot2::aes(x=1:15000, y=intercept + weight * 1:15000), color="blue") +
    ggplot2::xlab("Einwohner pro km²") +
    ggplot2::ylab("Mittlere STI über die Pandemie") %>% return()
}
