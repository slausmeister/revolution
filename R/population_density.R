# returnt ein tibble mit der bevdichte eines landkreises und dessen durschnitttliche
# sti über die pandemie
<<<<<<< HEAD
=======
#' @export
>>>>>>> c6f1cea897ca7446a6ca6f880a11b7e5b72452c5
get_pop_density_with_sti <- function(regions="all"){
  mean_stis <- c()

  if(all(regions=="all")){
<<<<<<< HEAD
    lk_ids <- population_lk_data[["IdLandkreis"]]
=======
    lk_ids <- rev.env$population_lk_data[["IdLandkreis"]]
>>>>>>> c6f1cea897ca7446a6ca6f880a11b7e5b72452c5
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

<<<<<<< HEAD
  population_lk_data %>% filter(IdLandkreis %in% lk_ids) %>%
    transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>% return()
=======
  rev.env$population_lk_data %>%
    dplyr::filter(IdLandkreis %in% lk_ids) %>%
    dplyr::transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>%
    return()
}

# plottet das obige tibble (ein scatter plot mit x Achse bevdichte und y Achse durchschntl sti)
# und fittet noch ein lineares durch, um einen zusammenhang festzustellen
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
>>>>>>> c6f1cea897ca7446a6ca6f880a11b7e5b72452c5
}
