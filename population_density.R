source("sti.R")

# USER funktion
# returnt ein tibble mit der bevdichte eines landkreises und dessen durschnitttliche
# sti über die pandemie
get_pop_density_with_sti <- function(regions="all"){
  mean_stis <- c()

  if(all(regions=="all")){
    lk_ids <- population_lk_data[["IdLandkreis"]]
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

  population_lk_data %>% filter(IdLandkreis %in% lk_ids) %>%
    transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>% return()
}

library(ggplot2)

# USER funktion
# plottet das obige tibble (ein scatter plot mit x Achse bevdichte und y Achse durchschntl sti)
# und fittet noch ein lineares durch, um einen zusammenhang festzustellen
plot_pop_density_with_linear_model <- function(regions="all"){
  sti_density <- get_pop_density_with_sti(regions)
  lm(mean_sti~pop_density, data=sti_density) -> model

  intercept <- model$coefficients[[1]]
  weight <- model$coefficients[[2]]

  ggplot(NULL, aes(x, y)) +
    geom_point(data=sti_density, aes(x=pop_density, y=mean_sti)) +
    geom_line(aes(x=1:15000, y=intercept + weight * 1:15000), color="blue") +
    xlab("Einwohner pro km²") +
    ylab("Mittlere STI über die Pandemie") -> plt

  return(plt)
}
