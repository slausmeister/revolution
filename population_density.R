source("sti.R")

get_pop_density_with_sti <- function(){
  mean_stis <- c()
  lk_ids <- population_lk_data[["IdLandkreis"]]
  n <- length(lk_ids)
  for(i in 1:n){
    lk_id <- lk_ids[i]
    if(i %% 10 == 0){
      print(paste("Progress: ", 100*i/n, "%"))
    }
    mean_stis <- c(mean_stis, mean(get_sti_series_simple(lk_id)))
  }

  population_lk_data %>%
    transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>% return()
}

library(ggplot2)

plot_pop_density_with_linear_model <- function(){
  sti_density <- get_pop_density_with_sti()
  lm(mean_sti~pop_density, data=sti_density) -> model

  intercept <- model$coefficients[[1]]
  weight <- model$coefficients[[2]]

  ggplot(NULL, aes(x, y)) +
    geom_point(data=sti_density, aes(x=pop_density, y=mean_sti)) +
    geom_line(aes(x=1:15000, y=intercept + weight * 1:15000), color="blue") +
    xlab("Einwohner pro km²") +
    ylab("Mittlere STI von November bis Januar") -> plt

  return(plt)
}
