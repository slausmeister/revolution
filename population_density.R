source("sti.R")

get_pop_density_with_sti <- function(){
  mean_stis <- c()
  for(lk_id in population_lk_data[["IdLandkreis"]]){
    mean_stis <- c(mean_stis, mean(get_sti_series_by_id(lk_id)[["sti"]]))
  }

  population_lk_data %>%
    transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>% return()
}


# model <- lm(mean_sti~pop_density, data=sti_density)
# intercept <- model$coefficients[[1]]
# weight <- model$coefficients[[2]]
#
# library(ggplot2)
#
# print(ggplot(NULL, aes(x, y)) +
#   geom_point(data=sti_density, aes(x=pop_density, y=mean_sti)) +
#   geom_line(aes(x=1:15000, y=intercept + weight * 1:15000), color="blue") +
#   xlab("Einwohner pro km²") +
#   ylab("Mittlere STI von November bis Januar"))
