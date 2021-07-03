source("incidence_lk.R")

mean_stis <- c()
for(lk in population_lk_data[["Landkreis"]]){
  mean_stis <- c(mean_stis, mean(calc_sti_lk(lk, print_name=F)))
}
# 3. Welle: [306:397]

population_lk_data %>%
  transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) ->
  sti_density

model <- lm(mean_sti~pop_density, data=sti_density)
intercept <- model$coefficients[[1]]
weight <- model$coefficients[[2]]

print(ggplot(NULL, aes(x, y)) +
  geom_point(data=sti_density, aes(x=pop_density, y=mean_sti)) +
  geom_line(aes(x=1:15000, y=intercept + weight * 1:15000), color="blue") +
  xlab("Einwohner pro km²") +
  ylab("Mittlere STI von November bis Januar"))
