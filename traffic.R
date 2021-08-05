source("sti.R")
library(ggplot2)

get_accidents_data <- function(){
  read_csv("csvs/unfaelle_jahre.csv") %>% mutate(Jahr=as.character(Jahr)) %>% return()
}

plot_accidents <- function(){
  unfaelle_data <- get_accidents_data()

  ggplot(data=unfaelle_data, aes(x=Monat, y=Unfaelle, fill=factor(Jahr))) +
    geom_col(position=position_dodge()) + scale_x_discrete(limits=unfaelle_data$Monat[1:12]) +
    labs(fill = "Jahr") -> unfaelle_plt

  ggplot(data=get_sti_series_for(to="2020-12-31"), aes(x=date, y=sti)) +
    geom_line() -> plt_germany

  cowplot::plot_grid(plotlist = list(plt_germany, unfaelle_plt), nrow=2) %>%
    return()
}

get_public_transportation_data <- function(){
  read_csv("csvs/oepnv_jahre.csv") %>% return()
}

plot_public_transportation <- function(){
  ggplot(data=oepnv_data, aes(x=Quartal, y=Personenkilometer, fill=factor(Jahr))) +
    geom_col(position=position_dodge()) + scale_x_discrete(limits=oepnv_data$Quartal[1:4]) +
    labs(fill = "Jahr") -> oepnv_plt

  ggplot(data=get_sti_series_for(to="2020-12-31"), aes(x=date, y=sti)) +
    geom_line() -> plt_germany

  cowplot::plot_grid(plotlist = list(plt_germany, oepnv_plt), nrow=2) %>%
    return()
}
