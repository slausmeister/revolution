source("sti.R")
library(ggplot2)

# USER FUNKTION
# gibt einen tibble mit unfällen pro monat in 2019 und 2020 aus
get_accidents_data <- function(){
  read_csv("csvs/unfaelle_jahre.csv") %>% mutate(Jahr=as.character(Jahr)) %>% return()
}

# USER FUNKTION
# plottet die sti und darunter den vergleich der unfälle in 2019 und 2020
plot_accidents_with_sti <- function(){
  unfaelle_data <- get_accidents_data()

  ggplot(data=unfaelle_data, aes(x=Monat, y=Unfaelle, fill=factor(Jahr))) +
    geom_col(position=position_dodge()) + scale_x_discrete(limits=unfaelle_data$Monat[1:12]) +
    labs(fill = "Jahr") -> unfaelle_plt

  ggplot(data=get_sti_series_for(to="2020-12-31"), aes(x=date, y=sti)) +
    geom_line() -> plt_germany

  cowplot::plot_grid(plotlist = list(plt_germany, unfaelle_plt), nrow=2) %>%
    return()
}

# USER FUNKTION
# returnt die Personenkilometer des öpvs über 2019 und 2020 nach Quartal und Bundesland
get_public_transportation_data <- function(){
  read_csv("csvs/oepnv_jahre.csv") %>% return()
}

# USER FUNKTION
# plottet die obigen Daten im Vergleich 2019 2020 und als referenz noch mit sti
plot_public_transportation_with_sti <- function(){
  oepnv_data <- get_public_transportation_data()
  ggplot(data=oepnv_data, aes(x=Quartal, y=Personenkilometer, fill=factor(Jahr))) +
    geom_col(position=position_dodge()) + scale_x_discrete(limits=oepnv_data$Quartal[1:4]) +
    labs(fill = "Jahr") -> oepnv_plt

  ggplot(data=get_sti_series_for(to="2020-12-31"), aes(x=date, y=sti)) +
    geom_line() -> plt_germany

  cowplot::plot_grid(plotlist = list(plt_germany, oepnv_plt), nrow=2) %>%
    return()
}
