source("utilities.R")
library(ggplot2)

unfaelle_data <- read_csv("csvs/unfaelle_jahre.csv")

unfaelle_data %>% mutate(Jahr=as.character(Jahr))

ggplot(data=unfaelle_data, aes(x=Monat, y=Unfaelle, fill=factor(Jahr))) +
  geom_col(position=position_dodge()) + scale_x_discrete(limits=unfaelle_data$Monat[1:12]) +
  labs(fill = "Jahr") -> unfaelle_plt

# sti_germany <- get_sti_series_for()
# tibble(date=days_since_2020[1:366], sti=sti_germany[1:366]) -> df
ggplot(data=get_sti_series_for(), aes(x=date, y=sti, color=Landkreis)) + geom_line() -> plt_germany




oepnv_data <- read_csv("csvs/oepnv_jahre.csv")

oepnv_data %>% filter(Bundesland=="Hessen") -> oepnv_data

ggplot(data=oepnv_data, aes(x=Quartal, y=Personenkilometer, fill=factor(Jahr))) +
  geom_col(position=position_dodge()) + scale_x_discrete(limits=oepnv_data$Quartal[1:4]) +
  labs(fill = "Jahr") -> oepnv_plt


cowplot::plot_grid(plotlist = list(unfaelle_plt, oepnv_plt), nrow=2) %>% print()
