#source("data_preparation.R",encoding="UTF-8")
library(readxl)
library(ggplot2)

daily_deaths <- read_excel("csvs/daily_deaths2.xlsx")
age <- read_csv("csvs/population_age.csv")

ggplt <- ggplot(data=daily_deaths,mapping=aes(x=date_numbers))
l1 <- geom_line(mapping=aes(y=year_2016,color="blue"))
l2 <- geom_line(mapping=aes(y=year_2017,color="red"))
l3 <- geom_line(mapping=aes(y=year_2018,color="orange"))
l4 <- geom_line(mapping=aes(y=year_2019,color="green"))
l5 <- geom_line(mapping=aes(y=year_2020,color="brown"))

plt <- ggplt+l1+l2+l3+l4+l5
plt
