library(readxl)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
source("pop_data_preparation.R",encoding="UTF-8")

pop_prep <- function(population_data){
  population_data %>% 
    pivot_wider(names_from="Altersgruppe",values_from="Bevölkerung") %>% 
    mutate(`A00-A34`=`A00-A04`+`A05-A14`+`A15-A34`) %>% 
    select(Jahr,`A00-A34`,`A35-A59`,`A60-A79`,`A80+`) %>% 
    pivot_longer(c(`A00-A34`,`A35-A59`,`A60-A79`,`A80+`),names_to="Altersgruppe",values_to="population")->prepared
  prepared
}

abs_deaths <- function(years=2020){
  daily_deaths <- read_excel("xlsx/daily_deaths.xlsx")
  for(jahr in years){
    stopifnot("data only available for the years 2000-2020"=jahr %in% daily_deaths$year)
  }  
  daily_deaths %>% filter(year %in% years)->deaths_filtered
  deaths_filtered <- mutate(deaths_filtered,year=as.character(year))
  ggplot(deaths_filtered,aes(x=days,y=deaths,color=year))+geom_line()-> plt
  plt
}

weekly_mortality <- function(years=2020,age="A80+"){
  deaths_per_age <- read_excel("xlsx/deaths_per_age.xlsx")
  age_pop <- pop_prep(population_age_data)
  for(jahr in years){
    stopifnot("data only available for the years 2000-2020"=jahr %in% age_pop$Jahr)
  }
  for(alter in age){
    stopifnot("data only available for the age groups 'A00-A34','A35-A59','A60-A79','A80+'"=age %in% age_pop$Altersgruppe)
  }
  deaths_per_age %>% 
    mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
    mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) %>% 
    select(1,17:21) -> deaths_new
  filtered_data <- filter(deaths_new,Jahr %in% years)
  temp <- select(filtered_data,Jahr,age,week)
  temp %>% 
    rename_with(~return("mortality"),starts_with("A")) -> temp
  for(i in (1:length(years))){
    div <- as.numeric(filter(age_pop,Jahr==years[i],Altersgruppe==age)[1,3])
    bool <- temp["Jahr"]==years[i]
    temp[bool[,1],2] <- temp[bool[,1],2]*(1/div)
  }
  temp <- mutate(temp,Jahr=as.character(Jahr))
  plt <- ggplot(temp,mapping=aes(x=week,y=mortality,color=Jahr))+geom_line()
  plt
}

mortality_per_age_group <- function(age="A80+"){
  age_pop <- pop_prep(population_age_data)
  for(alter in age){
    stopifnot("data only available for the age groups 'A00-A34','A35-A59','A60-A79','A80+'"=age %in% age_pop$Altersgruppe)
  }
  age_pop %>% 
    pivot_wider(names_from=Altersgruppe,values_from=population)-> pop
  
  deaths_per_age <- read_excel("xlsx/deaths_per_age.xlsx")
  
  deaths_per_age %>% 
    mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
    mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) -> deaths_summarized
  
  deaths_summarized <- deaths_summarized[-(1:16)]
  deaths_summarized %>% 
    group_by(Jahr) %>% 
    summarize(`A00-A34`=sum(`A00-A34`),`A35-A59`=sum(`A35-A59`),`A60-A79`=sum(`A60-A79`),`A80+`=sum(`A80+`))-> final_tibble
  deaths <- final_tibble[-nrow(final_tibble),]
  
  per_age_group <- deaths[2:5]/pop[2:5]
  per_age_group <- mutate(per_age_group,Jahr=c(2014,2015,2016,2017,2018,2019,2020),.before=`A00-A34`)
  
  per_age_group %>% 
    pivot_longer(age,names_to="ages",values_to="rate") %>% 
    ggplot(aes(x=Jahr,y=rate,color=ages))+geom_line()->plt
  plt
}

