library(readxl)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rlist)

deaths_per_age <- read_excel("xlsx/deaths_per_age.xlsx")
population_age <- read_csv("csvs/population_age.csv")

population_age %>%
  group_by(Jahr,Altersgruppe) %>% 
  summarize(population=sum(Bevölkerung)) %>% 
  pivot_wider(names_from=Altersgruppe,values_from=population) %>% 
  mutate(`A00-A34`=`A15-A34`+`A05-A14`+`A00-A04`,.after=Jahr)-> pop_age
pop_age <- pop_age[-(3:5)]
pop_age
deaths_per_age %>% 
  mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
  mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) -> deaths_summarized

deaths_summarized2 <- deaths_summarized[-(1:16)]
deaths_summarized2 %>% 
  group_by(Jahr) %>% 
  summarize(`A00-A34`=sum(`A00-A34`),`A35-A59`=sum(`A35-A59`),`A60-A79`=sum(`A60-A79`),`A80+`=sum(`A80+`))-> final_tibble

deaths <- final_tibble[-nrow(final_tibble),]

deaths_per_capita <- deaths[2:5]/pop_age[2:5]
deaths_per_capita <- mutate(deaths_per_capita,Jahr=c(2014,2015,2016,2017,2018,2019,2020),.before=`A00-A34`)

deaths_per_capita %>% 
  pivot_longer(c(`A00-A34`,`A35-A59`,`A60-A79`,`A80+`),names_to="ages",values_to="rate") %>% 
  ggplot(aes(x=Jahr,y=rate,color=ages))+geom_line()



weekly_mortality <- function(years=2020,age="A80+",age_pop){
  #weekly_deaths <- list()
  rates <- list(1:length(years))
  deaths_per_age %>% 
    mutate(`A00-A34`=(`0-29`+`30-34`),`A35-A59`=(`35-39`+`40-44`+`45-49`+`50-54`+`55-59`)) %>%
    mutate(`A60-A79`=(`60-64`+`65-69`+`70-74`+`75-79`),`A80+`=(`80-84`+`85-89`+`90-94`+`95+`)) %>% 
    select(1,17:21) -> deaths_new
  filtered_data <- filter(deaths_new,Jahr %in% years)
  #print(filtered_data)
  temp <- select(filtered_data,Jahr,age,week)
  temp %>% 
    rename_with(~return("mortality"),starts_with("A")) -> temp
  print(temp)
  for(i in (1:length(years))){
    div <- as.numeric(filter(select(age_pop,Jahr,age),Jahr==years[i])[[1,2]])
    bool <- temp["Jahr"]==years[i]
    temp[bool[,1],2] <- temp[bool[,1],2]*(1/div)
  }
  temp <- mutate(temp,Jahr=as.character(Jahr))
  plt <- ggplot(temp,mapping=aes(x=week,y=mortality,color=Jahr))+geom_line()
  plt
}

weekly_mortality(years=c(2017,2018,2020,2019),age="A80+",age_pop = pop_age)



