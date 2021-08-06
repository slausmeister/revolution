# Ich hab absolut keinen Plan welche header geladen werden müssen
library("readxl")
source("sti.R")

# returns the time_series for a single variant
variant_time_series_single <- function(variant_label="B.1.617.2+AY.1+AY.2+AY.3"){
  variant_label <- paste(variant_label, "_Anteil (%)", sep="")

  tablepath <- "xlsx/VOC_VOI_Tabelle.xlsx"
  tablepath %>% read_excel(sheet=1) %>% # reading data
      head(-1) %>% # Dropping last row, as it is a summary row
      select(variant_label) %>% # Selecting the VOC
      '/'(.,100) %>% # Dividing by 100 for acurate %
      rename("variant_prop"=variant_label) %>% # simplify colname
      slice(rep(1:n(), each = 7)) -> # Replicating the data values
      anteil

  # Adding a date column, starting at 2021/01/04, because the first week starts there
  anteil %>%
      mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
          length.out = dim(anteil)[1]), .before = "variant_prop") ->
      anteil

  tablepath %>% read_excel(sheet=1) %>% # reading data
      head(-1) %>% # Dropping last row, as it is a summary row
      select(variant_label) %>% # Selecting the VOC
      '/'(.,100) %>% # Dividing by 100 for acurate %
      rename("variant_prop"=variant_label) -> # simplify colname
      temp

  linear_fit <- rep(0,dim(temp)[1], each =7)
  for(i in 2:dim(temp)[1]){
      a <- temp[i-1,]
      b <- temp[i,]
      m <- (b-a)/7
      for(j in 0:6){
          linear_fit[7*(i-1)+j] <- a + j*m
      }
  }
  linear_fit[189] <- tail(temp, n=1)[1,]

  # Adding a date column
  linear_fit %>%
      as_tibble() %>%
      rename(variant_prop=value) %>%
      mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
          length.out = length(linear_fit)), .before = "variant_prop") ->
      linear_fit


  # Building Variant cases per day
  get_time_series_for() %>%
      select(c("date","cases")) %>%
      right_join(anteil, by = c("date" = "Datum")) %>%
      drop_na() %>%
      mutate(total = as.integer(cases * variant_prop)) %>%
      select(c("date","total")) ->
      variant_time_series

  get_sti_series_for() %>%
      select(c("date","sti")) %>%
      right_join(linear_fit, by = c("date" = "Datum")) %>%
      drop_na() %>%
      mutate(sti = sti * variant_prop) %>%
      select(c("date","sti")) ->
      variant_sti_time_series

  variant_r_sti <- variant_sti_time_series
  variant_r_sti$sti <- 0
  variant_r_sti %>% rename(sti_r_value=sti) -> variant_r_sti
  for(t in 8:nrow(variant_sti_time_series)){
    if(as.numeric(variant_sti_time_series[t-4, 2]) != 0){
      variant_r_sti$sti_r_value[t] <- as.numeric(variant_sti_time_series[t,2])/as.numeric(variant_sti_time_series[t-4,2])
    }
  }

  variant_r <- variant_time_series
  variant_r$total <- 0
  variant_r %>% rename(total_r_value=total) -> variant_r
  for(t in 8:nrow(variant_time_series)){
    if(as.numeric(variant_time_series[t-4, 2]) != 0){
      variant_r$total_r_value[t] <- as.numeric(variant_time_series[t,2])/as.numeric(variant_time_series[t-4,2])
    }
  }

  variant_sti_time_series %>% left_join(variant_time_series, by="date") %>%
    left_join(variant_r, by="date") %>% left_join(variant_r_sti, by="date") %>% return()
}

# creates a table with multiple variants and their time series
variants_time_series <- function(variant_labels=c("B.1.617.2+AY.1+AY.2+AY.3_Anteil (%)")){

  variant_time_series_single(variant_labels[1]) %>% mutate(variant=variant_labels[1]) -> data

  if(length(variant_labels) > 1){
    for(v in variant_labels[2:length(variant_labels)]){
      variant_time_series_single(v) %>% mutate(variant=v) -> tmp
      data %>% add_row(tmp) -> data
    }
  }

  return(data)
}

library(ggplot2)
library(ggstream)

plot_variant_r_value <- function(variant_labels=c("B.1.617.2+AY.1+AY.2+AY.3",
"B.1.1.7")){
  variants_time_series(variant_labels) %>% ggplot(aes(x=date, y=total_r_value, color=variant)) %>%
    `+`(geom_line()) %>% return()
}

plot_variant_share <- function(variant_labels=c("B.1.617.2+AY.1+AY.2+AY.3",
"B.1.1.7")){
  total_data <- get_time_series_for(from="2021/01/04")
  variants_time_series(variant_labels) %>% select(date, total, variant) -> variant_data

  total_data %>% mutate(variant="others") %>%
    select(date, cases, variant) -> total_data
  variant_data %>% group_by(date) %>% summarise(count=sum(total)) -> tmp

  tmp %>% left_join(total_data, by="date") %>% mutate(total=cases-count) %>%
    select(date, total, variant) -> total_data
  variant_data %>% add_row(total_data) -> combined

  combined %>% ggplot(aes(x=date)) %>%
    `+`(geom_stream(aes(y=total, fill=variant), type="ridge")) %>%
    return()
}
