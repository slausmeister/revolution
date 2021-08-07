source("sti.R")
library(ggplot2)
library(ggstream)

variant_case_time_series <- function(update_data=F, interpolation="none", format_long=F){

    if(update_data){
        source("update_VOC_table.R")
        get_latest_voc_data()
    }

    temp_data <- building_variant_data(interpolation)
    get_time_series_for() %>%
        select(c("date","cases")) %>%
        right_join(temp_data, by = c("date" = "Datum")) %>%
        drop_na() %>%
        mutate(alpha_cases = as.integer(cases * alpha)) %>%
        mutate(beta_cases = as.integer(cases * beta)) %>%
        mutate(gamma_cases = as.integer(cases * gamma)) %>%
        mutate(delta_cases = as.integer(cases * delta)) %>%
        mutate(others_cases = as.integer(cases * (1- alpha - beta - gamma - delta))) %>%
        select(c("date","alpha_cases","beta_cases", "gamma_cases","delta_cases",
        "others_cases")) ->
        temp_data

    if(format_long){
      rename_function <- function(s){
        return(str_sub(s, 1, nchar(s)-6))
      }
      temp_data %>% rename_with(rename_function, ends_with("cases")) %>%
        pivot_longer(!date, names_to="variant", values_to="cases") -> temp_data
    }

    return(temp_data)
}


variant_sti_time_series <- function(update_data=F, interpolation="none", format_long=F){

    if(update_data){
        source("update_VOC_table.R")
        get_latest_voc_data()
    }

    temp_data <- building_variant_data(interpolation)
    get_sti_series_for() %>%
        select(c("date","sti")) %>%
        right_join(temp_data, by = c("date" = "Datum")) %>%
        drop_na() %>%
        mutate(alpha_sti = sti * alpha) %>%
        mutate(beta_sti = sti * beta) %>%
        mutate(gamma_sti = sti * gamma) %>%
        mutate(delta_sti = sti * delta) %>%
        mutate(others_sti = sti * (1- alpha - beta - gamma - delta)) %>%
        select(c("date", "alpha_sti", "beta_sti", "gamma_sti", "delta_sti", "others_sti")) ->
        temp_data

    if(format_long){
      rename_function <- function(s){
        return(str_sub(s, 1, nchar(s)-4))
      }
      temp_data %>% rename_with(rename_function, ends_with("cases")) %>%
        pivot_longer(!date, names_to="variant", values_to="sti") -> temp_data
    }

    return(temp_data)
}

# Building ts of voc prop
building_variant_data <- function(interpolation="none", tablepath = "xlsx/VOC_VOI_Tabelle.xlsx"){

    library("readxl")

    tablepath %>% read_excel(sheet=1) %>% # reading data
        head(-1) %>% # Dropping last row, as it is a summary row
        select(matches(
            c("B\\.1\\.1\\.7.*Anteil","B\\.1\\.351.*Anteil", "P\\.1.*Anteil", "B\\.1\\.617.*Anteil")
            )) %>%
        rename(c("alpha"=1,"beta"=2,"gamma"=3,"delta"=4)) %>%
        '/'(.,100) %>% # Dividing by 100 for acurate %
        slice(rep(1:n(), each = 7)) -> # Replicating the data values
        anteil

    # Adding a date column
    anteil %>%
        mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
            length.out = dim(anteil)[1]), .before = "alpha") ->
        anteil
    # Mayor issues right here...
    if(interpolation=="linear"){

        for(i in seq(8, dim(anteil)[1], by=7) ){
            a <- anteil[i-7,2:5]
            b <- anteil[i,2:5]
            m <- (b-a)/7
            for(j in 0:6){
                anteil[i+j-7,2:5] <- a + j*m
            }
        }

    }

    return(anteil)
}

plot_variant_share <- function(update_data=F, interpolation="none", sti=F){
  if(sti){
    variant_sti_time_series(update_data, interpolation, format_long=T) %>%
      ggplot(aes(x=date)) %>%
      `+`(geom_stream(aes(y=sti, fill=variant), type="ridge")) %>%
      return()
  }
  else{
    variant_case_time_series(update_data, interpolation, format_long=T) %>%
      ggplot(aes(x=date)) %>%
      `+`(geom_stream(aes(y=cases, fill=variant), type="ridge")) %>%
      return()
  }
}
