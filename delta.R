# Ich hab absolut keinen Plan welche header geladen werden müssen

variant_case_time_series <- function(update_data=F, interpolation="none"){

    if(update_data){
        source("update_VOC_data.R")
        get_latest_voc_data()
    }

    temp_data <- building_variant_data(interpolation)
    get_time_series_for() %>%
        select(c("date","cases")) %>%
        right_join(temp_data, by = c("date" = "Datum")) %>%
        drop_na() %>%
        mutate(alpha_cases = cases * alpha) %>%
        mutate(beta_cases = cases * beta) %>%
        mutate(gamma_cases = cases * gamma) %>%
        mutate(delta_cases = cases * delta) %>%
        select(c("date","alpha_cases","beta_cases", "gamma_cases","delta_cases")) %>%
        return()
}


variant_sti_time_series <- function(update_data=F, interpolation="none"){

    if(update_data){
        source("update_VOC_data.R")
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
        select(c("date", "alpha_sti", "beta_sti", "gamma_sti", "delta_sti")) %>%
        return()
} 
    
    delta_r_sti <- delta_sti_time_series
    delta_r_sti$delta <- 0
    for(t in 8:nrow(delta_sti_time_series)){
        delta_r_sti$delta[t] <- as.numeric(delta_sti_time_series[t,2])/as.numeric(delta_sti_time_series[t-4,2])
    }
    
    delta_r <- delta_time_series
    delta_r$delta <- 0
    for(t in 8:nrow(delta_time_series)){
        delta_r$delta[t] <- as.numeric(delta_time_series[t,2])/as.numeric(delta_time_series[t-4,2])
    }


    plot(delta_r_sti, type="l")
    abline(v=seq(as.Date("2021/01/04"),by = "weeks", length.out = (dim(anteil)[1])/7), col="grey")
    abline(h=1, col="grey")



# Building ts of voc prop
building_variant_data <- function(interpolation="none", tablepath = "xlsx/VOC_VOI_Tabelle.xlsx"){

    library("readxl")
    #     source("sti_id.R")

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
