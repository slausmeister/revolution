# Ich hab absolut keinen Plan welche header geladen werden müssen
# variant_time_series <- function(tablepath){
    tablepath <- "xlsx/VOC_VOI_Tabelle.xlsx"
    
    library("readxl")
    source("sti_id.R")

    tablepath %>% read_excel(sheet=1) %>% # reading data
        head(-1) %>% # Dropping last row, as it is a summary row
        select(c("B.1.617.2_Anteil (%)")) %>% # Selecting the VOC
        '/'(.,100) %>% # Dividing by 100 for acurate %
        rename("delta_prop"="B.1.617.2_Anteil (%)") %>% # simplify colname
        slice(rep(1:n(), each = 7)) -> # Replicating the data values
        anteil
    
    # Adding a date column
    anteil %>%
        mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
            length.out = dim(anteil)[1]), .before = "delta_prop") ->
        anteil

    tablepath %>% read_excel(sheet=1) %>% # reading data
        head(-1) %>% # Dropping last row, as it is a summary row
        select(c("B.1.617.2_Anteil (%)")) %>% # Selecting the VOC
        '/'(.,100) %>% # Dividing by 100 for acurate %
        rename("delta_prop"="B.1.617.2_Anteil (%)") -> # simplify colname
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
        rename(delta_prop=value) %>%
        mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
            length.out = length(linear_fit)), .before = "delta_prop") ->
        linear_fit

    # Building Variant cases per day

    get_time_series_for() %>%
        select(c("date","cases")) %>%
        right_join(anteil, by = c("date" = "Datum")) %>%
        drop_na() %>%
        mutate(delta = cases * delta_prop) %>%
        select(c("date","delta")) ->
        delta_time_series
    
    get_sti_series_for() %>%
        select(c("date","sti")) %>%
        right_join(linear_fit, by = c("date" = "Datum")) %>%
        drop_na() %>%
        mutate(delta = sti * delta_prop) %>%
        select(c("date","delta")) ->
        delta_sti_time_series
    
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
