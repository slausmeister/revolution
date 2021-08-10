
#' @export
variant_case_time_series <- function(update_data=F, interpolation="none", format_long=F){

    if(update_data){
        get_latest_voc_data()
    }

    temp_data <- building_variant_data(interpolation)
    get_time_series_for() %>%
        dplyr::select(c("date","cases")) %>%
        dplyr::right_join(temp_data, by = c("date" = "Datum")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(alpha_cases = as.integer(cases * alpha)) %>%
        dplyr::mutate(beta_cases = as.integer(cases * beta)) %>%
        dplyr::mutate(gamma_cases = as.integer(cases * gamma)) %>%
        dplyr::mutate(delta_cases = as.integer(cases * delta)) %>%
        dplyr::mutate(others_cases = as.integer(cases * (1- alpha - beta - gamma - delta))) %>%
        dplyr::select(c("date","alpha_cases","beta_cases", "gamma_cases","delta_cases",
        "others_cases")) ->
        temp_data

    if(format_long){
      rename_function <- function(s){
        return(str_sub(s, 1, nchar(s)-6))
      }
      temp_data %>% dplyr::rename_with(rename_function, dplyr::ends_with("cases")) %>%
        tidyr::pivot_longer(!date, names_to="variant", values_to="cases") -> temp_data
    }

    return(temp_data)
}

#' @export
variant_case_R_value <- function(){
  cases <- c("alpha", "beta", "gamma", "delta")
  data <- variant_case_time_series()
  
  for (i in 1:length(cases)){
    R_value <- rep(NA, nrow(data))
    case <- paste(cases[i], "cases", sep="_")
    print(case)
    for (t in 8:nrow(data)) {
      R_value[t] <- sum(data[t-0:3, case]) / sum(data[t-4:7, case]) 
    }
    column_title <- gsub(" ", "", paste("R_value_", cases[i]))
    print(column_title)
    R_value <- unlist(R_value)
    print(R_value)
    data[i + 5] <- R_value
    names(data)[i + 5] <- column_title
  } 
  data %>% print(n = 100)
  return(data)
}


variant_sti_time_series <- function(update_data=F, interpolation="none", format_long=F){

    if(update_data){
        get_latest_voc_data()
    }

    temp_data <- building_variant_data(interpolation)
    get_sti_series_for() %>%
        dplyr::select(c("date","sti")) %>%
        dplyr::right_join(temp_data, by = c("date" = "Datum")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(alpha_sti = sti * alpha) %>%
        dplyr::mutate(beta_sti = sti * beta) %>%
        dplyr::mutate(gamma_sti = sti * gamma) %>%
        dplyr::mutate(delta_sti = sti * delta) %>%
        dplyr::mutate(others_sti = sti * (1- alpha - beta - gamma - delta)) %>%
        dplyr::select(c("date", "alpha_sti", "beta_sti", "gamma_sti", "delta_sti", "others_sti")) ->
        temp_data

    if(format_long){
      rename_function <- function(s){
        return(str_sub(s, 1, nchar(s)-4))
      }
      temp_data %>% dplyr::rename_with(rename_function, dplyr::ends_with("cases")) %>%
        tidyr::pivot_longer(!date, names_to="variant", values_to="sti") -> temp_data
    }

    return(temp_data)
}

#' @import ggplot2, ggstream
#' @export
# Building ts of voc prop
building_variant_data <- function(interpolation="none", tablepath = system.file("extdata", "VOC_VOI_Tabelle.xlsx",package= "revolution")){
  
  tablepath %>% readxl::read_excel(sheet=1) %>% # reading data
    head(-1) %>% # Dropping last row, as it is a summary row
    dplyr::select(matches(
      c("B\\.1\\.1\\.7.*Anteil","B\\.1\\.351.*Anteil", "P\\.1.*Anteil", "B\\.1\\.617.*Anteil")
    )) %>%
    dplyr::rename(c("alpha"=1,"beta"=2,"gamma"=3,"delta"=4)) %>%
    '/'(.,100) %>% # Dividing by 100 for accurate %
    dplyr::slice(rep(1:n(), each = 7)) -> # Replicating the data values
    anteil
  
  # Adding a date column
  anteil %>%
    dplyr::mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
                              length.out = dim(anteil)[1]), .before = "alpha") ->
    anteil
  anteil
  
  if(interpolation=="linear"){
    
    variants_vector <- c("alpha", "beta", "gamma", "delta")
    for(k in 1:length(variants_vector)){
      x_out <- rep(NA, nrow(anteil))
      for(i in seq(8, dim(anteil)[1], by=7) ){
        a <- anteil[i-7,k + 1]
        b <- anteil[i,k + 1]
        m <- (b-a)/7
        for(j in 0:6){
          x_out[i - 7 + j] <- a + j*m
        }
      }
      column_title <- gsub(" ", "", paste("x_out_", variants_vector[k]))
      anteil[k + 5] <- x_out
      names(anteil)[k + 5] <- column_title
    }
  }
  
  ## plot still needs some work ##
  ggplot()+
    geom_line(data=anteil, aes(x = Datum, y = alpha), color="red")
  return(anteil)
}

#' @import ggplot2 ggstream
#' @export
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
