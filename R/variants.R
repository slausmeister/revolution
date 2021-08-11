#' Variant case time series
#'
#' This function takes the weekly variant of concern data provided by the RKI and
#' approximates the case time series of the different variants.
#'
#' It assumes, that the proportions of the variants in the actual cases is the same
#' as the proportion in the sampled viral matter. Since the RKI only provides the
#' variant shares on a weekly bases, the function can interpolate the data, using a linear
#' spline interpolation.
#'
#' @param interpolation Specifies whether the voc data should be interpolated. Options
#'   include "linear" and "none". The default in "none", i.e. no interpolation.
#' @param format_long, boolean Specifies whether a long format should be used.
#'   A long format pivots the data. The default is F
#'
#' @return A tibble carrying a "date" column, and a column for each variant case number for
#'   that day.
#'
#' @examples
#' variant_case_time_series()
#' variant_case_time_series(interpolation="linear")
#'
#' \dontrun{variant_case_time_series(interpolation=T)}
#' @family Variants
#' @export
variant_case_time_series <- function(interpolation="linear", format_long=F){

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
        return(stringr::str_sub(s, 1, nchar(s)-6))
      }
      temp_data %>% dplyr::rename_with(rename_function, dplyr::ends_with("cases")) %>%
        tidyr::pivot_longer(!date, names_to="variant", values_to="cases") -> temp_data
    }

    return(temp_data)
}

#'Variant R value
#'
#'\code{variant_case_R_value()} is used to calculate the R value of different variants of COVID-19 (e.g. alpha, beta, gamma, delta).
#'The reproduction number (R value) describes how many people an infected person infects on average.
#'
#'@return A tibble that contains the share of cases with alpha, beta, gamma, delta variant and the according reproduction number (variant specific).
#'
#'@examples variant_case_R_value()
#'
#'@export
variant_case_r_value <- function(){
  cases <- c("alpha", "beta", "gamma", "delta")
  data <- variant_case_time_series()

  for (i in 1:length(cases)){
    R_value <- rep(NA, nrow(data))
    case <- paste(cases[i], "cases", sep="_")
    for (t in 8:nrow(data)) {
      R_value[t] <- sum(data[t-0:3, case]) / sum(data[t-4:7, case])
    }
    column_title <- gsub(" ", "", paste("R_value_", cases[i]))
    R_value <- unlist(R_value)
    data[i + 5] <- R_value
    names(data)[i + 5] <- column_title
  }

  data %>%
    dplyr::select(date, tidyselect::starts_with("R_val")) %>%
    tidyr::pivot_longer(!date, names_to="variant", values_to="r", names_prefix="R_value_") %>%
    dplyr::mutate(r=tidyr::replace_na(r, 0)) ->
    data
  return(data)
}

#' Variant STI time series
#'
#' This function takes the weekly variant of concern data provided by the RKI and
#' approximates the STI time series of the different variants.
#'
#' It assumes, that the proportions of the variants in the actual cases is the same
#' as the proportion in the sampled viral matter. Since the RKI only provides the
#' variant shares on a weekly bases, the function can interpolate the data, using a linear
#' spline interpolation.
#'
#' @param interpolation Specifies whether the voc data should be interpolated. Options
#'   include "linear" and "none". The default in "none", i.e. no interpolation.
#' @param format_long, boolean Specifies whether a long format should be used.
#'   A long format pivots the data. The default is F
#'
#' @return A tibble carrying a "date" column, and a column for each variant STI for
#'   that day.
#'
#' @examples
#' variant_sti_time_series()
#' variant_sti_time_series(interpolation="linear")
#'
#' \dontrun{variant_sti_time_series(interpolation=T)}
#' @family Variants
#' @export
variant_sti_time_series <- function(interpolation="linear", format_long=F){

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
        return(stringr::str_sub(s, 1, nchar(s)-4))
      }
      temp_data %>% dplyr::rename_with(rename_function, dplyr::ends_with("cases")) %>%
        tidyr::pivot_longer(!date, names_to="variant", values_to="sti") -> temp_data
    }

    return(temp_data)
}

# Building ts of voc prop
building_variant_data <- function(interpolation="linear", tablepath = system.file("extdata", "VOC_VOI_Tabelle.xlsx",package= "revolution")){

  tablepath %>% readxl::read_excel(sheet=1) %>% # reading data
    head(-1) %>% # Dropping last row, as it is a summary row
    dplyr::select(matches(
      c("B\\.1\\.1\\.7.*Anteil","B\\.1\\.351.*Anteil", "P\\.1.*Anteil", "B\\.1\\.617.*Anteil")
    )) %>%
    dplyr::rename(c("alpha"=1,"beta"=2,"gamma"=3,"delta"=4)) %>%
    '/'(.,100) %>% # Dividing by 100 for accurate %
    dplyr::slice(rep(1:dplyr::n(), each = 7)) -> # Replicating the data values
    anteil

  # Adding a date column
  anteil %>%
    dplyr::mutate(Datum = seq(as.Date("2021/01/04"),by = "days",
                              length.out = dim(anteil)[1]), .before = "alpha") ->
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
  return(anteil)
}

#' Plot variant data
#'
#' A function that plots share of variant data over time.
#'
#' @param interpolation Specifies whether the voc data should be interpolated.
#'   Possible options include "linear" or "none". Linear does a linear spline
#'   interpolation. None is the default.
#' @return Returns a ggplot2 plot.
#' @examples plot_variant_data(), plot_variant_data("linear")
#'   
#' @import ggplot2, ggstream
#' @export
plot_variant_data <- function(interpolation="none"){
  
  anteil <- building_variant_data("linear")
  anteil %>%
    rename(alpha_share = x_out_alpha) %>%
    rename(beta_share = x_out_beta) %>%
    rename(gamma_share = x_out_gamma) %>%
    rename(delta_share = x_out_delta) %>%
    pivot_longer(c("alpha_share", "beta_share", "gamma_share", "delta_share"),"variant") %>%
    rename(date = Datum) -> anteil_plot
  
  office_case_distribution_plot <- ggplot()+
    geom_line(data=anteil_plot, aes(x = date, y = value, color = variant))
  return(office_case_distribution_plot)
  
}

#' Plot variant share
#'
#' A function that plots variant data over time.
#'
#' The function can ether plot the case time series of different vocs, or the
#' sti time series of different vocs.
#'
#' @param interpolation Specifies whether the voc data should be interpolated.
#'   Possible options include "linear" or "none". Linear does a linear spline
#'   interpolation. None is the default.
#'
#' @param type String, specifies which value for the variants should be plotted
#'   and how it should be displayed
#' @return Returns a ggplot2 plot.
#'
#' @examples
#' plot_variants()
#' plot_variants(interpolation="linear", type="sti")
#' \dontrun{plot_variants(interpolation=T)}
#'
#' @family Variants
#' @import ggplot2 ggstream
#' @export
plot_variants <- function(interpolation="linear", type="cases"){
  stopifnot(type %in% c("cases", "percentage", "sti", "share", "r"))
  if(type=="sti"){
    variant_sti_time_series(interpolation=interpolation, format_long=T) %>%
      ggplot2::ggplot(ggplot2::aes(x=date)) %>%
      `+`(ggplot2::geom_line(ggplot2::aes(y=sti, color=variant))) ->
      plt
    return(plt)
  }
  if(type=="cases"){
    variant_case_time_series(interpolation=interpolation, format_long=T) %>%
      ggplot2::ggplot(ggplot2::aes(x=date)) %>%
      `+`(ggstream::geom_stream(ggplot2::aes(y=cases, fill=variant), type="ridge")) ->
      plt
    return(plt)
  }
  if(type=="share"){
    variant_case_time_series(interpolation=interpolation, format_long=T) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(total=sum(cases)) ->
      data1
    variant_case_time_series(interpolation=interpolation, format_long=T) %>%
      dplyr::left_join(data1, by="date")%>%
      dplyr::mutate(share=cases/total) %>%
      ggplot2::ggplot(ggplot2::aes(x=date)) %>%
      `+`(ggplot2::geom_area(ggplot2::aes(y=share, fill=variant))) ->
      plt
    return(plt)
  }
  if(type=="percentage"){
    variant_case_time_series(interpolation=interpolation, format_long=T) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(total=sum(cases)) ->
      data1
    variant_case_time_series(interpolation=interpolation, format_long=T) %>%
      dplyr::left_join(data1, by="date")%>%
      dplyr::mutate(share=cases/total) %>%
      ggplot2::ggplot(ggplot2::aes(x=date)) %>%
      `+`(ggplot2::geom_line(ggplot2::aes(y=share, color=variant, group=variant))) ->
      plt
    return(plt)
  }
  if(type=="r"){
    variant_case_r_value() %>%
      ggplot2::ggplot(ggplot2::aes(x=date)) %>%
      `+`(ggplot2::geom_line(ggplot2::aes(y=r, color=variant))) ->
      plt
    return(plt)
  }
}
