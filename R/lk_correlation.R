#' Incidence Correlation of District Pairs
#'
#' This function allows the user to calculate the empirical correlation of district pairs
#' over the pandemic. The correlation is calculated by looking at the empirical
#' correlation of the STI of one district with the STI of the other district on a daily basis
#'
#' @param regions A vector of strings with district names or district IDs which should be compared.
#' If the value is "all", all districts will be compared.
#' @return A tibble with all pairs of given regions, sorted by the ones with the highest correlation first.
#' @section Warning:
#' When calling this function with the paramater "all" (the default paramater),
#' the results may take a while, because this is a very expensive computation.
#' @examples
#' calc_sti_correlation_of_lks(c("Heidelberg", 8222, "1001"))
#'
#' @export
calc_sti_correlation_of_lks <- function(regions="all"){

  avg_correlation <- function(ts1, ts2, window=0){
    return(ccf(ts1, ts2, lag.max=window, plot=F)$acf %>% abs() %>% mean())
  }

  if(all(regions=="all")){
    lk_ids <- rev.env$population_lk_data[["IdLandkreis"]]
  }
  else{
    lk_ids <- c()
    for(lk in regions){
      lk_ids <- c(lk_ids, get_lk_id_from_string(lk, T))
    }
  }

  sti_list <- list()

  for(i in 1:length(lk_ids)){
    get_sti_series_simple(lk_ids[i]) %>% as.numeric() %>% list() %>%
      append(sti_list, .) -> sti_list

    print(paste("Progress: ", 50*i/length(lk_ids), "%"))
  }

  indices1 <- 1:length(lk_ids)
  indices2 <- 1:length(lk_ids)
  tidyr::crossing(indices1, indices2) %>% dplyr::filter(indices1 < indices2) -> index_pairs
  index_pairs %>% dplyr::count() %>% `[[`(1) -> no_of_pairs
  avg_corrs <- rep(0, length(no_of_pairs))

  for(i in 1:no_of_pairs){
    index_pairs %>% dplyr::slice(i) %>% unlist(use.names=F) -> indices
    avg_corrs[i] <- avg_correlation(sti_list[[indices[1]]], sti_list[[indices[2]]])
    print(paste("Progress: ", 50 + 50*i/no_of_pairs, "%"))
  }

  index_pairs %>% dplyr::mutate(correlation=avg_corrs) %>%
    dplyr::arrange(dplyr::desc(correlation)) %>% dplyr::mutate(lk_id1=0, lk_id2=0) -> data

  for(i in 1:nrow(data)){
    lk_ids[data[[i, "indices1"]]] -> id1
    lk_ids[data[[i, "indices2"]]] -> id2
    data[[i, "lk_id1"]]  <- id1
    data[[i, "lk_id2"]]  <- id2
  }

  data %>% dplyr::left_join(rev.env$population_lk_data, by=c("lk_id1"="IdLandkreis")) %>%
    dplyr::mutate(Landkreis1=Landkreis) %>%
    dplyr::select(correlation, Landkreis1, lk_id2) %>%
    dplyr::left_join(rev.env$population_lk_data, by=c("lk_id2"="IdLandkreis")) %>%
    dplyr::mutate(Landkreis2=Landkreis) %>%
    dplyr::select(correlation, Landkreis2, Landkreis1) %>% return()
}
