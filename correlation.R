source("sti.R")

# USER FUNKTION
# funktion, bei der man korrelation von landkreisen ausrechnen kann
# man gibt die lks als regions=c("Hamburg", "Passau", "München") an
# für regions="all" wird ein tibble mit allen lk Paaren zurückgegeben
calc_sti_correlation_of_lks <- function(regions="all"){

  avg_correlation <- function(ts1, ts2, window=0){
    return(ccf(ts1, ts2, lag.max=window, plot=F)$acf %>% abs() %>% mean())
  }

  if(all(regions=="all")){
    lk_ids <- population_lk_data[["IdLandkreis"]]
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
  crossing(indices1, indices2) %>% filter(indices1 < indices2) -> index_pairs
  index_pairs %>% count() %>% `[[`(1) -> no_of_pairs
  avg_corrs <- rep(0, length(no_of_pairs))

  for(i in 1:no_of_pairs){
    index_pairs %>% slice(i) %>% unlist(use.names=F) -> indices
    avg_corrs[i] <- avg_correlation(sti_list[[indices[1]]], sti_list[[indices[2]]])
    print(paste("Progress: ", 50 + 50*i/no_of_pairs, "%"))
  }

  index_pairs %>% mutate(correlation=avg_corrs) %>%
    arrange(desc(correlation)) %>% mutate(lk_id1=0, lk_id2=0) -> data

  for(i in 1:nrow(data)){
    lk_ids[data[[i, "indices1"]]] -> id1
    lk_ids[data[[i, "indices2"]]] -> id2
    data[[i, "lk_id1"]]  <- id1
    data[[i, "lk_id2"]]  <- id2
  }

  data %>% left_join(population_lk_data, by=c("lk_id1"="IdLandkreis")) %>%
    mutate(Landkreis1=Landkreis) %>%
    select(correlation, Landkreis1, lk_id2) %>%
    left_join(population_lk_data, by=c("lk_id2"="IdLandkreis")) %>%
    mutate(Landkreis2=Landkreis) %>%
    select(correlation, Landkreis2, Landkreis1) %>% return()
}
