# returnt ein tibble mit der bevdichte eines landkreises und dessen durschnitttliche
# sti über die pandemie
get_pop_density_with_sti <- function(regions="all"){
  mean_stis <- c()

  if(all(regions=="all")){
    lk_ids <- population_lk_data[["IdLandkreis"]]
  }
  else{
    lk_ids <- c()
    for(lk in regions){
      lk_ids <- c(lk_ids, get_lk_id_from_string(lk, T))
    }
  }

  n <- length(lk_ids)
  for(i in 1:n){
    lk_id <- lk_ids[i]
    print(paste("Progress: ", 100*i/n, "%"))
    mean_stis <- c(mean_stis, mean(get_sti_series_simple(lk_id)))
  }

  population_lk_data %>% filter(IdLandkreis %in% lk_ids) %>%
    transmute(Landkreis=Landkreis, mean_sti=mean_stis, pop_density=BevDichte) %>% return()
}
