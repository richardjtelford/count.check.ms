#resample to smaller count sum with multivariate hypergeometric & count singletons
recount_singletons <- function(counts, k, nrep = 10) {
  k %>% set_names() %>%
    map_df(~{
      y <- extraDistr::rmvhyper(nn = nrep, n = counts, k = .)
      tibble(no_singletons = rowSums(y == 1), rep = 1:nrep)
    }, .id = "new_count_sum")
}

#functions
#join and process at different taxonomic levels

bird_process <- function(bird_data, bird_species) {
  joined <- bird_data %>%
    left_join(bird_species, by = "AOU")
  
  out <- bind_rows(
    species = joined %>% rename(taxon = Latin_name),
    genus = joined %>% rename(taxon = Genus),
    family = joined %>% rename(taxon = Family),
    order = joined %>% rename(taxon = ORDER),
    .id = "taxonomic_level"
  ) %>%
    group_by(RouteDataID, Year, taxonomic_level, taxon) %>%
    summarise(count = sum(count)) %>%
    group_by(RouteDataID, Year, taxonomic_level) %>%
    summarise(
      n_taxa = n(),
      singletons = sum(count == 1),
      count_sum = sum(count), 
      GCD = ifelse(n_taxa > 1, numbers::mGCD(count), NA)#only find GCD where no singletons for speed ( and > 1 taxa). Using ifelse as it doesn't run TRUE code when it would crash
    )
  return(out)
}


