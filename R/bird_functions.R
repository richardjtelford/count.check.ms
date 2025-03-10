# resample to smaller count sum with multivariate hypergeometric & count singletons
recount_singletons <- function(counts, k, nrep = 10) {
  k |>
    set_names() |>
    map(~ {
      y <- extraDistr::rmvhyper(nn = nrep, n = counts, k = .x)
      tibble(no_singletons = rowSums(y == 1), rep = 1:nrep)
    }) |>
    list_rbind(names_to = "new_count_sum")
}

# functions
# join and process at different taxonomic levels

bird_process <- function(bird_data, bird_species) {
  joined <- bird_data |>
    left_join(bird_species, by = "AOU")

  out <- bind_rows(
    species = joined |> select(RouteDataID, taxon = Species, count = SpeciesTotal), # no need to aggregate
    genus = joined |> rename(taxon = Genus) |> agg_taxa(),
    family = joined |> rename(taxon = Family) |> agg_taxa(),
    order = joined |> rename(taxon = Order) |> agg_taxa(),
    .id = "taxonomic_level"
  ) |>
    group_by(RouteDataID, taxonomic_level) |>
    summarise_counts()

  return(out)
}


agg_taxa <- function(df) {
  df |>
    group_by(RouteDataID, taxon) |>
    summarise(count = sum(SpeciesTotal), .groups = "drop_last")
}
