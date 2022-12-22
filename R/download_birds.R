# breeding bird survey

# download data
download_bird <- function() {
  bbs <- bbsAssistant::grab_bbs_data(
    sb_id = "625f151ed34e85fa62b7f926",
    bbs_dir = "data/birds2022"
  )

  # select required columns
  bbs$observations |>
    (\(x){
      mutate(x, SpeciesTotal = rowSums(select(x, matches("^Stop\\d{1,2}"))))
    })() |> 
    select(RouteDataID, AOU, Year, SpeciesTotal)
}
  

# load taxonomic data
load_bird_species <- function(){ 
  bbsAssistant::import_species_list("data/birds2022") |> 
    select(AOU, Order = ORDER, Family, Genus, Species) |> 
    mutate(Species = paste(Genus, Species))
}


summarise_bird_singletons <- function(bird_singletons) {
  bird_singletons |>
    group_by(taxonomic_level) |>
    summarise(
      p_singletons = mean(n_singletons > 0) * 100,
      n_singletons = sum(n_singletons == 0),
      median_richness = median(n_taxa),
      n_routes = n(),
      GCD1 = mean(gcd == 1, na.rm = TRUE) * 100,
      GCD2plus = mean(gcd > 1, na.rm = TRUE) * 100,
      GCD_max = max(gcd, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    mutate(taxonomic_level = factor(
      taxonomic_level,
      levels = c("species", "genus", "family", "order")
    )) |>
    arrange(taxonomic_level)
}

summarise_bird <- function(bird_singletons) {
  list(
    n_without_singletons = bird_singletons |>
      filter(taxonomic_level == "species", n_singletons == 0) |>
      nrow(),
    n_GCD_gt1 = bird_singletons |>
      filter(taxonomic_level == "species", gcd > 1 | is.na(gcd))
  )
}

# simulate counts with mv hypergeometric
recount_bird_singletons <- function(bird_download, bird_species) {
  bird_download |>
    group_by(RouteDataID) |>
    filter(sum(SpeciesTotal) >= 400) |>
    left_join(bird_species, by = "AOU") |>
    group_by(RouteDataID, Order) |>
    summarise(count = sum(SpeciesTotal), .groups = "drop_last") |>
    nest() |>
    mutate(recount = map(data, ~ recount_singletons(counts = .x$count, k = c(30, 50, 100, 200, 300, 400)))) |>
    unnest(cols = recount) |>
    mutate(new_count_sum = as.numeric(new_count_sum)) |>
    group_by(new_count_sum, rep) |>
    summarise(m = mean(no_singletons == 0) * 100, .groups = "drop_last") |>
    summarise(m_no_singletons = mean(m), sd = sd(m), se = sd / sqrt(n()), .groups = "drop_last")
}

plot_bird_recount_singleton_plot <- function(bird_recount_singletons) {
  ggplot(
    data = bird_recount_singletons,
    aes(
      x = new_count_sum, y = m_no_singletons,
      ymin = m_no_singletons - 1.96 * se,
      ymax = m_no_singletons + 1.96 * se
    )
  ) +
    geom_errorbar() +
    geom_point(size = 0.5) +
    labs(x = "Count sum", y = "Percent without singletons")
}
