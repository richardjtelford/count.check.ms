# breeding bird survey

# download data
download_bird <- function() {
  sb_id <- "64ad9c3dd34e70357a292cee"
  bbs_dir <- "data/birds2023"
  
  # make dir if
  if (!fs::dir_exists(bbs_dir)) {
    fs::dir_create(bbs_dir)
  }
  
  bbsAssistant::download_bbs_data(
    sb_id = sb_id,
    bbs_dir = bbs_dir, 
    overwrite = FALSE
  )

  bbs <- read_csv(file.path(bbs_dir, "50-StopData.zip"))


  # select required columns
  bbs |>
    (\(x){
      mutate(x, SpeciesTotal = rowSums(select(x, matches("^Stop\\d{1,2}"))))
    })() |> 
    mutate(AOU = as.integer(AOU)) |> 
    select(RouteDataID, AOU, Year, SpeciesTotal)
}
  

# load taxonomic data
load_bird_species <- function(){ 
  read_fwf("data/birds2023/SpeciesList.txt", skip = 10) |> 
    janitor::row_to_names(1) |> 
    slice(-1) |> 
    select(AOU, Order = ORDER, Family, Genus, Species) |> 
    mutate(
      AOU = as.integer(AOU),
      Species = paste(Genus, Species)) |> 
    # Add Northwestern Crow - now merged with American Crow but in data
    add_row(AOU = 4890L, Order = "Passeriformes", Family = "Corvidae", Genus =  "Corvus", Species = "Corvus brachyrhynchos caurinus")
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
