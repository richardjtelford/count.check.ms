summarise_all_counts = function(bird_singletons, molten_singletons, foram_summ, foram_assem_res, pollen_assem_res){
  bind_rows(
    `Birds - species` = bird_singletons |> 
      ungroup() |>
      filter(taxonomic_level == "species") |> 
      summarise_singletons(),
    `Birds - genus` = bird_singletons |> 
      ungroup() |>
      filter(taxonomic_level == "genus") |> 
      summarise_singletons(),
    `Birds - family` = bird_singletons |> 
      ungroup() |>
      filter(taxonomic_level == "family") |> 
      summarise_singletons(),
    `Birds - order` = bird_singletons |> 
      ungroup() |>
      filter(taxonomic_level == "order") |> 
      summarise_singletons(),
    `Coastal diatoms` = molten_singletons |> 
      summarise_singletons(),
    `Planktic foraminifera` = foram_assem_res |> 
      summarise_singletons(), 
    `Testate amoebae` = testate_summ1 |> 
      summarise_singletons(), 
    `Pollen` = pollen_assem_res |> 
      ungroup() |> 
      summarise_singletons(),
    .id = "Dataset"
  )
}


make_richness_singleton_plot = function(bird_singletons, molten_singletons, foram_assem_res, testate_summ1, pollen_assem_res) {
  bind_rows(
  `Birds - species` = bird_singletons |> 
    ungroup() |>
    filter(taxonomic_level == "species"),
  `Birds - order` = bird_singletons |> 
    ungroup() |>
    filter(taxonomic_level == "order"),
  `Coastal diatoms` = molten_singletons,
  `Planktic foraminifera` = foram_assem_res, 
  `Testate amoebae` = testate_summ1, 
  `Pollen` = pollen_assem_res |> ungroup(),
  .id = "dataset"
)  |> 
  filter(n_taxa != 1) |> # remove monospecific samples - only birds mostly at order level
  mutate(
    dataset = factor(
      x = dataset, 
      levels = c("Birds - species", "Birds - order", "Coastal diatoms", 
                 "Planktic foraminifera", "Testate amoebae", "Pollen")),
    singletons = if_else(n_singletons > 0, 1, 0)
  ) |>
  arrange(gcd) |> 
  ggplot(aes(x = n_taxa, y = singletons, colour = gcd == 1)) +
  geom_point(position = position_jitter(width = 0.3, height = 0.2), show.legend = FALSE, alpha = 0.3) +
  geom_smooth(aes(group = 1), method = "gam", method.args = list(family = binomial), show.legend = FALSE, formula = y ~ s(x, bs = "cs"), se = FALSE, colour = "black") +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  scale_y_continuous(breaks = c(0, 1), labels = c("no", "yes")) +
  labs(x = "Taxonomic richness", y = "Singletons present", colour = "GCD = 1") +
  facet_wrap(facets = vars(dataset), scales = "free_x", ncol = 2)
  
}  