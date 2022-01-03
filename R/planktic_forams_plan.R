planktic_foram_plan <- drake_plan(
  foram_data = read_csv(file = "https://www.ncei.noaa.gov/pub/data/paleo/paleocean/brown_foram/bfd.csv") %>% 
    clean_names() %>% 
    select(-core_device, -average_depth, -split_fraction) %>% 
    mutate(sampleID = 1:n()) %>% 
    select(-ruber_total, -sacc_total, -(total_fragments:pteropods)) %>% 
    #filter(menardii + tumida + m_flexuosa == menardii_complex_total) %>% 
    select(-menardii_complex_total) %>% 
    pivot_longer(universa:other_un_id, names_to = "taxon", values_to = "count") %>% 
    filter(count > 0),
    
  foram_summ = foram_data %>% 
    group_by(sampleID) %>% 
    summarise_counts(),
  
  
  # summary table of all count data
  count_summary = bind_rows(
    `Birds - species` = bird_singletons %>% 
      ungroup() %>%
      filter(taxonomic_level == "species") %>% 
      summarise_singletons(),
    `Birds - genus` = bird_singletons %>% 
      ungroup() %>%
      filter(taxonomic_level == "genus") %>% 
      summarise_singletons(),
    `Birds - family` = bird_singletons %>% 
      ungroup() %>%
      filter(taxonomic_level == "family") %>% 
      summarise_singletons(),
    `Birds - order` = bird_singletons %>% 
      ungroup() %>%
      filter(taxonomic_level == "order") %>% 
      summarise_singletons(),
    `Coastal diatoms` = molten_singletons %>% 
      summarise_singletons(),
    `Planktic foraminifera` = foram_summ %>% 
      summarise_singletons(), 
    `Testate amoebae` = testate_summ1 %>% 
      summarise_singletons(), 
    `Pollen` = pollen_summ1 %>% 
      ungroup() %>% 
      summarise_singletons(),
    .id = "Dataset"
  ),
  
  richness_singleton_plot = bind_rows(
    `Birds - species` = bird_singletons %>% 
      ungroup() %>%
      filter(taxonomic_level == "species"),
    `Birds - order` = bird_singletons %>% 
      ungroup() %>%
      filter(taxonomic_level == "order"),
    `Coastal diatoms` = molten_singletons,
    `Planktic foraminifera` = foram_summ , 
    `Testate amoebae` = testate_summ1, 
    `Pollen` = pollen_summ1 %>% ungroup(),
    .id = "dataset"
  )  %>% 
    filter(n_taxa != 1) %>% # remove monospecific samples - only birds mostly at order level
    mutate(
      dataset = factor(
        x = dataset, 
        levels = c("Birds - species", "Birds - order", "Coastal diatoms", 
                   "Planktic foraminifera", "Testate amoebae", "Pollen")),
      singletons = if_else(n_singletons > 0, 1, 0)
    ) %>%
    arrange(gcd) %>% 
    ggplot(aes(x = n_taxa, y = singletons, colour = gcd == 1)) +
    geom_point(position = position_jitter(width = 0.3, height = 0.2), show.legend = FALSE, alpha = 0.3) +
    geom_smooth(aes(group = 1), method = "gam", method.args = list(family = binomial), show.legend = FALSE, formula = y ~ s(x, bs = "cs"), se = FALSE, colour = "black") +
    scale_colour_viridis_d(option = "C", end = 0.9) +
    scale_y_continuous(breaks = c(0, 1), labels = c("no", "yes")) +
    labs(x = "Taxonomic richness", y = "Singletons present", colour = "GCD = 1") +
    facet_wrap(~ dataset, scales = "free_x", ncol = 2)

)# end of plan