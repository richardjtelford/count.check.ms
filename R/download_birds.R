#breeding bird survey
source("R/bird_functions.R")

bird_plan <- drake_plan(
  #download data
  bird_download = {
    bbs <- get_bbs_data(bbs_version = 2020, sb_dir = "data/birds2020")
    #remove unneeded components
    bbs$weather <- NULL
    bbs$routes <- NULL
    #select required columns
    bbs$species_list <- bbs$species_list %>% 
      select(order, family, genus, species, AOU)
    bbs$observations <- bbs$observations %>% 
      select(RouteDataID, AOU, SpeciesTotal)
    bbs
    },
  
  #aggregate to different taxonomic levels
  bird_singletons = bird_process(bird_download$observations, bird_download$species_list),

  bird_singleton_summary = bird_singletons %>%
    group_by(taxonomic_level) %>%
    summarise(
      p_singletons = mean(singletons > 0) * 100,
      n_singletons = sum(singletons == 0),
      median_richness = median(n_taxa),
      n_routes = n(),
      GCD1 = mean(GCD == 1, na.rm = TRUE) * 100,
      GCD2plus = mean(GCD > 1, na.rm = TRUE) * 100,
      GCD_max = max(GCD, na.rm = TRUE)
    ) %>%
    mutate(taxonomic_level = factor(
      taxonomic_level,
      levels = c("species", "genus", "family", "order")
    )) %>%
    arrange(taxonomic_level),
  
  #simulate counts with mv hypergeometric
  bird_recount_singletons = bird_download$observations %>% 
    group_by(RouteDataID) %>% 
    filter(sum(SpeciesTotal) >= 400) %>% 
    left_join(bird_download$species_list, by = "AOU") %>% 
    group_by(RouteDataID, order) %>% 
    summarise(count = sum(SpeciesTotal)) %>% 
    nest() %>% 
    mutate(recount = map(data, ~recount_singletons(counts = .$count, k = c(30, 50, 100, 200, 300, 400)))) %>% 
    unnest(recount) %>% 
    mutate(new_count_sum = as.numeric(new_count_sum)) %>% 
    group_by(new_count_sum, rep) %>% 
    summarise(m = mean(no_singletons > 0) * 100) %>% 
    summarise(m_no_singletons = mean(m), sd = sd(m), se = sd/sqrt(n())),
  
  bird_recount_singleton_plot = ggplot(
      data = bird_recount_singletons, 
      aes(x = new_count_sum, y = m_no_singletons, 
          ymin = m_no_singletons - 1.96 * se, 
          ymax = m_no_singletons + 2 * se)
    ) + 
    geom_errorbar() + 
    geom_point(size = 0.5) +
    labs(x = "Count sum", y = "Percent with singletons")
  )#end of drake_plan
 