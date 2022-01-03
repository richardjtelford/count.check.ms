#breeding bird survey
source("R/bird_functions.R")

bird_plan <- drake_plan(
  #download data
  bird_download = {
    bbs <- get_bbs_data(bbs_version = 2020, sb_dir = "data/birds2020")

    #select required columns
    bbs$observations %>% 
      select(RouteDataID, AOU, SpeciesTotal)
    },
  
  bird_species = read_table("data/birds2020/SpeciesList.txt", skip = 9) %>% 
    slice(-1) %>% # row of dashes
    select(-Seq, -French_Common_Name, -English_Common_Name, -Species) %>% 
    rename(Species = Spanish_Common_Name, Order = ORDER), 
  
  #aggregate to different taxonomic levels
  bird_singletons = bird_process(bird_download, bird_species),

  bird_singleton_summary = bird_singletons %>%
    group_by(taxonomic_level) %>%
    summarise(
      p_singletons = mean(n_singletons > 0) * 100,
      n_singletons = sum(n_singletons == 0),
      median_richness = median(n_taxa),
      n_routes = n(),
      GCD1 = mean(gcd == 1, na.rm = TRUE) * 100,
      GCD2plus = mean(gcd > 1, na.rm = TRUE) * 100,
      GCD_max = max(gcd, na.rm = TRUE), 
      .groups = "drop_last"
    ) %>%
    mutate(taxonomic_level = factor(
      taxonomic_level,
      levels = c("species", "genus", "family", "order")
    )) %>%
    arrange(taxonomic_level),
  
  bird_summ = list(
    n_without_singletons = bird_singletons %>% filter(taxonomic_level == "species", n_singletons == 0) %>% nrow(),
    n_GCD_gt1 = bird_singletons %>% filter(taxonomic_level == "species", gcd > 1 | is.na(gcd))
    
  ),
  
  #simulate counts with mv hypergeometric
  bird_recount_singletons = bird_download %>% 
    group_by(RouteDataID) %>% 
    filter(sum(SpeciesTotal) >= 400) %>% 
    left_join(bird_species, by = "AOU") %>% 
    group_by(RouteDataID, Order) %>% 
    summarise(count = sum(SpeciesTotal), .groups = "drop_last") %>% 
    nest() %>% 
    mutate(recount = map(data, ~recount_singletons(counts = .$count, k = c(30, 50, 100, 200, 300, 400)))) %>% 
    unnest(cols = recount) %>% 
    mutate(new_count_sum = as.numeric(new_count_sum)) %>% 
    group_by(new_count_sum, rep) %>% 
    summarise(m = mean(no_singletons > 0) * 100, .groups = "drop_last") %>% 
    summarise(m_no_singletons = mean(m), sd = sd(m), se = sd/sqrt(n()), .groups = "drop_last"),
  
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
 