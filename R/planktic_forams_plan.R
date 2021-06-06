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
    summarise(count_sum = sum(count), 
              n_taxa = n(), 
              gcd = numbers::mGCD(count), 
              n_singletons = sum(count == 1), 
              min = min(count)),
  
  
  # 
  count_summary = bind_rows(
    `Planktic foraminifera` = foram_summ %>% 
      summarise(
        `No. Samples` = n(),
        `Median count sum` = median(count_sum),
        `Median richness` = median(n_taxa),
        `Singletons %` = mean(n_singletons >= 1) * 100,
        `GCD1 %` = mean(gcd == 1) * 100
      ), 
    `Testate amoebae` = testate_summ1 %>% 
      summarise(
        `No. Samples` = n(),
        `Median count sum` = median(count_sum),
        `Median richness` = median(n_taxa),
        `Singletons %` = mean(n_singletons >= 1) * 100,
        `GCD1 %` = mean(gcd == 1) * 100
      ), 
    
    .id = "Dataset"
  )
  
)# end of plan
