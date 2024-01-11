#Hershop bog, neotoma datasetID 3132


  process_hershop_pollen <- function(pollen_data) { 
    
  hershop_pollen <- extract_dataset(pollen_data, 3132)

  hershop_pollen_summ1 <- hershop_pollen %>% 
    group_by(sampleid) %>% 
    summarise(
      pollen_spore_count = sum(count), 
      pollen_count = sum(count[variablename != "Unknown (trilete)"]), 
      gcd = numbers::mGCD(count))
  
  hershop_pollen_summ <-  hershop_pollen_summ1 %>%
    ungroup() %>% 
    summarise(
      n = n(),
      has_singleton = sum(gcd == 1), 
      pollen_gte_200 = mean(pollen_count >= 200),
      pollen_spore_gte_200 = mean(pollen_spore_count >= 200),
      pollen_spore_200 = mean(pollen_spore_count == 200)
    )
  
  hershop_pollen_summ
}  
  
#### Wildhorse Lake ####  
  # wildhorse = pollen %>% filter(datasetID == 15894) %>% 
  # ungroup() %>% 
  # mutate(n_taxa_dataset = n_distinct(taxa)) %>% 
  # group_by(datasetID, sampleID) %>% 
  # summarise( #summarise samples
  #   count_sum = sum(count), 
  #   n_singletons = sum(count == 1), 
  #   n_taxa = n(), 
  #   n_taxa_dataset = first(n_taxa_dataset)) %>% 
  # summarise(#summarise dataset
  #   n_assemblage = n(), 
  #   median_count = median(count_sum), 
  #   no_singletons = mean(n_singletons == 0), 
  #   no_zeros = mean(n_taxa != n_taxa_dataset), 
  #   n_taxa_dataset = first(n_taxa_dataset))
