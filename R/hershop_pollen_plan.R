#Hershop bog, neotoma datasetID 3132

hershop_plan <- drake_plan(
  hershop_pollen = counts(pollen_data[["3132"]]) %>% 
    rownames_to_column(var = "sampleID") %>% 
    pivot_longer(-sampleID, names_to = "taxa", values_to = "count") %>%
    filter(count > 0),
    
  hershop_pollen_summ1 = hershop_pollen %>% 
    group_by(sampleID) %>% 
    summarise(
      pollen_spore_count = sum(count), 
      pollen_count = sum(count[taxa != "Unknown (trilete)"]), 
      gcd = numbers::mGCD(count)), 
  
  hershop_pollen_summ = hershop_pollen_summ1 %>%
    ungroup() %>% 
    summarise(
    n = n(),
    has_singleton = sum(gcd == 1), 
    pollen_gt_200 = mean(pollen_count >= 200),
    pollen_spore_gt_200 = mean(pollen_spore_count >= 200),
    pollen_spore_200 = mean(pollen_spore_count == 200)
    )
)#end plan
