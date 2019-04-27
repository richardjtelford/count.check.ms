loadd(testate_counts)

summarise_counts <- function(counts){#columns sampleID/taxon/count
  results <- list()
  
  results$count_summary <- counts %>% 
    group_by(sampleID) %>% 
    summarise(min = min(count), n_taxa = n(), sum = sum(count), singletons = sum(count == 1))
  
  results$prop_singletons <- results$count_summary %>% 
    summarise(m = mean(min == 1)) %>% 
    pull(m)

  results$median_singletons <- results$count_summary %>% 
    summarise(m = median(singletons)) %>% 
    pull(m)

    #samples without singletons  
  

  
  results$no_singletons <- counts %>% 
    group_by(sampleID) %>%
    mutate(m = min(count)) %>%
    filter(m > 1) %>% #remove samples with singletons
    summarise(gcd = numbers::mGCD(count), n_taxa = n()) #find greatest common divisor
    
  results$gcd2plus <- results$no_singletons %>%
    filter(gcd > 1)
  
  results$gcd1 <- results$no_singletons %>% 
    summarise(prop1 = mean(gcd == 1)) %>% 
    pull(prop1)

  return(results)
  
}
