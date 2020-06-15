#make thin pollen using correct column of taxon.list
mk_thin_pollen <- function(counts, taxon.list){
  if(any(names(taxon.list) == "alias")){
    name_column <- "alias"
  } else {
    name_column <- "taxon.name"
  }
  
  thin_pollen <- counts %>%
    rownames_to_column(var = "sampleID") %>% 
    pivot_longer(cols = -sampleID, names_to = "taxa", values_to = "count") %>% 
    filter(count > 0) %>% 
    left_join(taxon.list, by = c("taxa" = name_column)) %>% 
    
    #filter pollen/spores/ascospores - no charcoal in the counts 
    filter(variable.element %in% c("pollen", "spores", "ascospore")) %>% 
    as_tibble()
  return(thin_pollen)
}


# process rare data
process_rare_data <- function(ID, pollen_data){#browser()
  ID <- as.character(ID)
  #check has concentration data
  pollen_data[[ID]]$lab.data %>% 
    as.data.frame() %>% 
    select(matches("spike$")) %>% 
    verify(ncol(.) > 0)
  #check fungal etc also divisible by 2 - mostly 
  # pollen_data[[ID]] %>% 
  #   counts() %>% 
  #   rownames_to_column("sampleID") %>% 
  #   mutate(sampleID = as.numeric(sampleID)) %>% 
  #   gather(key = taxa, value = count, -sampleID) %>% 
  #   inner_join(pollen_summ1) %>% 
  #   filter(count > 0, gcd == 2) %>% 
  #   summarise(odd = sum(count %% 2 != 0)) %>% 
  #   assert(in_set(0), odd)
  
}

#dplyr function to summarise pollen counts
pollen_summarise <- . %>% 
  group_by(datasetID) %>% 
  mutate(n_taxa_dataset = n_distinct(taxa)) %>% 
  group_by(datasetID, sampleID) %>% 
  summarise(count_sum = sum(count),
            n_taxa = n(), 
            gcd = numbers::mGCD(count), 
            n_singletons = sum(count == 1), 
            min = min(count),
            n_taxa_dataset = first(n_taxa_dataset),
            has_zero = n_taxa_dataset > n_taxa
            )
