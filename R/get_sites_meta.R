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