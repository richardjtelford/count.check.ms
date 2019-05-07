
#source functions
source("R/get_sites_meta.R")

#### drake plan ####
neotoma_plan <- drake_plan(
  #get dataset list
  pollen_sites = get_dataset(datasettype = "pollen"),

  #download_data
  pollen_data = pollen_sites %>% get_download(), #SLOW
  pollen_download_date = {
    pollen_data
    lubridate::today()#datestamp
  },
  
  #extract pollen counts
  pollen = pollen_data %>% 
    #remove elements known to be backtransformed from diagrams
    #1776 mud lake - cohmap documentation
    list_modify(`1776` = NULL) %>% 
    map_df(~mk_thin_pollen(counts(.), .$taxon.list), .id = "datasetID") %>% 
    filter(ecological.group %in% pollen_wanted, variable.units == "NISP") %>% 
    assert(in_set("pollen"), variable.element) %>% 
    select(-variable.units, -variable.element) %>% 
    mutate(
      datasetID = as.integer(datasetID), 
      sampleID = as.integer(sampleID)
          ) %>% 
    group_by(datasetID, sampleID) %>% 
    #remove very low count sums and monospecific assemblages
    filter(sum(count) >= 50, n() > 1) %>%
    #round slightly to correct near-integers backtransformed? from percent
    mutate(count = round(count, 3)) %>% 
    #filter out counts of 0.01 (in datasetID 17391 - presumably slide scanning for rareties)
    filter(count != 0.01) %>% 
    #filter out two datasets with probable percent data
    filter(!datasetID %in% c(15059, 25609)) %>% 
    mutate(count = ceiling(count)), 


  #process
  pollen_summ = tibble(
    n_datasets = n_distinct(pollen$datasetID),
    n_assemblages = floor(n_distinct(pollen$sampleID)/1000),
    p_singletons = mean(pollen_summ1$n_singletons > 0) * 100,
    p_gcd1 = mean(pollen_summ1$gcd == 1) * 100,
    n_gcd1 = sum(pollen_summ1$gcd > 1),
    threshold = 0.5, 
    n_gcd1_high = pollen_summ1 %>% 
      summarise(dgcd = mean(gcd == 1),  n = n()) %>% 
      filter(dgcd < threshold) %>% 
      summarise(n = sum(n)),
    p_gcd1_high = n_gcd1_high/n_gcd1 * 100,
    
    dataset_gcd1 = mean(pollen_summ2$gcd1 == 1) * 100,
    dataset_gcd.5 = sum(pollen_summ2$gcd1 < threshold)
    
  ),
  
  
  pollen_summ1 = pollen %>% 
    filter(n() > 1) %>% #should not remove any as low counts already removed
    summarise(count_sum = sum(count), n_taxa = n(), gcd = numbers::mGCD(count), n_singletons = sum(count == 1), min = min(count)),
  
  
  pollen_summ2 = pollen_summ1 %>% 
    summarise(n = n(), gcd1 = mean(gcd == 1), nosingletons = mean(n_singletons > 0)),
  

  #ecological groups
  pollen_wanted = c("TRSH", "UPHE", "SUCC", "PALM", "MANG"),

  ##bad examples
  pollenA = {
    datasetID = neotoma_secrets %>% filter(dataset == "A") 
    summ1 = pollen_summ1 %>% 
      semi_join(datasetID) %>% 
      ungroup() %>% 
      assert(in_set(3), gcd)
    
    tibble(
      n_samp = nrow(summ1),
      n_count = sum(summ1$n_taxa),
      count_min = summ1$count_sum %>% min,
      count_max = summ1$count_sum %>% max,
      taxa_median = summ1$n_taxa %>% median
    )
  },
  
  pollen1234 = {
    datasets = neotoma_secrets %>% filter(dataset %in% 1:4) 
    #check has spike
    datasets %>% pull(datasetID) %>% map(process_rare_data, pollen_data = pollen_data)
    
    #table of results
    table <- pollen_summ1 %>% 
      inner_join(datasets, by  = "datasetID") %>% 
      group_by(dataset) %>% 
      summarise(`No. assemblages` = n(), `Median no. taxa` = median(n_taxa), `GCD = 1` = mean(gcd == 1), `Median no. singletons (GCD = 1)` = median(n_singletons[gcd == 1])) 

    #figure    
    figure <- pollen_summ1 %>% 
      inner_join(datasets, by = "datasetID") %>%
      filter(dataset %in% 1:2) %>% 
      mutate(gcd = factor(gcd)) %>% 
      ggplot(aes(x = count_sum, y = n_taxa, colour = gcd, shape = gcd)) + 
      geom_point(show.legend = FALSE) +
      facet_wrap(~dataset) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Count sum", y = "Number of taxa") 
    figure
   # poll = pollen %>% semi_join(datasetID)
    
    summ <- tibble(
      max_richness = pollen_summ1 %>% 
        inner_join(datasets, by = "datasetID") %>%
        filter(gcd == 2, dataset %in% 1:2) %>%
        ungroup() %>% 
        summarise(max = max(n_taxa)) %>% 
        pull(max)
        
    )
    
    list(table = table, figure = figure, summ = summ)
  }
)#end of drake plan


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


