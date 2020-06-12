
#source functions
source("R/get_sites_meta.R")
source("R/hershop_pollen_plan.R")
source("R/unexpected_pollen_plan.R")

#### drake plan ####
neotoma_plan <- drake_plan(
  #get dataset list
  pollen_sites = get_dataset(datasettype = "pollen"),

  #download_data
  pollen_data = pollen_sites %>% get_download(), #SLOW
  pollen_download_date = pollen_data[[1]]$dataset$access.date %>% #datestamp
    as.Date(),

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
    #filter out counts of 0.01 (in datasetID 17391 - presumably slide scanning for rareties)
    filter(count != 0.01) %>% 
    #remove very low count sums and monospecific assemblages
    filter(sum(count) >= 50, n() > 1) %>%
    #round slightly to correct near-integers backtransformed? from percent
    mutate(count = round(count, 3)) %>% 
    #filter out two datasets with probable percent data
    filter(!datasetID %in% c(15059, 25609)) %>% 
    mutate(count = ceiling(count)), 


  #process
  pollen_summ = tibble(
    n_datasets = n_distinct(pollen$datasetID),
    n_assemblages = (floor(n_distinct(pollen$sampleID)/1000) * 1000) %>% format(big.mark = ","),
    p_singletons = mean(pollen_summ1$n_singletons > 0) * 100,
    p_gcd1 = mean(pollen_summ1$gcd == 1) * 100,
    n_gcd1 = sum(pollen_summ1$gcd > 1),
    threshold = 0.5, 
    n_gcd1_high = pollen_summ1 %>% 
      summarise(dgcd = mean(gcd == 1),  n = n()) %>% 
      filter(dgcd < threshold) %>% 
      summarise(n = sum(n)),
    p_gcd1_high = n_gcd1_high/n_gcd1 * 100,
    
    dataset_gcd1 = mean(pollen_summ2$gcd1 != 1) * 100,
    dataset_gcd.5 = sum(pollen_summ2$gcd1 < threshold)
    
  ),
  
  
  pollen_summ1 = pollen %>% 
    pollen_summarise(),
  
  pollen_summ2 = pollen_summ1 %>% 
    summarise(n = n(), 
              gcd1 = mean(gcd == 1), 
              nosingletons = mean(n_singletons > 0)),
  

  #ecological groups
  pollen_wanted = c("TRSH", "UPHE", "SUCC", "PALM", "MANG"),
  
)#end of drake plan

# bind pollen plans

pollen_plan <- bind_plans(
  neotoma_plan, 
  hershop_plan,
  unexpected_pollen_plan
) 

