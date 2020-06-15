
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
    #filter out four datasets with probable percent data
    filter(!datasetID %in% c(15059, 25609, 16209, 16210)) %>% 
    mutate(count = ceiling(count)) %>% 
    #remove unnecessary columns
    select(datasetID, sampleID, taxa, count) %>% 
    # sometimes spikes have not been correctly identified
    filter(!(datasetID == 252 & taxa == "Alnus")) %>% 
    #remove duplicate dataset == 21903
    filter(datasetID != 47543), 
  


  #process
  pollen_summ = tibble(
    n_datasets = nrow(pollen_summ2),
    n_assemblages = (floor(nrow(pollen_summ1)/1000) * 1000) %>% format(big.mark = ","),
    p_singletons = mean(pollen_summ1$n_singletons > 0) * 100,
    p_gcd1 = mean(pollen_summ1$gcd == 1) * 100,
    n_gcd1 = sum(pollen_summ1$gcd > 1),
    dataset_gcd1 = mean(pollen_summ2$gcd1 < 1) * 100
    
  ) %>% bind_cols(#number of datasets needed to get half gcd > 1 assemblages
    pollen_summ1 %>% 
      group_by(datasetID) %>% 
      summarise(gcd = sum(gcd > 1), no_singletons = sum(min > 1), threshold = 0.5) %>% 
      ungroup() %>% 
      summarise(
        half_gcd = (1:n())[cumsum(sort(gcd, decreasing = TRUE)) >= threshold * sum(gcd)][1],
        half_no_sing = (1:n())[cumsum(sort(no_singletons, decreasing = TRUE)) >= threshold * sum(no_singletons)][1]),
    ## assemblage has zeros
    has_zeros = mean(pollen_summ1$has_zero) 
    
  ),
  
  
  pollen_summ1 = pollen %>% 
    pollen_summarise(),
  
  pollen_summ2 = pollen_summ1 %>% 
    summarise(n = n(), 
              gcd1 = mean(gcd == 1), 
              nosingletons = mean(n_singletons > 0), 
              n_taxa_dataset = first(n_taxa_dataset)
              ),
  

  #ecological groups
  pollen_wanted = c("TRSH", "UPHE", "SUCC", "PALM", "MANG"),
  
)#end of drake plan

# bind pollen plans

pollen_plan <- bind_plans(
  neotoma_plan, 
  hershop_plan,
  unexpected_pollen_plan
) 

