library("neotoma")

testate_plan <- drake_plan(
  #get dataset list
  testate_datasets = get_dataset(datasettype = "testate amoebae surface sample"),

  #download_data
  testate_data = testate_datasets %>% get_download(), #SLOW
  
  #
  #extract site information
  testate_meta = testate_datasets %>% 
    map("site.data") %>% 
    map_df(as_data_frame) %>% 
    bind_cols(testate_datasets %>% 
                map_df("dataset.meta")) %>% 
    mutate(authors = testate_datasets %>% 
             map("pi.data") %>% 
             map("ContactName") %>% 
             map_chr(paste, collapse = " ")),

  #some sites have only percent data
  testate_has_percent = testate_data %>%
    map("taxon.list") %>%
    map_lgl(~any(.$variable.units == "percent", na.rm = TRUE)),  
  
  #get counts
  testate_counts = testate_data[!testate_has_percent] %>%
    map(~({
      cnt <- counts(.)
      tl <- .$taxon.list %>% filter(variable.units == "NISP")
      cnt[, names(cnt) %in% tl$taxon.name]
      })) %>%
    map_df(gather, key = taxon, value = count, .id = "sampleID") %>% 
    mutate(sampleID = as.numeric(sampleID)) %>% 
    group_by(sampleID),
  
  #run analyses
  testate_summ1 = testate_counts %>% 
    summarise(count_sum = sum(count), n_taxa = n(), gcd = numbers::mGCD(count), n_singletons = sum(count == 1), min = min(count)) %>% 
    assert(within_bounds(1, 3), gcd),

 
  testate_summ = testate_summ1 %>% 
     summarise(
       n_assemblages = n(),
       count_max = max(count_sum),
       count_min = min(count_sum),
       count_median = median(count_sum),
       tax_min = min(n_taxa),
       tax_max = max(n_taxa),
       tax_median = median(n_taxa),
       p_singletons  = mean(n_singletons > 0) * 100,
       median_singletons = median(n_singletons),
       p_gcd1 = mean(gcd == 1) * 100,
       n_gcd1 = sum(gcd > 1),
       n_gcd2 = sum(gcd == 2),
       n_gcd3 = sum(gcd == 3),
       n_gcd2_low_div = sum(gcd[n_taxa < 5] > 1)
     )
)
