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
    mutate(sampleID = as.numeric(sampleID)),
  
  #run analyses
  testate_summary = summarise_counts(testate_counts),

  testate_summ = list(
    nsamples = n_distinct(testate_counts$sampleID),
    n_gcd2plus = nrow(testate_summary$gcd2plus),
    n_gcd2 = sum(testate_summary$gcd2plus$gcd == 2),
    n_gcd3 = sum(testate_summary$gcd2plus$gcd == 3),
    n_gcd4 = sum(testate_summary$gcd2plus$gcd > 3) %>% insist(. == 0),
    n_gcd2_low_div = sum(testate_summary$gcd2plus$n_taxa < 5) 
  )
)

# testate_config <- drake_config(testate_plan)
# 
# make(testate_plan, jobs = 2)
# vis_drake_graph(testate_config, targets_only = TRUE)

