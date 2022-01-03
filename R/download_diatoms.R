#diatoms
diatom_plan <- drake_plan(
  #### MOLTEN ####
  
  molten_download = target({
      sqlite_file <- "data/define.sqlite"
      download.file(
        url = "https://github.com/richardjtelford/kustkarnor/blob/master/data/define.sqlite?raw=true", 
        destfile = sqlite_file)
      sqlite_file
    },
    format = "file"),
  molten_data = {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = molten_download)
    
    tbl(con, "counts") %>%
      #merge synonyms
      left_join(tbl(con, "Synonyms"), by = c("taxonCode" = "synonymCode")) %>%
      mutate(taxonCode = coalesce(correctCode, taxonCode)) %>%
      #get country code for merges & drop samples with no chem - "RIB16102b"
      inner_join(tbl(con, "fchem") %>% select(siteId, countryId), by  = "siteId") %>%
      left_join(tbl(con, "Merges"), by = c("taxonCode" = "oldtaxonCode", "countryId")) %>%
      mutate(
        taxonCode = coalesce(mergedTaxonCode, taxonCode)
      ) %>%
      #remove excluded taxa
      anti_join(tbl(con, "ExcludedTaxa"), by = "taxonCode") %>%
      #select required columns
      select(siteId, taxonCode, count) %>%
      #sum merged taxa
      group_by(siteId, taxonCode) %>%
      collect() %>%
      assert(not_na, everything()) %>%
      summarise(count = sum(count, na.rm = TRUE)) # , na.rm = TRUE to avoid warning
    
    },
  molten_singletons = molten_data %>% summarise_counts(), 
  
  ####diatom1####
  diatom1_data = if(has_password){
    target <- secrets %>% 
      filter(dataset == "diatom1") %>% 
      pull(secret)
    read_delim(target, delim = "\t", comment = "#")
  } else {
    read_csv(file = "data_backup/diatom1.csv")
  },
  
  diatom1_estn = diatom1_data %>% 
    pivot_longer(cols = -(depth_cm:age_calBP), names_to = "taxon", values_to = "percent") %>%
    filter(percent > 0) %>% 
    estimate_n(ID_cols = c("depth_cm", "age_calBP"), digits = 2) %>% 
    assertr::verify(map_int(direct_search_est, nrow) == 1) %>% 
    unnest(direct_search_est) %>% #check only one row in each direct_search_est
    assertr::verify(score == 1),
  
  diatom1_pcheck = diatom1_data %>% 
    select(-age_calBP) %>%
    percent_checker( site_column = "depth_cm", digits = 2) %>% 
    select(-one_max,-one_min, -est_min, -est_max) %>% 
    count(contains) %>% 
    mutate(prop = n/sum(n)),
  
  diatom1_summ = diatom1_estn %>% 
    ungroup() %>% 
    summarise(
      nsamples = n(),
      median_taxa = median(n_taxa),
      maxpc = max(minpc),
      n_maxpc = sum(minpc == max(minpc)),
      prop_integer = diatom1_pcheck %>% 
        filter(contains == "integer") %>%
        pull(prop),
      integer_mult = ceiling(400/min(est_n_minpc)),
      low_n = min(est_n_direct),
      n_low_n = sum(est_n_direct < 400),
      n_taxa = ncol(diatom1_data) - 2
  ) %>% 
    insist(.$n_maxpc == 2),

  ####diatom2####
  diatom2_data = if(has_password){
    target <- secrets %>% 
      filter(dataset == "diatom2") %>% 
      pull(secret)
    read_delim(target, delim = "\t", skip = 216) %>%
      set_names(make.names(names(.))) %>% 
      filter(rowSums(select(., -(Event:Depth..m.))) > 0) %>%  #drop zero count
      select(-(Latitude:Depth..m.)) %>% 
      pivot_longer(cols = -Event, names_to = "taxon", values_to = "percent") %>%
      filter(percent > 0)
  } else {
    read_csv(file = "data_backup/diatom2.csv")
  },

  diatom2_est_n = diatom2_data %>% 
    estimate_n(ID_cols = "Event", digits = 2) %>% 
    assertr::verify(map_int(direct_search_est, nrow) == 1) %>% #check only one row in each direct_search_est, 
    unnest(direct_search_est) %>% 
    assertr::verify(score == 1),

  diatom2_summ  = diatom2_est_n %>% 
    ungroup() %>% 
    summarise(
      nsamples = n(),
      median_taxa = median(n_taxa),
      n400 = sum(est_n_direct < 400),
      n100 = sum(est_n_direct < 100),
      maxpc = max(minpc), 
      rich_maxpc = n_taxa[which.max(minpc)],
      meth_agree = all(est_n_direct >= est_min_minpc & est_n_direct <= est_max_minpc),
      integer_mult = ceiling(100/min(est_n_minpc))
  )
)