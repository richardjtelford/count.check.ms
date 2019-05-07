#diatoms
diatom_plan <- drake_plan(
  ####Owens Lake####
  owens = read_file(file = "http://diatom.ansp.org/dpdc/download.asp?type=count&DatasubsetID=51") %>% 
    gsub(pattern = "<.*?>", replacement = "", x = .) %>% 
    read_delim(delim = "\t"),
  
  owens_summ1 = owens %>% 
  group_by(SampleId) %>% 
  summarise(mn = min(Count), n_sing = sum(Count == 1), count_sum = sum(Count), n_taxa = n(), gcd = ifelse(n_taxa > 1, numbers::mGCD(Count), NA)),
  
  owens_summ = owens_summ1 %>% 
    filter(n_taxa > 1) %>% 
    summarise(
      n = n(),
      count_min = min(count_sum),
      count_max = max(count_sum),
      median_sing = median(n_sing),
      p_singletons  = mean(n_sing  > 0) * 100,
      no_singletons = mean(mn != 1) * 100,
      no_single_gt_50 = mean(mn[count_sum > 50] == 1) * 100, 
      no_single50_ = mean(mn[count_sum <= 50] == 1) * 100, 
      gcd2 = mean(gcd == 1) * 100,
      gcd_max = max(gcd, na.rm = TRUE)),
  
  ####owens diatoms percent####
  owens_est = owens %>% 
    estimate_n(ID_cols = c("SampleId", "SampleCode", "CountSum"), percent_col = "Percent", taxon_col = "TaxonShortName", digits = 4) %>%
    assertr::verify(map_int(direct_search_est, nrow) == 1) %>% #check only one row in each direct_search_est
    unnest(direct_search_est) %>% 
    assertr::verify(score == 1),#check score ==1
  
  owens_est_summ = owens_est %>% 
    select(est_n_minpc, est_n_direct, CountSum, n_taxa) %>% 
    filter(n_taxa > 1) %>% 
    summarise(
      minpc = mean(round(est_n_minpc, 3) == CountSum) * 100, 
      direct = mean(est_n_direct == CountSum) * 100
    ),
  
  owens_fail = owens_est %>% 
    select(est_n_minpc, est_n_direct, CountSum, n_taxa) %>%
    filter(n_taxa >1, est_n_direct!=CountSum),
  
  owens_plot = owens_est %>% 
    filter(between(n_taxa, 8, 15), between(CountSum, 80, 150)) %>% 
    slice(4) %>%  
    select(c(SampleId:n_taxa), score, est_n_direct) %>% 
    unnest(direct_search) %>% 
    gather(key = taxon, value = presence, -c(SampleId:est_n_direct, n, score1)) %>% 
    ggplot(aes(x = taxon, y = n, fill = presence)) + 
    geom_raster(show.legend = FALSE) +
    scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
    labs(y = "Putative count sum") +
    scale_fill_manual(values = c("white", "black")),
  
  ####diatom1####
  diatom1_data = {target <- secrets %>% 
    filter(dataset == "diatom1") %>% 
    pull(secret)
    read_delim(target, delim = "\t", comment = "#")
  },
  
  diatom1_estn = diatom1_data %>% 
    gather(key = taxon, value = percent, -(depth_cm:age_calBP)) %>%
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
  
  diatom1_summ = list(
    nsamples = nrow(diatom1_data),
    median_taxa = median(diatom1_estn$n_taxa),
    maxpc = max(diatom1_estn$minpc),
    n_maxpc = sum(diatom1_estn$minpc == max(diatom1_estn$minpc)),
    prop_integer = diatom1_pcheck %>% 
      filter(contains == "integer") %>%
      pull(prop),
    integer_mult = ceiling(400/min(diatom1_estn$est_n_minpc)),
    low_n = diatom1_estn %>% 
      summarise(n = min(est_n_direct)) %>% 
      pull(n),
    n_low_n = diatom1_estn %>% 
      summarise(n = sum(est_n_direct < 400)) %>% 
      pull(n),
    n_taxa = ncol(diatom1_data) - 2
     
  ) %>% 
    insist(.$n_maxpc == 2),

  ####diatom2####
  diatom2_data = {
    target <- secrets %>% 
      filter(dataset == "diatom2") %>% 
      pull(secret)
    read_delim(target, delim = "\t", skip = 216) %>% set_names(make.names(names(.))) %>% 
      filter(rowSums(select(., -(Event:Depth..m.))) > 0) #drop zero count
  },

  diatom2_est_n = diatom2_data %>% 
    select(-(Latitude:Depth..m.)) %>% 
    gather(key = taxon, value = percent, -Event) %>%
    filter(percent > 0) %>% 
    estimate_n(ID_cols = "Event", digits = 2) %>% 
    assertr::verify(map_int(direct_search_est, nrow) == 1) %>% #check only one row in each direct_search_est, 
    unnest(direct_search_est) %>% 
    assertr::verify(score == 1),

  diatom2_summ  = list(
    nsamples = nrow(diatom2_data),
    median_taxa = median(diatom2_est_n$n_taxa),
    n400 = sum(diatom2_est_n$est_n_direct < 400),
    n100 = sum(diatom2_est_n$est_n_direct < 100),
    maxpc = max(diatom2_est_n$minpc), 
    rich_maxpc = diatom2_est_n$n_taxa[which.max(diatom2_est_n$minpc)],
    meth_agree = diatom2_est_n %>% 
      transmute(agree = est_n_direct >= est_min_minpc & est_n_direct <= est_max_minpc) %>% 
      pull(agree) %>% 
      all(),
    integer_mult = ceiling(100/min(diatom2_est_n$est_n_minpc))
    
  )
)