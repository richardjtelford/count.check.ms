#diatoms
diatom_plan <- drake_plan(
  ####Owens Lake####
  owens = read_file(file = "http://diatom.ansp.org/dpdc/download.asp?type=count&DatasubsetID=51") %>% 
    gsub(pattern = "<.*?>", replacement = "", x = .) %>% 
    read_delim(delim = "\t"),
  
  owens_summ = owens %>% 
  group_by(SampleId) %>% 
  summarise(mn = min(Count), n_sing = sum(Count == 1), sum = sum(Count), n_taxa = n(), gcd = ifelse(n_taxa > 1, numbers::mGCD(Count), NA)),
  
  owens_summary2 = owens_summ %>% 
    filter(n_taxa > 1) %>% 
    summarise(
      n = n(),
      median_sing = median(n_sing),
      no_singletons = mean(mn != 1) * 100,
      no_single_lt_50 = mean(mn[sum < 50] != 1) * 100, 
      no_single50_ = mean(mn[sum >= 50] != 1) * 100, 
      gcd2 = mean(gcd[mn > 1] == 1) * 100,
      gcd_max = max(gcd, na.rm = TRUE)),
  
  
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
    
  # diatom2_integer = diatom2_data %>% 
  #   select(-(Event:Depth..m.)) %>% 
  #   filter(diatom2_est_count$est_n < 400) %>%
  #   percent_checker(digits = 2) %>% 
  #   count(precision, contains),

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



# Adirondack <- read_file(file = "GRIM/diatoms/download.asp.html") %>% 
#   gsub(pattern = "<.*?>", replacement = "", x = .) %>% 
#   read_delim(delim = "\t", skip = 1)
#   
  
# read_delim("http://diatom.ansp.org/dpdc/download.asp?type=count&DatasubsetID=48", delim = "\t")
# 
# 
# ne_diatoms <- read_file("https://archive.epa.gov/emap/archive-emap/web/txt/diataxva.txt")
# ne2 <- ne_diatoms  %>% gsub(pattern = "\"", replacement = "", x = .) %>%  read_delim(delim = ",", skip = 28) %>%
#   filter(COUNT != ".") %>% 
#   mutate(COUNT = as.numeric(COUNT), PERCENT = as.numeric(PERCENT))
# 
# ne_sum <- ne2  %>% 
#   group_by(COREPOS, LAKE_ID, INTERVAL, SAMP_ID) %>% 
#   summarise(n = n(), ss = sum(COUNT == 1), s = sum(COUNT), m = min(PERCENT))
# ne_sum
# 
# ne2 %>% group_by(COREPOS, LAKE_ID, INTERVAL, SAMP_ID) %>% 
#   mutate(sum = sum(COUNT)) %>% filter(sum < 500) %>% select(-INT_FRM, -INT_TO, -DATE_COL, -LON_DD, -LAT_DD, -`YEAR `) %>% arrange(sum) %>% View()
# 
# ggplot(ne_sum, aes(x = s, y = m)) + geom_point() + stat_function(geom = "line", fun = function(x)100/x)
# ggplot(ne_sum, aes(x = ss)) + geom_bar() 
# ggplot(ne2, aes(x = COUNT)) + geom_bar() + xlim(0, 30)
# 
# ne_sum %>% arrange(s)

# owens_summary %>% ggplot(aes(x = n_sing)) + geom_bar()
# owens %>% 
#   group_by(SampleId) %>% 
#   filter(CountSum >= 300) %>% 
#   nest() %>% 
#   sample_n(20) %>% 
#   unnest() %>% 
#   group_by(SampleId) %>% 
#   arrange(desc(Count)) %>% 
#   mutate(n = 1:n()) %>% 
#   ggplot(aes(x = n, y = Count, colour = factor(SampleId))) + 
#   geom_line(show.legend = FALSE) + scale_y_log10() + 
#   facet_wrap(~SampleId) + 
#   theme(strip.text = element_blank()) + 
#   labs(x = "Rank", y = "Abundance")
