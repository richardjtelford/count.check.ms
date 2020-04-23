#chironomids

chironomid_plan <- drake_plan(
  #data for Last Chance Lake
  last_chance = read.table("https://www1.ncdc.noaa.gov/pub/data/paleo/insecta/chironomidae/northamerica/greenland/last-chance2017chironomid.txt", header = TRUE),
  
  last_chance_n = last_chance %>% 
    pivot_longer(cols = c(-age_calBP, -totcaps), names_to = "taxon", values_to = "percent") %>% 
    filter(percent > 0) %>% 
    estimate_n(ID_cols = c("age_calBP", "totcaps"), digits = 2),
  
  last_chance_direct_search_plot = last_chance_n %>% 
    filter(age_calBP == 1099) %>% 
    unnest(direct_search) %>% 
    ggplot(aes(x  = n, y = score)) + 
    geom_point() +
    labs(x = "Putative count sum", y = "Score") + 
    xlim(NA, 300),
  
  last_chance_plot = last_chance_n %>% 
    unnest(direct_search_est) %>%   
    ggplot(aes(x = totcaps, y = est_n_minpc)) + 
    geom_abline(aes(slope = sl, intercept = 0, linetype = sf), data = tibble(sl = 1:2, sf = factor(sl)), show.legend = FALSE, colour = "grey50") +
    geom_point() +
    geom_errorbar(aes(ymax = est_max_minpc, ymin = est_min_minpc)) +
    geom_point(mapping = aes(x = totcaps, y = est_n_direct), shape = "x", colour = "red") +
    coord_equal() +
    labs(x = "Reported count sum", y = "Estimated count sum"),
  
  last_chance_summ = list(
    nsamples = nrow(last_chance)
  ),
  
  #chironomid1
  #download and import
  chironomid1_download = {
    if(has_password){
      target <- secrets %>% 
        filter(dataset == "chironomid1") %>% 
        pull(secret)
      download.file(url = target, destfile = file_out("data/chironomid1.xlsx"))
    }
  },
  
  chironomid1 = if(has_password){
    read_excel(file_in("data/chironomid1.xlsx"), sheet = 2, skip = 2) %>% 
    rename(sampleID = `Sample code`) %>% 
    pivot_longer(cols = c(-sampleID, -`Core Depth (cm)`, -`Year (AD)`), names_to = "taxon", values_to = "percent") %>% 
    filter(percent > 0)
  } else {
      read_csv(file = "data_backup/chironomid1.csv")
    }
  ,
  
  #estimate n
  chironomid1_est = chironomid1 %>% 
    estimate_n(digits = 4, ID_cols = c("sampleID", "Core Depth (cm)", "Year (AD)")) %>% 
    assertr::verify(map_int(direct_search_est, nrow) == 1) %>% #check only one row in each direct_search_est 
    unnest(direct_search_est),
  
  #summarise
  chironomid1_summ = list(
    nsamples = n_distinct(chironomid1$sampleID),
    n_below50 = chironomid1_est %>% 
      filter(est_n_direct < 50) %>% 
      nrow(),
    min_below50 = chironomid1_est %>% 
      filter(est_n_direct < 50) %>% 
      pull(est_n_direct) %>%
      min(),
    max_below50 = chironomid1_est %>% 
      filter(est_n_direct < 50) %>% 
      pull(est_n_direct) %>% 
      max(),
    richness = chironomid1_est %>% 
      filter(est_n_direct < 50) %>% 
      mutate(n_taxa = map_int(data, nrow)) %>% 
      pull(n_taxa) %>% 
      range()
  
  ),
  
  #chironomid2
  chironomid2_summ = read_csv(file_in("data/chironomid2.csv")) %>% 
    select(ID, starts_with("Chironomid")) %>% 
    pivot_longer(cols = -ID, names_to = "taxon", values_to = "count") %>% 
    group_by(ID) %>% 
    summarise(count_sum = sum(count), no_singletons = sum(count == 1)) %>% 
    summarise(n_samp = n(), count_min = min(count_sum), count_max = max(count_sum), min_singletons = min(no_singletons))
)
