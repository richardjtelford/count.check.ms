#breeding bird survey

bird_plan <- drake_plan(
  #download data
  bird_download = {
    URL <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/50-StopData/1997ToPresent_SurveyWide/Fifty"
    1:10 %>% 
      map(
        ~download.file(
          url = paste0(URL, ., ".zip"), 
          destfile = paste0("./data/birds/Fifty", ., ".zip")
          )
        )
  },
  ##species list
  bird_species = read_lines("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", skip = 7)[-2] %>% 
    paste(collapse = "\n") %>% 
    read_fwf(col_positions = fwf_widths(widths = c(6, 6, 51, 51, 51, 51, 51, 51, 51))) %>% 
    set_names(slice(., 1)) %>% 
    slice(-1) %>%
    select(-French_Common_Name, -Species) %>%
    rename(Latin_name = Spanish_Common_Name),
  
  #extract bird data
  bird_data = list.files(path = "data/birds/", pattern = "\\.zip$", full.names = TRUE) %>% 
    map(read_csv) %>% 
    map(mutate, statenum  = as.numeric(statenum)) %>% 
    map(~mutate(., count = rowSums(select(., starts_with("Stop"))))) %>% 
    map_df(select, -starts_with("Stop")),
  
  #aggregate to different taxonomic levels
  bird_singletons = bird_process(bird_data, bird_species)

)

bird_singletons %>% 
  group_by(taxonomic_level) %>%   
  summarise(mean_singletons = mean(singletons == 0), n_singletons = sum(singletons == 0), mean_richness = mean(n_species))

  

cf <- drake_config(bird_plan)
make(bird_plan)

loadd(bird_data)


p1 <- nsf %>%
  mutate(no_si = singletons == 0) %>% 
  ggplot(aes(x = n_species, fill = no_si)) +
  geom_bar()+
  facet_wrap(~no_si, ncol = 1, scales = "free_y")
p1
p1 + aes(x = count_sum) + scale_x_log10()

bird_recount_singletons = bird_data %>% 
  group_by(RouteDataID) %>% 
  filter(sum(count) >= 400) %>% 
  left_join(bird_species, by = "AOU") %>% 
  group_by(RouteDataID, ORDER) %>% 
  summarise(count = sum(count)) %>% 
  nest() %>% 
  mutate(recount = map(data, ~recount_singletons(counts = .$count, k = c(10, 20, 30, 50, 100, 200, 300, 400)))) %>% 
  unnest(recount)

bird_recount_singletons %>% 
  mutate(new_count_sum = as.numeric(new_count_sum)) %>% 
  group_by(new_count_sum, rep) %>% 
  summarise(m = mean(no_singletons == 0)) %>% 
  summarise(m_no_singletons = mean(m), sd = sd(m), se = sd/sqrt(n()), n = n())

.Last.value %>% ggplot(aes(x = new_count_sum, y = m_no_singletons, ymin = m_no_singletons - 1.96 * se, ymax = m_no_singletons + 1.96 * se)) + geom_pointrange()

#resample to smaller count sum with multivariate hypergeometric & count singletons
recount_singletons <- function(counts, k, nrep = 10) {
 k %>% set_names() %>%
   map_df(~{
     y <- extraDistr::rmvhyper(nn = nrep, n = counts, k = .)
     tibble(no_singletons = rowSums(y == 1), rep = 1:nrep)
   }, .id = "new_count_sum")
}

#functions
#join and process at different taxonomic levels

bird_process <- function(bird_data, bird_species) {
  joined <- bird_data %>%
    left_join(bird_species, by = "AOU")
  
  out <- bind_rows(
    species = joined %>% rename(taxon = Latin_name),
    genus = joined %>% rename(taxon = Genus),
    family = joined %>% rename(taxon = Family),
    order = joined %>% rename(taxon = ORDER),
    .id = "taxonomic_level"
  ) %>%
    group_by(RouteDataID, year, taxonomic_level, taxon) %>%
    summarise(count = sum(count)) %>%
    group_by(RouteDataID, year, taxonomic_level) %>%
    summarise(
      n_species = n(),
      singletons = sum(count == 1),
      count_sum = sum(count)
    )
  return(out)
}


