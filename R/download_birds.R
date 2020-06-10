#breeding bird survey
source("R/bird_functions.R")

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
  bird_species = read_lines("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", skip = 8)[-2] %>% 
    paste(collapse = "\n") %>% 
    read_fwf(col_positions = fwf_widths(widths = c(6, 6, 51, 51, 51, 51, 51, 51, 51))) %>% 
    set_names(slice(., 1)) %>% 
    slice(-1) %>%
    select(-French_Common_Name, -Species) %>%
    rename(Latin_name = Spanish_Common_Name) %>% 
    mutate(AOU = as.integer(AOU)),
  
  #extract bird data
  bird_data = {
    bird_download # force dependency
    list.files(path = "data/birds/", pattern = "\\.zip$", full.names = TRUE) %>% 
    map(read_csv) %>% 
    map(rename_at, vars(matches("StateNum")), tolower) %>%   
    map(mutate, statenum  = as.numeric(statenum)) %>% 
    map(~mutate(., count = rowSums(select(., starts_with("Stop"))))) %>% 
    map_df(select, -starts_with("Stop"))
    },
  
  #aggregate to different taxonomic levels
  bird_singletons = bird_process(bird_data, bird_species),

  bird_singleton_summary = bird_singletons %>%
    group_by(taxonomic_level) %>%
    summarise(
      p_singletons = mean(singletons > 0) * 100,
      n_singletons = sum(singletons == 0),
      median_richness = median(n_taxa),
      n_routes = n(),
      GCD1 = mean(GCD == 1, na.rm = TRUE) * 100,
      GCD2plus = mean(GCD > 1, na.rm = TRUE) * 100,
      GCD_max = max(GCD, na.rm = TRUE)
    ) %>%
    mutate(taxonomic_level = factor(
      taxonomic_level,
      levels = c("species", "genus", "family", "order")
    )) %>%
    arrange(taxonomic_level),
  
  # #bird order GCD summary
  # bird_order_GCD = bird_singletons %>% 
  #   ungroup() %>% 
  #   filter(singletons == 0, taxonomic_level == "order", !is.na(GCD)) %>%
  #   group_by(n_taxa) %>% 
  #   summarise(t = sum(GCD > 1, na.rm = TRUE), n = n()),
  
  #simulate counts with mv hypergeometric
  bird_recount_singletons = bird_data %>% 
    group_by(RouteDataID) %>% 
    filter(sum(count) >= 400) %>% 
    left_join(bird_species, by = "AOU") %>% 
    group_by(RouteDataID, ORDER) %>% 
    summarise(count = sum(count)) %>% 
    nest() %>% 
    mutate(recount = map(data, ~recount_singletons(counts = .$count, k = c(30, 50, 100, 200, 300, 400)))) %>% 
    unnest(recount) %>% 
    mutate(new_count_sum = as.numeric(new_count_sum)) %>% 
    group_by(new_count_sum, rep) %>% 
    summarise(m = mean(no_singletons > 0) * 100) %>% 
    summarise(m_no_singletons = mean(m), sd = sd(m), se = sd/sqrt(n())),
  
  bird_recount_singleton_plot = ggplot(
      data = bird_recount_singletons, 
      aes(x = new_count_sum, y = m_no_singletons, 
          ymin = m_no_singletons - 1.96 * se, 
          ymax = m_no_singletons + 2 * se)
    ) + 
    geom_errorbar() + 
    geom_point(size = 0.5) +
    labs(x = "Count sum", y = "Percent with singletons")
  )#end of drake_plan
 

# cf <- drake_config(bird_plan)
# make(bird_plan, keep_going = TRUE)
# vis_drake_graph(cf)


