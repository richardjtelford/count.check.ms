#pollen

pollen_plan <- drake_plan(
 #download
 pollen1_data = {
   target <- secrets %>% 
     filter(dataset == "pollen1") %>% 
     pull(secret) %>% 
     as.numeric()
   neotoma::get_download(target)
   },
 #reformat
 pollen1_counts = neotoma::counts(pollen1_data)[[1]] %>%
   as_tibble(rownames = ".id") %>% 
   gather(key = "taxa", value = "count", -.id) %>%
   filter(count > 0) %>% 
   group_by(.id),
 
 #summarise
 pollen1_summary = list(
   count = pollen1_counts %>% 
     summarise(GCD = numbers::mGCD(count), min = min(count), n = n(), sum = sum(count)) %>% 
     summarise(has_singletons = sum(min == 1), GCD2 = sum(GCD > 1), GCD4 = sum(GCD ==4), min_count = min(sum), max_count = max(sum), max2_count = max(sum[-which.max(sum)])),
   
   individual = pollen1_counts %>% 
     ungroup() %>%
     summarise(one = sum(count == 1), two = sum(count %% 2 == 0), n = n(), not_two = n - two, singleton_spp = taxa[count == 1]),
   
   max_trilete =  pollen1_counts %>% 
     filter(taxa == "Unknown (trilete)") %>% 
     ungroup() %>% 
     summarise(m = max(count)) %>% 
     pull(m)
 )
)


# cf <- drake_config(pollen_plan)
# make(pollen_plan, keep_going = TRUE)
# vis_drake_graph(cf)




