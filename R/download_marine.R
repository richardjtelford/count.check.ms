marine_plan <- drake_plan(

  marine1 = {
    target <- secrets %>%
      filter(dataset == "marine1") %>%
      pull(secret)
    read_delim(target, delim = "\t", skip = 324)
  },

  marine1_est_n = marine1 %>% 
    select(-(Event:`Si [µmol/l] (summer 10 m)`)) %>% 
    rowid_to_column("sampleID") %>%  
    gather(key = taxon, value = percent, -sampleID) %>% 
    filter(percent > 0) %>% 
    estimate_n(ID_cols = "sampleID", digits = 2, nmax = 10000) %>% 
    mutate(direct_search_est = map(direct_search_est, slice, 1)) %>% #take only first estimate if multiple  - only affects v large estimates
    unnest(direct_search_est) %>%
    assertr::verify(score == 1),
  
  marine1_summ = list(
    nsamples = nrow(marine1),
    median_taxa = median(marine1_est_n$n_taxa),
    n_one_taxa = sum(marine1_est_n$n_taxa == 1),
    n100 = sum(between(marine1_est_n$est_n_direct, 2, 100)),
    est_n_min = marine1_est_n %>% 
      filter(n_taxa > 1) %>% 
      pull(est_n_direct) %>% 
      min(na.rm = TRUE)
    )
  
)


# x %>% unnest(direct_search_est) %>% group_by(sampleID) %>% slice(1) %>% ungroup() %>% select(c(1:3, 6), score, est_n_direct, n_taxa) %>% filter(est_n_minpc < 100)
# 
# marine1 %>% 
#   select(-(Event:`Si [µmol/l] (summer 10 m)`)) %>% 
#   rowid_to_column("sampleID") %>%  
#   gather(key = taxon, value = percent, -sampleID) %>% 
#   filter(percent > 0) %>%
#   group_by(sampleID) %>% 
#   mutate(minpc = min(percent)) %>% 
#   filter(percent >= 5) %>% 
#   group_by(sampleID, percent) %>% 
#   mutate(n = n() - 1) %>% 
#   group_by(sampleID) %>% 
#   summarise(mn = first(minpc), s = sum(n))
#   
# 
# marine1 %>% 
#   select(-(Event:`Si [µmol/l] (summer 10 m)`)) %>% 
#   rowid_to_column("sampleID") %>%  
#   gather(key = taxon, value = percent, -sampleID) %>% 
#   filter(percent > 0) %>% 
#   estimate_n(ID_cols = "sampleID", digits = 2, nmax = 1000) %>% 
#   unnest(direct_search_est) %>% 
#   group_by(sampleID) %>% 
#   slice(1) %>% 
#   select(1:6, score, est_n_direct) -> z
# 
# ggplot(z, aes(x = score))+ geom_histogram()
