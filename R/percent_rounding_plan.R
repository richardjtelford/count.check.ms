
percent_rounding_plan <- drake_plan(
  percent_rounding_sim = {
    dat <- tibble(spp = c("a", "b", "c"), count = c(1, 5, 19)) %>% 
      crossing(count_sum = 50:1000) %>% 
      mutate(percent = count / count_sum * 100)
  
    1:4 %>% 
      set_names() %>% 
      map_dfr(~{
        mutate(dat, percent = round(percent, .x)) %>% 
        estimate_n(percent_col = "percent", taxon_col = "spp", ID_cols = "count_sum",digits = .x, nmax = 1500) %>% 
          select(-direct_search, -data, -n_taxa) %>% 
          unnest(cols = direct_search_est)
    }, .id = "round")
  },
  
  percent_rounding_plot =  percent_rounding_sim %>% 
    pivot_longer(c(est_n_direct, est_n_minpc), names_to = "method", values_to = "estimate") %>% 
    mutate(method = factor(method, levels = c("est_n_minpc", "est_n_direct"), labels = c("Minimum percent", "Direct search"))) %>% 
    ggplot(aes(x = count_sum, y = round(estimate) - count_sum, colour = round)) +
    geom_line() + 
    scale_colour_viridis_d() +
    labs(x = "Count sum", y = "Estimated - actual count sum", colour = "Digits") +
    facet_wrap(~method) +
    theme(legend.position = c(0.01, .99), legend.justification = c(0, 1))

)#end of plan