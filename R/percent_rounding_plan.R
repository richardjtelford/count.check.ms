# effect of rounding on precision of estimates

simulate_percent_rounding <- function(bird_download) {
    test_data <- bird_download |> 
      filter(RouteDataID == 6168100) |> 
      select(AOU, SpeciesTotal) |> 
      uncount(SpeciesTotal)
    
    counts <- map(.x = 50:1000, 
            ~ test_data |> 
              slice_sample(n = .x) |> 
              count(AOU) |> 
              mutate(percent = n/.x * 100, count_sum = .x)
            ) |> 
      list_rbind()
  
    1:3 |> 
      set_names() |> 
      map(~{
        mutate(counts, percent = round(percent, .x)) |> 
        estimate_n(percent_col = "percent", taxon_col = "AOU", 
                   ID_cols = "count_sum", digits = .x, nmax = 1500) |> 
          select(-direct_search, -data, -n_taxa) |> 
          unnest(cols = direct_search_est)
      }) |> 
      list_rbind(names_to = "round")
  }
  
plot_percent_rounding <- function(percent_rounding_sim){
  percent_rounding_sim |> 
    pivot_longer(c(est_n_direct, est_n_minpc), names_to = "method", values_to = "estimate") |> 
    mutate(method = factor(method, levels = c("est_n_minpc", "est_n_direct"), labels = c("Minimum percent", "Direct search"))) |> 
    ggplot(aes(x = count_sum, y = count_sum - round(estimate), colour = round)) +
    geom_line() + 
    scale_colour_viridis_d(end = 0.95) +
    labs(x = "Count sum", y = "Actual - estimated count sum", colour = "Digits") +
    facet_wrap(facets = vars(method)) +
    theme(legend.position = c(0.99, .01), legend.justification = c(1, 0))
}

