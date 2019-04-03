#chironomids

chironomid_plan <- drake_plan(
  #data for Last Chance Lake
  last_chance0 = read.table("https://www1.ncdc.noaa.gov/pub/data/paleo/insecta/chironomidae/northamerica/greenland/last-chance2017chironomid.txt", header = TRUE),
  
  last_chance_meta = last_chance0 %>% select(age_calBP, totcaps),
  last_chance = last_chance0 %>% select(-age_calBP, -totcaps),
  
  last_chance_n = estimate_n(spp = last_chance, digits = 2) %>% 
    bind_cols(last_chance_meta),
  
  last_chance_plot = last_chance_n %>%  
    ggplot(aes(x = totcaps, y = est_n, ymax = est_max, ymin = est_min)) + 
    geom_point() +
    geom_errorbar() +
    geom_abline(aes(slope = sl, intercept = 0, colour = sf, linetype = sf), data = tibble(sl = 1:2, sf = factor(sl)), show.legend = FALSE) +
    coord_equal() +
    labs(x = "Reported count sum", y = "Estimated count sum")
)
