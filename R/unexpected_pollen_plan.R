
unexpected_pollen_plan <- drake_plan(
  ##bad examples####
  #multiple 3
  pollenA_data = if(has_password){
    datasetID <- neotoma_secrets %>% 
      filter(dataset == 1) %>% 
      select(-problem)
    pollen %>% 
      inner_join(datasetID) %>% 
      mutate(datasetID = dataset) %>% 
      select(-dataset)
  } else {
    read_csv("data_backup/pollenA_data.csv") %>% 
      group_by(sampleID)
  },
  
  pollenA_summ = pollenA_data %>% 
    pollen_summarise() %>% 
    ungroup() %>% 
    assert(in_set(3), gcd),# check multiples of three
  
  pollenA = pollenA_summ %>% 
    summarise(
      n_samp = n(),
      n_count = sum(n_taxa),
      count_min = min(count_sum),
      count_max = max(count_sum),
      taxa_median = median(n_taxa)
    ),
  
  
  ## interpolated
  pollen23_data = if(has_password){
      datasetID <- neotoma_secrets %>% filter(dataset %in% 2:3) %>% 
        select(-problem)
      
      depths <- pollen_data[as.character(pull(datasetID, datasetID))] %>% 
        map_df("sample.meta") %>% 
        select(depth, sample.id)
      
      pollen %>% 
        ungroup() %>% 
        inner_join(datasetID) %>% 
        left_join(depths, by = c("sampleID" = "sample.id")) %>% 
        mutate(datasetID = dataset) %>% 
        select(-dataset) %>% 
        mutate(taxa = paste("Taxon", as.numeric(factor(taxa)))) # anonymise taxa
    } else {
      read_csv("data_backup/pollen23_data.csv")
    },
  
  #### dataset 2/3 plot ####
  pollen23_plot = {
    plot_data <- pollen23_data %>% 
      filter(datasetID == 2) %>% 
      ungroup() %>% 
      select(datasetID, depth, taxa, count) %>%
      complete(taxa, nesting(datasetID, depth), fill = list(count = 0)) %>%
      group_by(datasetID, depth) %>% 
      mutate(gcd = factor(numbers::mGCD(count))) 

    count_depth <- plot_data %>%
      filter(taxa %in% c("Taxon 41",
                         "Taxon 38",
                         "Taxon 100")) %>% 
      mutate(taxa = fct_reorder(taxa, -count)) %>% 
      ggplot(aes(x = depth, y = count)) + 
      geom_line(colour = "grey50") + 
      geom_point(aes(colour = gcd, shape = gcd), show.legend = FALSE) +
      scale_x_reverse() +
      coord_flip() +
      scale_colour_brewer(palette = "Set1") +
      labs(x = "Depth cm", y = "Count") +
      facet_wrap(~ taxa, scales = "free_x")
    
    prop_depth <- plot_data %>% 
      group_by(taxa) %>% 
      arrange(depth) %>% 
      mutate(
        mean_neighbour = if_else(
          condition = count == 0 & lead(count) == 0 & lag(count) == 0,
          true = NA, # run of zeros
          false = (lead(count) + lag(count)) / 2 == count)) %>%
      group_by(depth, gcd) %>% 
      summarise(m = mean(mean_neighbour, na.rm = TRUE)) %>%
      ggplot(aes(x = depth, y = m)) +
      geom_point(aes(colour = gcd, shape = gcd), show.legend = FALSE) +
      geom_line(colour = "grey50") +
      coord_flip() +
      scale_x_reverse() +
      scale_y_continuous(labels = scales::percent) +
      scale_colour_brewer(palette = "Set1") +
      labs(x = "Depth cm", y = "Percent of taxa") +
      theme(axis.title.y = element_blank())
    
    #combine plots
    count_depth + labs(tag = "A") + 
      prop_depth + labs(tag = "B") + 
      plot_layout(widths = c(2, 1))
  },
  
    #table of results
    unexpected_table = bind_rows(
      pollenA_data, 
      pollen23_data) %>% 
      group_by(datasetID, sampleID) %>% 
      summarise(
        richness = n(), 
        countsum = sum(count),
        gcd = numbers::mGCD(count)) %>% 
      summarise(
        `No. assemblages` = n(),
        `Median richness` = median(richness), 
        `Median countsum` = median(countsum),
        `Percent GCD > 1` = mean(gcd > 1) * 100
                )
)#end of plan

