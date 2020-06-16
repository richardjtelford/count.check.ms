
unexpected_pollen_plan <- drake_plan(
  ##bad examples####
  pollen_unexpected_data = if(has_password){
    datasetID <- neotoma_secrets %>% 
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
    read_csv("data_backup/pollen_unexpected_data.csv") %>% 
      group_by(sampleID)
  },

  #### multiples_plot ####
  multiples_plot = {
    multiples = pollen_unexpected_data %>%
      group_by(depth) %>% 
      filter(datasetID == 2) %>% 
      summarise(n = n(), 
                div_2 = mean(count %% 2 == 0),
                div_3 = mean(count %% 3 == 0)) %>% 
      pivot_longer(cols = starts_with("div"), names_to = "divisible", values_to = "prop") %>% 
      mutate(
        percent = prop * 100,
        divisible = str_replace(string = divisible, pattern = ".*(\\d)", replacement = "italic(k)==\\1"))
    
    # null 
    multiples_null <- pollen %>% 
      filter(n() >= 10) %>% #remove low diversity assemb
      summarise(
        div_2 = mean(count %% 2 == 0),
        div_3 = mean(count %% 3 == 0)
      ) %>% 
      pivot_longer(cols = starts_with("div"), names_to = "divisible", values_to = "prop")  %>% 
      mutate(
        percent = prop * 100,
        divisible = str_replace(string = divisible, pattern = ".*(\\d)", replacement = "italic(k)==\\1"))
    
    multiples_99 <- multiples_null  %>% 
      group_by(divisible) %>% 
      summarise(q99 = quantile(percent, prob = 0.995)) 
    
    
    div_null = ggplot(multiples_null, aes(x = percent, fill = divisible)) +
      geom_histogram(boundary = 0) +
      geom_vline(data = multiples_99, mapping = aes(xintercept = q99, colour = divisible), linetype = "dashed") +
      scale_fill_brewer(palette = "Set1") +
      scale_colour_brewer(palette = "Set1") +
      scale_x_continuous(expand = c(0.02, 0)) +
      facet_wrap(~divisible, ncol = 1, labeller = label_parsed) + 
      labs(x = expression('%'~counts~divisible~by~italic(k)), y = "Frequency") +
      theme(legend.position = "none")
    
    div_data = multiples %>% 
      ggplot(aes(x = depth, y = percent, colour = divisible)) + 
      geom_line() +
      geom_point(size = 1) +
      geom_hline(data = multiples_99, mapping = aes(yintercept = q99, colour = divisible), linetype = "dashed") +
      scale_y_continuous(expand = c(0.02, 0)) +
      scale_colour_brewer(palette = "Set1") +
      labs(x = "Depth cm", y = expression('%'~counts~divisible~by~italic(k))) +
      facet_wrap(~divisible, ncol = 1, labeller = label_parsed) +
      theme(legend.position = "none")
    
    div_null + labs(tag = "A") +
      div_data + labs(tag = "B") 
  },
  
  
  
  #### pollen 3 multiples ####  
  pollen3_summ = pollen_unexpected_data %>% 
    filter(datasetID == 3) %>% 
    group_by(sampleID) %>% 
    pollen_summarise() %>% 
    ungroup() %>% 
    assert(in_set(3), gcd),# check multiples of three
  
  pollen3 = pollen3_summ %>% 
    summarise(
      n_samp = n(),
      n_count = sum(n_taxa),
      count_min = min(count_sum),
      count_max = max(count_sum),
      taxa_median = median(n_taxa)
    ),
  
  
  ## interpolated
  
  #### dataset 4/5 plot ####
  pollen4_plot = {
    plot_data <- pollen_unexpected_data %>% 
      filter(datasetID == 4) %>% 
      ungroup() %>% 
      select(datasetID, depth, taxa, count) %>%
      complete(taxa, nesting(datasetID, depth), fill = list(count = 0)) %>%
      group_by(datasetID, depth) %>% 
      mutate(gcd = factor(numbers::mGCD(count))) 

    count_depth <- plot_data %>%
      filter(taxa %in% c("Taxon 74",
                         "Taxon 70",
                         "Taxon 177")) %>% 
      mutate(
        taxa = fct_reorder(taxa, -count), 
        taxa = paste("Taxon", as.numeric(taxa))) %>% 
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
    unexpected_table = pollen_unexpected_data %>% 
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
                ) %>% 
    mutate(datasetID = paste0("pollen", datasetID)) %>% 
    rename(Dataset = datasetID)
)#end of plan


