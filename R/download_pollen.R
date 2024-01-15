# Netoma pollen data

get_pollen_table <- function(pollen_data) {
  
  offset <- 100
  start <- seq(1, length(pollen_data), offset)
 # start = start[1:10]
  map(start, \(x){
    pd <- pollen_data[[x:min(x + offset - 1, length(pollen_data))]] 
    pd <- pd |> 
      filter(!datasetid %in% c( ##!neotoma2::
        55055, 15059, 25609, 39474, 54508, # percent 
        16209, 16210, 15696 # possible percent (not counts anyway)
      ))
    pd |> 
    samples() |> 
      filter(
        taxongroup != "Laboratory analyses", 
       units == "NISP",
       element %in% c("pollen", "spore", "pollen/spore"),
        ecologicalgroup %in% c("MANG", "PALM", "TRSH", "UPHE", "VACR", "SUCC", "VASC")) |> 
      select(value, element, taxongroup, 
             variablename, ecologicalgroup, 
             sampleid, siteid, datasetid, depth) |> 
      mutate(across(c(sampleid, datasetid, siteid), as.integer)) |> 
      group_by(siteid, datasetid, sampleid) |> 
      # remove zero counts (not sure why there are any)
      # remove negative counts and small positive values - possibly from slide scanning.
      filter(value > 0.1) |>
      # ensure count sum >= 50 and not monospecific
      filter(sum(value) >= 50, n() > 1) |> 
      # fix near integer values, poss from back transforming percent
      mutate(count = round(value, 3)) |> 
      # convert partial grains to integer by rounding up
      mutate(count = ceiling(count)) |> 
      group_by(datasetid, sampleid) 
    
  }, .progress = TRUE) |> 
    list_rbind() 
    # remove spikes listed as taxa
    # filter(!(datasetID == 252 & taxa == "Alnus")) 

}

make_pollen_results <- function(pollen_assem_res, pollen_dataset_res) {
 tibble(
  n_datasets = nrow(pollen_dataset_res),
  n_assemblages = nrow(pollen_assem_res),
  n_assemblages_txt = floor1000(n_assemblages) %>% format(big.mark = ","),
  p_singletons = mean(pollen_assem_res$n_singletons > 0) * 100,
  p_gcd1 = mean(pollen_assem_res$gcd == 1) * 100,
  n_gcd1 = sum(pollen_assem_res$gcd > 1)#,
#  
  
) %>% bind_cols(#number of datasets needed to get half gcd > 1 assemblages
  pollen_assem_res %>% 
    group_by(datasetid) %>% 
    summarise(gcd_gt1 = sum(gcd > 1), no_singletons = sum(min > 1), threshold = 0.5) %>% 
    ungroup() %>% 
    summarise(
      dataset_gcd1 = mean(gcd_gt1 == 0) * 100,
      half_gcd = (1:n())[cumsum(sort(gcd_gt1, decreasing = TRUE)) >= threshold * sum(gcd_gt1)][1],
      half_no_sing = (1:n())[cumsum(sort(no_singletons, decreasing = TRUE)) >= threshold * sum(no_singletons)][1])#,
  ## assemblage has zeros
 # has_zeros = mean(pollen_assem_res$has_zero) 
  
)

}