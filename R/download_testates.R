#process testate data


get_testate_table <- function(testate_data) {
  testate_data |> 
    samples() |> 
    filter(element == "test", # just testate data
           taxongroup == "Testate amoebae", # not Rotifers
           units == "NISP" # not percent
    ) |>
    filter(siteid != 26004) |> # misreported percent data
    select(count = value, variablename, sampleid, datasetid, siteid) |> 
    mutate(count = ceiling(count)) |> # half counts to full
    mutate(across(c(sampleid, datasetid), as.integer)) |> 
    group_by(sampleid, datasetid) |> 
    filter(sum(count) >= 50)
}




#  
#   testate_summ = testate_summ1 %>% 
#      summarise(
#        n_assemblages = n(),
#        count_max = max(count_sum),
#        count_min = min(count_sum),
#        count_median = median(count_sum),
#        tax_min = min(n_taxa),
#        tax_max = max(n_taxa),
#        tax_median = median(n_taxa),
#        p_singletons  = mean(n_singletons > 0) * 100,
#        median_singletons = median(n_singletons),
#        p_gcd1 = mean(gcd == 1) * 100,
#        n_gcd1 = sum(gcd > 1),
#        n_gcd2 = sum(gcd == 2),
#        n_gcd3 = sum(gcd == 3),
#        n_gcd2_low_div = sum(gcd[n_taxa < 5] > 1)
#      )
# 
