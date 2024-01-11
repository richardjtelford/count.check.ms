import_forams <- function() {
  foram_data = read_csv(
      file = "https://www.ncei.noaa.gov/pub/data/paleo/paleocean/brown_foram/bfd.csv", 
      locale = locale(encoding = "latin1")
    ) |> 
    janitor::clean_names() |> 
    select(-core_device, -average_depth, -split_fraction) |> 
    mutate(sampleID = 1:n()) |> 
    select(-ruber_total, -sacc_total, -(total_fragments:pteropods)) |> 
    #filter(menardii + tumida + m_flexuosa == menardii_complex_total) |> 
    select(-menardii_complex_total) |> 
    pivot_longer(universa:other_un_id, names_to = "taxon", values_to = "count") |> 
    filter(count > 0) |> 
    group_by(sampleID)
}    

