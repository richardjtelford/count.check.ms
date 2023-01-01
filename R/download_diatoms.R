#diatoms
  #### MOLTEN ####
  
download_molten <- function() {
      sqlite_file <- "data/define.sqlite"
      download.file(
        url = "https://github.com/richardjtelford/kustkarnor/blob/master/data/define.sqlite?raw=true", 
        destfile = sqlite_file)
      sqlite_file
    }
  
load_molten_data <- function(molten_file) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = molten_file)
    
    tbl(con, "counts") |>
      #merge synonyms
      left_join(tbl(con, "Synonyms"), by = c("taxonCode" = "synonymCode")) |>
      mutate(taxonCode = coalesce(correctCode, taxonCode)) |>
      #get country code for merges & drop samples with no chem - "RIB16102b"
      inner_join(tbl(con, "fchem") |> select(siteId, countryId), by  = "siteId") |>
      left_join(tbl(con, "Merges"), by = c("taxonCode" = "oldtaxonCode", "countryId")) |>
      mutate(
        taxonCode = coalesce(mergedTaxonCode, taxonCode)
      ) |>
      #remove excluded taxa
      anti_join(tbl(con, "ExcludedTaxa"), by = "taxonCode") |>
      #select required columns
      select(siteId, taxonCode, count) |>
      #sum merged taxa
      group_by(siteId, taxonCode) |>
      collect() |>
      assert(not_na, everything()) |>
      summarise(count = sum(count, na.rm = TRUE)) # , na.rm = TRUE to avoid warning
    
}


  ####diatom1####
download_diatom1 = function(has_password, secrets) {
    if(has_password){
    target <- secrets |>
      filter(dataset == "diatom1") |>
      pull(secret)
    read_delim(target, delim = "\t", comment = "#") |>
      select(-age_calBP) |> 
      rename(sampleID = depth_cm) |> 
      pivot_longer(cols = -sampleID, 
                   names_to = "taxon",
                   values_to = "percent") |>
      filter(percent > 0)
  } else {
    read_csv(file = "data_backup/diatom1.csv")
  }
}

estimate_diatom_n <- function(data, digits, id = "sampleID") {
  data  |>
    estimate_n(ID_cols = id, digits = digits) |>
    assertr::verify(map_int(direct_search_est, nrow) == 1) |>
    unnest(direct_search_est) |> #check only one row in each direct_search_est
    assertr::verify(score == 1)
}

  # diatom1_pcheck = diatom1_data |>
  #   select(-age_calBP) |>
  #   percent_checker( site_column = "depth_cm", digits = 2) |>
  #   select(-one_max,-one_min, -est_min, -est_max) |>
  #   count(contains) |>
  #   mutate(prop = n/sum(n)),

summarise_diatom <- function(est_n, data){
    est_n |>
    ungroup() |>
    summarise(
      nsamples = n(),
      median_taxa = median(n_taxa),
      n400 = sum(est_n_direct < 400),
      n100 = sum(est_n_direct < 100),
      maxpc = max(minpc),
      n_maxpc = sum(minpc == max(minpc)),
      # prop_integer = diatom1_pcheck |>
      #   filter(contains == "integer") |>
      #   pull(prop),
      rich_maxpc = n_taxa[which.max(minpc)],
      meth_agree = all(est_n_direct >= est_min_minpc & est_n_direct <= est_max_minpc),
      low_n = min(est_n_direct),
      n_low_n = sum(est_n_direct < 400),
      n_taxa = n_distinct(data$taxon)
  )
}
  ####diatom2####
 download_diatom2 <- function(has_password, secrets){
   if(has_password){
    target <- secrets |>
      filter(dataset == "diatom2") |>
      pull(secret)
    read_delim(target, delim = "\t", skip = 217) |>
      select(-(Latitude:`Depth [m]`)) |>
      rename(sampleID = Event) |> 
      pivot_longer(cols = -sampleID, names_to = "taxon", values_to = "percent") |>
      filter(percent > 0)
  } else {
    read_csv(file = "data_backup/diatom2.csv")
  }
 }


