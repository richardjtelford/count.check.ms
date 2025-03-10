# Download neotoma data in batches

get_download_batch <- function(datasets, offset = 50) {
  starts <- seq(1, length(datasets), offset)
  downloads <- map(starts, \(x) {
    get_downloads(datasets[x:min(x + offset - 1, length(datasets))])
  })
  reduce(downloads, .f = c)
}

## extract one pollen dataset

extract_dataset <- function(sites, datasetid) {
  siteid <- getids(sites) |> 
    filter(datasetid == {{datasetid}}) |> 
    pull(siteid)
  
  
  sites[which(map_int(sites@sites, "siteid") == siteid)] |> 
    samples() |> 
    select(age, units, count = value, element, variablename, ecologicalgroup, sampleid, depth, datasetid, siteid, sitename)
}

##  calculate GCD and singleton data per sample

summarise_counts <- function(df) {
  df |> 
  summarise(
    count_sum = sum(count), 
    n_taxa = n(), 
    min = min(count),
    n_singletons = sum(count == 1), 
    # GCD is trivially one if there are singletons so no need to calculate
    gcd = fast_mGCD(n_taxa = n_taxa, count = count, n_singletons = n_singletons, min = min), 
    .groups = "drop_last"
  )
}

#only find GCD where no singletons for speed
# When n_taxa == 1, mGCD will give an error, so return min
fast_mGCD <- function(n_taxa, count, n_singletons, min) {
  if (n_taxa == 1){# mGCD will crash. Result is trivially min
    return(min)
  }
  else if (n_singletons > 0){ #GCD is trivially 1
    return(1)
  }
  else {
    numbers::mGCD(count)
  }
}

## summarise GCD and singleton data per data set

summarise_singletons <- function(df) {
  df |> 
  filter(count_sum >= 50, n_taxa > 1) |> 
  summarise(
    `Nr. Samples` = n(),
    `Median count sum` = median(count_sum),
    `Median richness` = median(n_taxa),
    `Without singletons` = glue("{sum(n_singletons == 0)} ({round(mean(n_singletons == 0) * 100, 3)}%)"),
    `GCD > 1` = glue("{sum(gcd > 1)} ({round(mean(gcd > 1) * 100, 3)}%)"),
    .groups = "drop"
  ) |> 
  mutate(across(c(`Without singletons`, `GCD > 1`), as.character))

}

floor1000 <- function(x, base = 1000) {
  floor(x / base) * base
}

