#### marine data

#download marine1 dataset
download_marine1 <- function(has_password, secrets) {
  if(has_password){
      #get secret
      target <- secrets |>
        filter(dataset == "marine1") |>
        pull(secret)

      #download file
      m1 <- read_delim(target, delim = "\t", skip = 325) |>
        select(-(Event:`Si [µmol/l] (summer 10 m)`))

    } else {
      #get back-up if no saved secrets
      m1 <- read_csv("data_backup/marine1.csv")
    }
    m1
}


  #estimate n
estimate_marine1_n <- function(marine1) {
  marine1 |>
    rowid_to_column("sampleID") |>
    pivot_longer(-sampleID, names_to = "taxon", values_to = "percent") |>
    filter(percent > 0) |>
    estimate_n(ID_cols = "sampleID", digits = 2, nmax = 10000) |>
    mutate(direct_search_est = map(direct_search_est, slice, 1)) |> #take only first estimate if multiple  - only affects v large estimates
    unnest(direct_search_est) |>
    assertr::verify(score == 1)
}

  #summarise
summarise_marine1 <- function(marine1, marine1_est_n){
  list(
    nsamples = nrow(marine1),
    median_taxa = median(marine1_est_n$n_taxa),
    n_one_taxa = sum(marine1_est_n$n_taxa == 1),
    n100 = sum(between(marine1_est_n$est_n_direct, 2, 100)),
    est_n_min = marine1_est_n |>
      filter(n_taxa > 1) |>
      pull(est_n_direct) |>
      min(na.rm = TRUE)
    )
}