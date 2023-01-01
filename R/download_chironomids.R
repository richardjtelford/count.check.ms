# chironomids


# data for Last Chance Lake
download_last_chance <- function() {
  last_chance <- read.table(
    file = "https://www1.ncdc.noaa.gov/pub/data/paleo/insecta/chironomidae/northamerica/greenland/last-chance2017chironomid.txt",
    header = TRUE
  )

  last_chance
}

# estimate n
estimate_last_chance_n <- function(last_chance) {
  print(class(last_chance))
  print(filter)
  last_chance <- last_chance |>
    pivot_longer(
      cols = c(-age_calBP, -totcaps),
      names_to = "taxon", values_to = "percent"
    ) |>
    filter(percent > 0) |> 
    estimate_n(ID_cols = c("age_calBP", "totcaps"), digits = 2)
}


plot_last_chance_direct_search <- function(last_chance_n){
  last_chance_n |>
  filter(age_calBP == 1099) |>
  select(age_calBP, totcaps, direct_search) |>
  unnest(direct_search) |>
  filter(n <= 300) |>
  ggplot(aes(x = n, y = score)) +
  geom_point() +
  labs(x = "Putative count sum", y = "Score")
}

plot_last_chance <- function(last_chance_n){
  last_chance_n |>
  unnest(direct_search_est) |>
  ggplot(aes(x = totcaps, y = est_n_minpc)) +
  geom_abline(aes(slope = sl, intercept = 0, linetype = sf), data = tibble(sl = 1:2, sf = factor(sl)), show.legend = FALSE, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymax = est_max_minpc, ymin = est_min_minpc)) +
  geom_point(mapping = aes(x = totcaps, y = est_n_direct), shape = "x", colour = "red") +
  coord_equal() +
  labs(x = "Reported count sum", y = "Estimated count sum")
}

summarise_last_chance <- function(last_chance){
  list(
    nsamples = nrow(last_chance)
  )
}

# # chironomid1
# # download and import
download_chironomid1 <- function(has_password, secrets) {
  if (has_password) {
    target <- secrets |>
      filter(dataset == "chironomid1") |>
      pull(secret)
    download.file(url = target, destfile = "data/chironomid1.xlsx")
    read_excel("data/chironomid1.xlsx", sheet = 2, skip = 2) |>
      rename(sampleID = `Sample code`) |>
      pivot_longer(cols = c(-sampleID, -`Core Depth (cm)`, -`Year (AD)`),
                   names_to = "taxon",
                   values_to = "percent") |>
      filter(percent > 0)
  } else {
    read_csv(file = "data_backup/chironomid1.csv")
  }
}

# estimate n
estimate_chironomid1_n <- function(chironomid1) {
  chironomid1 |>
  estimate_n(digits = 4, 
             ID_cols = c("sampleID", "Core Depth (cm)", "Year (AD)")) |>
  assertr::verify(map_int(direct_search_est, nrow) == 1) |> # check only one row in each direct_search_est
  unnest(direct_search_est)
}

# summarise
summarise_chironomid1 <- function(chironomid1, chironomid1_est_n) {
    list(
    nsamples = n_distinct(chironomid1$sampleID),
    n_below50 = chironomid1_est_n |>
      filter(est_n_direct < 50) |>
      nrow(),
    min_below50 = chironomid1_est_n |>
      filter(est_n_direct < 50) |>
      pull(est_n_direct) |>
      min(),
    max_below50 = chironomid1_est_n |>
      filter(est_n_direct < 50) |>
      pull(est_n_direct) |>
      max(),
    richness = chironomid1_est_n |>
      filter(est_n_direct < 50) |>
      mutate(n_taxa = map_int(data, nrow)) |>
      pull(n_taxa) |>
      range()
  )
}
# 
# # chironomid2
# chironomid2_summ <- read_csv(file_in("data/chironomid2.csv")) |>
#   select(ID, starts_with("Chironomid")) |>
#   pivot_longer(cols = -ID, names_to = "taxon", values_to = "count") |>
#   group_by(ID) |>
#   summarise(count_sum = sum(count), no_singletons = sum(count == 1)) |>
#   summarise(n_samp = n(), count_min = min(count_sum), count_max = max(count_sum), min_singletons = min(no_singletons))
