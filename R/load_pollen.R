
# database and targets

library(tidyverse)

con <- DBI::dbConnect(drv  = RPostgreSQL::PostgreSQL(), dbname = "neotoma")

DBI::dbListTables(con) |> str_subset("dataset")

# set default schema
DBI::dbExecute(con, "SET search_path TO ndb;")


datasettypes <- tbl(con, "datasettypes") |>
  select(datasettypeid, datasettype) |>
  filter(datasettype == "pollen")


samples <- tbl(con, "datasets") |>
  select(datasetid, datasettypeid) |>
  semi_join(datasettypes, by = join_by(datasettypeid)) |>
  select(-datasettypeid) |>
  inner_join(tbl(con, "samples") |>
               select(sampleid, datasetid), by = join_by(datasetid))



taxa <- tbl(con, "taxa") |>
  select(taxonid, taxonname) |>
  inner_join(tbl(con, "ecolgroups") |>
               select(taxonid, ecolgroupid), by = join_by(taxonid))

data <- tbl(con, "variables") |>
  select(variableid, taxonid, variableelementid, variableunitsid, variablecontextid) |>
  inner_join(tbl(con, "data") |>
               select(sampleid, variableid, value), by  = join_by(variableid)) |>
  select(-variableid) |>
  inner_join(taxa, by = join_by(taxonid)) |>
  select(-taxonid)


sample_data <- samples |>
  inner_join(data, by = join_by(sampleid)) |>
  inner_join(tbl(con, "variableelements") |>
               select(variableelementid, variableelement), by = join_by(variableelementid)) |>
  inner_join(tbl(con, "variableunits") |>
               select(variableunitsid, variableunits), by = join_by(variableunitsid)) |>
  select(-variableunitsid, -variableelementid, -variablecontextid)

pollen_wanted = c("TRSH", "UPHE", "SUCC", "PALM", "MANG", "VACR")

collected_data <- sample_data  |>
  filter(variableelement %in% c("pollen", "pollen/spore", "spore")) |>
  filter(variableunits == "NISP") |>
  select(-variableelement, -variableunits) |>
  filter(ecolgroupid %in% pollen_wanted) |>
  select(-ecolgroupid) |>
  collect()

cleaned_data <- collected_data |>
  #filter out counts << 1 (eg in datasetID 17391 - presumably slide scanning for rareties)
  filter(value > 0.1) |>
  #remove very low count sums and monospecific assemblages
  filter(sum(value) >= 50, n() > 1, .by = sampleid) |>
  #round slightly to correct near-integers backtransformed? from percent
  mutate(value = round(value, 2)) |>
  #filter out four datasets with probable percent data
  filter(!datasetid %in% c(15059, 25609, 16209, 16210)) |>
  mutate(value = ceiling(value)) |>
  # sometimes spikes have not been correctly identified
  filter(!(datasetid == 252 & taxonname == "Alnus")) |>
  filter(!(datasetid %in% c(794, 2598, 17326, 41348, 41345, 46695, # either has spike in neotoma or explicit in paper
                            17324 # presumed given abundance, climate and absence from pollen diagram in paper, author used Eucalyptus as spike in other sites
                            ) & taxonname == "Eucalyptus")) |>
  filter(!(datasetid %in% c(4355, # constant count - probably spike concentration
                            55804, 42419, 49447 # Lycopodium spike in paper
  ) & taxonname == "Lycopodium"))

########

sites <- tbl(con, "sites") |>
  inner_join(tbl(con, "collectionunits"), by = join_by(siteid)) |>
  inner_join(tbl(con, "datasets"), by = join_by(collectionunitid)) |>
  collect()
sites |> filter(datasetid == 252)


k <- cleaned_data |> filter(stringr::str_detect(taxonname, "Lycop")) |>
  group_by(datasetid) |>
  summarise(m = mean(value)) |>
 filter(m > 10) |>
  inner_join(sites, by = join_by(datasetid))

world <- rnaturalearth::ne_countries()
ggplot(world) +
  geom_sf() +
  geom_point(aes(x = longitudeeast, y = latitudenorth, colour = m), data = k)


cleaned_data |> filter(stringr::str_detect(taxonname, "Lycopodium")) |>
  group_by(datasetid) |>
  inner_join(sites, by = join_by(datasetid)) |>
  filter(value > 100) |>
  View()

#dbDisconnect(con, shutdown = TRUE)

cleaned_data |>
  filter(datasetid == "21903")

library(numbers)

sample_summ <- cleaned_data |> group_by(datasetid, sampleid) |>
  summarise(mgcd = mGCD(value), n = n(), s = sum(value), p2 = mean(value %% 2 == 0), p3 = mean(value %% 3 == 0), single = mean(value == 1))

dataset_summ <- sample_summ |>
  group_by(datasetid) |>
  summarise(n = n(), gcd1 = mean(mgcd == 1), mx = max(mgcd),
            m2 = mean(p2 > 0.7),
            m3 = mean(p3 > 0.6),
            m23 = mean(p2 > 0.7 | p3 > 0.6),
            sing = mean(single))


dataset_summ |> arrange(gcd1)

world <- rnaturalearth::ne_countries()
k <- sites |> semi_join(dataset_summ |> filter(mx > 5))
k <- sites |> semi_join(dataset_summ |> filter(gcd1 < 0.9))

ggplot() +
  geom_sf(data = world) +
  geom_point(aes(x = as.numeric(longitudeeast), y = latitudenorth), data = k) +
  theme(axis.title = element_blank())



pubs <- function(datasetid) {
  x <- tbl(con, "datasetpublications") |>
    filter(datasetid == {{datasetid}}) |>
    left_join(tbl(con, "publications"), by = join_by(publicationid)) |>
    select(-starts_with("recdate"), -pubtypeid) |>
    collect() |>
    select(publicationid, primarypub, year, citation, doi)

  if(all(is.na(x$doi))){
    x$doi <- NULL
  }
  if(all(is.na(x$primarypub))){
    x$primarypub <- NULL
  }
  x
}

pub_data <- function(publicationid) {
  tbl(con, "datasetpublications") |>
    filter(publicationid == {{publicationid}}) |>
    left_join(tbl(con, "datasets"), by = join_by(datasetid)) |>
    select(-starts_with("recdate")) |>
    collect()

}

pubs(45393)
pub_data(16886)

pub_year <- tbl(con, "datasetpublications") |>
  left_join(tbl(con, "publications") |> select(publicationid, pubtypeid, year,  citation, doi), by = join_by(publicationid)) |>
  select(-starts_with("recdate")) |>
  select(-pubtypeid) |>
  collect()

dataset_summ |>
  filter(gcd1 < 0.9) |>
  left_join(pub_year, by = join_by(datasetid)) |>
  filter(year > 2000) |>
  print(n = Inf)

sample_summ |> filter(datasetid == 49000)


pbinom(9, 10, 0.5, lower.tail = FALSE)

# doubles and triples
dataset_summ |> filter(n > 10) |>  slice_max(m23, n = 10)

plot(0:20, dbinom(0:20, 20, 0.5), type = "l")

count_freq <- cleaned_data |> filter(value <= 100) |> count(value)
ggplot(count_freq, aes(x = log(value), y = log(n))) + geom_point() + geom_smooth()
mod <- lm(log(n) ~ log(value), data = count_freq)
ggplot(NULL, aes(x = 1:100, y = resid(mod), colour = factor((1:100) %% 10))) + geom_point() + geom_line()

count_freq2 <- cleaned_data |> semi_join(dataset_summ |> filter(gcd1 > 0.9)) |>
  filter(value <= 100) |>
  count(value)
ggplot(count_freq2, aes(x = log(value), y = log(n))) + geom_point() + geom_smooth()
mod2 <- lm(log(n) ~ log(value), data = count_freq2)
ggplot(NULL, aes(x = 1:100, y = resid(mod2), colour = factor((1:100) %% 10))) + geom_point() + geom_line()


count_freq2 <- cleaned_data |> filter(datasetid > 20000) |>
  filter(value <= 100) |>
  count(value)
ggplot(count_freq2, aes(x = log(value), y = log(n))) + geom_point() + geom_smooth()
mod2 <- lm(log(n) ~ log(value), data = count_freq2)
ggplot(NULL, aes(x = 1:100, y = resid(mod2), colour = factor((1:100) %% 10))) + geom_point() + geom_line()

data.frame(x = 1:100, y = resid(mod),y2 =resid(mod2)) |> ggplot(aes(x = y, y = y2)) + geom_point() + geom_abline(slope = 1, intercept = 0)

div_count <- cleaned_data |>
  summarise(
    p2 = sum(value %% 2 == 0),
    p3 = sum(value %% 3 == 0),
    p4 = sum(value %% 4 == 0),
    p5 = sum(value %% 5 == 0),
    p6 = sum(value %% 6 == 0),
    p7 = sum(value %% 7 == 0),
    p8 = sum(value %% 8 == 0),
    p9 = sum(value %% 9 == 0),
    p10 = sum(value %% 10 == 0),
    p11 = sum(value %% 11 == 0),
    p12 = sum(value %% 12 == 0)
  ) |>
  pivot_longer(everything(), names_to = "divisor", names_prefix = "p", names_transform = list(divisor = as.integer))

div_count |>
  ggplot(aes(x = divisor, y = value)) + geom_point()


div_count <- cleaned_data |>
  group_by(datasetid) |>
  filter(n() > 500) |>
  summarise(
    p2 = mean(value %% 2 == 0),
    p3 = mean(value %% 3 == 0),
    p4 = mean(value %% 4 == 0),
    p5 = mean(value %% 5 == 0),
    p6 = mean(value %% 6 == 0),
    p7 = mean(value %% 7 == 0),
    p8 = mean(value %% 8 == 0),
    p9 = mean(value %% 9 == 0),
    p10 = mean(value %% 10 == 0),
    p11 = mean(value %% 11 == 0),
    p12 = mean(value %% 12 == 0)
  ) |>
  pivot_longer(-datasetid, names_to = "divisor", names_prefix = "p", names_transform = list(divisor = as.integer))

div_count |>
  ggplot(aes(x = divisor, y = value, group = datasetid )) + geom_line()


div_count1 <- BCI |>
  pivot_longer(everything()) |>
  filter(value > 0) |>
  summarise(
    p2 = mean(value %% 2 == 0),
    p3 = mean(value %% 3 == 0),
    p4 = mean(value %% 4 == 0),
    p5 = mean(value %% 5 == 0),
    p6 = mean(value %% 6 == 0),
    p7 = mean(value %% 7 == 0),
    p8 = mean(value %% 8 == 0),
    p9 = mean(value %% 9 == 0),
    p10 = mean(value %% 10 == 0),
    p11 = mean(value %% 11 == 0),
    p12 = mean(value %% 12 == 0)
  ) |>
  pivot_longer(everything(), names_to = "divisor", names_prefix = "p", names_transform = list(divisor = as.integer))

div_count1 |>
  ggplot(aes(x = divisor, y = value )) + geom_line()
