# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library("targets")
library("tarchetypes")

# Set target options:
tar_option_set(
  packages = c("tidyverse", "patchwork", "readxl", "countSum", "assertr"), 
  format = "rds" # default storage format
  # Set other options as needed.
)


# ,
# "rjt.misc",
# "neotoma",
# "neotoma2tibble",
# "janitor",
# "glue",

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("R/download_birds.R")
# tar_source("R/download_testates.R")
# tar_source("R/download_chironomids.R")
# tar_source("R/download_neotoma.R")
# tar_source("R/download_diatoms.R")
# tar_source("R/download_marine.R")
# #tar_source("R/download_neotoma_diatoms.R")
# tar_source("R/planktic_forams_plan.R")
# tar_source("R/percent_rounding_plan.R")

#source("R/functions/general_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    #aggregate to different taxonomic levels
    name = bird_download,
    command = download_bird()
  ),
  
  tar_target(
    name = bird_species,
    command = load_bird_species()
#   format = "feather" # efficient storage of large data frames # nolint
  ),

  tar_target(
    #aggregate to different taxonomic levels
    name = bird_singletons,
    command = bird_process(bird_download, bird_species)
  ),


  tar_target(
    name = summarised_bird_singletons,
    command = summarise_bird_singletons(bird_singletons)
),


tar_target(
  name = recounted_bird_singletons,
  command = recount_bird_singletons(bird_download, bird_species)
),


tar_target(
  #aggregate to different taxonomic levels
  name = summarised_bird,
  command = summarise_bird(bird_singletons)
),
tar_target(
  #aggregate to different taxonomic levels
  name = recounted_bird_singletons_plot,
  command = plot_bird_recount_singleton_plot(recounted_bird_singletons)
)
)
