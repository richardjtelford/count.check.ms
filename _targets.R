# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library("targets")
library("tarchetypes")

# Set target options:
tar_option_set(
  packages = c("tidyverse", "patchwork", "readxl", "countSum", "assertr", "conflicted", "glue"), 
  imports = "countSum",
  format = "rds" # default storage format
  # Set other options as needed.
)

conflicted::conflict_prefer_all(winner = "dplyr", quiet = TRUE)

## askpass
options(askpass = function(x) readLines("pw.txt"))

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


#### target list ####
list(
  ## encryption ####
  tar_target(
    name = password_file,
    command =  "pw.txt",
    format = "file"
  ),
  tar_target(
    name = secrets_file,
    command =  "data/secrets.RDS",
    format = "file"
  ),
  tar_target(
    name = neotoma_secrets_file,
    command =  "data/neotoma_secrets.RDS",
    format = "file"
  ),
  tar_target(
    name = has_password,
    command =  fs::file_exists(password_file)
  ),
  tar_target(
    name = secrets,
    command = get_secrets(secrets_file, has_password)
  ),
  tar_target(
    name = neotoma_secrets,
    command = get_neotoma_secrets(neotoma_secrets_file, has_password)
  ),
  
  # birds ####
  tar_target(
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
    ),
 # simulate rounding on bird data
  tar_target(
    #aggregate to different taxonomic levels
    name = percent_rounding_sim,
    command = simulate_percent_rounding(bird_download)
  ),
  tar_target(
    #aggregate to different taxonomic levels
    name = percent_rounding_plot,
    command = plot_percent_rounding(percent_rounding_sim)
  ),


  # Diatoms - molten ####

  tar_target(
    # download molten data
    name = molten_file,
    command = download_molten(),
    format = "file" 
  ), 

  tar_target(
    # import molten data
    name = molten_data,
    command = load_molten_data(molten_file)
  ),

  tar_target(
    # summarise molten counts
    name = molten_singletons,
    command = summarise_counts(molten_data)
  ),

 ## diatoms ####
## diatom1
  tar_target(
    # download and import
    name = diatom1,
    command = download_diatom1(has_password, secrets)
  ),
  tar_target(
    # estimate n
    name = diatom1_est_n,
    command = estimate_diatom_n(diatom1, digits = 2)
  ),
  tar_target(
    #summarise diatom1
    name = diatom1_summ,
    command = summarise_diatom(diatom1_est_n, diatom1),
  ),
  ## diatom2
  tar_target(
    # download and import
    name = diatom2,
    command = download_diatom2(has_password, secrets)
  ),
  tar_target(
    # estimate n
    name = diatom2_est_n,
    command = estimate_diatom_n(diatom2, digits = 2)
  ),
  tar_target(
    #summarise diatom1
    name = diatom2_summ,
    command = summarise_diatom(diatom2_est_n, diatom2),
  ),
  # last chance chronomids ####
  tar_target(
   # download and import last chance
    name = last_chance_data,
    command = download_last_chance()
  ),

  tar_target(
    # estimate n
    name = last_chance_estimate_n,
    command = estimate_last_chance_n(last_chance_data)
  ),
  tar_target(
    name = last_chance_direct_search_plot,
    command = plot_last_chance_direct_search(last_chance_estimate_n)
  ),
  
  tar_target(
    name = last_chance_plot,
    command = plot_last_chance(last_chance_estimate_n)
  ),  
  tar_target(
    name = last_chance_summ,
    command = summarise_last_chance(last_chance_data)
  ),

  ## marine1
  tar_target(
    name = marine1,
    command = download_marine1(has_password, secrets)
  ),
  tar_target(
    name = marine1_est_n,
    command = estimate_marine1_n(marine1)
  ),
  tar_target(
    name = marine1_summ,
    command = summarise_marine1(marine1, marine1_est_n)
  ),
## chironomid1 ####
  tar_target(
    name = chironomid1,
    command = download_chironomid1(has_password, secrets)
  ),
  tar_target(
    name = chironomid1_est_n,
    command = estimate_chironomid1_n(chironomid1)
  ),
  tar_target(
    name = chironomid1_summ,
    command = summarise_chironomid1(chironomid1, chironomid1_est_n)
  ),

### pollen ####
  tar_target(
    name = pollen_datasets,
    command = get_datasets(
      datasettype = "pollen", 
      all_data = TRUE)
  ),
  tar_target(
    name = pollen_data,
    command = get_download_batch(pollen_datasets)
  ),
  tar_target(
    name = pollen,
    command = get_pollen_table(pollen_data)
  ),
  tar_target(
    name = pollen_assem_res,
    command = summarise_counts(pollen)
  ),
  tar_target(
    name = pollen_dataset_res,
    command = summarise_singletons(pollen_assem_res)
  ),
  tar_target(
    name = pollen_results,
    command = make_pollen_results(pollen_assem_res, pollen_dataset_res)
  ),

## hershop
  tar_target(
    name = hershop_pollen,
    command = process_hershop_pollen(pollen_data)
  ),
  
### forams ####
  tar_target(
    name = forams,
    command = import_forams()
  ),
  tar_target(
    name = foram_assem_res,
    command = summarise_counts(forams)
  ),
   
## testate ####
  tar_target(
    name = testate_datasets,
    command = get_datasets(
      datasettype = "testate amoebae surface sample", 
      all_data = TRUE)
  ),
  tar_target(
    name = testate_data,
    command = get_download_batch(testate_datasets, offset = 10)
  ),
  tar_target(
    name = testate,
    command = get_testate_table(testate_data)
  ),
  tar_target(
    name = testate_assem_res,
    command = summarise_counts(testate)
  ),

## multi taxa tables/figures
  tar_target(
    name = count_summary,
    command = summarise_all_counts(bird_singletons, molten_singletons,
                                   foram_summ, foram_assem_res,
                                   testate_assem_res, pollen_assem_res) 
  ),
  tar_target(
    name = richness_singleton_plot,
    command = make_richness_singleton_plot(bird_singletons, molten_singletons,
                                           foram_assem_res, testate_assem_res,
                                           pollen_assem_res)
  ),


  ## manuscript


  tar_target(
    # bibliography
    name = raw_bibliography,
    command = "manuscript_extra/countMS.bib",
    format = "file" 
  ), 
  tar_target(
    # bibliography
    name = bibliography,
    command = package_citations(
          packages = c("extraDistr", "countSum", "numbers", "bbsAssistant",
                       "targets", "tidyverse", "quarto", "renv"),
          old_bib = raw_bibliography,
          new_bib = "manuscript_extra/countMS2.bib"),
    format = "file" 
  ), 

 
  tar_quarto(
    name = manuscript,
    path = "count_check_MS.qmd"
  )
)
