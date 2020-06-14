#import packages
library("drake")
library("tidyverse")
library("patchwork")
library("readxl")
#remotes::install_github("richardjtelford/countSum")
library("countSum")
library("assertr")
#remotes::install_github("richardjtelford/rjt.misc")
library("rjt.misc")
library("english")
library("neotoma")
#remotes::install_github("richardjtelford/neotoma2tibble")
library("neotoma2tibble")

#extra packages required
requireNamespace("visNetwork")
requireNamespace("rticles")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")
#set up parallel processing for drake
options(future.fork.enable = TRUE)
future::plan(future::multiprocess) 

## askpass
options(askpass = function(x)readLines("pw.txt"))

#import scripts
source("R/download_birds.R")
source("R/download_testates.R")
source("R/download_chironomids.R")
source("R/download_neotoma.R")
source("R/download_diatoms.R")
source("R/download_marine.R")


#construct drake plan
analyses <- drake_plan(
  
  #secrets. NULL if no password file
  has_password = fs::file_exists(file_in("pw.txt")),
  secrets = if(has_password){
    readRDS(file_in("data/secrets.RDS")) %>%
      encryptr::decrypt(secret)
    } else {
      NULL
    },
  neotoma_secrets = if(has_password){
    readRDS(file_in("data/neotoma_secrets.RDS")) %>%
    encryptr::decrypt(datasetID) %>% 
    mutate(datasetID = as.integer(datasetID))
  } else {
    NULL
  },
  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("extraDistr", "countSum", "numbers", "neotoma", "drake", "tidyverse"), 
    old_bib = file_in("Rmd/extra/countMS.bib"), 
    new_bib = file_out("Rmd/extra/countMS2.bib")),
  
  #knit manuscript
  manuscript = {
    file_in("Rmd/extra/elsevier-harvard_rjt.csl")
    file_in("Rmd/extra/countMS2.bib")
    rmarkdown::render(
      input = knitr_in("Rmd/count_check_MS.Rmd"), 
      knit_root_dir = "../", 
      clean = FALSE)
  }, 
  
  # save backup data
  data_backup = {      #save anonymised data
    if(has_password){
      #marine1
      marine1 %>% 
        set_names(paste0("marine_", 1:ncol(.))) %>% 
        write_csv(path = "data_backup/marine1.csv")
      #chironomid1
      chironomid1 %>% 
        mutate(taxon = fct_anon(factor(taxon), prefix = "chiron_")) %>% 
      write_csv(path = "data_backup/chironomid1.csv")
      
      #diatom1
      diatom1_data %>% 
        set_names(c("depth_cm", "age_calBP", paste0("diatom_", 1:(ncol(.) - 2)))) %>% 
        write_csv(path = "data_backup/diatom1.csv")
      
      # diatom2
      diatom2_data %>% 
        mutate(taxon = fct_anon(factor(taxon), prefix = "diatom_")) %>%
        write_csv(path = "data_backup/diatom2.csv")
      
      #unexpected pollen 
      pollen_unexpected_data %>% 
        ungroup() %>% 
        mutate(sampleID = fct_anon(factor(sampleID), prefix = "sample_")) %>%
        write_csv("data_backup/pollen_unexpected_data.csv")
      
      }
    }
)

#put plans together
plans <- bind_rows(
  bird_plan, 
  testate_plan, 
  chironomid_plan, 
  pollen_plan,
  diatom_plan,
  marine_plan,
  analyses)

#quick network plot
plot(plans)

#configure and make drake plan
config <- drake_config(plans, jobs = 3, parallelism = "future", keep_going = TRUE)

config
