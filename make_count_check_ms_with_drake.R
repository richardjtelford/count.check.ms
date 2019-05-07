#import package
library("drake")
library("tidyverse")
library("readr")
library("readxl")
#library("gridExtra")
#library("broom")
#devtools::install_github("richardjtelford/count_checker")#need to make package and repo names identical (_ not permitted)
library("countSum")
library("assertr")
#devtools::install_github("richardjtelford/rjt.misc")
library("rjt.misc")
library("neotoma")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")
#set up parallel processing for drake
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
  
  #secrets
  secrets = readRDS(file_in("data/secrets.RDS")) %>%
      encryptr::decrypt(secret),
  neotoma_secrets = readRDS(file_in("data/neotoma_secrets.RDS")) %>%
    encryptr::decrypt(datasetID) %>% 
    mutate(datasetID = as.integer(datasetID)),
  
  #make plots
  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("extraDistr", "countSum", "numbers"), 
    old_bib = file_in("Rmd/extra/countMS.bib"), 
    new_bib = file_out("Rmd/extra/countMS2.bib")),
  
  #knit manuscript
  #knit manuscript
  manuscript = {
    file_in("Rmd/extra/countMS2.bib")
    rmarkdown::render(
      input = knitr_in("Rmd/count_check_MS.Rmd"), 
      knit_root_dir = "../", 
      clean = FALSE)
  }
)

#put plans together
plans <- bind_rows(
  bird_plan, 
  testate_plan, 
  chironomid_plan, 
  neotoma_plan,
  diatom_plan,
  marine_plan,
  analyses)

#configure and make drake plan
config <- drake_config(plans)

#view dependency graph
vis_drake_graph(config, targets_only = TRUE)

#Build the right things
make(plans, jobs = 2, parallelism = "future", keep_going = TRUE)
failed()

if(length(failed()) == 0) {
  system("evince Rmd/count_check_MS.pdf", wait = FALSE)#display pdf - only linux
}

#view dependency graph
vis_drake_graph(config, targets_only = TRUE)
