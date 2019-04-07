#import package
library("drake")
library("tidyverse")
library("readr")
library("gridExtra")
library("broom")
#devtools::install_github("richardjtelford/rjt.misc")
library("rjt.misc")
library("countChecker")
library("english")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

## askpass
options(askpass = function(x)readLines("pw.txt"))

#import scripts
source("R/download_birds.R")
source("R/download_testates.R")
source("R/download_chironomids.R")
source("R/download_pollen.R")
source("R/summarise_counts.R")


#construct drake plan
analyses <- drake_plan(
  
  #secrets
  secrets = readRDS("data/secrets.RDS") %>%
      encryptr::decrypt(secret),

  #run analyses
  testate_summary = summarise_counts(testate_counts),
  
  #make plots
  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("extraDistr"), 
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
plans <- bind_rows(bird_plan, testate_plan, chironomid_plan, analyses, pollen_plan)

#configure and make drake plan
config <- drake_config(plans)

#set up parallel processing for drake
future::plan(future::multiprocess) 

#Build the right things
make(plans, jobs = 2, parallelism = "future")

system("evince Rmd/count_check_MS.pdf", wait = FALSE)#display pdf - only linux

#view dependency graph
vis_drake_graph(config, targets_only = TRUE)
