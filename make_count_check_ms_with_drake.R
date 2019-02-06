#import package
library("drake")
library("tidyverse")
library("readr")
library("gridExtra")
library("broom")
#devtools::install_github("richardjtelford/rjt.misc")
library("rjt.misc")

#import scripts


#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#construct drake plan
analyses <- drake_plan(
  
  #import & clean data
  

  #make plots
  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("rioja"), 
    old_bib = file_in("Rmd/extra/countMS.bib"), 
    new_bib = file_out("Rmd/extra/countMS2.bib")),
  
  #knit manuscript
  #knit manuscript
  manuscript = target(
    command = rmarkdown::render(input = knitr_in("Rmd/count_check_MS.Rmd"), output_dir = "./output", output_file = file_out("output/count_check_MS.pdf")), 
    trigger = trigger(change =list(biblio2))
  )
)

#configure and make drake plan
config <- drake_config(analyses)
outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.

system("evince output/count_check_MS.pdf", wait = FALSE)#display pdf - only linux

#view dependency graph
vis_drake_graph(config, targets_only = TRUE)
