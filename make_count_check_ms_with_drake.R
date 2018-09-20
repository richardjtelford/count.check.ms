#import package
library("drake")
library("tidyverse")
library("readr")
library("gridExtra")
library("broom")
#devtools::install_github("richardjtelford/rjt.misc")
library("rjt.misc")

#import scripts




#construct drake plan
analyses <- drake_plan(
  
  #import & clean data

  #make plots
  
  #get bibliography
  biblio = download.file("https://raw.githubusercontent.com/richardjtelford/Zabinskie/master/chironomid.bib", destfile = file_out("Rmd/extra/chironomid.bib")),
  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("vegan", "rioja", "analogue", "palaeoSig"), 
    old_bib = file_in("Rmd/extra/chironomid.bib"), 
    new_bib = file_out("Rmd/extra/chironomid2.bib")),
  
  #knit manuscript
  manuscript = rmarkdown::render(input = knitr_in("Rmd/count_check_MS.Rmd"), output_dir = "./output", output_file = file_out("output/count_check_MS.pdf")),
  
  strings_in_dots = "literals"
)

#configure and make drake plan
config <- drake_config(analyses)
outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.

#voew dependency graph
vis_drake_graph(config, targets_only = TRUE)
