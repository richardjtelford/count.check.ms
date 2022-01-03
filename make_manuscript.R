#Use renv to install correct version of all necessary package
#renv::restore()

#load packages
library("drake")

plan <- "plan_count_check_ms_with_drake.R"

#make ms
r_make(source = plan) # Build the right things.
drake_failed()

if(length(drake_failed()) == 0){
  fs::file_show("Rmd/count_check_MS.pdf")#display pdf
}

#show dependency graph
r_vis_drake_graph(source = plan, targets_only = TRUE, main = "count ms dependency graph")

