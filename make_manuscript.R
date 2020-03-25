library("drake")
library("rjt.misc")

plan <- "plan_count_check_ms_with_drake.R"

#make ms
r_make(source = plan) # Build the right things.
failed()

system("evince Rmd/count_check_MS.pdf", wait = FALSE)#display pdf - only linux

#show dependency graph
r_vis_drake_graph(source = plan, targets_only = TRUE, main = "count ms dependency graph")
