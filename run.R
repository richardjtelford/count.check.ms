#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint


targets::tar_visnetwork()

if(length(drake_errored()) == 0){
  fs::file_show("manuscript/count_check_MS.pdf") # display pdf is no errors
}
