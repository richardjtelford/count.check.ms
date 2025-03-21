# "Tools for identifying unexpectedly low microfossil count sums"

This repo reproduces the analyses and generates the manuscript "Tools for identifying unexpectedly low microfossil count sums". 

The number of individuals counted in a community or assemblage is a key data quality diagnostic: small counts are associated with increased uncertainty in the assemblage composition and in derived statistics such as diversity or transfer function derived reconstructions. Unfortunately, count sums are often not reported, and may be misreported. 

The manuscript develops methods for estimating count sums and then shows that some papers appear to have misreported their count sums. The details of these papers are in an encrypted file. The manuscript cannot be fully reproduced without the password to this file (Better solutions are welcome). 

The manuscript is written in markdown with quarto. Package versions are recorded with renv, and targets creates the analytical pipeline.

To reproduce the manuscript, first run `renv::restore()` to install required packages. Then open run.R and run `targets::tar_make()`.
