# Host repertoire model example

``` r
library(treepplr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(ape)

if(!require("evolnets", quietly = TRUE)) {
 library(devtools)
 if (!require("BiocManager", quietly = TRUE))
   install.packages("BiocManager")
 library(BiocManager)
 BiocManager::install("ggtree")
 devtools::install_github("maribraga/evolnets")
 library(ggtree)
 library(evolnets)
} else {
 library(evolnets)
}
```

This is a very simplified workflow in **treepplr** for analysis of host
repertoire evolution. The data used here is simulated and the inference
is too simple. A real analysis would include many other steps such as
testing of different inference methods and check of convergence.

The purpose of this vignette is to show how to run **TreePPL** with
**treepplr**, and how to process the output with **evolnets**.

## Load model and data files

Load the 3-state host repertoire model and example data available within
**treepplr**.

``` r
model <- tp_model("hostrep3states")
data <- tp_data("hostrep3states")
```

## Run treeppl
