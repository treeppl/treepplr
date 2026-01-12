# treepplr

## Overview

treepplr is the R Interface to [TreePPL](https://treeppl.org), a
probabilistic programming language for phylogenetics.

treepplr converts data to a format readable by TreePPL, reads the
TreePPL output, and connects to downstream analyses in model-specific
packages, such as the [evolnets](https://github.com/maribraga/evolnets)
package for the host repertoire evolution model.

See the package’s documentation [here](https://treeppl.org/treepplr).
There you will find all package functions under
[Reference](https://treeppl.org/treepplr/reference/index.html), and
basic examples of running various models under
[Articles](https://treeppl.org/treepplr/articles/index.html).

## Installation

You can install treepplr like so:

``` r
if(!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
} else {
 library(devtools)
}

devtools::install_github("treeppl/treepplr")
```
