# treepplr

## Overview

treepplr is the R Interface to [TreePPL](treeppl.org), a probabilistic programming language for phylogenetics.

treepplr converts data to a format readable by TreePPL, reads the TreePPL output, and connects to downstream analyses in model-specific packages, such as the [evolnets](github.com/maribraga/evolnets) package for the host repertoire evolution model.

See the package's documentation [here](treeppl.org/treepplr).


## Installation

You can install treepplr like so:

``` r
# install.packages("devtools")
# library(devtools)

devtools::install_github("treeppl/treepplr")
```

This will only install the R package. The TreePPL compiler will not be downloaded and installed until you run your first analysis, and the TreePPL compiler is called.  During the download, you will see a message like this

```
[xx%] Downloaded xxxxxx bytes...
```

In subsequent analyses, the TreePPL compiler will be called directly, skipping this step.
