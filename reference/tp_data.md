# Import data for TreePPL program

Prepare data input for
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
tp_data(data_input, data_file_name = "tmp_data_file", dir = tp_tempdir())
```

## Arguments

- data_input:

  One of the following options:

  - A named list (or structured list) containing TreePPL data, OR

  - The full path of a multiple sequence alignment in fasta (.fasta,
    .fas) or nexus (.nexus, .nex) format, OR

  - For test data, a string with the name of a model supported by
    treepplr (see
    [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)).

- data_file_name:

  An optional name for the file created. Ignored if `data_input` is the
  name of a model from the TreePPL library.

- dir:

  The directory where you want to save the data file in JSON format.
  Default is [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).
  Ignored if `data_input` is the name of a model from the TreePPL
  library.

## Value

The path for the data file that will be used by
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Details

`data_input`: The name of each list element has to match the name of a
model input, which is defined in the TreePPL model code.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example using a model name supported by TreePPL
input <- tp_data("tree_inference")
input

# Example using an internal FASTA file (same input data as before, but in fasta format)
fasta_file <- system.file("extdata", "tree_inference.fasta", package = "treepplr")
input <- tp_data(fasta_file)
input
} # }
```
