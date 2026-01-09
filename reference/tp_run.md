# Run a TreePPL program

`tp_treeppl` execute TreePPL and return TreePPL output (string JSON
format).

## Usage

``` r
tp_run(
  model_file_name = "tmp_model_file",
  data_file_name = "tmp_data_file",
  n_runs = 1
)
```

## Arguments

- model_file_name:

  a character vector giving a model name.

- data_file_name:

  a character vector giving a data name.

- n_runs:

  a [base::integer](https://rdrr.io/r/base/integer.html) giving the
  number of runs (mcmc)/sweaps (smc).

## Value

A list of TreePPL output in parsed JSON format.

## Details

\#'

`model_file_name` : a character vector giving to
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md) as a
model name. Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your model with
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md).

`data_file_name` : a character vector giving to
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md) a data
name. Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your data with
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md).

`n_runs` : The number of run (mcmc) / sweap (smc) used for the
inference.
