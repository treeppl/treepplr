# Parse TreePPL json output for host repertoire model

`tp_parse_host_rep` takes TreePPL json output from inference with the
model of host repertoire evolution and returns a data.frame

## Usage

``` r
tp_parse_host_rep(treeppl_out, n_runs = 1)
```

## Arguments

- treeppl_out:

  a character vector giving the TreePPL json output produced by
  [tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md).

- n_runs:

  a [base::integer](https://rdrr.io/r/base/integer.html) giving the
  number of runs (MCMC) or sweeps (SMC).

## Value

A list (n = n_runs) of data frames with the output from inference in
TreePPL under the host repertoire evolution model.
