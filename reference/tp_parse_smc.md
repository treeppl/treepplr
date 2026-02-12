# Parse simple TreePPL json SMC output

`tp_parse_smc` takes TreePPL json SMC output and returns a data.frame

## Usage

``` r
tp_parse_smc(treeppl_out)
```

## Arguments

- treeppl_out:

  a character vector giving the TreePPL json output produced by
  [tp_run](http://treeppl.org/treepplr/reference/tp_run.md) using an SMC
  method.

## Value

A data frame with the output from inference in TreePPL.

## Details

Particles with -Inf weight are removed.
