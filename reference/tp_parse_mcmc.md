# Parse simple TreePPL json MCMC output

`tp_parse_mcmc` takes TreePPL json MCMC output and returns a data.frame

## Usage

``` r
tp_parse_mcmc(treeppl_out)
```

## Arguments

- treeppl_out:

  a character vector giving the TreePPL json output produced by
  [tp_run](http://treeppl.org/treepplr/reference/tp_run.md) using an
  MCMC method.

## Value

A data frame with the output from inference in TreePPL.
