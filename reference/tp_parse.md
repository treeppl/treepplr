# Parse simple TreePPL json output

`tp_parse` takes TreePPL json output and returns a data.frame

## Usage

``` r
tp_parse(treeppl_out)
```

## Arguments

- treeppl_out:

  a character vector giving the TreePPL json output produced by
  [tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md).

## Value

A data frame with the output from inference in TreePPL.
