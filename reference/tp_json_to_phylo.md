# Convert TreePPL multi-line JSON to R phylo/multiPhylo object with associated weights

`tp_json_to_phylo` takes the path to a TreePPL json output and returns
an object of class `phylo`.

## Usage

``` r
tp_json_to_phylo(json_out)
```

## Arguments

- json_out:

  One of two options:

  - A list of TreePPL output in parsed JSON format (output from
    [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md)), OR

  - The full path of the json file containing the TreePPL output.

## Value

A list with two elements: \$trees: A 'phylo' object (if one tree) or
'multiPhylo' object (if multiple). \$weights: A numeric vector of sample
weights.
