# Import a TreePPL model

`tp_model` takes TreePPL code and prepares it to be used by
[`tp_treeppl()`](http://treeppl.org/treepplr/reference/tp_treeppl.md).

## Usage

``` r
tp_model(model_input)
```

## Arguments

- model_input:

  One of tree options:

  - The full path of the model file that contains the TreePPL code, OR

  - A string with the name of a model supported by treepplr (see
    [`tp_model_names()`](http://treeppl.org/treepplr/reference/tp_model_names.md)),
    OR

  - A string containing the entire TreePPL code.

## Value

A TreePPL model (S3). A structured list with the string containing the
TreePPL model and a class
([`tp_model_names()`](http://treeppl.org/treepplr/reference/tp_model_names.md)
or "custom")
