# Import a TreePPL model

`tp_model` takes TreePPL code and prepares it to be used by
[`tp_compile()`](http://treeppl.org/treepplr/reference/tp_compile.md).

## Usage

``` r
tp_model(model_input)
```

## Arguments

- model_input:

  One of tree options:

  - The full path of the model file that contains the TreePPL code, OR

  - A string with the name of a model supported by treepplr (see
    [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)),
    OR

  - A string containing the entire TreePPL code.

## Value

The path to the TreePPL model file
