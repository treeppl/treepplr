# Create a TreePPL model

`tp_compile` takes TreePPL model and create a sampler to be used by
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
tp_compile(model, method = "mcmc", ...)
```

## Arguments

- model:

  One of tree options:

  - The full path of the model file that contains the TreePPL code, OR

  - A string with the name of a model supported by treepplr (see
    [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)),
    OR

  - A string containing the entire TreePPL code.

- method:

  Inference method to be used. See
  [`tp_compile_methods()`](http://treeppl.org/treepplr/reference/tp_compile_methods.md)
  for all supported methods.

- ...:

  See
  [`tp_compile_options()`](http://treeppl.org/treepplr/reference/tp_compile_options.md)
  for all supported arguments.

## Value

sampler from a sampler_T
