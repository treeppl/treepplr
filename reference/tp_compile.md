# Compile a TreePPL model and create inference machinery

`tp_compile` compile a TreePPL model and create inference machinery to
be used by [tp_run](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
tp_compile(
  model,
  method,
  iterations = NULL,
  particles = NULL,
  dir = NULL,
  output = NULL,
  ...
)
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

  Inference method to be used. See tp_compile_options() for all
  supported methods.

- iterations:

  The number of MCMC iterations to be run.

- particles:

  The number of SMC particles to be run.

- dir:

  The directory where you want to save the executable. Default is
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html)

- output:

  Complete path to the compiled TreePPL program that will be created.
  Default is dir/.exe

- ...:

  See tp_compile_options() for all supported arguments.

## Value

The path for the compiled TreePPL program.
