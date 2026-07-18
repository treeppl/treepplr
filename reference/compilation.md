# Compile a TreePPL model and create inference machinery

`compilation` compile a TreePPL model and create inference machinery to
be used by [tp_run](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
compilation(path, args_str)
```

## Arguments

- path:

  [base::character](https://rdrr.io/r/base/character.html) to a treppl
  model

- args_str:

  [base::character](https://rdrr.io/r/base/character.html) of options
  for treeppl compiler

## Value

The path for the compiled TreePPL program.
