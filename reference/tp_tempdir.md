# Temporary directory for running treeppl

`tp_tempdir` returns a normalized path for a temporary directory where
the executables can read and write temporary files.

## Usage

``` r
tp_tempdir(temp_dir = NULL, sep = NULL, sub = NULL)
```

## Arguments

- temp_dir:

  NULL, or a path to be used; if NULL, R's
  [base::tempdir](https://rdrr.io/r/base/tempfile.html) is used.

- sep:

  Better ignored; non-default values are passed to
  [base::normalizePath](https://rdrr.io/r/base/normalizePath.html).

- sub:

  Extension for defining a sub-directory within the directory defined by
  [base::tempdir](https://rdrr.io/r/base/tempfile.html).

## Value

Normalized path with system-dependent terminal separator.
