# Run a TreePPL program

Run TreePPL and return output.

## Usage

``` r
tp_run(sampler, data, dir = NULL, out_file_name = "out", ...)
```

## Arguments

- sampler:

  a
  [sampler_T](http://treeppl.org/treepplr/reference/sampler_T-class.md)
  outputted by
  [tp_compile](http://treeppl.org/treepplr/reference/tp_compile.md).

- data:

  a [base::character](https://rdrr.io/r/base/character.html) with the
  full path to the data file in TreePPL JSON format (as outputted by
  [tp_data](http://treeppl.org/treepplr/reference/tp_data.md)).

- dir:

  a [base::character](https://rdrr.io/r/base/character.html) with the
  full path to the directory where you want to save the output. Default
  is [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- out_file_name:

  a [base::character](https://rdrr.io/r/base/character.html) with the
  name of the output file in JSON format. Default is "out".

- ...:

  See tp_run_options for all supported arguments.

## Value

A list of TreePPL output in parsed JSON format.

## Examples

``` r
if (FALSE) { # \dontrun{
# When using SMC
# compile model and create SMC inference machinery
exe_path <- tp_compile(model = "coin", method = "smc-bpf", particles = 2000)

# prepare data
data_path <- tp_data(data_input = "coin")

# run TreePPL
result <- tp_run(exe_path, data_path, sweeps = 2)


# When using MCMC
# compile model and create MCMC inference machinery
exe_path <- tp_compile(model = "coin", method = "mcmc-naive", iterations = 2000)

# prepare data
data_path <- tp_data(data_input = "coin")

# run TreePPL
result <- tp_run(exe_path, data_path)
} # }
```
