# Run a TreePPL sampler

Executes a compiled TreePPL sampler on the given data, saves the raw
JSON output to disk, prints a run summary, and, when a parser is
available for the model/method combination, parses the output into tidy
data frames.

## Usage

``` r
tp_run(
  sampler,
  data,
  dir = NULL,
  out_file_name = "out",
  n_runs = 1,
  n_processes = 3,
  verbose = TRUE,
  ...
)
```

## Arguments

- sampler:

  a sampler produced by
  [`tp_compile()`](http://treeppl.org/treepplr/reference/tp_compile.md).

- data:

  input data, produced by
  [`tp_data()`](http://treeppl.org/treepplr/reference/tp_data.md).

- dir:

  the full path to the directory where you want to save the output.
  Defaults to
  [`tp_tempdir()`](http://treeppl.org/treepplr/reference/tp_tempdir.md).

- out_file_name:

  the name of the output file in JSON format. Defaults to `"out"`.

- n_runs:

  (`integer`) the number of sweeps (SMC) or runs (MCMC).

- n_processes:

  (`integer`) the number of parallel processes to use. Cannot be greater
  than `n_runs`.

- verbose:

  (`logical`) Whether to print a run summary on completion. Default is
  `TRUE`.

- ...:

  See
  [`tp_runtime_options()`](http://treeppl.org/treepplr/reference/tp_runtime_options.md)
  for all supported arguments.

## Value

If a parser is available for the model/method combination, a parsed tidy
data frame of TreePPL output via
[`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
or
[`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md).
Otherwise, the full path(s) to the raw JSON output file(s), along with a
console message explaining that no parser is available.

## Details

If the model belongs to a category without an available parser (e.g.
`"host-repertoire-evolution"`, `"tree-inference"`), or if the inference
method is neither SMC nor MCMC, no parsing is attempted: a message is
printed pointing to the output directory, and the raw output file
path(s) are returned instead.

## Examples

``` r
if (FALSE) { # \dontrun{
# When using SMC
# compile model and create SMC inference machinery
exe_path <- tp_compile(model = "coin", method = "smc-bpf", particles = 2000)

# prepare data
data_path <- tp_data(data_input = "coin")

# run TreePPL
result <- tp_run(exe_path, data_path, n_runs = 2)


# When using MCMC
# compile model and create MCMC inference machinery
exe_path <- tp_compile(model = "coin", method = "mcmc-naive", iterations = 2000)

# prepare data
data_path <- tp_data(data_input = "coin")

# run TreePPL
result <- tp_run(exe_path, data_path)
} # }
```
