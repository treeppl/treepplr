# Parse TreePPL MCMC output into a tidy data frame

Converts a list of parsed MCMC runs (from
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md)) into a
single tidy tibble of samples, one row per iteration.

## Usage

``` r
tp_parse_mcmc(treeppl_out)
```

## Arguments

- treeppl_out:

  A list of MCMC runs parsed from MCMC JSON output files: i.e., the
  output object of
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Value

A tibble with one row per iteration, containing:

- run:

  Run index, corresponding to the position of the run in `treeppl_out`.

- parameter:

  Parameter name, if present in the input JSON.

- samples:

  Sampled value.

## Examples

``` r
if (FALSE) { # \dontrun{
# example using a CRBD model with two MCMC chains
path_data <- tp_data(data_input = "crbd")
sampler_mcmc <- tp_compile(model = "crbd", method = "mcmc", iterations = 10)
mod_mcmc <- tp_run(
  sampler = sampler_mcmc,
  data = path_data,
  n_runs = 2
)

tp_parse_mcmc(mod_mcmc)
} # }
```
