# Parse TreePPL SMC output into a tidy data frame

Converts a list of parsed SMC sweeps (from
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md)) into a
single tidy tibble of particles, their samples, and normalized weights.
The function internally removes sweeps with an undefined normalizing
constant.

## Usage

``` r
tp_parse_smc(treeppl_out)
```

## Arguments

- treeppl_out:

  A list of sweeps parsed from a SMC JSON output: i.e., the output
  object of
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Value

A tibble with one row per particle, containing:

- sweep:

  Sweep index.

- parameter:

  Parameter name, if present in the input JSON.

- samples:

  Sampled value.

- log_weight:

  Log weight of the particle.

- norm_constant:

  Log normalizing constant for the sweep.

- norm_weight:

  Normalized weight, rescaled so the maximum total log weight across all
  particles is 1.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fit a quick CRBD model:
path_data <- tp_data(data_input = "crbd")
sampler_smc <- tp_compile(
  model = "crbd",
  method = "smc-apf",
  sweeps = 2,
  particles = 10
)
mod_smc <- tp_run(sampler = sampler_smc, data = path_data)

tp_parse_smc(mod_smc)
} # }
```
