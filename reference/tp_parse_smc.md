# Parse TreePPL SMC output into a tidy data frame

Converts a JSON file from an SMC analysis produced by TreePPL into a
single tidy tibble of particles, their samples, and normalized weights.
The function internally removes sweeps with an undefined normalizing
constant.

## Usage

``` r
tp_parse_smc(json_path, wide = TRUE)
```

## Arguments

- json_path:

  The full path to the (SMC) JSON file produced by TreePPL.

- wide:

  Logical. If `TRUE` (default), return the data frame in wide format,
  with one column per parameter. If `FALSE`, return the data frame in
  long format, with parameter names and values stored in `parameter` and
  `sample` columns.

## Value

A tibble with one row per particle, containing:

- sweep:

  Sweep index.

- parameter:

  Parameter name, if present in the input JSON.

- sample:

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
# fit a CRBD model
run_smc <- tp_run(
  data = tp_data(data_input = "crbd"),
  sampler = tp_compile(
    model = "crbd",
    method = "smc-apf",
    sweeps = 2,
    particles = 10
  )
)

# get the path to the output JSON file:
out_file <- list.files(
  path = tp_tempdir(),
  pattern = "out",
  full.names = TRUE
)

# parse JSON to a tidy data frame
tp_parse_smc(json_path = out_file)
} # }
```
