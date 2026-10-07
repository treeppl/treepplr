# Parse TreePPL MCMC output into a tidy data frame

Converts JSON file(s) produced by an MCMC analysis in TreePPL into a
single tidy tibble of samples, one row per iteration.

## Usage

``` r
tp_parse_mcmc(json_path, wide = TRUE)
```

## Arguments

- json_path:

  The full path to the (MCMC) JSON file(s) produced by TreePPL.

- wide:

  Logical. If `TRUE` (default), return the data frame in wide format,
  with one column per parameter. If `FALSE`, return the data frame in
  long format, with parameter names and values stored in `parameter` and
  `sample` columns.

## Value

A tibble with one row per iteration, containing:

- run:

  Run index.

- parameter:

  Parameter name, if present in the input JSON.

- sample:

  Sampled value.

## Examples

``` r
if (FALSE) { # \dontrun{

# Let's use a quick CRBD model as example
run_mcmc <- tp_run(
sampler = tp_compile(model = "crbd", method = "mcmc", iterations = 10),
data = tp_data(data_input = "crbd"),
n_runs = 2, # this will produce two JSON files as output
n_processes = 2
)

# get the path to the output JSON file; note that the number of JSON
# files produced is equal to n_runs specified above
out_file <- list.files(
  path = tp_tempdir(),
  pattern = "out",
  full.names = TRUE
)

# parse JSON to a tidy data frame
tp_parse_mcmc(json_path = out_file)
} # }
```
