# Assess MCMC convergence for a TreePPL analysis

Computes per-parameter effective sample size (ESS) and, when multiple
runs are available, the upper limit of the Gelman-Rubin potential scale
reduction factor (R-hat), using the coda package.

## Usage

``` r
tp_mcmc_convergence(treeppl_out)
```

## Arguments

- treeppl_out:

  A tibble produced by
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md),
  with columns `run`, `iteration`, `parameter`, and `samples`. The input
  must have a `parameter` column; output produced from an unnamed
  TreePPL return type is not supported and will raise an error, since
  there is no reliable way to distinguish multiple parameters.

## Value

A tibble with one row per parameter, containing:

- parameter:

  Parameter name.

- ess:

  Effective sample size, pooled across all runs.

- rhat_upper:

  Upper limit of the Gelman-Rubin R-hat statistic. Only computed when
  `treeppl_out` contains more than one run; otherwise `NA`, with a
  message explaining why.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- tp_parse_mcmc(mod_mcmc)
tp_mcmc_convergence(d)
} # }
```
