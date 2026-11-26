# treepplr : an R package for TreePPL

## \[Unreleased\]

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security

## v0.5.0

### Added

- ([treepplr#12](https://github.com/treeppl/treepplr/pull/12)) Add
  `tp_runtime_options`
- ([treepplr#13](https://github.com/treeppl/treepplr/pull/13))
  `tp_run(n_runs)` to run multiple sweeps/runs
- ([treepplr#13](https://github.com/treeppl/treepplr/pull/13))
  `tp_run(n_processes)` to run MCMC or SMC in parallel using n cores
- ([treepplr#13](https://github.com/treeppl/treepplr/pull/13))
  [`tp_compile_methods()`](http://treeppl.org/treepplr/reference/tp_compile_methods.md)
  to find available methods
- ([treepplr#14](https://github.com/treeppl/treepplr/pull/14)) Tim and
  Stenio as package authors in DESCRIPTION.
- ([treepplr#31](https://github.com/treeppl/treepplr/pull/31)) At the
  end of a run,
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) now
  prints a summary with runtime information and output
  directories/files.
- ([treepplr#31](https://github.com/treeppl/treepplr/pull/31)) New
  argument `wide` in
  [`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
  and
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md)
  to choose between wide (standard in other software) and long (good for
  plotting) format. Default = `TRUE`.

### Changed

- ([treepplr#12](https://github.com/treeppl/treepplr/pull/12)) Change
  “compiled_model” to “sampler” (temporary)
- ([treepplr#13](https://github.com/treeppl/treepplr/pull/13)) Updated
  the documentation
- ([treepplr#14](https://github.com/treeppl/treepplr/pull/14)) treepplr
  package installation should be done with
  [`pak::pak`](https://pak.r-lib.org/reference/pak.html) because
  [`devtools::install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)
  will soon be deprecated.
- ([treepplr#30](https://github.com/treeppl/treepplr/pull/30)) Updated
  roxygen documentation for
  [`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
  and
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md).
  Changed their output objects to tidy data frames.
- ([treepplr#31](https://github.com/treeppl/treepplr/pull/31))
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) now
  returns a tidy data frame (in wide format by default) instead of a
  list. It automatically calls
  [`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
  or
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md),
  depending on the inference method and model passed to
  [`tp_compile()`](http://treeppl.org/treepplr/reference/tp_compile.md).
  For models without a parser yet (e.g., models that return trees), it
  returns the path to the output JSON file along with a message.
- ([treepplr#31](https://github.com/treeppl/treepplr/pull/31))
  [`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
  and
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md)
  now take a character vector with the path(s) to the JSON output
  file(s) instead of the list returned by
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).
- ([treepplr#31](https://github.com/treeppl/treepplr/pull/31))
  [`tp_mcmc_convergence()`](http://treeppl.org/treepplr/reference/tp_mcmc_convergence.md)
  and
  [`tp_smc_convergence()`](http://treeppl.org/treepplr/reference/tp_smc_convergence.md)
  now accept data frames in either long or wide format.

### Deprecated

- ([treepplr#12](https://github.com/treeppl/treepplr/pull/12)) all
  references to `n_runs` have been remove

### Removed

### Fixed

- ([treepplr#12](https://github.com/treeppl/treepplr/pull/12)) Always
  check if tpplc is installed before calling it
- ([treepplr#12](https://github.com/treeppl/treepplr/pull/12))
  `tp_compile_options` update for new `--help` format
- ([treepplr#30](https://github.com/treeppl/treepplr/pull/30))
  [`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
  and
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md)
  now parse per-particle/iteration with explicit indices and (for SMC)
  join weights by index using `left_join(..., by = "particle")` instead
  of relying on positional recycling, which is the default behavior of
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) when handling
  vectors of different lengths.

### Security
