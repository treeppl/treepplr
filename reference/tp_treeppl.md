# Compile and run a TreePPL program

`tp_treeppl` execute TreePPL and return TreePPL output (string JSON
format).

## Usage

``` r
tp_treeppl(
  model = NULL,
  model_file_name = "tmp_model_file",
  data = NULL,
  data_file_name = "tmp_data_file",
  compile_model = TRUE,
  samples = 1000,
  seed = NULL,
  n_runs = 1,
  method = "smc-bpf",
  align = FALSE,
  cps = "none",
  delay = NULL,
  kernel = NULL,
  mcmc_lw_gprob = NULL,
  pmcmc_particles = NULL,
  prune = FALSE,
  subsample = NULL,
  resample = NULL
)
```

## Arguments

- model:

  a TreePPL model (S3).

- model_file_name:

  a character vector giving a model name.

- data:

  a phyjson object (S3).

- data_file_name:

  a character vector giving a data name.

- compile_model:

  a [base::logical](https://rdrr.io/r/base/logical.html) to tell if the
  model need to be compile

- samples:

  a [base::integer](https://rdrr.io/r/base/integer.html) giving the
  number of samples (mcmc) or particules (smc).

- seed:

  a [base::numeric](https://rdrr.io/r/base/numeric.html) to use as a
  random seed.

- n_runs:

  a [base::integer](https://rdrr.io/r/base/integer.html) giving the
  number of run (mcmc)/sweap (smc).

- method:

  a character vector giving the inference method name.

- align:

  a [base::logical](https://rdrr.io/r/base/logical.html) to tell if need
  to align the model.

- cps:

  a character vector giving the configuration of CPS transformation.

- delay:

  a character vector giving the configuration of delayed sampling.

- kernel:

  a [base::numeric](https://rdrr.io/r/base/numeric.html) value giving
  the driftScale for driftKernel in MCMC.

- mcmc_lw_gprob:

  a [base::numeric](https://rdrr.io/r/base/numeric.html) probability of
  performing a global MCMC step.

- pmcmc_particles:

  a [base::integer](https://rdrr.io/r/base/integer.html) number of
  particles for the smc proposal computation

- prune:

  a [base::logical](https://rdrr.io/r/base/logical.html) to tell if the
  model will try to be pruned.

- subsample:

  a [base::integer](https://rdrr.io/r/base/integer.html) number of draw
  to subsample from the posterior distribution.

- resample:

  a character vector giving the selected resample placement method

## Value

TreePPL output in JSON format.

## Details

This function takes TreePPL object (S3) and phyjson object (S3), compile
TreePPL model, run it with data and returning TreePPL output.

TreePPL need to be install on your computer and the PATH set for
R/RSTUDIO (see [install](https://treeppl.org/docs/Howtos) manual). The
executable and the output files will be written in R's
[`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

`model` : A TreePPL model (S3), see
[tp_model](http://treeppl.org/treepplr/reference/tp_model.md) for
further details. Use 'NULL' if you have previously provide an model.
Check already provide model with
[tp_stored_model](http://treeppl.org/treepplr/reference/tp_stored_model.md).

`model_file_name` : a character vector giving to tp_treeppl as a model
name. Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your model with tp_treeppl.

`data` : A list, see
[`tp_check_input()`](http://treeppl.org/treepplr/reference/tp_check_input.md)
for further details. Use 'NULL' if you have previously provide data.
Check already provide data with
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md).

`data_file_name` : a character vector giving to tp_treeppl a data name.
Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your data with tp_treeppl.

`compile_model` : a [base::logical](https://rdrr.io/r/base/logical.html)
telling if the model need to be compiled. Can be use to avoid to compile
a model again in R's
[`base::tempdir()`](https://rdrr.io/r/base/tempfile.html) if you have
already compile a `model` in a previous call of tp_treeppl. Check
already compile model with
[tp_stored_compiled](http://treeppl.org/treepplr/reference/tp_stored_compiled.md).

`samples` : The number of samples (mcmc) / particules (smc) during
inference.

`seed` : The random seed to use. Using 'NULL' initialized randomly.

`n_runs` : The number of run (mcmc) / sweap (smc) used for the
inference.

`method` : Inference method to be used. The selected inference method.
The supported methods are: is-lw, smc-bpf, smc-apf, mcmc-lightweight,
mcmc-trace, mcmc-naive, pmcmc-pimh.

The following options are highly dependable of the method used. Check
\[not implemented yet\] for more information.

`align` : Whether or not to align the model for certain inference
algorithms.

`cps` : Configuration of CPS transformation (only applicable to certain
inference algorithms). The supported options are: none, partial, and
full.

`delay` : The model is transformed to an efficient representation if
possible. The supported options are: static or dynamic. Use 'NULL' to
ignore.

`kernel` : The value of the driftScale for driftKernel in MCMC. Use
'NULL' to ignore. Use in conjuction with `method` mcmc-lightweight". Use
'NULL' to ignore

`mcmc_lw_gprob` : The probability of performing a global MH step
(non-global means only modify a single sample in the previous trace).
Use in conjuction with `method` mcmc-lightweight". Use 'NULL' to ignore

`pmcmc_particles` : The number of particles for the smc proposal
computation. This option is used if one of the following methods are
used: pmcmc-\*. Use 'NULL' to ignore

`prune` : The model is pruned if possible.

`subsample` : The number of draw to subsample from the posterior
distribution. Use in conjuction with `method` smc-apf or smc-bpf. Use
'NULL' to ignore.

`resample`: The selected resample placement method, for inference
algorithms where applicable. The supported methods are: likelihood
(resample immediately after all likelihood updates), align (resample
after aligned likelihood updates, forces –align), and manual (sample
only at manually defined resampling locations). Use 'NULL' to ignore.
