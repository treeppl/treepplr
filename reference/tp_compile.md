# Compile for [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md)

`tp_compile` compile a TreePPL model to use by
[tp_run](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
tp_compile(
  model_file_name = "tmp_model_file",
  samples = 1000,
  seed = NULL,
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

- model_file_name:

  a character vector giving a model name.

- samples:

  a [base::integer](https://rdrr.io/r/base/integer.html) giving the
  number of samples (mcmc) or particules (smc).

- seed:

  a [base::numeric](https://rdrr.io/r/base/numeric.html) to use as a
  random seed.

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

  a character vector giving the selected resample placement method.

## Value

The R's [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html) whreŕe
the compile file is stored.

## Details

`model_file_name` : a character vector giving to
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md) as a
model name. Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your model with
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md).

`seed` : The random seed to use. Using 'NULL' initialized randomly.

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
