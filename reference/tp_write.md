# Prepare input for [`tp_compile()`](http://treeppl.org/treepplr/reference/tp_compile.md)

`tp_write` writes an JSON file to be used by
[`tp_compile()`](http://treeppl.org/treepplr/reference/tp_compile.md).

## Usage

``` r
tp_write(
  model = NULL,
  model_file_name = "tmp_model_file",
  data = NULL,
  data_file_name = "tmp_data_file"
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

## Details

This function takes TreePPL object (S3) and phyjson object (S3) and
write them in [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

`model` : A TreePPL model (S3), see
[tp_model](http://treeppl.org/treepplr/reference/tp_model.md) for
further details. Use 'NULL' if you have previously provide an model.
Check already provide model with
[tp_stored_model](http://treeppl.org/treepplr/reference/tp_stored_model.md).

`model_file_name` : a character vector giving to
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md) as a
model name. Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your model with tp_write.

`data` : A list, see
[`tp_check_input()`](http://treeppl.org/treepplr/reference/tp_check_input.md)
for further details. Use 'NULL' if you have previously provide data.
Check already provide data with
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md).

`data_file_name` : a character vector giving to
[tp_treeppl](http://treeppl.org/treepplr/reference/tp_treeppl.md) a data
name. Use a
[tp_stored_data](http://treeppl.org/treepplr/reference/tp_stored_data.md)
name if you have already write your data with tp_write.
