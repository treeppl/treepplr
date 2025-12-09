# Import data for TreePPL program

`tp_data` takes data and prepares it to be used by
[`tp_treeppl()`](http://treeppl.org/treepplr/reference/tp_treeppl.md).

## Usage

``` r
tp_data(data_input)
```

## Arguments

- data_input:

  One of tree options:

  - The full path of the JSON file that contains the data, OR

  - A string with the name of a model supported by treepplr (see
    [`tp_model_names()`](http://treeppl.org/treepplr/reference/tp_model_names.md)),
    OR

  - A list (or structured list) containing TreePPL data.

## Value

a list, see
[`tp_check_input()`](http://treeppl.org/treepplr/reference/tp_check_input.md)
for further details.
