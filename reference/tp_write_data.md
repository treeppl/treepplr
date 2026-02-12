# Write data to file

Write data to file

## Usage

``` r
tp_write_data(data_list, data_file_name = "tmp_data_file", dir = tp_tempdir())
```

## Arguments

- data_list:

  A named list of data input

- data_file_name:

  An optional name for the file created

- dir:

  The directory where you want to save the data file in JSON format.
  Default is [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

The path to the created file
