# Coin flipping example

``` r
library(treepplr)
library(dplyr)
library(ggplot2)
```

## Load model and data files

Load the coin model and example data available within *treepplr.*

``` r
model <- tp_model("coin")
data <- tp_data("coin")
```

The data in this example is a sequence of coin flip results. *treeppl*
can only read data in JSON format, that’s why all example data are in
this format.

``` r
str(data)
#> List of 1
#>  $ coinflips: logi [1:20] FALSE TRUE TRUE TRUE FALSE TRUE ...
```

## Run TreePPL

Now we can compile and run the TreePPL program. The function
*tp_treeppl()* has many optional arguments to change the inference
method used. Here, we will use the default settings and only pass the
model and the data.

``` r
output_list <- tp_treeppl(model = model, data = data)
```

## Plot the posterior distribution

TreePPL outputs the log weight of each sample, so first we need to get
the normalized weights and then we can plot the posterior distribution
produced.

``` r
# turn list into a data frame where each row represents one sample 
# and calculate normalized weights from log weights
output <-  tp_parse(output_list) %>% 
  dplyr::mutate(weight = exp(log_weight - max(.$log_weight)))

ggplot(output, aes(samples, weight=weight)) +
  geom_histogram(aes(y = after_stat(density)), 
                 col = "white", fill = "lightblue", binwidth=0.04) +
  geom_density() +
  theme_bw()
```

![](coin-example_files/figure-html/unnamed-chunk-6-1.png)
