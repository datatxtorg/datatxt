
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datatxt

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

datatxt provides utilities for working with the `data.txt`
specification. Currently, it provides a function, `narrate()`, that
enables automatic generation of data documentation. See
<https://datatxtdoc.netlify.com/> for an example.

## Installation

You can install the development version of datatxt with

``` r
# install.packages("remotes")
remotes::install_github("datatxtorg/datatxt")
```

## Example

We can generate data documentation by passing the URL of the `data.txt`
to `narrate()`:

``` r
library(datatxt)

temp_dir <- tempdir()
narrate("https://datatxt.org/data.txt", output_dir = temp_dir)

list.files(temp_dir)
#> [1] "iris.md"   "mtcars.md"
```

We can then inspect the contents of one of these files:

``` r
cat(readLines(file.path(temp_dir, "iris.md")), sep = "\n")
#> ---
#> title: iris
#> ---
#> 
#> ## Description
#> 
#> This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
#> 
#> ## Summary
#> 
#> Skim summary statistics  
#>  n obs: 150    
#>  n variables: 5    
#> 
#> Variable type: factor
#> 
#>  variable    missing    complete     n     n_unique               top_counts               ordered 
#> ----------  ---------  ----------  -----  ----------  ----------------------------------  ---------
#>  Species        0         150       150       3        set: 50, ver: 50, vir: 50, NA: 0     FALSE  
#> 
#> Variable type: numeric
#> 
#>    variable      missing    complete     n     mean     sd     p0     p25    p50     p75    p100      hist   
#> --------------  ---------  ----------  -----  ------  ------  -----  -----  ------  -----  ------  ----------
#>  Petal.Length       0         150       150    3.76    1.77     1     1.6    4.35    5.1    6.9     ▇▁▁▂▅▅▃▁ 
#>  Petal.Width        0         150       150    1.2     0.76    0.1    0.3    1.3     1.8    2.5     ▇▁▁▅▃▃▂▂ 
#>  Sepal.Length       0         150       150    5.84    0.83    4.3    5.1    5.8     6.4    7.9     ▂▇▅▇▆▅▂▂ 
#>  Sepal.Width        0         150       150    3.06    0.44     2     2.8     3      3.3    4.4     ▁▂▅▇▃▂▁▁
```
