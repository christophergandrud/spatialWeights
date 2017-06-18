
spatialWeights
==============

[![Build Status](https://travis-ci.org/christophergandrud/spatialWeights.svg?branch=master)](https://travis-ci.org/christophergandrud/spatialWeights)

This R package is designed to implement some of the spatial weights for time series cross sectional data discussed in [Neumayer and Plumper (2010)](http://eprints.lse.ac.uk/30750/1/Making%20spatial%20analysis%20operational(lsero).pdf).

Weights can be found in parallel for improved speed.

Examples
--------

Here is a simple example with fake data with spatial clustering:

``` r
library(spatialWeights)

# Create fake time series data
faked1 <- expand.grid(ID = letters, year = 2010:2015)
faked1$located_continuous <- nrow(faked1):1
faked1$y <- nrow(faked1):1 - 200
#'
# Find weights for continuous data
df_weights_cont1 <- monadic_spatial_weights(df = faked1, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1)
```

    ## Continuous location variable detected. Proximity found using method = euclidean.

    ## 2010: Moran's I p-value: <2e-16

    ## 2011: Moran's I p-value: <2e-16

    ## 2012: Moran's I p-value: <2e-16

    ## 2013: Moran's I p-value: <2e-16

    ## 2014: Moran's I p-value: <2e-16

    ## 2015: Moran's I p-value: <2e-16

``` r
summary(df_weights_cont1)
```

    ##        ID          year           sp_wght_located_continuous_y
    ##  a      :  6   Length:156         Min.   :-62075              
    ##  b      :  6   Class :character   1st Qu.:-34380              
    ##  c      :  6   Mode  :character   Median :-26210              
    ##  d      :  6                      Mean   :-27338              
    ##  e      :  6                      3rd Qu.:-18289              
    ##  f      :  6                      Max.   : -9409              
    ##  (Other):120

Here is a simple example with fake data without spatial clustering:

``` r
# Create fake time series data
faked2 <- expand.grid(ID = letters, year = 2010:2015)
faked2$located_continuous <- rnorm(nrow(faked2))
faked2$y <- nrow(faked2):1 - 200
#'
# Find weights for continuous data
df_weights_cont2 <- monadic_spatial_weights(df = faked2, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1)
```

    ## Continuous location variable detected. Proximity found using method = euclidean.

    ## 2010: Moran's I p-value: 0.412

    ## 2011: Moran's I p-value: 0.344

    ## 2012: Moran's I p-value: 0.249

    ## 2013: Moran's I p-value: 0.122

    ## 2014: Moran's I p-value: 0.851

    ## 2015: Moran's I p-value: 0.455

Install
-------

The package can be installed with:

``` r
devtools::install_github("christophergandrud/spatialWeights")
```
