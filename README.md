
spatialWeights
==============

[![Build Status](https://travis-ci.org/christophergandrud/spatialWeights.svg?branch=master)](https://travis-ci.org/christophergandrud/spatialWeights)

This R package is designed to implement some of the spatial weights discussed in [Neumayer and Plumper (2010)](http://eprints.lse.ac.uk/30750/1/Making%20spatial%20analysis%20operational(lsero).pdf).

The package's focus is on weights in time-series cross-sectional data. It can find spatial weights and test statistics for spatial autocorrelation (Moran's I) on a per-time interval basis. It can be important to test for spatial autocorrelation on a per-time interval basis in order to assess if a spatial autocorrelation process is temporally bounded.

Examples
--------

### Data with spatial clustering

Here is a simple example with fake data with spatial clustering:

``` r
library(spatialWeights)

# Create fake time series data
faked1 <- expand.grid(ID = letters, year = 2010:2015)
faked1$located_continuous <- nrow(faked1):1
faked1$y <- nrow(faked1):1 - 200

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
head(df_weights_cont1)
```

    ##   ID year sp_wght_located_continuous_y
    ## 1  a 2010                       -19825
    ## 2  b 2010                       -18444
    ## 3  c 2010                       -17153
    ## 4  d 2010                       -15954
    ## 5  e 2010                       -14849
    ## 6  f 2010                       -13840

### Data without spatial clustering

``` r
# Create fake time series data
faked2 <- expand.grid(ID = letters, year = 2010:2015)
faked2$located_continuous <- rnorm(nrow(faked2)) 
faked2$y <- nrow(faked2):1 - 200

# Find weights for continuous data
df_weights_cont2 <- monadic_spatial_weights(df = faked2, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1)
```

    ## Continuous location variable detected. Proximity found using method = euclidean.

    ## 2010: Moran's I p-value: 0.457

    ## 2011: Moran's I p-value: 0.154

    ## 2012: Moran's I p-value: 0.116

    ## 2013: Moran's I p-value: 0.591

    ## 2014: Moran's I p-value: 0.337

    ## 2015: Moran's I p-value: 0.653

### Temporally Lagged Spatial Lags

By default `monadic_spatial_weights` creates a spatially lagged variable. Using the arguement `tlsl = TRUE` will lag this variable in time by one period creating an additional time lag spatial lag. For example:

``` r
# Create fake time series data
faked1 <- expand.grid(ID = letters, year = 2010:2015)
faked1$located_continuous <- nrow(faked1):1
faked1$y <- nrow(faked1):1 - 200

# Find weights for continuous data
df_weights_cont_tlsl <- monadic_spatial_weights(df = faked1, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1, tlsl = TRUE)
```

    ## Continuous location variable detected. Proximity found using method = euclidean.

    ## 2010: Moran's I p-value: <2e-16

    ## 2011: Moran's I p-value: <2e-16

    ## 2012: Moran's I p-value: <2e-16

    ## 2013: Moran's I p-value: <2e-16

    ## 2014: Moran's I p-value: <2e-16

    ## 2015: Moran's I p-value: <2e-16

``` r
head(df_weights_cont_tlsl)
```

    ##   ID year sp_wght_located_continuous_y lag_sp_wght_located_continuous_y
    ## 1  a 2010                       -19825                               NA
    ## 2  a 2011                       -28275                           -19825
    ## 3  a 2012                       -36725                           -28275
    ## 4  a 2013                       -45175                           -36725
    ## 5  a 2014                       -53625                           -45175
    ## 6  a 2015                       -62075                           -53625

### Multi-threaded

The argument `mc_cores` allows you to specify the number of cores you would like to use find the spatial weights in parellel. This can lead to substantial speed improvements depending on how many cores you have available and how your data set is organised. To determine how many cores you have available use: `parallel::detectCores()`.

Install
-------

The package can be installed with:

``` r
devtools::install_github("christophergandrud/spatialWeights")
```
