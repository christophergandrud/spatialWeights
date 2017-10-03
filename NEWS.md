# Changes are be documented here

## Version 0.1.5

- `row_standard` argument allows users to find row-standardized weights 
matrixes.

## Version 0.1.4

- If no `time_var` is given, then `monadic_spatial_weights` assumes all 
observations are from the same time period.

- Message returned if user supplies a numeric `location_var` with fewer than 5 
unique values. This warns of possible problems calculating Moran's I.

- More robust error handling if `id_var` is unspecified.

## Version 0.1.3

- `tlsl` argument added to `monadic_spatial_weights`. If `TRUE` then
temporally-lagged spatial lags are created by lagging the weight one
time unit.

## Version 0.1.2

- **Breaking Changes**: `morans_i` argument syntax.

- `morans_i` argument now accepts `none`, `message`, and `table` to specify if
and how to output p-values from Moran's I.

## Version 0.1.1

- **Breaking Changes**: changes argument syntax.

- Find distances using Euclidean method by default with the ability to use 
others.

- Ability to print p-value from Moran's I Spatial Autocorrelation index.
