# Changes will be documented here

## Version 0.1.4

- Message returned if supplies a numeric `location_var` with fewer than 5 
unique values. This warns of possible problems calculating Moran's I.

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
