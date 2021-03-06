% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monadic_spatial_weights.R
\name{monadic_spatial_weights}
\alias{monadic_spatial_weights}
\title{Find monadic spatial weights for a cross-sectional time-series data set}
\source{
Neumayer, Eric, and Thomas Plumper. "Making spatial analysis operational:
commands for generating spatial effect variables in monadic and dyadic data."
Stata Journal 10.4 (2010): 585-605.
 \url{http://eprints.lse.ac.uk/30750/1/Making\%20spatial\%20analysis\%20operational(lsero).pdf}.
}
\usage{
monadic_spatial_weights(df, id_var, time_var, location_var, y_var,
  location_var_type, weight_name, method = "euclidean",
  row_standard = FALSE, tlsl = FALSE, mc_cores = 1, na_rm = TRUE,
  morans_i = "message", ...)
}
\arguments{
\item{df}{a data frame containing the unit ID variable and time variables as
well as location' and dependent variables.}

\item{id_var}{a character string identifying the unit ID variable in
\code{df}.}

\item{time_var}{a character string identifying the time variable in
\code{df}. If not specified, then all observations are assumed to be from the
same time period.}

\item{location_var}{a character string identifying the location of the units
in \code{df}. This is used to create the weighting matrix. Note that the
function finds the relative distance between the units by subtracting their
'location'.}

\item{y_var}{a character string identifying the dependent variable in
\code{df}. Note that an independent variable could also be supplied.}

\item{location_var_type}{character string allowing you to hard code
the measurement type of the location variable and thus how to determine the
weighting method Can be either \code{continuous} or \code{categorical}
If not specified, the type with be determined automatically.}

\item{weight_name}{character string providing a custom weighting variable
name.}

\item{method}{the distance measure to be used. Only relevant for numeric
location variables. This must be one of
\code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"},
\code{"binary"} or \code{"minkowski"}. Any unambiguous substring can be
given. See \code{\link{dist}} for details.}

\item{row_standard}{logical. Whether or not to row-standardize the adjacency
matrix, i.e. dividing each neighbor weight for a feature by the sum of all
neighbor weights for that feature.}

\item{tlsl}{logical whether or not to create a temporally-lagged spatial
lag by lagging the spatial weight one time unit. Note, Moran's I
statistic refer to the non-lagged spatial weights.}

\item{mc_cores}{The number of cores to use, i.e. at most how many child
processes will be run simultaneously. The option is initialized from
environment variable \code{MC_CORES} if set. Must be at least one, and
parallelization requires at least two cores.}

\item{na_rm}{logical whether or not to remove missing values.}

\item{morans_i}{character. Whether to print the p-value of Moran's I
Autocorrelation Index to the console (\code{message}), return only a table of
p-values (\code{table}), or \code{none}. Note only relevant for continuous
\code{y_var}. Join count analysis for categorical data not yet implemented.}

\item{...}{arguments to pass to methods.}
}
\value{
A data frame with three columns, one each for \code{id_var},
\code{time_var}, and the newly created spatial weights.
}
\description{
Find monadic spatial weights for a cross-sectional time-series data set
}
\details{
Finds spatial effects in monadic data. See Neumayer and Plumper
(2010, 591) for details.

The weights are found for each unit \eqn{i} given other units \eqn{k} at
time \eqn{t} using \eqn{\sum_{k}w_{kit}y_{kt}}.

For continuous \code{location_var} \eqn{w} is the euclidean distance.
You can choose an alternate method with the \code{method} argument. See
\code{\link{dist}} for details.

For factor/character \code{location_var} \eqn{w} is 1 if they share the same
location, 0 otherwise. The weight is further found by averaging over the
number of units at \eqn{t} that share the location of \eqn{i}.
}
\examples{
# Create fake time series data
faked <- expand.grid(ID = letters, year = 2010:2015)
faked$located_continuous <- nrow(faked):1
faked$located_categorical <- sample(c('E', 'S', 'W', 'N'),
                                 nrow(faked), replace = TRUE)
faked$y <- nrow(faked):1 - 200

# Find weights for continuous data
df_weights_cont <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1)

# Find row standardized weights for continuous data
df_weights_cont_st <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                         time_var = 'year',
                                         row_standard = TRUE,
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1)

# Find weights and TLSL for continuous data
df_weights_cont_tlsl <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_continuous',
                                         y_var = 'y', mc_cores = 1,
                                         tlsl = TRUE)

# Find weights for character data
df_weights_cat <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_categorical',
                                         y_var = 'y', mc_cores = 1)

# Return a table of p-values from Moran's I spatial autocorrelation test statistic
moran_i_table <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                         time_var = 'year',
                                         location_var = 'located_categorical',
                                         y_var = 'y', mc_cores = 1,
                                         morans_i = 'table')

}
