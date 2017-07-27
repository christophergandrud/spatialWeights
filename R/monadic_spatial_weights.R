#' Find monadic spatial weights for a cross-sectional time-series data set
#' @param df a data frame containing the unit ID variable and time variables as
#' well as location' and dependent variables.
#' @param id_var a character string identifying the unit ID variable in
#' \code{df}.
#' @param time_var a character string identifying the time variable in
#' \code{df}.
#' @param location_var a character string identifying the location of the units
#' in \code{df}. This is used to create the weighting matrix if
#' \code{weight_matrix} is not given. If \code{weight_matrix} is given, then
#' \code{location_var} simply acts as a variable label.
#' Note that if \code{weight_matrix} is not given then the function finds the
#' relative distance between the units by subtracting their 'location'.
#' @param y_var a character string identifying the dependent variable in
#' \code{df}. Note that an independent variable could also be supplied.
#' @param location_var_type character string allowing you to hard code
#' the measurement type of the location variable and thus how to determine the
#' weighting method Can be either \code{continuous} or \code{categorical}
#' If not specified, the type with be determined automatically.
#' @param weight_name character string providing a custom weighting variable
#' name.
#' @param weight_matrix
#' @param method the distance measure to be used. Only relevant for numeric
#' location variables. This must be one of
#' \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"},
#' \code{"binary"} or \code{"minkowski"}. Any unambiguous substring can be
#' given. See \code{\link{dist}} for details.
#' @param tlsl logical whether or not to create a temporally-lagged spatial
#' lag by lagging the spatial weight one time unit. Note, Moran's I
#' statistic refer to the non-lagged spatial weights.
#' @param mc_cores The number of cores to use, i.e. at most how many child
#' processes will be run simultaneously. The option is initialized from
#' environment variable \code{MC_CORES} if set. Must be at least one, and
#' parallelization requires at least two cores.
#' @param na_rm logical whether or not to remove missing values.
#' @param morans_i character. Whether to print the p-value of Moran's I
#' Autocorrelation Index to the console (\code{message}), return only a table of
#' p-values (\code{table}), or \code{none}. Note only relevant for continuous
#' \code{y_var}. Join count analysis for categorical data not yet implemented.
#' @param ... arguments to pass to methods.
#'
#' @details Finds spatial effects in monadic data. See Neumayer and Plumper
#'(2010, 591) for details.
#'
#' The weights are found for each unit \eqn{i} given other units \eqn{k} at
#' time \eqn{t} using \eqn{\sum_{k}w_{kit}y_{kt}}.
#'
#' For continuous \code{location_var} \eqn{w} is the euclidean distance.
#' You can choose an alternate method with the \code{method} argument. See
#' \code{\link{dist}} for details.
#'
#' For factor/character \code{location_var} \eqn{w} is 1 if they share the same
#' location, 0 otherwise. The weight is further found by averaging over the
#' number of units at \eqn{t} that share the location of \eqn{i}.
#'
#' @return A data frame with three columns, one each for \code{id_var},
#' \code{time_var}, and the newly created spatial weights.
#'
#' @examples
#' # Create fake time series data
#' faked <- expand.grid(ID = letters, year = 2010:2015)
#' faked$located_continuous <- nrow(faked):1
#' faked$located_categorical <- sample(c('E', 'S', 'W', 'N'),
#'                                  nrow(faked), replace = TRUE)
#' faked$y <- nrow(faked):1 - 200
#'
#' # Find weights for continuous data
#' df_weights_cont <- monadic_spatial_weights(df = faked, id_var = 'ID',
#'                                          time_var = 'year',
#'                                          location_var = 'located_continuous',
#'                                          y_var = 'y', mc_cores = 1)
#'
#' # Find weights and TLSL for continuous data
#' df_weights_cont <- monadic_spatial_weights(df = faked, id_var = 'ID',
#'                                          time_var = 'year',
#'                                          location_var = 'located_continuous',
#'                                          y_var = 'y', mc_cores = 1,
#'                                          tlsl = TRUE)
#'
#' # Find weights for character data
#' df_weights_cat <- monadic_spatial_weights(df = faked, id_var = 'ID',
#'                                          time_var = 'year',
#'                                          location_var = 'located_categorical',
#'                                          y_var = 'y', mc_cores = 1)
#'
#' # Return a table of p-values from Moran's I spatial autocorrelation test
#' # statistic
#' moran_i_table <- monadic_spatial_weights(df = faked, id_var = 'ID',
#'                                          time_var = 'year',
#'                                          location_var = 'located_categorical',
#'                                          y_var = 'y', mc_cores = 1,
#'                                          morans_i = 'table')
#'
#' @source Neumayer, Eric, and Thomas Plumper. "Making spatial analysis operational:
#' commands for generating spatial effect variables in monadic and dyadic data."
#' Stata Journal 10.4 (2010): 585-605.
#'  \url{http://eprints.lse.ac.uk/30750/1/Making\%20spatial\%20analysis\%20operational(lsero).pdf}.
#'
#' @importFrom parallel mclapply
#' @importFrom dplyr bind_rows
#'
#' @export

monadic_spatial_weights <- function(df, id_var, time_var, location_var, y_var,
                                    location_var_type,
                                    weight_name,
                                    weight_matrix,
                                    method = 'euclidean',
                                    tlsl = FALSE,
                                    mc_cores = 1,
                                    na_rm = TRUE,
                                    morans_i = 'message',
                                    ...)
{
    temp <- NULL

    if (missing(location_var)) location_var <- "Location_Var"

    if (missing(weight_matrix)) weight_matrix <- NA

    morans_i <- tolower(morans_i)
    if (!(morans_i %in% c('none', 'message', 'table')))
        stop("morans_i must be one of 'none', 'message', or 'table'.",
             call. = FALSE)

    if (morans_i != 'none' & mc_cores > 1) {
        morans_i <- 'none'
        message("Note: p-value of Moran's I is only found when mc_cores = 1.\n")
    }
    if (missing(weight_name))
        weight_name <- sprintf('sp_wght_%s_%s', location_var, y_var)

    if (!missing(location_var_type)) {
        if (location_var_type == 'continuous') type_cont <- TRUE
        else if (location_var_type == 'factor') type_cont <- FALSE
        else stop(message("location_var_type can only be 'continuous' or 'categorical'."),
                    call. = FALSE)
    }
    if (missing(location_var_type)) {
        # Determine weighting scheme and inform user
        if (is.numeric(df[, location_var])) {
            message(sprintf(
                'Continuous location variable detected. Proximity found using method = %s.\n',
                method))
            type_cont <- TRUE
        }
        else if (is.character(df[, location_var]) |
                 is.factor((df[, location_var]))) {
            message('Categorical location detected.\nProximity found using Xi == Xk and group averaging.\n')
            if (method != 'euclidean')
                message('method argument ignored for categorical location variables')
            type_cont <- FALSE
        }
    }

    # Remove missing values
    if (na_rm) df <- DropNA(df, c(id_var, time_var, location_var, y_var))

    # Split by time period
    time_split <- split(df, f = df[, time_var])

    # Find all weights for each time period
    weighted <- mclapply(time_split, weights_at_t,
                   id_var = id_var,
                   location_var = location_var,
                   time_var = time_var,
                   weight_name = weight_name,
                   weight_matrix = weight_matrix,
                   y_var = y_var,
                   type_cont = type_cont,
                   mc.cores = mc_cores,
                   method = method,
                   morans_i = morans_i)
    weighted <- suppressWarnings(bind_rows(weighted, .id = time_var))

    if (tlsl) {
        if (missing(time_var)) stop('time_var must be specified if tlsl = TRUE',
                                    call. = FALSE)
        weighted <- lagger(weighted, id_var = id_var, time_var = time_var,
                           weight_name = weight_name)
    }
browser()
    if (morans_i != 'table') {
        names(weighted) <- c(time_var, names(weighted)[-1])
        if (tlsl)
            weighted <- weighted[, c(id_var, time_var, weight_name,
                                     sprintf('lag_%s', weight_name))] %>%
                data.frame
        else
            weighted <- weighted[, c(id_var, time_var, weight_name)]
    }
    return(weighted)
}
