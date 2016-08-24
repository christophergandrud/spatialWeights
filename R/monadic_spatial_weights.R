#' Find monadic spatial weights for continuous numeric data in a time series
#' data set
#' @param df a data frame containing the unit ID variable and time variables as
#' well as location' and dependent variables.
#' @param id_var a character string identifying the unit ID variable in
#' \code{df}.
#' @param time_var a character string identifying the time variable in
#' \code{df}.
#' @param location_var a character string identifying the location of the units
#' in \code{df}. This is used to create the weighting matrix. Note that the
#' function finds the relative distance between the units by subtracting their
#' 'location'.
#' @param y_var a character string identifying the dependent variable in
#' \code{df}. Note that
#' an independent variable could also be supplied.
#' @param mc_cores The number of cores to use, i.e. at most how many child
#' processes will be run simultaneously. The option is initialized from
#' environment variable \code{MC_CORES} if set. Must be at least one, and
#' parallelization requires at least two cores.
#' @param na_rm logical whether or not to remove missing values.
#'
#' @details Finds spatial effects in monadic data. See Neumayer and Plumper
#'(2010, 591) for details.
#'
#' @return A data frame with three columns, one each for \code{id_var},
#' \code{time_var}, and the newly created spatial weights.
#'
#' @examples
#' # Create fake time series data
#' faked <- expand.grid(ID = 1:10, year = 2010:2015)
#' faked$located <- nrow(faked):1
#' faked$y <- nrow(faked):1 - 200
#'
#' # Find weights using 4 cores
#' df_weights <- monadic_spatial_weights(df = faked, id_var = 'ID',
#'                                       time_var = 'year',
#'                                       location_var = 'located', y_var = 'y',
#'                                       mc_cores = 2)
#'
#' @source Neumayer, Eric, and Thomas Plumper. "Making spatial analysis operational:
#' commands for generating spatial effect variables in monadic and dyadic data."
#' Stata Journal 10.4 (2010): 585-605.
#'
#' @importFrom parallel mclapply
#' @importFrom dplyr %>% full_join bind_rows select
#' @importFrom igraph graph_from_data_frame as_adjacency_matrix
#'
#' @export

monadic_spatial_weights <- function(df, id_var, time_var, location_var, y_var,
                                    mc_cores = 1, na_rm = TRUE) {
    temp <- NULL

    if (na_rm) df <- DropNA(df, c(id_var, time_var, location_var, y_var))

    time_split <- split(df, f = df[, time_var])

    # Function to find weights at one time point
    weights_at_t <- function(df, id_var, location_var, y_var) {
        # Find w_{ikt}
        df$temp <- 1
        joined <- full_join(df, df, by = 'temp') %>% select(-temp)
        joined$weighting <- joined[, paste0(location_var, '.y')] -
            joined[, paste0(location_var, '.x')]
        joined <- joined[, c(paste0(id_var, '.x'),
                    paste0(id_var, '.y'), 'weighting')]
        joined <- joined[joined[, 1] != joined[, 2], ]
        grph <- graph_from_data_frame(joined, directed = FALSE, vertices = NULL)
        t_matrix <- as_adjacency_matrix(grph, attr = 'weighting',
                        sparse = FALSE)

        # Find y_{kt}
        dependent_y <- df[, c(id_var, y_var)]

        matrix_product <- t_matrix * dependent_y[, 2]
        out <- colSums(matrix_product) %>% as.data.frame
        out[, id_var] <- row.names(out)
        names(out) <- c(sprintf('sp_weights_%s_%s', location_var, y_var), id_var)
        return(out)
    }

    # Find all weights
    weighted <- mclapply(time_split, weights_at_t,
                   id_var = id_var,
                   location_var = location_var,
                   y_var = y_var,
                   mc.cores = mc_cores)
    weighted <- bind_rows(weighted, .id = time_var)
    names(weighted) <- c(time_var, names(weighted)[-1])
    weighted <- weighted[, c(3, 1, 2)]
    return(weighted)
}
