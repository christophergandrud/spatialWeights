#' Function to find monadic weights at one time point
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
#' \code{df}. Note that an independent variable could also be supplied.
#' @param return_matrix logical. Whether or not to return the adjacency matrix.
#' Could be useful for debugging.
#'
#' @source Neumayer, Eric, and Thomas Plumper. "Making spatial analysis operational:
#' commands for generating spatial effect variables in monadic and dyadic data."
#' Stata Journal 10.4 (2010): 585-605.
#'  \url{http://eprints.lse.ac.uk/30750/1/Making\%20spatial\%20analysis\%20operational(lsero).pdf}.
#'
#' @importFrom dplyr %>% full_join bind_rows select
#' @importFrom igraph graph_from_data_frame as_adjacency_matrix
#'
#' @noRd
weights_at_t <- function(df, id_var, location_var, y_var, type_numeric,
                        return_matrix = FALSE) {
    freq <- NULL

    if (missing(type_numeric)) stop(
        'type_numeric must be specified as TRUE or FALSE.', call. = FALSE)

    if (any(duplicated(df[, id_var]))) stop('Duplicate observations found',
                                            call. = FALSE)

    # Find w_{ikt}
    df$temp <- 1
    joined <- full_join(df, df, by = 'temp')
    joined <- joined[, c(paste0(id_var, '.y'), paste0(id_var, '.x'),
                         paste0(location_var, '.y'),
                         paste0(location_var, '.x'))]

    if (type_numeric) {
        joined$weighting <- joined[, 3] - joined[, 4]
    }
    else if (!isTRUE(type_numeric)) {
        joined$weighting[joined[, 3] == joined[, 4]] <- 1
        joined$weighting[joined[, 3] != joined[, 4]] <- 0
    }
    joined <- joined[, c(paste0(id_var, '.x'),
                         paste0(id_var, '.y'), 'weighting')]
    joined <- joined[joined[, 1] != joined[, 2], ]
    grph <- graph_from_data_frame(joined, directed = FALSE, vertices = NULL)
    t_matrix <- as_adjacency_matrix(grph, attr = 'weighting',
                    sparse = FALSE)

    if (return_matrix) return(t_matrix)
    else {
        # Find y_{kt}
        dependent_y <- df[, c(id_var, y_var)]

        matrix_product <- t_matrix * dependent_y[, 2]
        out <- colSums(matrix_product) %>% as.data.frame
        out[, id_var] <- row.names(out)
        weight_name <- sprintf('sp_wght_%s_%s', location_var, y_var)
        names(out) <- c(weight_name, id_var)
        if (!isTRUE(type_numeric)) {
            # Find group averages
            counts <- table(df[, location_var]) %>% data.frame
            names(counts) <- c(location_var, 'freq')
            id_located <- merge(df, counts, by = location_var)
            id_located <- id_located[, c(id_var, location_var, 'freq')]
            out <- merge(out, id_located, by = id_var)
            out$freq <- out$freq - 1
            out$freq[out$freq < 1] <- 0
            out[, weight_name] <- out[, weight_name] / out$freq
            out <- out %>% select(-freq)
        }
        return(out)
    }
}



#' Drop rows from a data frame with missing values on a given variable(s).
#' @source From the DataCombine package
#' @noRd

DropNA <- function(data, Var)
{
    # Find term number
    DataNames <- names(data)
    if (missing(Var)) {
        if (isTRUE(message)) {
            message('No Var specified. Dropping all NAs from the data frame.\n')
        }
        Var <- names(data)
    }
    TestExist <- Var %in% DataNames
    if (!all(TestExist)){
        stop("Variable(s) not found in the data frame.", call. = FALSE)
    }

    # Drop if NA
    if (length(Var) == 1){
        DataNoNA <- data[!is.na(data[, Var]), ]

        DataVar <- data[, Var]
        DataNA <- DataVar[is.na(DataVar)]
        TotalDropped <- length(DataNA)
    }
    else{
        RowNA <- apply(data[, Var], 1, function(x){any(is.na(x))})
        DataNoNA <- data[!RowNA, ]

        TotalDropped <- sum(RowNA)
    }

    if (TotalDropped > 0)
        message(paste(TotalDropped,
            "rows dropped from the data frame because of missing values.\n",
            "Use na_rm = FALSE to keep observations with missing values."))

    return(DataNoNA)
}
