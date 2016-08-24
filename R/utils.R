#' Drop rows from a data frame with missing values on a given variable(s).
#'
#'
#' @source From the DataCombine package
#'
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
