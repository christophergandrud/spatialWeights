# Return expected columns/rows -------------------------------------------------
test_that("REQUIRE TESTS examples", {
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

    expect_equal(nrow(df_weights_cont), 156)

    # Find weights and TLSL for continuous data
    df_weights_cont_tlsl <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                               time_var = 'year',
                                               location_var = 'located_continuous',
                                               y_var = 'y', mc_cores = 1,
                                               tlsl = TRUE)
    expect_equal(ncol(df_weights_cont_tlsl), 4)

    # Find weights for character data
    df_weights_cat <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                              time_var = 'year',
                                              location_var = 'located_categorical',
                                              y_var = 'y', mc_cores = 1)
    expect_equal(ncol(df_weights_cat), 3)

    # Return a table of p-values from Moran's I spatial autocorrelation test statistic
    moran_i_table <- monadic_spatial_weights(df = faked, id_var = 'ID',
                                             time_var = 'year',
                                             location_var = 'located_categorical',
                                             y_var = 'y', mc_cores = 1,
                                             morans_i = 'table')
    expect_equal(nrow(moran_i_table), 6)
})


test_that("REQUIRE TEST no time variable specified", {
    faked_no_time <- expand.grid(ID = letters)
    faked_no_time$located_continuous <- nrow(faked_no_time):1
    faked_no_time$located_categorical <- sample(c('E', 'S', 'W', 'N'),
                                                nrow(faked_no_time),
                                                replace = TRUE)
    faked_no_time$y <- nrow(faked_no_time):1 - 26

    df_weights_cont_no_time <- monadic_spatial_weights(
                                    df = faked_no_time, id_var = 'ID',
                                    location_var = 'located_continuous',
                                    y_var = 'y', mc_cores = 1)

    expect_equal(ncol(df_weights_cont_no_time), 2)
})

test_that("FAIL TESTS", {
    # Create fake time series data
    faked <- expand.grid(ID = letters, year = 2010:2015)
    faked$located_continuous <- nrow(faked):1
    faked$located_categorical <- sample(c('E', 'S', 'W', 'N'),
                                        nrow(faked), replace = TRUE)
    faked$y <- nrow(faked):1 - 200

    # No id_var specified
    expect_error(
        monadic_spatial_weights(df = faked, time_var = 'year',
                           location_var = 'located_continuous',  y_var = 'y',
                           mc_cores = 1),
        "id_var is required."
)
})
