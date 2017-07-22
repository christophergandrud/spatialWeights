test_that("REQUIRE TESTS", {
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
})