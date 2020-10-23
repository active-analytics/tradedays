test_that("backtest_weekly_expirations() works", {

    # testing against the example stored in /data
    df = backtest_weekly_expirations(20020101, 20201231)
    expect_identical(df, tradedays::weekly_expirations_2002_2020)
})
