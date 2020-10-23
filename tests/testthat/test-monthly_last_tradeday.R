test_that("monthly_last_tradeday() works", {

    year = 2020
    month = 10
    expect_identical(monthly_last_tradeday(year, month), as.Date("2020-10-16"))

    year = 2014
    month = 12
    expect_identical(monthly_last_tradeday(year, month), as.Date("2014-12-19"))
})
