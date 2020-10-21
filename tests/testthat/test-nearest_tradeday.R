test_that("nearest_tradeday works on holidays", {
    christmas_2002 <- "2002-12-25"
    independence_2006 <- "2006-07-04"
    new_years_2010 <- "2010-01-01"
    labor_2014 <- "2014-09-01"
    good_friday_2018 <- "2018-03-30"

    expect_identical(
        nearest_tradeday(christmas_2002), as.Date("2002-12-26")
    )
    expect_identical(
        nearest_tradeday(independence_2006), as.Date("2006-07-05")
    )
    expect_identical(
        nearest_tradeday(new_years_2010), as.Date("2010-01-04")
    )
    expect_identical(
        nearest_tradeday(labor_2014), as.Date("2014-09-02")
    )
    expect_identical(
        nearest_tradeday(good_friday_2018), as.Date("2018-04-02")
    )

})
