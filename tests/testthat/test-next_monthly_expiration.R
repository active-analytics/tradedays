test_that("next_monthly_expiration() works", {

    dt = "2020-10-01"
    expect_identical(next_monthly_expiration(dt), as.Date("2020-10-16"))

    dt = "2020-10-16"
    expect_identical(next_monthly_expiration(dt), as.Date("2020-11-20"))

    dt = "2014-01-01"
    expect_identical(next_monthly_expiration(dt), as.Date("2014-01-18"))

    dt = "2014-01-18"
    expect_identical(next_monthly_expiration(dt), as.Date("2014-02-22"))
})
