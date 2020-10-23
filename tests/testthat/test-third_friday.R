test_that("third_friday() works", {

    year = 2020
    month = 10
    expect_identical(third_friday(year, month), as.Date("2020-10-16"))

    year = 2010
    month = 2
    expect_identical(third_friday(year, month), as.Date("2010-02-19"))

})
