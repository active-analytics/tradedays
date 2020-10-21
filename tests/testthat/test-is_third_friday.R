test_that("is_third_friday() works", {

    dt = as.Date("2020-10-16")
    expect_true(is_third_friday(dt))

    dt = as.Date("2020-10-23")
        expect_false(is_third_friday(dt))
})
