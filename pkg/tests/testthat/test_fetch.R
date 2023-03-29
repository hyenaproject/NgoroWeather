context("Test fetch functions work...")

test_that("Use fetch functions...", {

  manual <- fetch_weather_fn(from = "2021-09-01", to = "2021-09-30",
                   variable = "air_temp", fn = max, suffix = "max")

  wrapper <- fetch_weather_temp.max(from = "2021-09-01", to = "2021-09-30")

  expect_identical(manual, wrapper)

})
