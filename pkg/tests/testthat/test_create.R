context("Test create functions work...")

test_that("Can create raw data table...", {

  test_data <- create_weather_raw.table(system.file("extdata/working_weather",
                                                    package = "NgoroWeather"))

  ## Should return a data frame with data and meta-data
  expect_identical(c("weather", "weather_metadata"),
                   test_data$name)

  ## Weather data should be at 30 min intervals
  weather_data <- test_data$data[[1]] |>
    dplyr::mutate(datetime = lubridate::ymd_hms(paste(.data$date, .data$time, sep = " "))) |>
    tail()

  expect_identical(30,
                   as.numeric(weather_data$datetime[2] - weather_data$datetime[1]))

})

test_that("Can create summary data table...", {

  summary_data_day <- create_crater_weather.summary.table(resolution = "day")

  summary_data_week <- create_crater_weather.summary.table(resolution = "week")

  summary_data_2h <- create_crater_weather.summary.table(resolution = "2 hour")

  ## Should return a data frame
  expect_true(inherits(summary_data_day, "data.frame"))
  expect_true(inherits(summary_data_week, "data.frame"))
  expect_true(inherits(summary_data_2h, "data.frame"))

  ## Should have summary cols but only temp and precip by default
  expected_cols <- c("air_temp_mean", "air_temp_max", "air_temp_min", "precip_mean",
                     "precip_max", "precip_total")
  expect_true(all(expected_cols %in% colnames(summary_data_day)))
  expect_true(all(expected_cols %in% colnames(summary_data_week)))
  expect_true(all(expected_cols %in% colnames(summary_data_2h)))

  ## Should have different time steps
  expect_identical(1, as.numeric(lubridate::ymd(summary_data_day$date[2]) - lubridate::ymd(summary_data_day$date[1])))
  expect_identical(7, as.numeric(lubridate::ymd(summary_data_week$date[2]) - lubridate::ymd(summary_data_week$date[1])))

  ## Time col should be present when <1day
  expect_identical(2,
                   as.numeric(lubridate::ymd_hm(paste(summary_data_2h$date[2],
                                     summary_data_2h$time[2], sep = " ")) - lubridate::ymd_hm(paste(summary_data_2h$date[1],
                                                                                                    summary_data_2h$time[1], sep = " "))))

})
