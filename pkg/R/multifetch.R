#' Fetch basic summary statistics of temperature for given weather station(s) on given date.
#'
#' @inheritParams arguments
#'
#' @return A tibble with mean, max, min, and standard deviation of temperature in the focal period.
#' @export
#'
#' @examples
#' load_package_database.weather(system.file("extdata/working_weather",
#' package = "NgoroWeather"), overwrite.db = "yes")
#'
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") |>
#'            dplyr::mutate(multifetch_weather_temp.summary(from = from, to = to, station = station))
multifetch_weather_temp.summary <- function(from = NULL, to = NULL, at = NULL,
                                            station = NULL, location = NULL) {

  fetch_weather_temp.mean(from = from, to = to, at = at, station = station, location = location) |>
    dplyr::bind_cols(fetch_weather_temp.max(from = from, to = to, at = at, station = station, location = location)) |>
    dplyr::bind_cols(fetch_weather_temp.min(from = from, to = to, at = at, station = station, location = location)) |>
    dplyr::bind_cols(fetch_weather_temp.sd(from = from, to = to, at = at, station = station, location = location))

}

#' Fetch basic summary statistics of rainfall for given weather station(s) on given date.
#'
#' @inheritParams arguments
#'
#' @return A tibble with mean, max, min, and standard deviation of rainfall in the focal period.
#' @export
#'
#' @examples
#' load_package_database.weather(system.file("extdata/working_weather",
#' package = "NgoroWeather"), overwrite.db = "yes")
#'
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") |>
#'            dplyr::mutate(multifetch_weather_rain.summary(from = from, to = to, station = station))
multifetch_weather_rain.summary <- function(from = NULL, to = NULL, at = NULL,
                                            station = NULL, location = NULL) {

  fetch_weather_rain.mean(from = from, to = to, at = at, station = station, location = location) |>
    dplyr::bind_cols(fetch_weather_rain.max(from = from, to = to, at = at, station = station, location = location)) |>
    dplyr::bind_cols(fetch_weather_rain.min(from = from, to = to, at = at, station = station, location = location)) |>
    dplyr::bind_cols(fetch_weather_rain.sd(from = from, to = to, at = at, station = station, location = location))

}
