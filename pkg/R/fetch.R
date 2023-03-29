#' @describeIn fetch_family Generate summary statistics column(s) for weather variable(s).
#'
#' NOTE: This function is best suited for applying custom functions, or apply a single function to multiple.
#' weather variables. For common statistics (mean, min, max, sd)
#' applied to temperature and rainfall see wrapper functions like [hyenaR::fetch_weather_temp.mean()].
#'
#' @return A tibble with summary statistic column(s).
#' Summary statistics are calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_fn(from = from, to = to, station = station,
#'                          variable = c("temp", "rain"), fn = mean, suffix = "mean"))
#'
#' #Apply a custom function to find difference between temperature min/max
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_fn(from = from, to = to, station = station,
#'                          variable = "temp", fn = ~{max(.) - min(.)}, suffix = "diff"))
fetch_weather_fn <- function(from = NULL, to = NULL, at = NULL,
                             station = NULL, location = NULL,
                             variable = NULL, fn = NULL, suffix = NULL) {

  ## Extract from/to/at dates
  date_range <- hyenaR::check_function_arg.date.fromtoat(from = from, to = to, at = at,
                                                         data.type = "weather")

  from       <- date_range$from
  to         <- date_range$to

  #Identify the unique from/to combinations over which we need to extract data.
  input_full <- dplyr::tibble(from = from, to = to)
  input <- unique(input_full)

  #Convert variable arg to use regex internally
  variable_regex <- check_function_arg.variable.weather(variable)

  #Requires a suffix to make it clear that we applied some summary stat
  #TODO: Create a check function for this
  if (is.null(suffix)) {

    stop("You must provide a suffix to define your new summary column.")

  }

  input %>%
    dplyr::mutate(purrr::pmap_df(.l = .,
                                 .f = ~{

                                   create_crater_weather.table(from = ..1, to = ..2, variable = variable,
                                                                 station = station, location = location) %>%
                                     dplyr::group_by(.data$site_name) %>%
                                     dplyr::summarise(across(.cols = dplyr::matches(variable_regex, perl = TRUE),
                                                             .fns = fn, .names = paste0("{.col}_", suffix))) %>%
                                     tidyr::pivot_wider(names_from = site_name,
                                                        values_from = -"site_name", names_glue = "{site_name}_{.value}")

                                 })) -> output

  hyenaR::check_function_output(input.tbl = input_full, output.tbl = output, join.by = c("from", "to")) %>%
    #Can't use output.IDcolumn because it only allows one col (and we need to know the name)
    dplyr::select(-"from", -"to")

}

#' @describeIn fetch_family Fetch mean temperature from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with mean temperature. Mean temperature is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_temp.mean(from = from, to = to, station = station))
fetch_weather_temp.mean <- function(from = NULL, to = NULL, at = NULL,
                                    station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = mean, variable = "air_temp", suffix = "mean")

}

#' @describeIn fetch_family Fetch max temperature from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with max temperature. Max temperature is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_temp.max(from = from, to = to, station = station))
fetch_weather_temp.max <- function(from = NULL, to = NULL, at = NULL,
                                   station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = max, variable = "air_temp", suffix = "max")

}

#' @describeIn fetch_family Fetch min temperature from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with min temperature. Min temperature is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_temp.min(from = from, to = to, station = station))
fetch_weather_temp.min <- function(from = NULL, to = NULL, at = NULL,
                                   station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = min, variable = "air_temp", suffix = "min")

}

#' @describeIn fetch_family Fetch standard deviation of temperature from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with standard deviation of temperature.
#' Standard deviation is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Determine mean of temperature and rainfall from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_temp.sd(from = from, to = to, station = station))
fetch_weather_temp.sd <- function(from = NULL, to = NULL, at = NULL,
                                  station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = stats::sd, variable = "air_temp", suffix = "sd")

}

#' @describeIn fetch_family Fetch mean rainfall from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with mean rainfall Mean rainfall is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_rain.mean(from = from, to = to, station = station))
fetch_weather_rain.mean <- function(from = NULL, to = NULL, at = NULL,
                                    station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = mean, variable = "precip", suffix = "mean")

}

#' @describeIn fetch_family Fetch max rainfall from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with max rainfall. Max rainfall is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_rain.max(from = from, to = to, station = station))
fetch_weather_rain.max <- function(from = NULL, to = NULL, at = NULL,
                                   station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = max, variable = "precip", suffix = "max")

}

#' @describeIn fetch_family Fetch min rainfall from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with min rainfall. Min rainfall is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_rain.min(from = from, to = to, station = station))
fetch_weather_rain.min <- function(from = NULL, to = NULL, at = NULL,
                                   station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = min, variable = "precip", suffix = "min")

}

#' @describeIn fetch_family Fetch standard deviation of rainfall from a weather station during a given period
#'
#' NOTE: To apply custom functions please see [hyenaR::fetch_weather_fn()].
#'
#' @return A tibble with standard deviation of rainfall.
#' Standard deviation is calculated separately for each station/location.
#' @export
#'
#' @examples
#' #Summary stat from station jua in October 2021
#' data.frame(from = "2021-10-01", to = "2021-10-31",
#'            station = "jua") %>%
#'            dplyr::mutate(fetch_weather_rain.sd(from = from, to = to, station = station))
fetch_weather_rain.sd <- function(from = NULL, to = NULL, at = NULL,
                                  station = NULL, location = NULL) {

  fetch_weather_fn(from = from, to = to, at = at, station = station, location = location,
                   fn = stats::sd, variable = "precip", suffix = "sd")

}
