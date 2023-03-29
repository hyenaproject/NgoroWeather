#' @describeIn create_family Create a table of the meta-data attribute of a dataframe.
#'
#' @return Tibble of meta-data information.
#' @export
#'
#' @examples
#' #Example weather data collected from IZW
#' test_data <- load_data_weatherstation.file(system.file("extdata/weather_Mlima",
#'                                           "weather_data_test1.xlsx",
#'                                           package = "hyenaR"))
#'
#' #Extract metadata
#' create_tbl_metadata(test_data)
create_tbl_metadata <- function(input.tbl){

  attributes(input.tbl)$metadata

}

#' @describeIn create_family Create a table of all weather data at 30 min intervals
#'
#' @return Tibble of meta-data information.
#' @export
#'
#' @examples
#' #Example weather data collected from IZW
#' test_data <- create_weather_raw.table(system.file("extdata/working_weather",
#'                                           package = "NgoroWeather"))
create_weather_raw.table <- function(input.folder){

  weather_folders <- list.dirs(input.folder, recursive = FALSE)

  weather_data <- purrr::map(.x = weather_folders, .f = function(dir){

    load_data_weatherstation.all(dir)

  })

  weather_metadata <- purrr::map_df(.x = weather_data, .f = function(station){

    create_tbl_metadata(station) |>
      dplyr::mutate(station_name = unique(station$station_name)) |>
      #If we want to add this into our sqlite database we can't have nested lists
      tidyr::unnest(cols = c("data", "messages"))

  })

  ### Join in activity dates and filter
  weather_data_active <- purrr::map(.x = weather_data, .f = function(station){

    station |>
      dplyr::left_join(weather_station_activity, by = c("station_name", "site_name"), multiple = "all") |>
      dplyr::filter(lubridate::as_date(date_time) >= start_date & lubridate::as_date(date_time) <= end_date) |>
      dplyr::select(-start_date, -end_date, -comment)

  })

  weather_data_active <- dplyr::bind_rows(weather_data_active) |>
    #Split date and time columns and treat as characters. This way they can be easily stored in db
    #And easily re-compiled when loading db in R
    tidyr::separate(col = .data$date_time, into = c("date", "time"), sep = " ", convert = TRUE)

  return(dplyr::tibble(name = c("weather", "weather_metadata"), data = list(weather_data_active, weather_metadata), datecheck = TRUE)) ## no date check necessary

}

create_weather_summary.table <- function(resolution = "day"){

  hyenaR::check_database_is.loaded()

  raw_data <- hyenaR::extract_database_table("weather")

  ## Take the weather data only...
  summary_data <- raw_data |>
    ## Work out which group each record will fall into (i.e. which day, hour, week etc)
    dplyr::mutate(datetime = lubridate::floor_date(lubridate::ymd_hms(paste(date, time, sep = " ")),
                                                   resolution)) |>
    ## Now, for each station/time step, create summary stats
    dplyr::group_by(.data$site_name, .data$datetime) |>
    dplyr::summarise(station_name = dplyr::first(.data$station_name),
                     latitude = dplyr::first(.data$latitude),
                     longitude = dplyr::first(.data$longitude),
                     period = dplyr::first(.data$observation_period),
                     dplyr::across(.cols = c("air_temp", "relative_humidity", "atmospheric_pressure", "battery_percent"),
                                   .fns = list(mean = mean,
                                               max = max,
                                               min = min)),
                     dplyr::across(.cols= "precip", .fns = list(mean = mean,
                                                                max = max,
                                                                total = sum)),
                     dplyr::across(.cols = "precip_max_hourly", .fns = list(mean = mean,
                                                                            max = max)),
                     .groups = "drop") |>
    ## Convert back to date and time cols for simplicity
    dplyr::mutate(date = format(.data$datetime, "%Y-%m-%d"),
                  time = format(.data$datetime, "%H:%M")) |>
    dplyr::select("site_name", "station_name", "date", "time",
                  "latitude":"period",
                  "air_temp_mean":"air_temp_min",
                  "precip_mean":"precip_max_hourly_max",
                  "relative_humidity_mean":"battery_percent_min")

  ## If there's only one time (i.e. we grouped by date or higher)
  ## Then just remove this col.
  if (length(unique(summary_data$time)) == 1) {

    summary_data <- summary_data |>
      dplyr::select(-"time")

  }

  summary_data

}

#' @describeIn create_family Create a table of weather data in a given date range
#'
#' @return Tibble of weather data
#' @export
#'
#' @examples
#' #Get temp data from jua station
#' create_weather_starting.table(system.file("extdata/working_weather", package = "NgoroWeather"),
#'                               variable = "temp", station = "jua")
#'
create_weather_starting.table <- function(from = NULL, to = NULL, at = NULL,
                                          resolution = "30 minute",
                                          variable = c("temp", "rain", "rainmax", "humidity", "pressure", "battery"),
                                          station = NULL,
                                          location = NULL){

  #User must provide one of either station or location, not both
  if ((!is.null(station) & !is.null(location))) {

    stop("Please provide only one of `station` or `location`.")

  }

  #Check station, location, and variable names
  station   <- check_function_arg.station(station)
  location  <- check_function_arg.location.weather(location)
  variable <- check_function_arg.variable.weather(variable)

  ## Create summary table
  summary_weather <- create_weather_summary.table(resolution = resolution)

  ## If time is missing, add it back as 00:00. This could be removed if we were creating summary
  ## data at scale >= day
  if (!"time" %in% colnames(summary_weather)) {
    summary_weather$time <- "00:00"
  }

  summary_weather <- summary_weather |>
    ## Combine to a date time file (we separate them for saving as .csv for output)
    dplyr::mutate(date_time = lubridate::ymd_hm(paste(.data$date, .data$time, sep = " "),
                                                tz = "Africa/Dar_es_Salaam"))

  ## Data are already subset to only include the active period of each station (e.g. exclude periods of repair)
  ## Therefore, if from/to/at not provided just use Inf!
  date_range <- hyenaR::check_function_arg.date.fromtoat(from, to, at,
                                                         .fill = TRUE,
                                                         min.date = min(weather_station_activity$start_date),
                                                         max.date = max(weather_station_activity$end_date),
                                                         arg.max.length = 1L, data.type = "weather")

  ## Convert to POSIXct so we can compare to date-time data
  from       <- as.POSIXct(date_range$from)
  to         <- as.POSIXct(date_range$to)

  output <- summary_weather |>
    dplyr::filter(.data$site_name %in% !!location & .data$station_name %in% !!station & .data$date_time >= from & .data$date_time <= to) |>
    dplyr::select("site_name", "station_name", "date_time", "latitude", "longitude", matches(variable))

  output

}
