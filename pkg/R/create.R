#' Create a table of the meta-data attribute of a dataframe.
#'
#' @param input.tbl Weather data table to extract meta-data.
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

#' Create a table of all weather data at 30 min intervals
#'
#' @inheritParams arguments
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
      ## Join relationship is many-to-many (multiple records per station/site and multiple activity periods)
      dplyr::left_join(weather_station_activity, by = c("station_name", "site_name"), relationship = "many-to-many") |>
      dplyr::filter(lubridate::as_date(.data$date_time) >= .data$start_date & lubridate::as_date(.data$date_time) <= .data$end_date) |>
      dplyr::select(-"start_date", -"end_date", -"comment")

  })

  weather_data_active <- dplyr::bind_rows(weather_data_active) |>
    #Split date and time columns and treat as characters. This way they can be easily stored in db
    #And easily re-compiled when loading db in R
    tidyr::separate(col = .data$date_time, into = c("date", "time"), sep = " ", convert = TRUE)

  return(dplyr::tibble(name = c("weather", "weather_metadata"), data = list(weather_data_active, weather_metadata), datecheck = TRUE)) ## no date check necessary

}

#' Create summary statistics of weather
#'
#' @inheritParams arguments
create_crater_weather.summary.table <- function(resolution = "day",
                                                from = NULL, to = NULL, at = NULL,
                                                variable = c("air_temp", "precip"),
                                                station = NULL,
                                                location = NULL){

  thirtymin_data <- create_crater_weather.table(from = from, to = to, at = at,
                                                ## We read in all variables and calculate summary stats then filter later...
                                                ## Otherwise we need lots of if/else to check which summary fn we can do
                                                variable = NULL,
                                                station = station, location = location)

  variable <- check_function_arg.variable.weather(variable)

  ## Take the weather data only...
  summary_data <- thirtymin_data |>
    ## Work out which group each record will fall into (i.e. which day, hour, week etc)
    dplyr::mutate(datetime = lubridate::floor_date(.data$date_time, resolution)) |>
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
                  ## Need perl = TRUE to accept lookahead regex
                  dplyr::matches(variable, perl = TRUE))

  ## If there's only one time (i.e. we grouped by date or higher)
  ## Then just remove this col.
  if (length(unique(summary_data$time)) == 1) {

    summary_data <- summary_data |>
      dplyr::select(-"time")

  }

  summary_data

}

#' Create a table of weather data in a given date range
#'
#' @inheritParams arguments
#'
#' @return Tibble of weather data
#' @export
#'
#' @examples
#' load_package_database.weather(system.file("extdata/working_weather", package = "NgoroWeather"), overwrite.db = "yes")
#'
#' #Get temp data from jua station
#' create_crater_weather.table(variable = "air_temp", station = "jua")
create_crater_weather.table <- function(from = NULL, to = NULL, at = NULL,
                                        variable = c("air_temp", "precip"),
                                        station = NULL,
                                        location = NULL){

  if (!exists(".database") | (exists(".database") & !"weather" %in% .database$database$table_name)) {

    stop("Use `load_package_database.weather()` to load weather data")

  }

  #User must provide one of either station or location, not both
  if ((!is.null(station) & !is.null(location))) {

    stop("Please provide only one of `station` or `location`.")

  }

  #Check station, location, and variable names
  station  <- check_function_arg.station(station)
  location <- check_function_arg.location.weather(location)
  variable <- check_function_arg.variable.weather(variable)

  ## Create summary table
  weather_data <- hyenaR::extract_database_table("weather") |>
    ## Combine to a date time file (we separate them for saving as .csv for output)
    dplyr::mutate(date_time = lubridate::ymd_hms(paste(.data$date, .data$time, sep = " "),
                                                tz = "Africa/Dar_es_Salaam"))

  focal_station_activity <- weather_station_activity |>
    dplyr::filter(.data$station_name %in% !!station & .data$site_name %in% !!location)

  ## Data are already subset to only include the active period of each station (e.g. exclude periods of repair)
  ## Therefore, if from/to/at not provided just use Inf!
  date_range <- hyenaR::check_function_arg.date.fromtoat(from, to, at,
                                                         .fill = TRUE,
                                                         min.date = min(focal_station_activity$start_date),
                                                         max.date = max(focal_station_activity$end_date),
                                                         arg.max.length = 1L, data.type = "weather")

  ## Convert to POSIXct so we can compare to date-time data
  from       <- as.POSIXct(date_range$from)
  to         <- as.POSIXct(date_range$to)

  output <- weather_data |>
    dplyr::filter(.data$site_name %in% !!location & .data$station_name %in% !!station & .data$date_time >= from & .data$date_time <= to) |>
    dplyr::select("site_name", "station_name", "date_time", "latitude", "longitude",
                  ## Need perl = TRUE to accept lookahead regex
                  dplyr::matches(variable, perl = TRUE),
                  "observation_period")

  output

}
