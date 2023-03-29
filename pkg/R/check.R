#' Check weather station names are valid.
#'
#' @return A character vector of weather station names
#' @export
#'
#' @examples
#' #Will work
#' check_function_arg.station("mvua")
#'
#' #Won't work
#' #check_function_arg.station("not_a_station")
check_function_arg.station <- function(station){

  possible_stations <- unique(weather_station_activity$station_name)

  if (is.null(station)) {
    return(possible_stations)
  }

  if (!all(station %in% possible_stations)) {

    stop(paste0(setdiff(station, possible_stations), " is not a weather station name. Please use one of: ", paste(possible_stations, collapse = ", ")))

  } else {

    return(station)

  }

}


#' Check weather station locations are valid.
#'
#' @return A character vector of weather station locations
#' @export
#'
#' @examples
#' #Will work
#' check_function_arg.location.weather("jackal_hill")
#'
#' #Won't work
#' #check_function_arg.location.weather("not_a_location")
check_function_arg.location.weather <- function(location){

  ## FIXME: Repetitive other checks that just compare to a possible set of categories
  # Could create a single func `check_function_arg.category(value, variable_name)`
  possible_values <- unique(weather_station_activity$site_name)

  if (is.null(location)) {
    return(possible_values)
  }

  if (!all(location %in% possible_values)) {

    stop(paste0(setdiff(location, possible_values), " is not a weather station location. Please use one of: ", paste(possible_values, collapse = ", ")))

  } else {

    return(location)

  }

}

#' Check input variable exist in the weather dataset
#'
#' @return A character vector of column names
#' @export
#'
#' @examples
#' check_function_arg.variable.weather(c("temp", "rain"))
check_function_arg.variable.weather <- function(variable, .fill = TRUE){

  possible_variable_df <- dplyr::tibble(possible_variable = c("air_temp", "precip", "precip_max_hourly",
                                                           "relative_humidity", "atmospheric_pressure",
                                                           "battery_percent"),
                                     ## Need to convert precip to regex internally to distinguish between two precip metrics
                                     colname = c("air_temp", "^precip(?!_max_)", "precip_max_hourly", "relative_humidity", "atmospheric_pressure", "battery_percent"))

  #If no variable provided...
  if (is.null(variable)) {

    if (.fill) { #If we are filling (default) just return all colnames

      return(possible_variable_df$colname)

    } else {

      stop("Please provide atleast one variable.")

    }

  }

  if (!all(variable %in% possible_variable_df$possible_variable)) {

    stop(paste0(setdiff(variable, possible_variable_df$possible_variable), " is not a valid variable. Please use one of: ", possible_variable_df$possible_variables))

  } else {

    possible_variable_df |>
      dplyr::filter(.data$possible_variable %in% {{variable}}) |>
      dplyr::pull(.data$colname) -> output

    return(output)

  }

}

#' Check for meta-data issues in a single weather station file
#'
#' Checks include:
#' 1. Check that weather station is located within Ngorongoro Crater
#' 2. Check that there are no errors recorded in the meta-data
#'
#' @inheritParams arguments
#'
#' @return Data frame of meta-data
#' @export
check_weatherstation_metadata.file <- function(metadata, verbose = TRUE){

  #We will run all expected checks on weatherstation metadata

  #Create a column where we can store error information.
  metadata <- metadata |>
    dplyr::mutate(error = FALSE,
                  message = NA_character_)

  #1. Check that weather station is within the Ngorongoro Crater
  #FIXME: May need to expand the acceptable area once we have a station on the rim
  weatherstation_location <- metadata |>
    dplyr::filter(, .data$info %in% c("latitude", "longitude")) |>
    tidyr::pivot_wider(names_from = "info", values_from = "value") |>
    #For this quick check, we just work with geographic CRS
    hyenaR::recode_df_sf(crs = 'EPSG:32736')

  projected_rim <- hyenaR::sf_hyenaR$rim_polygon |>
    sf::st_transform(crs = 'EPSG:32736')

  location_check_pass <- sf::st_within(weatherstation_location, projected_rim, sparse = FALSE)[1, ]

  message <- "Weather station location is outside the crater!"

  if (!location_check_pass) {

    metadata$error[metadata$info %in% c("latitude", "longitude")] <- TRUE
    metadata$message[metadata$info %in% c("latitude", "longitude")] <- "Weather station location is outside the crater!"

    if (verbose) {

      warning(message)

    }

  }

  #2. Check there are no errors
  error_data <- metadata |>
    dplyr::filter(.data$metadata_category == "errors" & .data$info != "last_updated" & .data$value != 0)

  error_check_pass <- nrow(error_data) == 0

  message <- paste0("Weather station error in: ", crayon::yellow(error_data$info), "\n")

  if (!error_check_pass) {

    metadata$error[metadata$info %in% error_data$info] <- TRUE
    metadata$message[metadata$info %in% error_data$info] <- message

    if (verbose) {

      warning(message)

    }

  }

  metadata

}

#' Check for meta-data issues when combining weather data
#'
#' Checks include:
#' 1. Check data is from the same device (serial number)
#' 2. Check data is collected at same measurement interval
#' 3. Check data has been collected at the same locaation (lat/long)
#'
#' Check will pass if data locations are within 10m.
#' This allows for uncertainty in coordinate and the possibility
#' that weather stations will need to be moved slightly over time.
#'
#' @inheritParams arguments
#'
#' @return Data frame of meta-data.
#' @export
check_weatherstation_metadata.all <- function(metadata){

  #Combine meta-data so we can more easily run checks
  metadata <- dplyr::bind_rows(metadata) |>
    dplyr::select("file_number", dplyr::everything())

  #1. Check configuration info is comparable
  config_data <- metadata |>
    dplyr::filter(.data$metadata_category == "configuration") |>
    tidyr::unnest(cols = "data") |>
    tidyr::pivot_wider(names_from = "info", values_from = "value")

  #1a: Check that serial number is the same! Don't combine data from different stations
  serial_check_pass <- length(unique(config_data$serial_number)) == 1

  if (!serial_check_pass) {

    stop("Serial numbers of weather stations are different! Cannot combine data from different stations.")

  }

  #1b: Check that measurement intervals are the same. If not, throw warning (data are still usable)
  measurementint_check_pass <- length(unique(config_data$measurement_interval)) == 1

  if (!measurementint_check_pass) {

    warning("Data have been collected at different measurement intervals.")

  }

  #2. Check that locations are the same for each station
  location_distances <- metadata |>
    dplyr::filter(.data$metadata_category == "location") |>
    tidyr::unnest(cols = "data") |>
    dplyr::filter(.data$info %in% c("latitude", "longitude")) |>
    tidyr::pivot_wider(names_from = "info", values_from = "value") |>
    #Here we're working with distances so would be good to use projected CRS by default
    #(regardless of what the user chooses as default)
    hyenaR::recode_df_sf(crs = 'EPSG:32736') |>
    sf::st_distance() |>
    as.numeric()

  location_check_pass <- all(location_distances <= 10)

  #If files are more than 10m apart then we will not combine data and throw an error!
  if (!location_check_pass) {

    stop("Different weather station files are from different locations! Check file meta-data.")

  }

  metadata

}
