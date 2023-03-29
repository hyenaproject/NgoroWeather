## If we need to remove any objects from global
withr::defer(rm(list = ".database", envir = .GlobalEnv), teardown_env())

load_package_database.weather(system.file("extdata/working_weather", package = "NgoroWeather"),
                              overwrite.db = "yes")
