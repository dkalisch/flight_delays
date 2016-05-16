# Load required libraries
library(dplyr) # For data manipulation
library(lazyeval) # For function interp

# Connect to the database
source("src/settings/dbSettings.R")

flights.db <- src_postgres(dbname = dbname,
                           host = host, port = port,
                           user = user, password = pwd)
flights <- tbl(flights.db, "flights")

getDelays <- function(times){
  flights.delays <- flights %>%
    filter_(interp(times)) %>%
    select(FlightDate, DepTime, Carrier, Origin, Dest, ArrDelayMinutes,
           CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>%
    collect()
  return(flights.delays)
}

