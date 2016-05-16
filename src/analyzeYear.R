# Load required libraries
library(plyr) # For spliting, applying and combining data
library(tidyr) # For cleaning and structuring data
library(lubridate) # For data manipulation
library(ggplot2) # For plotting
library(dplyr) # For data manipulation

# Load script to get data
if (!exists("flights")) {
  source("src/getData.R")
}

# Get data from the database
delaysData.year <- flights %>%
  select(DayOfWeek, FlightDate, Carrier, OriginCityMarketID, Origin,
         CRSDepTime, DepDelay, ArrDelay) %>% # Select variables of interest
  filter(Year == 2015) %>% # Get only data from 2015
  collect() %>% # Collect the data from db
  mutate(
    DayOfWeek = factor(DayOfWeek, levels = c(1:7,9), # Factorise day of the week
                       labels = 
                         c("Monday",
                           "Tuesday",
                           "Wednesday",
                           "Thursday",
                           "Friday",
                           "Saturday",
                           "Sunday",
                           "Unknown"
                         )),
    Month = substr(FlightDate, 6, 7), # Get month from FlightDate
    DateNum = substr(FlightDate, 9, 10), # Get day number from FlightDate
    CRSDepTime = round(as.numeric(CRSDepTime) / 100, 0), # Get the hour of the departure
    DepDelay = ifelse(DepDelay < 0, 0, DepDelay), # Remove early departures (0)
    ArrDelay = ifelse(ArrDelay < 0, 0, ArrDelay)) %>% # Remove early arrivals (0)
  filter(CRSDepTime > 5 & CRSDepTime < 24) # Get only data between 5am and 11:59pm (24h time)


# Analyze arrival and departure delays as function of departure time
## Prepare data for plotting
plotData.depTime <- delaysData.year %>%
  gather(DelayType, NewDelay, DepDelay:ArrDelay) %>% # Restructure data to long format
  mutate(DelayType = ifelse(DelayType == "DepDelay", "Departure Delay", "Arrival Delay")) %>% # Rename values in DepDelay
  group_by(CRSDepTime, DelayType) %>% # Group data by hour and delay type
  dplyr::summarise(mu = mean(as.numeric(NewDelay), na.rm=TRUE), # Get averages and stdandard error
                   se = sqrt(var(as.numeric(NewDelay), na.rm=TRUE) / length(na.omit(as.numeric(NewDelay)))),
                   obs = length(na.omit(as.numeric(NewDelay))))

## Create plot
p <- ggplot(plotData.depTime, 
            aes(x = CRSDepTime, y = mu, min = mu-se, max = mu+se,
                group = DelayType, color = DelayType)) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = .33) +
  scale_x_continuous(breaks = seq(6,23)) +
  labs(x = "Hour of Day",
       y = "Average Delay (Minutes)",
       title = "Flight Delays by Departure Time") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Delay Type")

## Save plot
ggsave(p, file = "img/year/Flight_Delays_By_Hour_DelayType.pdf",dpi = 300)


# Analyze every day of the year
## Prepare data for plotting
plotData.days <- delaysData.year %>% 
  group_by(Month, DateNum) %>% # Group data by month and day of month
  dplyr::summarise(mu = median(as.numeric(DepDelay), na.rm=TRUE), # Get averages and stdandard error
                   se = sqrt(var(as.numeric(DepDelay), na.rm=TRUE) / length(na.omit(as.numeric(DepDelay)))),
                   obs = length(na.omit(as.numeric(DepDelay))))

## Create plot
p <- ggplot(plotData.days, aes(x = DateNum, y = mu, min = mu-se, max = mu+se, group = Month)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = c(0,10)) +
  coord_cartesian(ylim = c(-4,16)) +
  labs(x = "Day of month",
       y = "Median Departure Delay (Minutes)",
       title = "Median Flight Delays by Departure Date") +
  theme(legend.position = "bottom") +
  facet_grid(Month ~.) +
  theme_bw()

## Save plot
ggsave(p, file = "img/year/Flight_Delays_By_Departure_Date.pdf", dpi = 300)


# Analyze the 10 busiest airports
# Source: http://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_United_States
# Section: Busiest_US_airports_by_total_passenger_boardings

## Prepare data for plotting
plotData.airports <- delaysData.year %>%
  filter(Origin %in% c( # Get only data for the 10 busiest airports
    "ATL",
    "LAX",
    "ORD",
    "DFW",
    "JFK",
    "DEN",
    "SFO",
    "CLT",
    "LAS",
    "PHX"
  ))  %>%
  group_by(CRSDepTime, Origin) %>% # Group by hour and origin
  dplyr::summarise(mu = mean(as.numeric(DepDelay), na.rm = TRUE), # Get averages and stdandard error
                   se = sqrt(var(as.numeric(DepDelay), na.rm = TRUE) / length(na.omit(as.numeric(DepDelay)))),
                   obs = length(na.omit(as.numeric(DepDelay)))) %>%
  mutate(mu = ifelse((mu - 0 < .001), NA, mu), # Correct negative values to NA
         Origin = factor(Origin, levels=c( # Factorize Origin
           "ATL",
           "LAX",
           "ORD",
           "DFW",
           "JFK",
           "DEN",
           "SFO",
           "CLT",
           "LAS",
           "PHX")))

## Create plot
### Top 5 airports
p <- ggplot(subset(plotData.airports, as.numeric(Origin) <= 5),
            aes(x = CRSDepTime, y = mu, min = mu-se, max = mu+se,
                group = Origin, color = Origin, shape = Origin)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(5,23)) +
  labs(x = "Hour of Day",
       y = "Average Departure Delay (Minutes)",
       title = "Top Five Most Popular Airports") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Airport") + 
  scale_shape_discrete(name = "Airport")

## Save plot
ggsave(p, file = "img/year/Flight_Delays_By_Hour_Airport_Top5.pdf",dpi = 300)

### Top 6-10 airports
p <- ggplot(subset(plotData.airports, as.numeric(Origin) >5),
            aes(x = CRSDepTime, y = mu, min = mu-se, max = mu+se,
                group = Origin, color = Origin, shape = Origin)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(5,23)) +
  labs(x="Hour of Day",y="Average Departure Delay (Minutes)",title="Airports Six through Ten") +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Airport") + 
  scale_shape_discrete(name = "Airport")

## Save plot
ggsave(p,file = "img/year/Flight_Delays_By_Hour_Airport_6to10.pdf", dpi = 300)


# Analyze 95% and 75% quantiles
## Prepare data for plotting
plotData.quantile = delaysData.year %>%
  group_by(CRSDepTime) %>% # Group data by hour
  dplyr::summarise(Quantile_95 = quantile(as.numeric(DepDelay), .95, na.rm = TRUE), # Calculate quantiles
                   Quantile_75 = quantile(as.numeric(DepDelay), .75, na.rm = TRUE),
                   obs = length(na.omit(as.numeric(DepDelay)))) %>% 
  gather(variable, value, Quantile_75:Quantile_95) %>% # Restructure data to long format
  mutate(variable = factor(variable, levels = c("Quantile_95", "Quantile_75"))) # Factorize variable

## Create plot
p <- ggplot(plotData.quantile, aes(x = CRSDepTime, y = value,
                                group = variable, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(5,23)) +
  labs(x = "Hour of Day",
       y = "Departure Delay (Minutes)",
       title = "95th and 75th Percentiles of Departure Delays") +
  scale_color_discrete(name = "Quantile") +
  theme(legend.position = "bottom") 

## Save plot
ggsave(p, file = "img/year/Flight_Delays_By_Hour_95th.pdf", dpi = 300)
