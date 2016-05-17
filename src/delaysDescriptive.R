# Load required libraries
library(plotrix) # For calculating the standard error
library(moments) # For calculationg skewness and kurtosis
library(ggplot2) # For nice plotting
library(scales) # For scaling plots

# Load script to get data
if (!exists("flights")) {
  source("src/getData.R")
}

# Descriptive analysis
## Get data as defined by times
delaysData <- getDelays(times = 'Year == 2016L & Month == 2L')
delaysData$DepartureTime <- as.POSIXct(paste(delaysData$FlightDate, delaysData$DepTime), format="%Y-%m-%d %H%M")
delaysData <- delaysData[!is.na(delaysData$ArrDelayMinutes),]

## Calculate descriptive values
delays.desc <- delaysData %>%
  ### Group data by carriers
  group_by(Carrier) %>%
  ### Summarize the following indicators by carrier
  summarise(
    count = n(), # Count
    sum = sum(ArrDelayMinutes, na.rm = TRUE), # Overall sum
    min = min(ArrDelayMinutes, na.rm = TRUE), # Minimum delay
    max = max(ArrDelayMinutes, na.rm = TRUE), # Maximum delay
    mean = mean(ArrDelayMinutes, na.rm = TRUE), # Average delay
    median = median(ArrDelayMinutes, na.rm = TRUE), # Median delay
    range = max - min, # Range of delay
    q1 = as.numeric(quantile(ArrDelayMinutes, na.rm = TRUE)[2]), # First quartile of delays
    q3 = as.numeric(quantile(ArrDelayMinutes, na.rm = TRUE)[4]), # Third quartile of delays
    iqr = q3 - q1, # Interquartile range
    wmin = if((q1 - 1.5*iqr) < min){min} else {q1 - 1.5*iqr}, # Lower outlier value of delays
    wmax = if((q3 + 1.5*iqr) > max){max} else {q3 + 1.5*iqr}, # Upper outlier value of delays
    sd = sd(ArrDelayMinutes, na.rm = TRUE), # Standard deviation of delays
    var = var(ArrDelayMinutes, na.rm = TRUE), # Variance of delays
    se = std.error(ArrDelayMinutes, na.rm = TRUE), # Standard error of delays
    kurt = kurtosis(ArrDelayMinutes, na.rm = TRUE), # Kurtosis of delays
    skew = skewness(ArrDelayMinutes, na.rm = TRUE), # Skewness of delays
    delay_pct = sum(ArrDelayMinutes, na.rm = TRUE) / count # Percentage of delays
  )

# Plot results
## Box-Whisker plot of delays by carrier
### Create plot
p <- ggplot(delays.desc, aes(x = as.factor(Carrier))) +
  #### Add whiskers
  geom_errorbar(aes(ymin = wmin,
                    ymax = wmax)) +
  #### Add box
  geom_boxplot(aes(lower = q1,
                   upper = q3,
                   middle = median,
                   ymin = wmin,
                   ymax = wmax), stat = "identity") +
  #### Change axis titles
  ylab("Count") + 
  xlab("Carrier")

### Save plot
ggsave(p, filename = "img/boxWhiskerDelays.pdf", dpi = 300)

## Barplot of delays in minutes by carrier
### Create plot
p <- ggplot(delays.desc, aes(x = as.factor(Carrier), y = 1 - (delay_pct/100))) +
  #### Add bars
  geom_bar(stat = "identity") + 
  #### Change axis numbers to percent
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  #### Change axis titles
  ylab("Percent of flights On-Time") + 
  xlab("Carrier")

### Save plot
ggsave(p, filename = "img/onTime.pdf", dpi = 300)

