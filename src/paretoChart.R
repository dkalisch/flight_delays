# Load required libraries
library(reshape2) # For melt transformation
library(grid) # For adjusting plots on a grid

# Ensure you got the data from the db
if (!exists("delaysData")) {
  source("src/delaysDescriptive.R")
}

# Get data that we need to analyze delay reasons
delayReasons <- delaysData %>% # Use original data set from descriptive analysis
  select(Carrier, ArrDelayMinutes,
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>% # Select needed variables
  filter(ArrDelayMinutes > 10) %>% # Filter out only these with more then 10 min delay
  group_by(Carrier,add = TRUE) %>% # Group the data by Carrier
  summarise_each(funs(sum(. != 0, na.rm = TRUE))) %>% # Count for each carrier the occurence of delays for each group
  melt(id = "Carrier") # Transform the data into long format

# Get every carrier that is in our data set
carriers <- unique(delayReasons$Carrier)

# Create for each carrier a pareto chart of delays
for (i in 1:length(carriers)){
  # Get only the entries for the current carrier
  df <- delayReasons %>%
    filter(Carrier == carriers[i] & variable != "ArrDelayMinutes") %>% # Get only varibales of interest
    arrange(desc(value)) # Arrange them into descending order
  
  # Transform variable (reasons) into factors
  df$variable <- factor(df$variable, levels = df$variable[order(df$value, decreasing = TRUE)])
  
  # Set up needed statistics
  df$pct <- df$value/sum(df$value) # Get percentage for each value
  df$cum <- cumsum(df$value) # Get cumulative values for amount of delays
  df$cumPct <- df$cum/sum(df$value) # Get the corresponding percentatge for the cumulative values
  
  # Plot results
  ## Create cummulative line graph
  p1 <- ggplot(df, aes(x = variable, y = cumPct, group = 1)) +
    geom_point(aes(color = variable), size = 4) + # Add points
    geom_path() + # Add line
    ggtitle(sprintf("Reasons for Delay of %s", carriers[i])) + # Add dynamic title
    scale_y_continuous(labels = scales::percent) + # Set axis to percent
    ylab("Cumulative Percentage") + # Add y-axis title
    theme(axis.ticks.x = element_blank(), # Tidy up graph
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position="none")
  
  ## Create bar plot for reasons of delay
  p2 <- ggplot(df, aes(x = variable, y = pct, colour = variable, fill = variable)) +
    geom_bar(stat = "identity") + # Add bars
    scale_y_continuous(labels = scales::percent) + # Set axis to percent
    ylab("Percentage") + # Add y-axis title
    xlab("Reasons") + # Add x-axis title
    theme(legend.position="none") # Tidy up graph
  
  ## Save polt
  pdf(file = sprintf("img/paretoCharts/%s.pdf", carriers[i])) # Open output device (pdf)
  grid.newpage() # Start a new grid
  pushViewport(viewport(layout = grid.layout(2, 1))) # Setup the grid layout
  print(p1, vp = viewport(layout.pos.row = 1,layout.pos.col = 1)) # Add first graph to page (line graph)
  print(p2, vp = viewport(layout.pos.row = 2,layout.pos.col = 1)) # Add second grpah to page (barplot)
  dev.off() # Close output device
}
