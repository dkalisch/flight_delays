# Required libraries. Make sure they are available on your computer
library(data.table)
library(dplyr)

source("src/settings/connectDB.R") # Load connection to database
source("src/settings/dataStructureDB.R") # Load initial data structure

# Create initial data structure in the database
dbWriteTable(con, name = "flights", df, append = TRUE, row.names = FALSE)
rm(df)

# Get the list of available flights files
flights.files <-  list.files(path = "data/", pattern="*.zip")

# Load the flights into the database
for (i in 1:length(flights.files)){
  sprintf("Extracting file %s", flights.files[i])
  unzip(paste0("data/", flights.files[i]), exdir = "tmp/")
  
  print("Read data from extracted file...")
  file.name <- paste0("tmp/", gsub(".zip", ".csv", flights.files[i]))
  flights <- fread(file.name, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  #flights <- as.data.frame(flights[,1:109, with = FALSE])
  
  print("Write file to DB...")
  dbWriteTable(con, name = "flights", flights[,1:109, with = FALSE], append = TRUE, row.names = FALSE)
  
  print("Cleaning up...")
  file.remove(file.name, "tmp/readme.html")
}

print("Done!")