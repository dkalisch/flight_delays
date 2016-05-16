# Load needded libraries
library(DBI) # For connecting to the DB
library(RPostgreSQL) # Driver for PostgreSQL

# Set the driver
drv <- dbDriver("PostgreSQL")

# Read the database settings
source('src/dbSettings.R')

# Open connection to database
con <- dbConnect(drv,
                 host = host,
                 port = port, 
                 dbname = dbname,
                 user = user,
                 password = pwd)

