tbl(flights.db, sql('SELECT "Carrier", avg("DepDelayMinutes")
                                 FROM flights
                                 WHERE "Year" = 2016 AND "Month" = 2 AND "DepDelayMinutes" > 0
                                 GROUP BY "Carrier"'))

flights %>%
  filter(Year == 2016L && Month == 2L && DepDelayMinutes > 0L) %>%
  group_by(Carrier) %>%
  summarise(mu = mean(DepDelayMinutes)) %>%
  collect()

sql.statement <- flights %>%
  filter(Year == 2016L && Month == 2L && DepDelayMinutes > 0L) %>%
  group_by(Carrier) %>%
  summarise(mu = mean(DepDelayMinutes),
            sd = sd(DepDelayMinutes))

sql.statement$query
