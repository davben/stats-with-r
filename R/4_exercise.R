library("nycflights13")
flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)
write.csv(flights2, file="./data/exercise/flights.csv", row.names = FALSE)
al <- airlines %>%
  rename(carr = carrier)
write.csv(al, file="./data/exercise/airlines.csv", row.names = FALSE)

# exercise
flights <- read.csv("./data/exercise/flights.csv", stringsAsFactors = FALSE)
airlines <- read.csv("./data/exercise/airlines.csv", stringsAsFactors = FALSE)

str(flights)
str(airlines)

names(airlines) <- c("carrier", "name")

flights_airlines <- left_join(flights, airlines, by = c("carrier" = "carr"))

# or

flights_airlines2 <- merge(x=flights, y=airlines, 
                           by.x = "carrier", by.y = "carr")
