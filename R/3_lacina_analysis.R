library(countrycode)
library(ggplot2)

# Was ist die Forschungsfrage?

# Was ist die abhängige Variable?

# Wie wird die abhängige Variable operationalisiert?

# Was sind wichtige unabhängige Variablen?



# Daten unterschiedlicher Quellen zusammenführen
ucdp <- read.csv("./data/lacina/ucdp_data.csv", stringsAsFactors = FALSE)
population <- read.csv("./data/lacina/population.csv", stringsAsFactors = FALSE)
gdp <- read.csv("./data/lacina/gdp.csv", stringsAsFactors = FALSE)
controls <- read.csv("./data/lacina/controls.csv", stringsAsFactors = FALSE)


# zum Zusammenführen muss eine eindeutige Zuordnung möglich sein: gemeinsame Variable

# population
library(countrycode)
population$ccode <- countrycode(sourcevar = population$cname, origin = "cowc",
                                destination = "cown")

table(is.na(population$ccode))

population$cname <- sub("RUM", "ROM", population$cname)

population$ccode <- countrycode(sourcevar = population$cname, origin = "cowc",
                                destination = "cown")

population$cname <- toupper(population$cname)

population$ccode <- countrycode(sourcevar = population$cname, 
                                origin = "cowc",
                                destination = "cown")


data_test <- merge(ucdp, population, by = "ccode")

data <- merge(ucdp, population, 
              by.x = c("ccode", "begin"), 
              by.y = c("ccode", "year"))

## alternative:
library(dplyr)
data_dplyr <- left_join(ucdp, population, 
                        by = c("ccode" = "ccode", "begin" = "year"))


