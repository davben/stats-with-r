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
population$ccode <- countrycode(sourcevar = population$cname, origin = "cowc",
                                destination = "cown")


data_test <- merge(ucdp, population, by = "ccode")

data <- merge(ucdp, population[, c("year", "ccode", "cname", "pop")], 
              by.x = c("ccode", "begin"), by.y = c("ccode", "year"))

## alternative:
library(dplyr)
data_dplyr <- left_join(ucdp, population, by = c("ccode" = "ccode", "begin" = "year"))

# gdp
library(tidyr)
gdp_long <- gather(gdp, cname, gdp, -year)
gdp_long <- na.omit(gdp_long)

gdp_long$ccode <- countrycode(sourcevar = gdp_long$cname, origin = "cowc",
                                destination = "cown")
table(is.na(gdp_long$ccode))
gdp_long$cname <- sub("RUM", "ROM", gdp_long$cname)
gdp_long$ccode <- countrycode(sourcevar = gdp_long$cname, origin = "cowc",
                                destination = "cown")

data <- merge(data, gdp_long[, c("year", "ccode", "gdp")], by.x = c("ccode", "begin"), 
               by.y = c("ccode", "year"), all.x = TRUE)

## mit dplyr:
data_dplyr <- left_join(data_dplyr, gdp_long[, c("year", "ccode", "gdp")], 
                        by = c("ccode" = "ccode", "begin" = "year"))

# controls
data <- merge(data, controls, by = c("ccode", "begin"))

## mit dplyr:
data_dplyr <- left_join(data_dplyr, controls, by = c("ccode", "begin"))

# oder:
data_dplyr2 <- ucdp %>%
  left_join(population, by = c("ccode" = "ccode", "begin" = "year")) %>%
  left_join(gdp_long[, c("year", "ccode", "gdp")], 
            by = c("ccode" = "ccode", "begin" = "year")) %>%
  left_join(controls, by = c("ccode", "begin"))

all.equal(data_dplyr, data_dplyr2)


# Variablen, die generiert werden müssen:
## Konfliktdauer

## Tote pro Jahr (Rate)

## logarithmen

## Cold War Dummy




# Replikation
## Fig. 1
hist(lacina$battledeadbest, breaks = 200)

ggplot(data=lacina, aes(battledeadbest)) +
  geom_histogram(binwidth = 10000)

# Summary Table

median(lacina[lacina$cw==0,]$battledeadbest, na.rm = FALSE)


# regression

model1 <- lm(lnbdb ~ lnduration + lnpop + lnmilqual + lngdp + cw + lnmountain + 
               democ + ethnicpolar + relpolar,
             data = lacina)

summary(model1)   


