library(tidyr)
library(countrycode)
library(dplyr)

# Daten unterschiedlicher Quellen zusammenführen
ucdp <- read.csv("./data/lacina/ucdp_data.csv", stringsAsFactors = FALSE)
population <- read.csv("./data/lacina/population.csv", stringsAsFactors = FALSE)
gdp <- read.csv("./data/lacina/gdp.csv", stringsAsFactors = FALSE)
controls <- read.csv("./data/lacina/controls.csv", stringsAsFactors = FALSE)

# gdp
gdp_long <- gather(gdp, cname, gdp, -year) # mit tidyr

gdp_long_melt <- melt(gdp, "year", variable.name = "cname", value.name = "gdp") # mit reshape2

all.equal(gdp_long, gdp_long_melt) # Vergleich beider Methoden


## Reduktion auf vorliegende BIP-Werte (ohne NAs)
gdp_long <- na.omit(gdp_long)

gdp_long$ccode <- countrycode(sourcevar = gdp_long$cname, origin = "cowc",
                              destination = "cown")
table(is.na(gdp_long$ccode))

gdp_long[is.na(gdp_long$ccode), ]

gdp_long$cname <- sub("RUM", "ROM", gdp_long$cname)
gdp_long$ccode <- countrycode(sourcevar = gdp_long$cname, origin = "cowc",
                              destination = "cown")

data <- merge(data, gdp_long[, c("year", "ccode", "gdp")], by.x = c("ccode", "begin"), 
              by.y = c("ccode", "year"), all.x = TRUE) # all.x hier notwendig!

## mit dplyr:
data_dplyr <- left_join(data_dplyr, gdp_long[, c("year", "ccode", "gdp")], 
                        by = c("ccode" = "ccode", "begin" = "year"))


# controls
lacina <- merge(data, controls, by = c("ccode", "begin"))

## mit dplyr:
lacina_dplyr <- left_join(data_dplyr, controls, by = c("ccode", "begin"))

# oder:
lacina_dplyr2 <- ucdp %>%
  left_join(population, by = c("ccode" = "ccode", "begin" = "year")) %>%
  left_join(gdp_long[, c("year", "ccode", "gdp")], 
            by = c("ccode" = "ccode", "begin" = "year")) %>%
  left_join(controls, by = c("ccode", "begin"))

all.equal(lacina_dplyr, lacina_dplyr2)


# Übung -------------------------------------------------------------------

## Lesen Sie die beiden csv-Dateien im Verzeichnis "data/exercise/" ein.

## Welche Daten scheinen die beiden Tabellen zu enthalten?

## Welche Struktur haben die Daten? Wie viele Beobachtungen gibt es? 

## Welche Variablen sind enthalten? Gibt es gemeinsame Variable?

## Führen Sie beide Tabellen zusammen.


# Replikation von Lacina (fortgesetzt) ------------------------------------

# Variablen, die generiert werden müssen:
## Konfliktdauer
lacina$duration <- lacina$end - lacina$begin + 1

## Tote pro Jahr (Rate)
lacina$rate <- lacina$battledeadbest / lacina$duration
## logarithmen
lacina$ln_duration <- log(lacina$duration)
lacina$ln_pop <- log(lacina$pop)
lacina$ln_gdp <- log(lacina$gdp)
lacina$ln_milqual <- log(lacina$milqual)
## Cold War Dummy
lacina$cw <- lacina$begin < 1989




# Replikation
library(ggplot2)
## Fig. 1
hist(lacina$battledeadbest, breaks = 200, main = "", xlab = "Battle Deaths (best estimate)")

ggplot(data=lacina, aes(battledeadbest)) +
  geom_histogram(binwidth = 10000)

# Summary Table

median(lacina[lacina$cw==0,]$battledeadbest, na.rm = FALSE)


# regression

model1 <- lm(lnbdb ~ lnduration + lnpop + lnmilqual + lngdp + cw + lnmountain + 
               democ + ethnicpolar + relpolar,
             data = lacina)

summary(model1)   


