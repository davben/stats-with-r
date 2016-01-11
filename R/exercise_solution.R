#################################################
# Bitte eintragen:
#
# Name: Vorname Nachname
# Matrikelnummer: 1234567890
#
#################################################


# Aufgabe 1 ---------------------------------------------------------------


# 1a ----------------------------------------------------------------------
# "This paper examines how strategic objectives and geographical factors affect 
# the location, relative to the capitalm and scope (measured conflict area) of 
# armed civil conflict." (418)


# 1b ----------------------------------------------------------------------
# (1) location
# (2) scope


# 1c ----------------------------------------------------------------------
# (424)
# location: distance of conflict center point to capital city (log-transformed)
# scope:
#   - absolute: area of conflict zone (defined to be circular with conflict center 
#                                      and radius equal to distance from center to 
#                                      most distant battle zone, rounded to 50km,
#                                      log-transformed)
#   - relative: conflict area divided by total land area


# 1d ----------------------------------------------------------------------
# - identity (dummy if rebels originate from different ethnic/religious groups than government)
# - incompatibility (PRIO: territory or governance as source of conflict)
# - duration (conflict duration)
# - border (dummy, whether or not conflict is next to a border)
# - resource (dummy, whether or not conflict zone contains essential natural resources)
# - land area
# - forest (proportion of land area covered by forest)
# - mountain (proportion of mountainour terrain)

# 1e ----------------------------------------------------------------------
# "The unit of analysis in our study is armed civil conflicts, as defined by..." (423)

# 1f ----------------------------------------------------------------------
# conflict severity <-> geography


# Aufgabe 2 ---------------------------------------------------------------
buhaug <- read.csv("./data/aufgabenblatt/buhaug_gates.csv", stringsAsFactors = FALSE)

# 2a ----------------------------------------------------------------------
dim(buhaug)

# 2b ----------------------------------------------------------------------
summary(buhaug$begin)
min(buhaug$begin)
max(buhaug$begin)
range(buhaug$begin)


# 2c ----------------------------------------------------------------------
plot(buhaug$begin, buhaug$location)
plot(buhaug$begin, buhaug$ln_abs_scope)
plot(buhaug$begin, buhaug$rel_scope)

library(ggplot2)
ggplot(buhaug, aes(begin, location)) +
  geom_point()

library(dplyr)
library(tidyr)
dep_variables <- buhaug %>%
  select(begin, location, ln_abs_scope, rel_scope) %>%
  gather(variable, value, -begin)

ggplot(dep_variables, aes(begin, value, colour = variable)) +
  geom_point(alpha=0.3, position = "jitter")


# 2d ----------------------------------------------------------------------
hist(buhaug$location)
hist(buhaug$ln_abs_scope)
hist(buhaug$rel_scope)

ggplot(dep_variables, aes(value, fill = variable)) +
  geom_histogram(alpha=0.5, position = "dodge")

ggplot(dep_variables, aes(value, fill = variable)) +
  geom_density(alpha=0.5, position = "dodge")

# Aufgabe 3 ---------------------------------------------------------------

# 3a ----------------------------------------------------------------------
descriptive_stats_df <- buhaug[, c("location", "ln_abs_scope", "rel_scope", 
                                   "ln_land_area", "identity", "incompatibility", 
                                   "duration",  "border", "resource", "mountain", 
                                   "forest")]

descriptive_stats_df <- select(buhaug, location, ln_abs_scope, rel_scope, 
                               ln_land_area, identity, incompatibility, 
                               duration,  border, resource, mountain, 
                               forest)

# individually:
sum(!is.na(descriptive_stats_df$location))
mean(descriptive_stats_df$location, na.rm = TRUE)
sd(descriptive_stats_df$location, na.rm = TRUE)
min(descriptive_stats_df$location, na.rm = TRUE)
max(descriptive_stats_df$location, na.rm = TRUE)

# for all:
Observations <- sapply(descriptive_stats_df, function(x){
  sum(!is.na(x))
})

Mean <- sapply(descriptive_stats_df, function(x){
  mean(x, na.rm = TRUE)
})

SD <- sapply(descriptive_stats_df, function(x){
  sd(x, na.rm = TRUE)
})

Min <- sapply(descriptive_stats_df, function(x){
  min(x, na.rm = TRUE)
})

Max <- sapply(descriptive_stats_df, function(x){
  max(x, na.rm = TRUE)
})


# 3b ----------------------------------------------------------------------
table_1 <- data.frame(Observations, Mean, SD, Min, Max)
table_1 <- round(table_1, 2)

# Aufgabe 4 ---------------------------------------------------------------

# 4a ----------------------------------------------------------------------


# 4b ----------------------------------------------------------------------


# 4c ----------------------------------------------------------------------

cor_matrix_df <- buhaug[, c("location", "ln_abs_scope", "rel_scope", "ln_land_area", 
                            "identity", "incompatibility", "duration", "border", 
                            "resource", "mountain", "forest")]

cor_matrix_df <- select(buhaug, location, ln_abs_scope, rel_scope, 
                        ln_land_area, identity, incompatibility, 
                        duration,  border, resource, mountain, 
                        forest)

table_2 <- as.data.frame(cor(cor_matrix_df, use="complete.obs"))
table_2 <- round(table_2, 3)
table_2[upper.tri(table_2, diag = TRUE)] <- NA

# Aufgabe 5 ---------------------------------------------------------------

# location
model_1 <- lm(location ~ ln_abs_scope + ln_land_area + identity + incompatibility,
              data = buhaug)
summary(model_1)

model_2 <- lm(location ~ rel_scope + ln_land_area + identity + incompatibility,
              data = buhaug)
summary(model_2)

# absolute scope
model_3 <- lm(ln_abs_scope ~ location + ln_land_area + duration + border + resource,
              data = buhaug)
summary(model_3)

model_4 <- lm(ln_abs_scope ~ location + ln_land_area + duration + border + resource +
                mountain + forest,
              data = buhaug)
summary(model_4)

# relative scope
model_5 <- lm(rel_scope ~ location + ln_land_area + duration + border + resource,
              data = buhaug)
summary(model_5)

model_6 <- lm(rel_scope ~ location + ln_land_area + duration + border + resource +
                mountain + forest,
              data = buhaug)
summary(model_6)

# nice output
library(stargazer)
stargazer(list(model_1, model_2), type="html", out = "./table_3.html")
stargazer(list(model_3, model_4, model_5, model_6), type="html", out = "./table_4.html")



# Aufgabe 6 ---------------------------------------------------------------
# source: https://www.clio-infra.eu/datasets/search
# found via: ourworldindata.org/data/growth-and-distribution-of-prosperity/gdp-data/
library(countrycode)
gdp <- read.csv("./data/aufgabenblatt/clioinfra.csv", stringsAsFactors = FALSE)
buhaug <- gdp %>%
  filter(!is.na(ccode)) %>%
  select(ccode, country.name, starts_with("X")) %>%
  gather(year, gdp, -c(ccode, country.name)) %>%
  mutate(year = as.integer(gsub("X", "", year)),
         cow = countrycode(ccode, "iso3n", "cown")) %>%
  select(year, cow, gdp) %>%
  right_join(buhaug, c("cow" = "cow", "year" = "begin"))


# location
model_1b <- lm(location ~ ln_abs_scope + ln_land_area + identity + incompatibility + log(gdp),
               data = buhaug)
summary(model_1b)

model_2b <- lm(location ~ rel_scope + ln_land_area + identity + incompatibility + log(gdp),
               data = buhaug)
summary(model_2b)


# absolute scope
model_3b <- lm(ln_abs_scope ~ location + ln_land_area + duration + border + resource + log(gdp),
               data = buhaug)
summary(model_3b)

model_4b <- lm(ln_abs_scope ~ location + ln_land_area + duration + border + resource +
                 mountain + forest + log(gdp),
               data = buhaug)
summary(model_4b)

# relative scope
model_5b <- lm(rel_scope ~ location + ln_land_area + duration + border + resource + log(gdp),
               data = buhaug)
summary(model_5b)

model_6b <- lm(rel_scope ~ location + ln_land_area + duration + border + resource +
                 mountain + forest + log(gdp),
               data = buhaug)
summary(model_6b)

# nice output
stargazer(list(model_1b, model_2b), type="html", out = "./table_3b.html")
stargazer(list(model_3b, model_4b, model_5b, model_6b), 
          type="html", out = "./table_4b.html")
