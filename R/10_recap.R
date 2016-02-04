library(reshape2)
#library(dplyr)
library(tidyr)


df <- data.frame(country = c("A", "B", "C", "D"),
                 "2002" = rnorm(4, mean = 8, sd = 3),
                 "2003" = rnorm(4, mean = 9, sd = 3),
                 "2004" = rnorm(4, mean = 10, sd = 3),
                 "2005" = rnorm(4, mean = 5, sd = 3),
                 "2006" = rnorm(4, mean = 3, sd = 3),
                 "2007" = rnorm(4, mean = 7, sd = 3))

df_long1 <- melt(df, id.vars = "country", variable.name = "year",
                 value.name = "growth")

df_long1$year <- as.numeric(gsub("X", "", df_long1$year))


df_long2 <- gather(df, year, growth, -country)
df_long2$year <- as.numeric(gsub("X", "", df_long1$year))


#