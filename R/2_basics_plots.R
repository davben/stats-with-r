## Wie lese ich Daten ein?

gdp <- read.csv("https://raw.githubusercontent.com/davben/stats-with-r/master/data/gdp_ppp.csv",
                stringsAsFactors = FALSE)
# online-Pfad: "https://raw.githubusercontent.com/davben/stats-with-r/master/data/gdp_ppp.csv"
save(gdp, file="./gdp_example.Rdata")
## Optimal: Rdata-files
load(file = "./data/gdp_ppp.Rdata")


## Auch möglich: xlsx-Dateien
install.packages("xlsx")
library(xlsx) # Durch das Laden eines Pakets erhalten wir zusätzliche Funktionen
gdp_xlsx <- read.xlsx(file = "./data/gdp_growth.xlsx", sheetIndex = 1) #, colIndex = 2:5)


# Vokabeln zum Begutachten von Daten:
head(gdp, 10)

tail(gdp)

str(gdp)

dim(gdp)

summary(gdp)

table(gdp$country)


## Erste Grafiken mit Base Plot

plot(x = gdp$year, y = gdp$gdp)

auswahl <- c("Belgium", "Germany", "France", "Denmark")

gdp_auswahl <- gdp[gdp$country %in% auswahl, ]

plot(x = gdp_auswahl$year, y = gdp_auswahl$gdp, type = "l")

# Deutschland, absolute Zahlen
load("./data/gdp_deu.Rdata")
plot(gdp_deu$year, gdp_deu$gdp)
lines(gdp_deu$year, gdp_deu$gdp)

plot(gdp_deu$year, gdp_deu$gdp, type = "l", lty = 2)

# weitere Argumente und Funktionen des base plot-Befehls unter
?plot

# Wachstumsraten
load("./data/gdp_growth.Rdata")
plot(gdp_growth$year, gdp_growth$growth, xlab = "Jahr", ylab = "Wachstumsrate")
# Verteilung
hist(gdp_growth$gdp, breaks=30) 
hist(gdp_growth$growth, breaks=30)


## Bessere Grafiken mit ggplot2
library(ggplot2)

ggplot(data = gdp_auswahl, aes(x = year, y = gdp, colour=country)) +
  geom_bar(stat="identity")

gdp_sample <- gdp[gdp$country %in% c("Denmark", "France", "Germany", "Netherlands"), ]

ggplot(data = gdp_sample, aes(x = year, y = gdp, colour = factor(country))) +
  geom_point() +
  geom_line() +
  scale_color_discrete(name = "Country")



## Exkurs Pakete
install.packages("ggplot2") # zur einmaligen Installation eines zusätzlichen Pakets
install.packages("xlsx")

library(ggplot2) # Lädt das Paket innerhalb einer Session, um die jeweiligen Funktionen bereitzustellen.

## Exkurs Subsetting
nationalspieler <- c("Manuel Neuer", "Philipp Lahm", "Jérome Boateng", "Mats Hummels", 
                     "Benedikt Höwedes", "Christoph Kramer", "Bastian Schweinsteiger", "Toni Kroos",
                     "Thomas Müller", "Mesut Özil", "Miroslav Klose")

nationalspieler_df <- data.frame(name = c("Manuel Neuer", "Philipp Lahm", "Jérome Boateng", 
                                          "Mats Hummels", "Benedikt Höwedes","Christoph Kramer", 
                                          "Bastian Schweinsteiger", "Toni Kroos",
                                          "Thomas Müller","Mesut Özil", "Miroslav Klose"),
                                 position = factor(c("Torwart", rep("Abwehr", 4), rep("def. Mittelfeld", 3),
                                                     rep("off. Mittelfeld", 2), "Sturm")),
                                 nummer = c(1, 16, 20, 5, 4, 23, 7, 18, 13, 8, 11), 
                                 stringsAsFactors = FALSE)

# Zeige nur die ersten drei Nationalspieler:
nationalspieler[1:3] # Die Schreibweise "1:3" ist eine Vereinfachung von "c(1, 2, 3)"; 
                     # weitere Möglichkeit: seq(1, 3, 1)

nationalspieler_df[1:3, ] # da dies ein data frame ist, haben wir zwei Dimensionen, in denen wir unterteilen können

nationalspieler_df[1:3, c(2,3)]

nationalspieler_df$name[c(2,4,7)]


## Statistische Modelle

ggplot(data = gdp_growth, aes(year, growth)) +
  geom_point() +
  xlab("Year") +
  ylab("Growth") +
  theme_bw()

### Mittelwert
ggplot(data = gdp_growth, aes(year, growth)) +
  geom_point() +
  geom_hline(yintercept = mean(gdp_growth$growth, na.rm = TRUE), colour = "red") +
  xlab("Year") +
  ylab("Growth") +
  theme_bw()
### Passt dieses Modell gut zur Realität? 
### -> Sum of errors -> sum of squared errors -> variance (Stichprobenvarianz & Varianz)

mean(gdp_growth$growth, na.rm = TRUE)

gdp_growth$mittelwert <- mean(gdp_growth$growth, na.rm = TRUE)

gdp_growth$sum_error <- gdp_growth$growth - gdp_growth$mean

sum(gdp_growth$sum_error)

gdp_growth$sum_squared_error <- (gdp_growth$growth - gdp_growth$mean)^2

sum(gdp_growth$sum_squared_error)

### Varianz
### Normierung auf Anzahl der Beobachtungen N
### Wie groß ist die Zahl der Beobachtungen?

sum(gdp_growth$sum_squared_error)

var(gdp_growth$growth, na.rm = TRUE)







ggplot(data = gdp_growth, aes(year, growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Year") +
  ylab("Growth") +
  theme_bw()

ggplot(data = gdp_growth, aes(year, growth)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Year") +
  ylab("Growth") +
  theme_bw()


