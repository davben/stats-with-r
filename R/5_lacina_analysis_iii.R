# Kompletter Datensatz
lacina <- read.csv("./data/lacina/lacina.csv", stringsAsFactors = FALSE)

# notwendige Pakete
library(ggplot2)


# Begutachtung der Daten --------------------------------------------------

## Fig. 1
hist(lacina$battledeadbest, breaks = 200, main = "", xlab = "Battle Deaths (best estimate)")

ggplot(data=lacina, aes(battledeadbest)) +
  geom_histogram(binwidth = 10000, fill="white", colour="black") +
  scale_x_continuous(name = "Battle Deaths (best estimate)") +
  scale_y_continuous(limits = c(0, 60)) +
  theme_bw()



## Summary Table

median(lacina[lacina$cw==0,]$battledeadbest, na.rm = FALSE)



# Mittelwert und Varianz --------------------------------------------------
lacina <- read.csv("./data/lacina/lacina.csv", stringsAsFactors = FALSE)
ggplot(lacina, aes(lnduration, lnbdb)) +
  geom_point()

## Mittelwert
ggplot(lacina, aes(lnduration, lnbdb)) +
  geom_point() +
  geom_hline(yintercept = mean(lacina$lnbdb, na.rm = TRUE), colour = "red")

### Passt dieses Modell gut zur Realität? 
### -> Sum of errors -> sum of squared errors -> variance (Stichprobenvarianz & Varianz)

mean(lacina$lnbdb, na.rm = TRUE)

lacina$mean_lnbdb <- mean(lacina$lnbdb, na.rm = TRUE)

lacina$error <- lacina$lnbdb - lacina$mean_lnbdb

sum(lacina$error)

lacina$squared_error <- (lacina$lnbdb - lacina$mean_lnbdb)^2

sum(lacina$squared_error) # Wächst mit der Anzahl der Beobachtungen!

### Varianz
### Normierung auf Anzahl der Beobachtungen N
n <- nrow(lacina)
sum(lacina$sum_squared_error) / (n - 1)

### schnellere Berechnung der Varianz:
var(lacina$lnbdb, na.rm = TRUE)



# Einfaches lineares Modell -----------------------------------------------

lacina_one <- lm(lnbdb ~ lnduration, data = lacina)
summary(lacina_one)

lacina$fit <- fitted(lacina_one)   # 8.079 + 0.8242 * lacina$lnduration
lacina$residual <- resid(lacina_one) # lacina$lnduration - lacina$fit

## Wieviel der Variabilität wird erklärt?
sum((fitted(lacina_one) - mean(lacina$lnbdb))^2) / sum((lacina$lnbdb - mean(lacina$lnbdb))^2)

summary(lacina_one)$r.squared

# Replikation von Lacina ------------------------------------

## Regression

model1 <- lm(lnbdb ~ lnduration + lnpop + lnmilqual + lngdp + cw + lnmountain + 
               democ + ethnicpolar + relpolar,
             data = lacina)

summary(model1)   


