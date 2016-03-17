# Einfaches lineares Modell -----------------------------------------------

lacina_one <- lm(lnbdb ~ lnduration, data = lacina)
summary(lacina_one)

plot(lacina_one) # Begutachtung der Modellqualität


lacina$fit <- fitted(lacina_one)   # 8.079 + 0.8242 * lacina$lnduration
lacina$residual <- resid(lacina_one) # lacina$lnduration - lacina$fit

## Wieviel der Variabilität wird erklärt? -> R^2

sum((fitted(lacina_one) - mean(lacina$lnbdb))^2) / sum((lacina$lnbdb - mean(lacina$lnbdb))^2)

summary(lacina_one)$r.squared



# Multivariates Modell (Replikation von Lacina) ---------------------------

model1 <- lm(lnbdb ~ lnduration + lnpop + lnmilqual + lngdp + cw + lnmountain + 
               democ + ethnicpolar + relpolar,
             data = lacina)

summary(model1) 

model2 <- lm(lnbdb ~ lnduration + cw + democ + ethnicpolar,
             data = lacina)

summary(model2)

# Deskriptive Statistik ---------------------------------------------------



table(lacina$intervention,lacina$cw)
