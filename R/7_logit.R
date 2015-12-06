# Kompletter Datensatz
lacina <- read.csv("./data/lacina/lacina.csv", stringsAsFactors = FALSE)

# neue abhängige Variable: 
# Ist die Konfliktintensität (gemessen an der Zahl der Toten) über oder unter dem Median?
med_lnbdb <- median(lacina$lnbdb)

lacina$intense <- lacina$lnbdb > med_lnbdb

model1_logit <- glm(intense ~ lnduration + lnpop + lnmilqual + lngdp + cw + lnmountain + 
                    democ + ethnicpolar + relpolar, data = lacina, family = "binomial")

summary(model1_logit)

exp(coef(model1_logit))
