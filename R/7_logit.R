# Logit Einführung und Demonstration ------------------------
logit_data <- read.csv("./data/logit_example.csv")

head(logit_data)

summary(logit_data)

logit_data$rank <- factor(logit_data$rank)
logit_model <- glm(admit ~ gre + gpa + rank, data = logit_data, family = "binomial")

summary(logit_model)

exp(coef(logit_model))



# Anwendung auf Lacina ----------------------------------------------------
# Datensatz
lacina <- read.csv("./data/lacina/lacina.csv", stringsAsFactors = FALSE)

# neue abhängige Variable: 
# Ist die Konfliktintensität (gemessen an der Zahl der Toten) über oder unter dem Median?
med_lnbdb <- median(lacina$lnbdb)

lacina$intense <- lacina$lnbdb > med_lnbdb

model1_logit <- glm(intense ~ lnduration + lnpop + lnmilqual + lngdp + cw + lnmountain + 
                    democ + ethnicpolar + relpolar, data = lacina, family = "binomial")

summary(model1_logit)

exp(coef(model1_logit))
