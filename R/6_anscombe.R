# devtools::install_github('stephenturner/Tmisc')
library(Tmisc)
data(quartet)
str(quartet)


library(dplyr)
quartet %>%
  group_by(set) %>%
  summarize(mean(x), sd(x), mean(y), sd(y), cor(x,y))

library(ggplot2)
p <- ggplot(quartet, aes(x, y)) + geom_point() + 
  geom_smooth(method = lm, se = FALSE) + 
  facet_wrap(~set)

print(p)

set1 <- lm(y ~ x, data = quartet[quartet$set=="I", ])
summary(set1)

set2 <- lm(y ~ x, data = quartet[quartet$set=="II", ])
summary(set2)

set3 <- lm(y ~ x, data = quartet[quartet$set=="III", ])
summary(set3)

set4 <- lm(y ~ x, data = quartet[quartet$set=="IV", ])
summary(set4)
