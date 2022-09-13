install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

multivariate <- read.csv("multivariate.csv")
attach(multivariate)
head(multivariate)
mm <- lm(Homeowners~Immigrants)
mm

summary(mm)$coef

plot(Homeowners~Immigrants)
abline(mm)

newImmigrantData <- data.frame(Immigrants = c(0,20))
mm %>% predict(newImmigrantData)

abline(mm)
abline(mm, col=3, lwd=3)
attributes(mm)

