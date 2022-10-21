mtcars
dim(mtcars)
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~ cyl + wt, data=mtcars)
model1
help("cooks.distance")
plot(model1, pch=18, col="red", which = c(4))

cooks.distance(model1)
CooksDistance <- cooks.distance(model1)

round(CooksDistance, 5)
sort(round(CooksDistance, 5))

library(ISLR)
head(Hitters)
dim(Hitters)
is.na(Hitters)

HittersData <- na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)
head(HittersData)

SalaryPredictModel1 <- lm(Salary ~., data=HittersData)
summary(SalaryPredictModel1)

cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm=T)))]
influential

names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary ~., data=Hitters_Without_Outliers)
summary(SalaryPredictModel2)
