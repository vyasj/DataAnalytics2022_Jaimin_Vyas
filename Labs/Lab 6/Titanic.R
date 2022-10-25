library(rpart)

data("Titanic")
df <- data.frame(Titanic)

rpart <- rpart(Survived ~., data=df)
summary(rpart)
plotcp(rpart)
plot(rpart, compress=T)
text(rpart, use.n=T)

