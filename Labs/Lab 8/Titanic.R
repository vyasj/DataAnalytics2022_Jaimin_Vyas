library(rpart)

data("Titanic")
head(Titanic)

rpart <- rpart(Survived ~., data=Titanic)
summary(rpart)
plotcp(rpart)
plot(rpart, compress=T)
text(rpart, use.n=T)

library(party)

ctree <- ctree(Survived ~., data=Titanic)
plot(ctree, type="simple")

cforest(Survived ~., data=Titanic, controls=cforest_control(mtry=2, mincriterion=0))

library(randomForest)
