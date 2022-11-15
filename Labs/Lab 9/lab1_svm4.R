library("kernlab")

## example using the promotergene data set
data(promotergene)
promotergene <-  read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/molecular-biology/promoter-gene-sequences/promoters.data", col.names = c("Class","Gene","Sequence"))

## create test and training set
ind <- sample(1:dim(promotergene)[1],20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]

## train a support vector machine
gene <-  ksvm(as.factor(Class)~.,data=genetrain,kernel="rbfdot",kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)

## predict gene type probabilities on the test set
genetype <- predict(gene,genetest,type="probabilities")
