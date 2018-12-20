require(farff)
require(data.table)
require(nnet)
require(glmnet)
require(e1071)
require(caret)
library(tree)
require(gbm)
require(xgboost)
require(e1071)
require(Ckmeans.1d.dp)
library(randomForest)
require(fastDummies)
require(dplyr)
require(glmnet)
require(gbm)
require(rpart)

### 1. Data is ok
### 2. all working models
### 3. Now, cross val

######### DATA PREPROCESSING
#### ANNEALING DATA
anneal.train <- as.data.table(read.csv("Data/Anneal/anneal_train.csv", header = FALSE))
anneal.test <- as.data.table(read.csv("Data/Anneal/anneal_test.csv", header = FALSE))
names.anneal <- (read.csv("Data/Anneal/anneal_names.csv", header = FALSE))
names.anneal <- as.character(names.anneal$V1)
names.anneal <- c(names.anneal, "class")
colnames(anneal.train) <- names.anneal
colnames(anneal.test) <- names.anneal

set.seed(5)
anneal.index <- sample(nrow(anneal.train), 150)
anneal.test <- rbind(anneal.train[anneal.index,], anneal.test)
anneal.train <- anneal.train[-anneal.index,]
anneal.train_class <- as.factor(anneal.train$class)
anneal.test_class <- as.factor(anneal.test$class)
#### factor problem
anneal.names <- as.data.table(cbind(names.anneal, ifelse(n <- sapply(anneal.train, function(x) length(levels(x))) == 1, "DROP", "NODROP")))
anneal.names <- anneal.names[V2 == "NODROP"]
anneal.names <- anneal.names$names

anneal.x <- anneal.train[,..anneal.names]
anneal.x <- anneal.x[,1:(ncol(anneal.x)-1)]
anneal.y <- anneal.test[,..anneal.names]
anneal.y <- anneal.y[,1:(ncol(anneal.y)-1)]
## end of factor problem


###Digits 
train.digit <- read.csv("Data/Digits/optdigits_train.csv", header = FALSE)
test.digit <- read.csv("Data/Digits/optdigits_test.csv", header = FALSE)

names <- c("col1","col2","col3","col4","col5","col6","col7","col8","col9","col10","col11","col12","col13","col14","col15","col16","col17","col18","col19","col20","col21","col22","col23","col24","col25","col26","col27","col28","col29","col30","col31","col32","col33","col34","col35","col36","col37","col38","col39","col40","col41","col42","col43","col44","col45","col46","col47","col48","col49","col50","col51","col52","col53","col54","col55","col56","col57","col58","col59","col60","col61","col62","col63","col64","class")
colnames(train.digit) <- names
colnames(test.digit) <- names

train.digit$class <- as.factor(train.digit$class)
test.digit$class <- as.factor(test.digit$class)



###Parkinsons 
parkinsons <- as.data.table(read.csv("Data/Parkinsons/parkinsons_updrs.csv", header = TRUE))

parkinsons[,subject. := NULL]
parkinsons[,motor_UPDRS := NULL]
set.seed(19)
parkinsons.test.index <- sample(nrow(parkinsons), 500)

train.parkinsons <- parkinsons[-parkinsons.test.index,]
test.parkinsons <- parkinsons[parkinsons.test.index,]



######### BANKRUPTCY
bankruptcy <- as.data.table(readARFF("Data/Bankruptcy/1year.arff"))
set.seed(15)
bankruptcy <- bankruptcy[complete.cases(bankruptcy)]

bankruptcy.test.index <- sample(nrow(bankruptcy), 750)

train.bankruptcy <- bankruptcy[-bankruptcy.test.index,]
test.bankruptcy <- bankruptcy[bankruptcy.test.index,]
train.bankruptcy$classchar <- as.character(train.bankruptcy$class)
test.bankruptcy$classchar <- as.character(test.bankruptcy$class)

############# END OF PREPROCESSING




####MODELSSS
####### treee

set.seed(122)
k_levels=c(1:10)
nofReplications=10
nFolds=10
indices=generateCVRuns(trainclass,nofReplications,nFolds,stratified=TRUE)
cvresult=data.table()

cv_euc_time <- system.time(
  for(i in 1:nofReplications) {
    thisReplication=indices[[i]]
    for(j in 1:nFolds){
      testindices=thisReplication[[j]]
      
      cvtrain=traindata[-testindices,]        
      cvtest=traindata[testindices,]
      
      
      for(y in 1:length(k_levels)){
        param_k=k_levels[y]
        predict_knn=knn(cvtrain, cvtest,trainclass[-testindices], k = param_k)
       
        tree.anneal <- rpart(class~., anneal.train, control = anneal.tree.control, method = "class")
        anneal.tree.pred <- predict(tree.anneal, anneal.test[,-c("class")], type = "class")
        anneal.tree.conf.mat <- table(anneal.tree.pred,anneal.test$class)
        anneal.tree.error <- sum(anneal.tree.pred==anneal.test$class)/nrow(anneal.test)
        cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=testindices,
                                           Predictions=as.numeric(as.character(predict_knn)),Real=trainclass[testindices]))
        
      }   
    }    
  }
)
anneal.tree.control <- rpart.control(minsplit = 10, cp = 0.1)




######### svm
anneal.svmfit <- svm(anneal.train_class~., data = anneal.x, kernel = "linear", cost = 0.1, scale = TRUE)
anneal.svmpred <- predict(anneal.svmfit, anneal.y)
table(anneal.svmpred,anneal.test$class)
sum(anneal.svmpred==anneal.test$class)/nrow(anneal.test)

###### rf

anneal.train_class <- as.factor(anneal.train$class)

rf_anneal <- randomForest(anneal.train[,-39], y = anneal.train_class, ntree = 100, proximity = TRUE)
anneal.prob_classes <- predict(rf_anneal, anneal.test[,-39])
table(anneal.prob_classes, anneal.test$class)
sum(anneal.prob_classes==anneal.test$class)/nrow(anneal.test)



#### boost

anneal.gbm.model <- gbm(anneal.train$class~., data = anneal.x, n.trees = 100, interaction.depth = 1,
                        n.minobsinnode = 10, shrinkage =0.1, distribution = "multinomial")
anneal.gbm.prob <- predict(anneal.gbm.model, anneal.y, n.trees = 100, type = "response")
anneal.gbm.pred <- apply(anneal.gbm.prob, 1, which.max)
table(anneal.gbm.pred, anneal.test_class)
sum(anneal.gbm.pred==anneal.test_class)/nrow(anneal.penalized.y1)

### penalized regression
anneal.penalized.x1 <- anneal.a[1:648,]
anneal.penalized.y1 <- anneal.a[649:898,]
anneal.penalized_model <- glmnet(x = as.matrix(anneal.penalized.x1), y = anneal.train_class, family = "multinomial")
anneal.results <- predict(anneal.penalized_model,newx =  as.matrix(anneal.penalized.y1), s = 0.01, type = "class")
table(anneal.results, anneal.test_class)
sum(anneal.results==anneal.test_class)/nrow(anneal.penalized.y1)


#######digits
####### treee
tree.digit <- tree(class~., data=train.digit)
digit.pred <- predict(tree.digit, test.digit, type = "class")
table(digit.pred,test.digit$class)
sum(digit.pred==test.digit$class)/nrow(test.digit)



######### svm
digit.svmfit <- svm(class~., data = train.digit, kernel = "linear", cost = 0.1, scale = FALSE)
digit.svmpred <- predict(digit.svmfit, test.digit[,-65])
table(digit.svmpred,test.digit$class)
sum(digit.svmpred==test.digit$class)/nrow(test.digit)

###### rf
rf_digit <- randomForest(train.digit[,-65], y = train.digit$class, ntree = 100, proximity = TRUE)
digit.prob_classes <- predict(rf_digit, test.digit[,-65])
table(digit.prob_classes, test.digit$class)
sum(digit.prob_classes==test.digit$class)/nrow(test.digit)


#### boost
digit.gbm.model <- gbm(class~., data = train.digit[,c(-1,-40)], n.trees = 100, interaction.depth = 1,
                        n.minobsinnode = 10, shrinkage =0.1, distribution = "multinomial")
digit.gbm.prob <- predict(digit.gbm.model, test.digit[,-65], n.trees = 100, type = "response")
digit.gbm.pred <- apply(digit.gbm.prob, 1, which.max)
table(digit.gbm.pred-1, test.digit$class)
sum((digit.gbm.pred-1)==test.digit$class)/nrow(test.digit)


### penalized regression

digit.penalized_model <- glmnet(x = as.matrix(train.digit[,-65]), y = train.digit$class, family = "multinomial")
digit.results <- predict(digit.penalized_model,newx =  as.matrix(test.digit[,-65]), s = 0.01, type = "class")
table(digit.results, test.digit$class)
sum(digit.results==test.digit$class)/nrow(test.digit)


############### PARKINSONS


####### treee
tree.parkinsons <- tree(total_UPDRS~., data=train.parkinsons)
parkinsons.pred <- predict(tree.parkinsons, test.parkinsons)
sum((parkinsons.pred-test.parkinsons$total_UPDRS)^2)/nrow(test.parkinsons)


######### svm
parkinsons.svmfit <- svm(total_UPDRS~., data = train.parkinsons, kernel = "linear", cost = 0.1, scale = FALSE)
parkinsons.svmpred <- predict(parkinsons.svmfit, test.parkinsons[,-c("total_UPDRS")])
sum((parkinsons.svmpred-test.parkinsons$total_UPDRS)^2)/nrow(test.parkinsons)


###### rf
rf_parkinsons <- randomForest(train.parkinsons[,-c("total_UPDRS")], y = train.parkinsons$total_UPDRS, ntree = 100, proximity = TRUE)
parkinsons.prob_classes <- predict(rf_parkinsons, test.parkinsons[,-c("total_UPDRS")])
sum((parkinsons.prob_classes-test.parkinsons$total_UPDRS)^2)/nrow(test.parkinsons)

#### boost


parkinsons.gbm.model <- gbm(total_UPDRS~., data = train.parkinsons, n.trees = 100, interaction.depth = 1,n.minobsinnode = 10, shrinkage =0.1, distribution = "gaussian")
parkinsons.gbm.pred <- predict(parkinsons.gbm.model, test.parkinsons[,-c("total_UPDRS")], n.trees = 100)
sum((parkinsons.gbm.pred-test.parkinsons$total_UPDRS)^2)/nrow(test.parkinsons)


### penalized regression

parkinsons.penalized_model <- glmnet(x = as.matrix(train.parkinsons[,-c("total_UPDRS")]), y = train.parkinsons$total_UPDRS, family = "gaussian")
parkinsons.results <- predict(parkinsons.penalized_model,newx =  as.matrix(test.parkinsons[,-c("total_UPDRS")]), s = 0.01, type = "class")
sum((parkinsons.results-test.parkinsons$total_UPDRS)^2)/nrow(test.parkinsons)

############BANKRUPTCY

####### treee
tree.bankruptcy <- tree(class~., data=train.bankruptcy[,-c("classchar")])
bankruptcy.pred <- predict(tree.bankruptcy, test.bankruptcy[,-c("class", "classchar")], type = "class")
table(bankruptcy.pred,test.bankruptcy$class)
sum(bankruptcy.pred==test.bankruptcy$class)/nrow(test.bankruptcy)




######### svm
bankruptcy.svmfit <- svm(class~., data = train.bankruptcy[,-c("classchar")], kernel = "linear", cost = 0.1, scale = TRUE)
bankruptcy.svmpred <- predict(bankruptcy.svmfit, test.bankruptcy[,-c("class","classchar")])
table(bankruptcy.svmpred,test.bankruptcy$class)
sum(bankruptcy.svmpred==test.bankruptcy$class)/nrow(test.bankruptcy)



###### rf

rf_bankruptcy <- randomForest(train.bankruptcy[,-c("class","classchar")], y = train.bankruptcy$class, ntree = 500, proximity = TRUE)
bankruptcy.prob_classes <- predict(rf_bankruptcy, test.bankruptcy[,-c("class","classchar")])
table(bankruptcy.prob_classes, test.bankruptcy$class)
sum(bankruptcy.prob_classes==test.bankruptcy$class)/nrow(test.bankruptcy)

#### boost

bankruptcy.gbm.model <- gbm(classchar~., data = train.bankruptcy[,-c("class")], n.trees = 100, interaction.depth = 1,
                       n.minobsinnode = 10, shrinkage =0.1)
bankruptcy.gbm.prob <- predict(bankruptcy.gbm.model, test.bankruptcy[,-c("class","classchar")], n.trees = 100, type = "response")
bankruptcy.gbm.pred <- (bankruptcy.gbm.prob >= 0.5)*1
table(bankruptcy.gbm.pred, test.bankruptcy$class)
sum((bankruptcy.gbm.pred)==test.bankruptcy$class)/nrow(test.bankruptcy)


### penalized regression

bankruptcy.penalized_model <- glmnet(x = as.matrix(train.bankruptcy[,-c("class", "classchar")]), y = train.bankruptcy$class, family = "binomial")
bankruptcy.results <- predict(bankruptcy.penalized_model,newx =  as.matrix(test.bankruptcy[,-c("class", "classchar")]), s = 0.01, type = "class")
table(bankruptcy.results, test.bankruptcy$class)
sum(bankruptcy.results==test.bankruptcy$class)/nrow(test.bankruptcy)


