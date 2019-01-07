#Libraries Used
require(data.table)
require(cluster)
require(kmed)
require(stats)
require(pdist)
require(glmnet)
require(TunePareto)
require(ROCR)
require(AUC)


#path
data_path = "C:/IE_582_Rep/fall18-bugracnr/HW5/Musk1.csv"

#read data
muskdata <- as.data.table(read.csv(data_path, header = TRUE))

#seperate data and IDs
muskclass <- muskdata$Class
muskId <- muskdata$Id

#seperate bag classes
bag_classes <- muskdata[,1:2]
bag_classes <- aggregate(bag_classes$Class,list(bag_classes$Id),mean)
bag_classes <- bag_classes$x

muskdata[,c("Class", "Id"):=NULL]


## Distances
euc_dist <- dist(muskdata, method = "euclidean", diag = TRUE, upper = TRUE)
manh_dist <- dist(muskdata, method = "manhattan", diag = TRUE, upper = TRUE)


set.seed(270)
ncluster=c(3,6,9,12,15,18,21,24,27,30)
nofReplications=10
nFolds=5
indices=generateCVRuns(bag_classes,nofReplications,nFolds,stratified=TRUE)
cvresult=data.table()

for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=thisReplication[[j]]
    
     for(y in ncluster){
     
      
      #k-medoids
      euc_pam <- pam(euc_dist, y, diss = TRUE)
      manh_pam <- pam(manh_dist, y, diss = TRUE)
      
      #centroids
      euc_kmeans_centroids <- muskdata[euc_pam$medoids,]
      manh_kmeans_centroids <- muskdata[manh_pam$medoids,]
      
      #distance btw instances and centroids
      euc_inst_dist_pam <- as.matrix(pdist(muskdata,euc_kmeans_centroids))
      manh_inst_dist_pam <- as.matrix(pdist(muskdata,manh_kmeans_centroids))
      
      #bag distances to centroids
      euc_bag_dist <- as.matrix(aggregate(euc_inst_dist_pam, list(muskId), mean))
      manh_bag_dist <- as.matrix(aggregate(manh_inst_dist_pam, list(muskId), mean))
      
      
      #create hierarchical clusters
      euc_hclust <- hclust(euc_dist)
      manh_hclust <- hclust(manh_dist)
      
      #determine classes
      manh_classes <- cutree(manh_hclust,k=y)
      euc_classes <- cutree(euc_hclust,k=y)
      
      #means are centers
      euc_hclust_centers <- as.matrix(aggregate(muskdata,list(euc_classes),mean))
      manh_hclust_centers <- as.matrix(aggregate(muskdata,list(manh_classes),mean))
      
      #find distances of each instance to class centers
      euc_inst_dist_mat_hclust <- as.matrix(pdist(muskdata,euc_hclust_centers))
      manh_inst_dist_mat_hclust <- as.matrix(pdist(muskdata,manh_hclust_centers))
      
      #bag distances to centroids
      euc_hclust_bag_dist <- as.matrix(aggregate(euc_inst_dist_mat_hclust, list(muskId), mean))
      manh_hclust_bag_dist <- as.matrix(aggregate(manh_inst_dist_mat_hclust, list(muskId), mean))
      
      
      train1 <- euc_bag_dist[testindices,2:ncol(euc_bag_dist)]
      train2 <- manh_bag_dist[testindices,2:ncol(manh_bag_dist)]
      train3 <- euc_hclust_bag_dist[testindices,2:ncol(euc_hclust_bag_dist)]
      train4 <- manh_hclust_bag_dist[testindices,2:ncol(manh_hclust_bag_dist)]
      
      test1 <- euc_bag_dist[-testindices,2:ncol(euc_bag_dist)]
      test2 <- manh_bag_dist[-testindices,2:ncol(manh_bag_dist)]
      test3 <- euc_hclust_bag_dist[-testindices,2:ncol(euc_hclust_bag_dist)]
      test4 <- manh_hclust_bag_dist[-testindices,2:ncol(manh_hclust_bag_dist)]
      
      
      trainclass <- bag_classes[testindices]
      testclass <- bag_classes[-testindices]
      
      
      #euclidean dist k-medoids
      muskfit <- cv.glmnet(train1,trainclass, family = "binomial", nfolds=10)
      musk_predict <- predict(muskfit, test1, type = "class", s = "lambda.min")
      
      table(musk_predict, testclass)
      accuracy = sum(musk_predict==testclass)/nrow(test1)
      accuracy
      
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='Euc_Dist_k-medoids',Num_of_Classes = y, TestId=testindices,
                                         Predictions = as.numeric(musk_predict),Real=testclass))
      
      #manhattan dist k-medoids
      muskfit <- cv.glmnet(train2,trainclass, family = "binomial", nfolds=10)
      musk_predict <- predict(muskfit, test2, type = "class", s = "lambda.min")
      
      table(musk_predict, testclass)
      accuracy = sum(musk_predict==testclass)/nrow(test1)
      accuracy
      
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='Manh_Dist_k-medoids',Num_of_Classes = y, TestId=testindices,
                                         Predictions=as.numeric(musk_predict),Real=testclass))
      
      #euclidean dist hierarchical
      muskfit <- cv.glmnet(train3,trainclass, family = "binomial", nfolds=10)
      musk_predict <- predict(muskfit, test3, type = "class", s = "lambda.min")
      
      table(musk_predict, testclass)
      accuracy = sum(musk_predict==testclass)/nrow(test1)
      accuracy
      
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='Euc_Dist_Hierarchical',Num_of_Classes = y, TestId=testindices,
                                         Predictions=as.numeric(musk_predict),Real=testclass))
      
      #manhattan dist hierarchical
      muskfit <- cv.glmnet(train4,trainclass, family = "binomial", nfolds=10)
      musk_predict <- predict(muskfit, test4, type = "class", s = "lambda.min")
      
      table(musk_predict, testclass)
      accuracy = sum(musk_predict==testclass)/nrow(test1)
      accuracy
      
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='Manh_Dist_Hierarchical',Num_of_Classes = y, TestId=testindices,
                                         Predictions=as.numeric(musk_predict),Real=testclass))
      
    }   
  }    
}

comparison <- cvresult[,list(Accu=mean(Predictions==Real), 
                             TPR = sum(Predictions*Real == 1)/(sum(Predictions*Real == 1)+sum(Predictions-Real == -1)),
                             FPR = sum(Predictions-Real == 1)/(sum(Predictions-Real == 1)+sum(Predictions+Real == 0))),by=list(Method,Num_of_Classes,Replication)]

comparison <- comparison[order(Accu)]


set.seed(10)
index <- sample(1:length(bag_classes), 20)

## Manhattan - K_Medoids - 27 Classes
#k-medoids
manh_pam <- pam(manh_dist, 27, diss = TRUE)

#centroids
manh_kmeans_centroids <- muskdata[manh_pam$medoids,]

#distance btw instances and centroids
manh_inst_dist_pam <- as.matrix(pdist(muskdata,manh_kmeans_centroids))

#bag distances to centroids
manh_bag_dist <- as.matrix(aggregate(manh_inst_dist_pam, list(muskId), mean))


train <- manh_bag_dist[index,2:ncol(manh_bag_dist)]
test <- manh_bag_dist[-index,2:ncol(manh_bag_dist)]

trainclass <- bag_classes[index]
testclass <- bag_classes[-index]


#manhattan dist k-medoids
muskfit <- cv.glmnet(train,trainclass, family = "binomial", nfolds=10)
musk_predict <- predict(muskfit, test, type = "response", s = "lambda.min")

perf <- performance(prediction(musk_predict,testclass), "tpr", "fpr")
auc <- performance(prediction(musk_predict,testclass), measure = "auc")
auc <- auc@y.values[[1]]



### Euc_Dist k-medoids 24 clusters

#k-medoids
euc_pam <- pam(euc_dist, 24, diss = TRUE)

#centroids
euc_kmeans_centroids <- muskdata[euc_pam$medoids,]

#distance btw instances and centroids
euc_inst_dist_pam <- as.matrix(pdist(muskdata,euc_kmeans_centroids))

#bag distances to centroids
euc_bag_dist <- as.matrix(aggregate(euc_inst_dist_pam, list(muskId), mean))



train2 <- euc_bag_dist[index,2:ncol(euc_bag_dist)]
test2 <- euc_bag_dist[-index,2:ncol(euc_bag_dist)]

trainclass2 <- bag_classes[index]
testclass2 <- bag_classes[-index]


#eucattan dist k-medoids
muskfit2 <- cv.glmnet(train2,trainclass2, family = "binomial", nfolds=10)
musk_predict2 <- predict(muskfit2, test2, type = "response", s = "lambda.min")

perf2 <- performance(prediction(musk_predict2,testclass2), "tpr", "fpr")
auc2 <- performance(prediction(musk_predict2,testclass2), measure = "auc")
auc2 <- auc2@y.values[[1]]


## manh dist hierarchical 12

#create hierarchical clusters
manh_hclust <- hclust(manh_dist)

#determine classes
manh_classes <- cutree(manh_hclust,k=12)


#means are centers
manh_hclust_centers <- as.matrix(aggregate(muskdata,list(manh_classes),mean))

#find distances of each instance to class centers
manh_inst_dist_mat_hclust <- as.matrix(pdist(muskdata,manh_hclust_centers))

#bag distances to centroids
manh_hclust_bag_dist <- as.matrix(aggregate(manh_inst_dist_mat_hclust, list(muskId), mean))


train3 <- manh_hclust_bag_dist[index,2:ncol(manh_hclust_bag_dist)]


test3 <- manh_hclust_bag_dist[-index,2:ncol(manh_hclust_bag_dist)]


trainclass3 <- bag_classes[index]
testclass3 <- bag_classes[-index]


#manhattan dist hierarchical
muskfit3 <- cv.glmnet(train3,trainclass3, family = "binomial", nfolds=10)
musk_predict3 <- predict(muskfit, test3, type = "response", s = "lambda.min")

perf3 <- performance(prediction(musk_predict3,testclass3), "tpr", "fpr")
auc3 <- performance(prediction(musk_predict3,testclass3), measure = "auc")
auc3 <- auc3@y.values[[1]]



## euc dist hierarchical 12

#create hierarchical clusters
euc_hclust <- hclust(euc_dist)

#determine classes
euc_classes <- cutree(euc_hclust,k=12)


#means are centers
euc_hclust_centers <- as.matrix(aggregate(muskdata,list(euc_classes),mean))

#find distances of each instance to class centers
euc_inst_dist_mat_hclust <- as.matrix(pdist(muskdata,euc_hclust_centers))

#bag distances to centroids
euc_hclust_bag_dist <- as.matrix(aggregate(euc_inst_dist_mat_hclust, list(muskId), mean))


train4 <- euc_hclust_bag_dist[index,2:ncol(euc_hclust_bag_dist)]


test4 <- euc_hclust_bag_dist[-index,2:ncol(euc_hclust_bag_dist)]


trainclass4 <- bag_classes[index]
testclass4 <- bag_classes[-index]


#eucattan dist hierarchical
muskfit4 <- cv.glmnet(train4,trainclass4, family = "binomial", nfolds=10)
musk_predict4 <- predict(muskfit4, test4, type = "response", s = "lambda.min")

perf4 <- performance(prediction(musk_predict4,testclass4), "tpr", "fpr")
auc4 <- performance(prediction(musk_predict4,testclass4), measure = "auc")
auc4 <- auc4@y.values[[1]]


par(mfrow = c(2,2))
plot(perf, main = paste("ROC Curve, K-Medoids with","\n" ,"Manhattan Distances, 27 Clusters",
                        "\n", "Area Under Curve = ", auc), colorize = TRUE)
plot(perf2, main = paste("ROC Curve, K-Medoids with","\n","Euclidean Distances, 24 Clusters", "\n", 
                        "Area Under Curve = ", auc2), colorize = TRUE)
plot(perf3, main = paste("ROC Curve, Hierarchical Clustering"
                         ,"\n","with Manhattan Distances, 12 Clusters", "\n", "Area Under Curve = ", auc3), colorize = TRUE)
plot(perf4, main = paste("ROC Curve, Hierarchical Clustering"
                         ,"\n","with Euclidean Distances, 12 Clusters", "\n", "Area Under Curve = ", auc4), colorize = TRUE)


