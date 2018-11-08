require(plotly)
require(scatterplot3d)
require(FNN)
require(glmnet)
require(TunePareto)
require(data.table)
require(RANN.L1)


path_xtest <- gsub  ( "\\\\",  "/", "C:/IE_582_Rep/fall18-bugracnr/HW3/HW3_Files/uWaveGestureLibrary_X_TEST"   )
path_xtrain <- gsub  ( "\\\\",  "/", "C:/IE_582_Rep/fall18-bugracnr/HW3/HW3_Files/uWaveGestureLibrary_X_TRAIN"   )
path_ytest <- gsub  ( "\\\\",  "/", "C:/IE_582_Rep/fall18-bugracnr/HW3/HW3_Files/uWaveGestureLibrary_Y_TEST"   )
path_ytrain <- gsub  ( "\\\\",  "/", "C:/IE_582_Rep/fall18-bugracnr/HW3/HW3_Files/uWaveGestureLibrary_Y_TRAIN"   )
path_ztest <- gsub  ( "\\\\",  "/", "C:/IE_582_Rep/fall18-bugracnr/HW3/HW3_Files/uWaveGestureLibrary_Z_TEST"   )
path_ztrain <- gsub  ( "\\\\",  "/", "C:/IE_582_Rep/fall18-bugracnr/HW3/HW3_Files/uWaveGestureLibrary_Z_TRAIN"   )

xtest <- read.table(path_xtest)
ytest <- read.table(path_ytest)
ztest <- read.table(path_ztest)
xtrain <- read.table(path_xtrain)
ytrain <- read.table(path_ytrain)
ztrain <- read.table(path_ztrain)

x <- cumsum(unlist(xtrain[1,2:ncol(xtrain)]))
y <- cumsum(unlist(ytrain[1,2:ncol(xtrain)]))
z <- cumsum(unlist(ztrain[1,2:ncol(xtrain)]))

scatterplot3d(x,y,z)


x57 <- cumsum(unlist(xtrain[57,2:ncol(xtrain)]))
y57 <- cumsum(unlist(ytrain[57,2:ncol(xtrain)]))
z57 <- cumsum(unlist(ztrain[57,2:ncol(xtrain)]))

scatterplot3d(x57,y57,z57)


#### seems unnecessary start ####
column_names <- c("x","y","z")
sample_vector <- rep(NA, 315)
sample_frame <- data.frame(sample_vector,sample_vector,sample_vector)
colnames(sample_frame) <- column_names
all_train <-rep(list(sample_frame), nrow(xtrain))
names(all_train) <- c(1:nrow(xtrain))

for (i in 1:nrow(xtrain)) {
  all_train[[i]]$x <- cumsum(unlist(xtrain[i,2:ncol(xtrain)]))
  all_train[[i]]$y <- cumsum(unlist(ytrain[i,2:ncol(ytrain)]))
  all_train[[i]]$z <- cumsum(unlist(ztrain[i,2:ncol(ztrain)]))
}


all_test <-rep(list(sample_frame), nrow(xtest))
names(all_test) <- c(1:nrow(xtest))

for (i in 1:nrow(xtest)) {
  all_test[[i]]$x <- cumsum(unlist(xtest[i,2:ncol(xtest)]))
  all_test[[i]]$y <- cumsum(unlist(ytest[i,2:ncol(ytest)]))
  all_test[[i]]$z <- cumsum(unlist(ztest[i,2:ncol(ztest)]))
}

par(mfrow = c(1,1))
scatterplot3d(all_train[[57]]$x,all_train[[57]]$y,all_train[[57]]$z)

### seems unnecessary end ###

trainclass <- xtrain[,1]
testclass <- xtest[,1]

colnum <- ncol(xtrain)

testdata <- cbind(xtest[,2:colnum],ytest[,2:colnum],ztest[,2:colnum])

traindata <- cbind(xtrain[,2:colnum],ytrain[,2:colnum],ztrain[,2:colnum])


k_levels=c(1)
nofReplications=10
nFolds=10
indices=generateCVRuns(trainclass,nofReplications,nFolds,stratified=TRUE)
cvresult=data.table()
for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=thisReplication[[j]]
    
    cvtrain=traindata[-testindices,]        
    cvtest=traindata[testindices,]
    

    for(y in 1:length(k_levels)){
      param_k=k_levels[y]
      predict_knn=knn(cvtrain, cvtest,trainclass[-testindices], k = param_k)
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=testindices,
                                         Predictions=as.numeric(as.character(predict_knn)),Real=trainclass[testindices]))
    }   
  }    
}


cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]
table(cvresult$Predictions, cvresult$Real)

cvresult=data.table()
for(i in 1:nofReplications) {
    thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=thisReplication[[j]]
    
    cvtrain=traindata[-testindices,]        
    cvtest=traindata[testindices,]
    tclass <- trainclass[-testindices]
    
 
    for(y in 1:length(k_levels)){
      param_k=k_levels[y]
      predict_knn=nn2(cvtrain, cvtest, k = param_k)
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=testindices,
                                         Predictions=tclass[as.vector(predict_knn$nn.idx)],
                                                                Real=trainclass[testindices]))
    
    }   
  }    
}

cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]
table(cvresult$Predictions, cvresult$Real)

#backup start 
combined=rbind(traindata[testindices,],traindata)
distMatrix=dist(combined, method = "euclidean")
distMatrix=as.matrix(distMatrix)

neighborhood=order(distMatrix[1,]) #first array entry is 1, why?: first entry is the distance between 1st elements which is 0

#1-NN
neighbor <- (neighborhood[2] >  testindices)*neighborhood[2] 
neighbor <- (neighborhood[2] <= testindices)*(neighborhood[2]-1)

prediction=trainclass[neighbor]
#backup end