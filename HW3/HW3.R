require(plotly)
require(scatterplot3d)
require(FNN)
require(glmnet)
require(TunePareto)
require(data.table)
require(RANN.L1)
require(dplyr)

rm(list=ls())
gc()

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

cols <- rep("black",315)
cols[1] <- "blue"
cols[315] <- "red"
shapes <- rep(".",315)
shapes[1] <- 1
shapes[315] <- 4

pathplotter <- function(ax,ay,az)
{
  vx <- cumsum(ax)
  vy <- cumsum(ay)
  vz <- cumsum(az)
  
  xx <- rep(NA, 315)
  xx[1] <- 0
  xy <- rep(NA, 315)
  xy[1] <- 0
  xz <- rep(NA, 315)
  xz[1] <- 0
  
  
  for (i in 2:315) {
    xx[i] <- (vx[i] - vx[i-1])/2
    xy[i] <- (vy[i] - vy[i-1])/2
    xz[i] <- (vz[i] - vz[i-1])/2
  }
  
  xx <- cumsum(xx) + cumsum(vx)
  xy <- cumsum(xy) + cumsum(vy)
  xz <- cumsum(xz) + cumsum(vz)
  
  scatterplot3d(xz,xy,xx, color = cols, pch = shapes)
}

############### TASK 1.1
#class 1 instance
xtrain[11,1]

ax <- unlist(xtrain[11,2:ncol(xtrain)])
ay <- unlist(ytrain[11,2:ncol(xtrain)])
az <- unlist(ztrain[11,2:ncol(xtrain)])


pathplotter(ax,ay,az)


#class 2 instance
xtrain[20,1]

ax <- unlist(xtrain[20,2:ncol(xtrain)])
ay <- unlist(ytrain[20,2:ncol(xtrain)])
az <- unlist(ztrain[20,2:ncol(xtrain)])


pathplotter(ax,ay,az)


#class 3 instance
xtrain[4,1]

ax <- unlist(xtrain[4,2:ncol(xtrain)])
ay <- unlist(ytrain[4,2:ncol(xtrain)])
az <- unlist(ztrain[4,2:ncol(xtrain)])


pathplotter(ax,ay,az)

#class 4 instance
xtrain[5,1]

ax <- unlist(xtrain[5,2:ncol(xtrain)])
ay <- unlist(ytrain[5,2:ncol(xtrain)])
az <- unlist(ztrain[5,2:ncol(xtrain)])


pathplotter(ax,ay,az)

#class 5 instance
xtrain[2,1]

ax <- unlist(xtrain[2,2:ncol(xtrain)])
ay <- unlist(ytrain[2,2:ncol(xtrain)])
az <- unlist(ztrain[2,2:ncol(xtrain)])


pathplotter(ax,ay,az)


#class 6 instance
xtrain[1,1]

ax <- unlist(xtrain[1,2:ncol(xtrain)])
ay <- unlist(ytrain[1,2:ncol(xtrain)])
az <- unlist(ztrain[1,2:ncol(xtrain)])


pathplotter(ax,ay,az)


#class 7 instance
xtrain[7,1]

ax <- unlist(xtrain[7,2:ncol(xtrain)])
ay <- unlist(ytrain[7,2:ncol(xtrain)])
az <- unlist(ztrain[7,2:ncol(xtrain)])


pathplotter(ax,ay,az)

#class 8 instance

xtrain[6,1]

ax <- unlist(xtrain[6,2:ncol(xtrain)])
ay <- unlist(ytrain[6,2:ncol(xtrain)])
az <- unlist(ztrain[6,2:ncol(xtrain)])


pathplotter(ax,ay,az)


rm(ax,ay,az)
gc()

############ TASK 1.2

trainclass <- xtrain[,1]
testclass <- xtest[,1]

colnum <- ncol(xtrain)

testdata <- cbind(xtest[,2:colnum],ytest[,2:colnum],ztest[,2:colnum])

traindata <- cbind(xtrain[,2:colnum],ytrain[,2:colnum],ztrain[,2:colnum])

########## Distance Measure : Euclidean
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
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=testindices,
                                         Predictions=as.numeric(as.character(predict_knn)),Real=trainclass[testindices]))
    }   
  }    
}
)

cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]
##### optimal k is 1 ##########


###### Distance Measure: Manhattan
cvresult=data.table()

cv_manh_time <- system.time(
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
)

cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]
##### optimal k is 1 ##########

###### End of Task 1.2

########### Task 1.3

## Euclidean
euc_pred_time <- system.time(predict_knn <- knn(traindata,testdata, trainclass, k=1))
Predictions=as.numeric(as.character(predict_knn))
euc_comp <- (Predictions==testclass)
table(Predictions,testclass)
accuracy <- sum(euc_comp)/length(testclass)
accuracy

## Manhattan
manh_pred_time <- system.time(predict_knn <- nn2(traindata, testdata, k = 1))
Predictions <- trainclass[as.vector(predict_knn$nn.idx[,1])]
man_comp <- (Predictions==testclass)
table(Predictions,testclass)
accuracy <- sum(man_comp)/length(testclass)
accuracy

#### System Times
cv_euc_time
cv_manh_time
euc_pred_time
manh_pred_time

