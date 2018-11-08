require(plotly)
require(scatterplot3d)


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

head(xtest)

x <- cumsum(unlist(xtrain[1,2:ncol(xtrain)]))
y <- cumsum(unlist(ytrain[1,2:ncol(xtrain)]))
z <- cumsum(unlist(ztrain[1,2:ncol(xtrain)]))





scatterplot3d(x,y,z)


x57 <- cumsum(unlist(xtrain[57,2:ncol(xtrain)]))
y57 <- cumsum(unlist(ytrain[57,2:ncol(xtrain)]))
z57 <- cumsum(unlist(ztrain[57,2:ncol(xtrain)]))

scatterplot3d(x57,y57,z57)

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

trainclass <- xtrain[,1]
testclass <- xtest[,1]

colnum <- ncol(xtrain)

testdata <- cbind(xtest[,2:colnum],ytest[,2:colnum],ztest[,2:colnum])

traindata <- cbind(xtrain[,2:colnum],ytrain[,2:colnum],ztrain[,2:colnum])

dim(traindata)

#one option to compute Euclidean distance of first time series in test data to training
#series (inefficient)
testsubject <- 550
combined=rbind(testdata[testsubject,],traindata)
distMatrix=dist(combined, method = "euclidean")
distMatrix=as.matrix(distMatrix)


neighborhood=order(distMatrix[1,]) #first array entry is 1, why?: first entry is the distance between 1st elements which is 0

#1-NN
neighbor=neighborhood[2]-1 #why minus 1? first element is the test data. we need original index
prediction=trainclass[neighbor]

par(mfrow=c(2,1)) #2x1 grid
scatterplot3d(all_test[[testsubject]]$x, all_test[[testsubject]]$y, all_test[[testsubject]]$z,type='l',main='Test Data', color = testclass[testsubject]+2)
scatterplot3d(all_train[[neighbor]]$x,all_train[[neighbor]]$y,all_train[[neighbor]]$z,type='l',main='Training data with minimum Euclidean distance',color=trainclass[neighbor]+2)

