require(penalized)
require(data.table)


fname='C:/R/ECG/ecgTRAIN' # data path
traindata <- as.matrix(read.table(fname))  # read data into a matrix named traindata
#first column is the class variable
trainclass=traindata[,1] # takes -1 and 1
#drop first column
traindata=traindata[,2:ncol(traindata)]

print(dim(traindata)) #shows that there 100 series (rows) of length 96 time units (columns)
tlength=ncol(traindata)
noftimeseries=nrow(traindata)

#read test data
fname='C:/R/ECG/ecgTEST' # data path
testdata <- as.matrix(read.table(fname))  # read data into a matrix named traindata
#first column is the class variable
testclass=testdata[,1] # takes -1 and 1
#drop first column
testdata=testdata[,2:ncol(testdata)]


testclass[testclass < 0] <- 0
trainclass[trainclass < 0] <- 0


l1 <- optL1(trainclass,traindata,fusedl = TRUE, fold = 10, model = "logistic", minlambda1 = 0, maxlambda1 = 20)
cvfused <- cvl(trainclass, traindata, lambda1 = l1[[1]],lambda2 = 1,fusedl = TRUE, model = "logistic", fold = 10 )

##### Answer 1
c <- coefficients(cvfused$fullfit, "all")
data1 <- cbind(rep(1,100),traindata)
probs1 <- t(c)%*%t(data1)
predictions1 <- (exp(probs1)/(1+exp(probs1)))
pred1 <- (predictions1>0.5) * 1
traincomp <- (trainclass == pred1)
sum(traincomp)
    
  
data2 <- cbind(rep(1,100),testdata)
probs2 <- t(c)%*%t(data2)
predictions2 <- (exp(probs2)/(1+exp(probs2)))
pred2 <- (predictions2>0.5) * 1
testcomp <- (testclass == pred2)*1
sum(testcomp)
    
table(testclass, pred2) 
    
##### Answer 2


plot(traindata[1,], type = "l", col = "red")
points(traindata[2,], type = "l", col = "blue")
points(c, type = "l")


#### Answer 3
traindata
traindata_c <- matrix(0,100,96)
testdata_c <- matrix(0,100,96)


for (i in 2:96) {
  traindata_c[,i-1] <-  traindata[,i] - traindata[,i-1]
  testdata_c[,i-1] <-  testdata[,i] - testdata[,i-1]
}

traindata_c <- traindata_c[,-96]
testdata_c <- testdata_c[,-96]
 
l1 <- optL1(trainclass,traindata_c,fusedl = TRUE, fold = 10, model = "logistic", minlambda1 = 0, maxlambda1 = 20)
#l2 <- optL2(trainclass,traindata_c,fusedl = TRUE, fold = 10, model = "logistic", lambda1 = l1[[1]])
cvfused2 <- cvl(trainclass, traindata_c, lambda1 = l1[[1]],lambda2 = 1,fusedl = TRUE, model = "logistic", fold = 10 )

c <- coefficients(cvfused2$fullfit, "all")
data1c <- cbind(rep(1,100),traindata_c)
probs1 <- t(c)%*%t(data1c)
predictions1 <- (exp(probs1)/(1+exp(probs1)))
pred1 <- (predictions1>0.5) * 1
traincomp <- (trainclass == pred1)
sum(traincomp)


data2c <- cbind(rep(1,100),testdata_c)
probs2 <- t(c)%*%t(data2c)
predictions2 <- (exp(probs2)/(1+exp(probs2)))
pred2 <- (predictions2>0.5) * 1
testcomp <- (testclass == pred2)*1
sum(testcomp)

table(testclass, pred2) 


##### Answer 4
c <- coefficients(cvfused2$fullfit, "all")
trainclass[2]
testclass[5]
testclass[1]
testclass[1]

plot(traindata_c[2,], type = "l", col = "blue")
points(testdata_c[1,], type = "l", col = "blue")
points(testdata_c[5,], type = "l", col = "red")
points(traindata_c[1,], type = "l", col = "red")
points(c, type = "l")
