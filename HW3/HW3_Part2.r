require(penalized)
rm(list=ls())
# change folder names accordingly (based on your computer settings, use slashes '/' 
# no backslashes '\' in defining paths

fname='C:/R/ECG/ecgTRAIN' # data path
traindata <- as.matrix(read.table(fname))  # read data into a matrix named traindata
head(traindata)
a <- as.matrix(read.table(fname))
a
#first column is the class variable
trainclass=traindata[,1] # takes -1 and 1
#drop first column
traindata=traindata[,2:ncol(traindata)]

print(dim(traindata)) #shows that there 100 series (rows) of length 96 time units (columns)
tlength=ncol(traindata)
noftimeseries=nrow(traindata)

#let's plot some of the time series
plot(traindata[1,])
#let's use line format
plot(traindata[1,],type='l')

#multiple plots on single plot
par(mfrow=c(2,2)) #2x2 grid
plot(traindata[1,],type='l')
plot(traindata[2,],type='l')
plot(traindata[3,],type='l')	
plot(traindata[4,],type='l')

par(mfrow = c(1,1))
#let's plot them overlaid with colors representing their class
plot(traindata[1,],type='l',col=trainclass[1]+2)
points(traindata[2,],type='l',col=trainclass[2]+2)
points(traindata[3,],type='l',col=trainclass[3]+2)	
points(traindata[4,],type='l',col=trainclass[4]+2) 

#problem with limits?
lim=max(abs(traindata[1:4,])) #maximum of the absolute value of the observations for first 4 time series
plot(traindata[1,],type='l',col=trainclass[1]+2,ylim=c(-1.1*lim,1.1*lim))
points(traindata[2,],type='l',col=trainclass[2]+2)
points(traindata[3,],type='l',col=trainclass[3]+2)	
points(traindata[4,],type='l',col=trainclass[4]+2) 

#read test data
fname='C:/R/ECG/ecgTEST' # data path
testdata <- as.matrix(read.table(fname))  # read data into a matrix named traindata
#first column is the class variable
testclass=testdata[,1] # takes -1 and 1
#drop first column
testdata=testdata[,2:ncol(testdata)]




testclass[testclass < 0] <- 0
trainclass[trainclass < 0] <- 0

traindata

#generating arbitrary lambda2 sequences
l2=exp (seq (-6, 1, length = 10))
print(lambda2) #check what they are
#parameters to be tried is lambda1 for L1 penalty and lambda2 for L2 (fused lasso penalty)
#fixing lambda1 to 1, I try to find the optimal lambda2 value from the sequence
lambdas=list(1,lambda2)

cvfused <- cvl(trainclass, traindata, lambda1 = 0.75,lambda2 = 0.5,fusedl = TRUE, model = "logistic", fold = 10 )
c <- coefficients(cvfused$fullfit, "all")
c
plot(c)

par(mfrow = c(1,1))
points(c, type = "l")
plot(traindata[5,], type = "l", col = "red")
points(traindata[3,], type = "l", col = "blue")

t(traindata)
length(trainclass)
fit <- penalized(trainclass, traindata, lambda1 = 3, lambda2 = 2, model = "logistic", fusedl = TRUE)
fit
c <- coefficients(fit,"all")
plot(c)
plot(c)

datam <- cbind(rep(1,100),traindata)
predictions <- t(c)%*%t(datam)
predictions <- exp(predictions)
probabilities <- 1-(1/(1+predictions))
pred <- (probabilities>0.5) * 1
comp <- (trainclass == pred)
sum(comp)


datam <- cbind(rep(1,100),testdata)
predictions <- t(c)%*%t(datam)
predictions <- exp(predictions)
probabilities <- 1-(1/(1+predictions))
comp <- (testclass == pred)
sum(comp)





denom <- t(cvfused$predictions)%*% traindata
denom <- exp(denom)
trainpred <- (1/(1+denom) > 0.5)*1
comparison <- data.frame(trainpred,trainclass)

##### dursun burada

#one option to compute Euclidean distance of first time series in test data to training
#series (inefficient)
combined=rbind(testdata[1,],traindata)
distMatrix=dist(combined)
print(str(distMatrix)) #it is distance matrix where first row (or column) has the distances we need
#let's convert it to a regular matrix
distMatrix=as.matrix(distMatrix)


neighborhood=order(distMatrix[1,]) #first array entry is 1, why?

#1-NN
neighbor=neighborhood[2]-1 #why minus 1?
prediction=trainclass[neighbor]

par(mfrow=c(2,1)) #2x1 grid
plot(testdata[1,],type='l',main='Test Data',col=testclass[1]+2)
plot(traindata[neighbor,],type='l',main='Training data with minimum Euclidean distance',col=trainclass[neighbor]+2)
########## dursun burada end