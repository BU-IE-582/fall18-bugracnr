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

x <- cumsum(unlist(xtrain[1,2:ncol(xtrain)]))
y <- cumsum(unlist(ytrain[1,2:ncol(xtrain)]))
z <- cumsum(unlist(ztrain[1,2:ncol(xtrain)]))

scatterplot3d(x,y,z)


x57 <- cumsum(unlist(xtrain[57,2:ncol(xtrain)]))
y57 <- cumsum(unlist(ytrain[57,2:ncol(xtrain)]))
z57 <- cumsum(unlist(ztrain[57,2:ncol(xtrain)]))

scatterplot3d(x57,y57,z57)


