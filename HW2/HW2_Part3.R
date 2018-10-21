library(jpeg)
library(imager)

path <- "C:/IE_582_Rep/fall18-bugracnr/HW2"

setwd(path)

pic <- readJPEG("HW2_Files/HW2_Part3.jpg")
class(pic)
typeof(pic)
str(pic)

x <- 1:512
y <- 1:512

plot(x,y)
rasterImage(pic, 1,1,512,512)

r <- pic[,,1]*255
g <- pic[,,2]*255
b <- pic[,,3]*255


image(x,y,pic[,,1])
par(new=T)
image(x,y,pic[,,2])
par(new=T)
image(x,y,pic[,,3])

set.seed(15)
noise_1 <-  matrix(runif(512*512, min = 0, max = 0.1),512)
set.seed(73)
noise_2 <-  matrix(runif(512*512, min = 0, max = 0.1),512)
set.seed(392)
noise_3 <-  matrix(runif(512*512, min = 0, max = 0.1),512)

pic[,,1] <- pic[,,1] + noise_1
pic[,,2] <- pic[,,2] + noise_2
pic[,,3] <- pic[,,3] + noise_3


image(x,y,pic[,,1])
par(new=T)
image(x,y,pic[,,2])
par(new=T)
image(x,y,pic[,,3])


