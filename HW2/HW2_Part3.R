library(jpeg)
library(imager)
rm(list=ls())
gc()
path <- "C:/IE_582_Rep/fall18-bugracnr/HW2"

setwd(path)

pic <- readJPEG("HW2_Files/HW2_Part3.jpg")
class(pic)
typeof(pic)
str(pic)
range(pic[,,2])


x <- 1:512
y <- 1:512

plot(x,y)
rasterImage(pic, 1,1,512,512)

r <- t(apply(pic[,,1],2,rev))
g <- t(apply(pic[,,2],2,rev))
b <- t(apply(pic[,,3],2,rev))

par(mfrow = c(3,1))
image(x,y,r, col = rgb(r,0,0))
image(x,y,g, col = rgb(0,g,0))
image(x,y,b, col = rgb(0,0,b))

set.seed(1)
noise_1 <-  matrix(runif(512*512, min = 0, max = 0.1),512)
set.seed(2)
noise_2 <-  matrix(runif(512*512, min = 0, max = 0.1),512)
set.seed(3)
noise_3 <-  matrix(runif(512*512, min = 0, max = 0.1),512)

noisy_pic <- pic



noisy_pic[,,1] <- pic[,,1] + noise_1
noisy_pic[,,2] <- pic[,,2] + noise_2
noisy_pic[,,3] <- pic[,,3] + noise_3

noisy_pic <- noisy_pic/max(noisy_pic)
range(pic)
range(noise_1)

image(x,y,t(apply(noisy_pic[,,1],2,rev)), col = rgb(r,0,0))
image(x,y,t(apply(noisy_pic[,,2],2,rev)))
image(x,y,t(apply(noisy_pic[,,3],2,rev)))

plot(x,y)


jpeg("HW2_Files/Noisy_Pic.jpg")
plot(x,y)
rasterImage(noisy_pic, 1,1,512,512)
dev.off()
