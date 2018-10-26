library(jpeg)
library(imager)
library(data.table)



path <- "C:/IE_582_Rep/fall18-bugracnr/HW2"

setwd(path)

pic <- readJPEG("HW2_Files/HW2_Part3.jpeg")

class(pic)
typeof(pic)
str(pic)
dim(pic)
range(pic)

par(mfrow = c(1,1))
x <- 1:512
y <- 1:512
plot(x,y, ann = FALSE, axes = FALSE, col = 0)
rasterImage(pic, 0,0,512,512)

r <- t(apply(pic[,,1],2,rev))
g <- t(apply(pic[,,2],2,rev))
b <- t(apply(pic[,,3],2,rev))

redPal <- colorRampPalette(c("black", "red"))
greenPal <- colorRampPalette(c("black", "green"))
bluePal <- colorRampPalette(c("black", "blue"))

par(mfrow = c(1,3))
image(r, col = redPal(256))
image(g, col = greenPal(256))
image(b, col = bluePal(256))


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


noisy_r <- t(apply(noisy_pic[,,1],2,rev))
noisy_g <- t(apply(noisy_pic[,,2],2,rev))
noisy_b <- t(apply(noisy_pic[,,3],2,rev))

par(mfrow = c(1,1))
plot(x,y, ann = FALSE, axes = FALSE, col = 0)
rasterImage(noisy_pic, 0,0,512,512)



par(mfrow = c(1,3))
image(noisy_r, col = redPal(256))
image(noisy_g, col = greenPal(256))
image(noisy_b, col = bluePal(256))


noisy_grey <- noisy_pic[,,1] + noisy_pic[,,2] + noisy_pic[,,3]
noisy_grey <- noisy_grey/max(noisy_grey)  
dim(noisy_grey)
par(mfrow = c(1,1))
plot(x,y, ann = FALSE, axes = FALSE, col = 0)
rasterImage(noisy_grey,0,0,512,512)



sample_vector <- rep(NA,9)
imgdata <- rep(list(sample_vector), 510*510)

k <- 1

for (i in 2:511) {
  for (j in 2:511) {
    imgdata[[k]] <- as.vector(noisy_grey[(i-1):(i+1),(j-1):(j+1)])
    k <- k+1
  }
}

imgdata <- as.data.table(matrix(unlist(imgdata), ncol = 9, byrow = FALSE))

imgdata

noisy_pca <- princomp(imgdata)
summary(noisy_pca)

plot(noisy_pca)
str(noisy_pca)

summary(noisy_pca)

noisy_pca$loadings

score_1 <- noisy_pca$scores[,1]
score_2 <- noisy_pca$scores[,2]
score_3 <- noisy_pca$scores[,3]

score_1 <- (score_1 - min(score_1))/(max(score_1)-min(score_1))
score_2 <- (score_2 - min(score_2))/(max(score_2)-min(score_2))
score_3 <- (score_3 - min(score_3))/(max(score_3)-min(score_3))


mat_score_1 <- matrix(score_1, 510, byrow=TRUE)
mat_score_2 <- matrix(score_2, 510, byrow=TRUE)
mat_score_3 <- matrix(score_3, 510, byrow=TRUE)

par(mfrow = c(2,2))
plot(1:512,1:512, ann = TRUE, axes = FALSE, col = 0, main = "Unprocessed Image", xlab = "", ylab = "")
rasterImage(noisy_grey,0,0,512,512)
plot(x,y, ann = TRUE, axes = FALSE, col = 0, main = "Component 1 Reconstruction", xlab = "", ylab = "")
rasterImage(mat_score_1,0,0,510,510)
plot(x,y, ann = TRUE, axes = FALSE, col = 0, main = "Component 2 Reconstruction", xlab = "", ylab = "")
rasterImage(mat_score_2,0,0,510,510)
plot(x,y, ann = TRUE, axes = FALSE, col = 0, main = "Component 3 Reconstruction", xlab = "", ylab = "")
rasterImage(mat_score_3,0,0,510,510)


eigen_1 <- noisy_pca$loadings[,1]
eigen_2 <- noisy_pca$loadings[,2]
eigen_3 <- noisy_pca$loadings[,3]

eigen_1 <- (eigen_1 - min(eigen_1))/(max(eigen_1)-min(eigen_1))
eigen_2 <- (eigen_2 - min(eigen_2))/(max(eigen_2)-min(eigen_2))
eigen_3 <- (eigen_3 - min(eigen_3))/(max(eigen_3)-min(eigen_3))

eigenmat_1 <- matrix(eigen_1,3)
eigenmat_2 <- matrix(eigen_2,3)
eigenmat_3 <- matrix(eigen_3,3)

par(mfrow=c(1,3))
plot(1:3,1:3, ann = FALSE, axes = FALSE, col = 0, main = "Eigenvector 1", xlab = "", ylab = "")
rasterImage(eigenmat_1,0,0,3,3)
plot(1:3,1:3, ann = FALSE, axes = FALSE, col = 0, main = "Eigenvector 2", xlab = "", ylab = "")
rasterImage(eigenmat_2,0,0,3,3)
plot(1:3,1:3, ann = FALSE, axes = FALSE, col = 0, main = "Eigenvector 3", xlab = "", ylab = "")
rasterImage(eigenmat_3,0,0,3,3)


noisy_pca$loadings
