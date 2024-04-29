setwd("D:/0.coolyeah/0.skripsi/github")
source('Stationary Test.R')

library(readxl)
data <- read_excel("data.xlsx")

# -------------------------------Stationary check-------------------------------
stData <- data[1:180,]
for (i in 4:9) {
  stData[1:180,i] <- varStationary(stData[1:180, i])$data
  stData <- data.frame(round(stData, 2))
  stData[1:180,i] <- meanStationary(stData[1:180, i])$data
}

# ---------------------------------Testing Data---------------------------------
test <- data.frame(data[181:192,4:9])
testTr <- data.frame(Banten = (test$Banten+1)^(0.25), DIY = log(test$DIY+1),
                     DKI.Jakarta = (test$DKI.Jakarta+1)^(-0.5),
                     Jawa.Barat = (test$Jawa.Barat+1)^(0.25),
                     Jawa.Tengah = (test$Jawa.Tengah+1)^(0.25),
                     Jawa.Timur = log(test$Jawa.Timur+1))

allData <- data.frame(stData[,4:9])
allData[181:192,] <- testTr

# All matrix from 'Weight Matrix.R'
# --------------------------------Uniform Matrix--------------------------------
resUni <- read.csv("GSTARIMA/GSTAR/GSTAR Using Uniform Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resUni)[2]) {
  whiteNoise <- Box.test(resUni[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resUni, center = FALSE, WS)
round(dsquare, 3)

m1 <- c(1.173675, 0.263698, 0.767625, 1.260119, 1.351706, 0.324877)
m2 <- c(-0.3235, 0.269139, 0.325251, -0.47391, -0.63805, 0.251376)
n <- length(m1)
mm1 <- diag(0, n)
mm2 <- diag(0, n)
for (i in 1:n) {
  mm1[i, i] <- m1[i]
  mm2[i, i] <- m2[i]
}
mpar <- mm1+mm2%*%WS

# Create table for forecast result
colForc <- c("t", "Banten", "DIY", "DKI", "Jabar", "Jateng", "Jatim")
forcUni <- data.frame(matrix(nrow = 0, ncol = length(colForc)))
colnames(forcUni) <- colForc

forcUni[1,] <- c(1, data[1,4:9])
for (i in 2:dim(resUni)[1]) {
  forc <- data[i,4:9]-resUni[i,]
  forcUni[i,] <- c(i, round(as.numeric(forc), 2))
}

# Forecast testing data
for (i in 181:192) {
  dz <- mpar%*%as.matrix(t(allData[i-1,]))
  resUni[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcUni[i,] <- c(i, round(dz, 2))
}

# ----------------------------Distance Inverse Matrix----------------------------
resInv <- read.csv("GSTARIMA/GSTAR/GSTAR Using Inverse Distance Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resInv)[2]) {
  whiteNoise <- Box.test(resInv[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resInv, center = FALSE, WIJ)
round(dsquare, 3)

m1 <- c(0.394775, 0.256997, 0.649867, 0.53185, 1.152046, 0.284299)
m2 <- c(0.755148, 0.215492, 0.322169, 0.59741, -0.32484, 0.24972)
n <- length(m1)
mm1 <- diag(0, n)
mm2 <- diag(0, n)
for (i in 1:n) {
  mm1[i, i] <- m1[i]
  mm2[i, i] <- m2[i]
}
mpar <- mm1+mm2%*%WIJ

# Create table for forecast result
forcInv <- data.frame(matrix(nrow = 0, ncol = length(colForc)))
colnames(forcInv) <- colForc

forcInv[1,] <- c(1, data[1,4:9])
for (i in 2:dim(resInv)[1]) {
  forc <- data[i,4:9]-resInv[i,]
  forcInv[i,] <- c(i, round(as.numeric(forc), 2))
}

#forecast outsample
for (i in 181:192) {
  dz <- mpar%*%as.matrix(t(allData[i-1,]))
  resInv[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcInv[i,] <- c(i, round(dz, 2))
}

# ---------------------------Cross Correlation Matrix---------------------------
resCor <- read.csv("GSTARIMA/GSTAR/GSTAR Using Cross Correlation Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resCor)[2]) {
  whiteNoise <- Box.test(resCor[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resCor, center = FALSE, WKS)
round(dsquare, 3)

m1 <- c(1.168243, 0.256997, 0.649867, 0.53185, 1.152046, 0.284299)
m2 <- c(-0.25628, 0.215492, 0.322169, 0.59741, -0.32484, 0.24972)
n <- length(m1)
mm1 <- diag(0, n)
mm2 <- diag(0, n)
for (i in 1:n) {
  mm1[i, i] <- m1[i]
  mm2[i, i] <- m2[i]
}
mpar <- mm1+mm2%*%WKS

# Create table for forecast result
forcCor <- data.frame(matrix(nrow = 0, ncol = length(colForc)))
colnames(forcCor) <- colForc

forcCor[1,] <- c(1, data[1,4:9])
for (i in 2:dim(resCor)[1]) {
  forc <- data[i,4:9]-resCor[i,]
  forcCor[i,] <- c(i, round(as.numeric(forc), 2))
}

#forecast outsample
for (i in 181:192) {
  dz <- mpar%*%as.matrix(t(allData[i-1,]))
  resCor[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcCor[i,] <- c(i, round(dz, 2))
}