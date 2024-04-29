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
resUni <- read.csv("GSTARIMA/GSTMA/GSTMA Using Uniform Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resUni)[2]) {
  whiteNoise <- Box.test(resUni[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resUni, center = FALSE, WS)
round(dsquare, 3)

t1 <- c(0.87466, 0.37633, 0.83383, 0.96467, -0.37702, 0.43418)
t2 <- c(-0.013515, 0.31259, 0.30780, -0.21032, 3.28740, 0.22507)
n <- length(t1)
m1 <- diag(0, n)
m2 <- diag(0, n)
for (i in 1:n) {
  m1[i, i] <- t1[i]
  m2[i, i] <- t2[i]
}
tpar <- m1+m2%*%WS

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
  dz <- -tpar%*%as.matrix(t(resUni[i-1,]))
  resUni[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcUni[i,] <- c(i, round(dz, 2))
}

# ----------------------------Distance Inverse Matrix----------------------------
resInv <- read.csv("GSTARIMA/GSTMA/GSTMA Using Inverse Distance Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resInv)[2]) {
  whiteNoise <- Box.test(resInv[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resInv, center = FALSE, WIJ)
round(dsquare, 3)

t1 <- c(-0.08044, 0.262475, 0.64495, -0.29091, 0.959125, 0.190578)
t2 <- c(2.407512, 0.43759, -0.04384, 2.730367, -0.24595, 0.581672)
n <- length(t1)
m1 <- diag(0, n)
m2 <- diag(0, n)
for (i in 1:n) {
  m1[i, i] <- t1[i]
  m2[i, i] <- t2[i]
}
tpar <- m1+m2%*%WIJ

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
  dz <- -tpar%*%as.matrix(t(resInv[i-1,]))
  resInv[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcInv[i,] <- c(i, round(dz, 2))
}

# ---------------------------Cross Correlation Matrix---------------------------
resCor <- read.csv("GSTARIMA/GSTMA/GSTMA Using Cross Correlation Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resCor)[2]) {
  whiteNoise <- Box.test(resCor[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resCor, center = FALSE, WKS)
round(dsquare, 3)

t1 <- c(0.858022, 0.355948, 0.826122, 0.969612, -0.39353, 0.371318)
t2 <- c(-0.00337, 0.283667, 0.330116, -0.19355, 2.684873, 0.297799)
n <- length(t1)
m1 <- diag(0, n)
m2 <- diag(0, n)
for (i in 1:n) {
  m1[i, i] <- t1[i]
  m2[i, i] <- t2[i]
}
tpar <- m1+m2%*%WKS

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
  dz <- -tpar%*%as.matrix(t(resCor[i-1,]))
  resCor[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcCor[i,] <- c(i, round(dz, 2))
}