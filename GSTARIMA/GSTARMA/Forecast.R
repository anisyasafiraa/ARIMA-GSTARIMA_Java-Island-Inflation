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
resUni <- read.csv("GSTARIMA/GSTARMA/GSTARMA Using Uniform Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resUni)[2]) {
  whiteNoise <- Box.test(resUni[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resUni, center = FALSE, WS)
round(dsquare, 3)

m1 <- c(1.374781, -0.07566, 0.705832, 1.30966, 1.473196, 0.035085)
m2 <- c(-0.68136, 0.387089, 0.418344, -0.56024, -0.85047, 0.354126)
t1 <- c(-0.44414, 0.346595, -0.28391, -0.23041, -0.53839, 0.330983)
t2 <- c(0.432615, 0.303479, 0.29531, 0.186847, 0.553564, 0.228802)
n <- length(t1)
mm1 <- diag(0, n)
mm2 <- diag(0, n)
mt1 <- diag(0, n)
mt2 <- diag(0, n)
for (i in 1:n) {
  mm1[i, i] <- m1[i]
  mm2[i, i] <- m2[i]
  mt1[i, i] <- t1[i]
  mt2[i, i] <- t2[i]
}
mpar <- mm1+mm2%*%WS
tpar <- mt1+mt2%*%WS

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
  dz <- mpar%*%as.matrix(t(allData[i-1,]))-tpar%*%as.matrix(t(resUni[i-1,]))
  resUni[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcUni[i,] <- c(i, round(dz, 2))
}

# ----------------------------Distance Inverse Matrix----------------------------
resInv <- read.csv("GSTARIMA/GSTARMA/GSTARMA Using Inverse Distance Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resInv)[2]) {
  whiteNoise <- Box.test(resInv[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resInv, center = FALSE, WIJ)
round(dsquare, 3)

m1 <- c(-0.15555, -0.13862, 0.619504, -0.69211, 1.234527, -0.06959)
m2 <- c(1.445115, 0.32785, 0.352327, 2.174984, -0.49305, 0.372857)
t1 <- c(0.566767, 0.409139, -0.03885, 0.928695, -0.27799, 0.456695)
t2 <- c(-0.76129, 0.0751, -0.20126, -1.28358, 0.251554, -0.12286)
n <- length(t1)
mm1 <- diag(0, n)
mm2 <- diag(0, n)
mt1 <- diag(0, n)
mt2 <- diag(0, n)
for (i in 1:n) {
  mm1[i, i] <- m1[i]
  mm2[i, i] <- m2[i]
  mt1[i, i] <- t1[i]
  mt2[i, i] <- t2[i]
}
mpar <- mm1+mm2%*%WIJ
tpar <- mt1+mt2%*%WIJ

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
  dz <- mpar%*%as.matrix(t(allData[i-1,]))-tpar%*%as.matrix(t(resInv[i-1,]))
  resInv[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcInv[i,] <- c(i, round(dz, 2))
}

# ---------------------------Cross Correlation Matrix---------------------------
resCor <- read.csv("GSTARIMA/GSTARMA/GSTARMA Using Cross Correlation Matrix Residual Result.csv")[, 4:9] # From SAS

for (i in 1:dim(resCor)[2]) {
  whiteNoise <- Box.test(resCor[i], type = "Ljung-Box")
}
# Chi-square table df=6 alpha=0.05 is 12.59
dsquare <- mahalanobis(resCor, center = FALSE, WKS)
round(dsquare, 3)

m1 <- c(1.340556, -0.08192, 0.504449, 0.395889, 1.217193, -0.04557)
m2 <- c(-0.50527, 0.307795, 0.458544, 0.774376, -0.45863, 0.360673)
t1 <- c(-0.40347, 0.297408, -0.06033, 0.341886, -0.56267, 0.379854)
t2 <- c(0.250606, 0.337791, -0.26464, -0.41098, 0.619415, 0.053297)
n <- length(t1)
mm1 <- diag(0, n)
mm2 <- diag(0, n)
mt1 <- diag(0, n)
mt2 <- diag(0, n)
for (i in 1:n) {
  mm1[i, i] <- m1[i]
  mm2[i, i] <- m2[i]
  mt1[i, i] <- t1[i]
  mt2[i, i] <- t2[i]
}
mpar <- mm1+mm2%*%WKS
tpar <- mt1+mt2%*%WKS

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
  dz <- mpar%*%as.matrix(t(allData[i-1,]))-tpar%*%as.matrix(t(resCor[i-1,]))
  resCor[i,] <- c(round(testTr[i-180,]-dz, 2))
  forcCor[i,] <- c(i, round(dz, 2))
}