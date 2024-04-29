# --------------------------------Uniform Matrix--------------------------------
n <- dim(data)[2]
WS <- diag(0, n)
for (i in 1:n) {
  weight <- 1/n
  for (k in 1:(n-1)) {
    if (k<i) {WS[i, k] <- weight}
    else {WS[i, k+1] <- weight}
  }
}
round(WS, 3)

# ----------------------------Inverse Distance Matrix----------------------------
library(rgdal)
library(spdep)

java <- readOGR("~/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")
java <- java[c(3, 5, 6, 9, 10, 11),]

coord <- coordinates(java)
d <- dnearneigh(coord, 0, 1000)
dlist <- nbdists(d, coord)
idlist <- lapply(dlist, function(x) 1/x)

W <- nb2listw(d, glist = idlist, style = "W",
              zero.policy = TRUE)

n <- length(idlist)
WIJ <- diag(0, n)
for (i in 1:n) {
  weight <- unlist(W$weights[i])
  for (k in 1:(n-1)) {
    if (k<i) {WIJ[i, k] <- weight[k]}
    else {WIJ[i, k+1] <- weight[k]}
  }
}
round(WIJ, 3)

# ---------------------------Cross Correlation Matrix---------------------------
r <- diag(0, n)
k <- 1
dt <- as.matrix(dataOri[1:180,])
for (i in 1:n) {
  for (j in 1:(n-1)) {
    if (j<i) {
      a <- sum((dt[(k+1):180,i]-mean(dt[,i]))*(dt[1:(180-k),j]-mean(dt[,j])))
      b <- sum((dt[1:180,i]-mean(dt[,i]))^2)*sum((dt[1:180,j]-mean(dt[,j]))^2)
      rij <- a/sqrt(b)
      r[i, j] <- rij
    } 
    else {
      a <- sum((dt[(k+1):180,i]-mean(dt[,i]))*(dt[1:(180-k),j+1]-mean(dt[,j+1])))
      b <- sum((dt[1:180,i]-mean(dt[,i]))^2)*sum((dt[1:180,j+1]-mean(dt[,j+1]))^2)
      rij <- a/sqrt(b)
      r[i, j+1] <- rij
    }
  }  
}
WKS <- diag(0, n)
for (i in 1:n) {
  for (j in 1:(n-1)) {
    if (j<i) {
      w <- abs(r[i,j])/sum(abs(r[i,]))
      WKS[i, j] <- w
    } else {
      w <- r[i,j+1]/sum(r[i,])
      WKS[i, j+1] <- w
    }
  }
}
round(WKS, 3)