# This assumes the bearing files from Appendix 1 are in the C:\ directory.
library(moments)
tuneR_explr<-function(dataset,sample=TRUE){
    library(robustbase)
    source("R/tuneR_common.r")
    if(dataset == '2nd_test') {
      if(sample) count = 20
      else count = 984
      channel_count = 4
      data_path <- dataset_path(2)
      results_path<- dataset_path(2)
    }
    else if(dataset == '1st_test') {
      if(sample) count = 20
      else count = 2156
      channel_count = 8
      data_path <- dataset_path(1)
      results_path<- dataset_path(1)
    }
    else {
      count = 20
      channel_count =  4 
      data_path <- dataset_path(2)
      results_path<- dataset_path(2)
      
    }
 
source("R/tuneR_common.r")

  
b1 <- read.csv(file.path(data_path, "channel_1.txt"))
b2 <- read.csv(file.path(data_path, "channel_2.txt"))
b3 <- read.csv(file.path(data_path, "channel_3.txt"))
b4 <- read.csv(file.path(data_path, "channel_4.txt"))
# Grouping Kurtosis into a single dataset, to look for correlations


col1 <- c(b1[,1], b2[,1], b3[,1],b4[,1])
col2 <- c(b1[,2], b2[,2], b3[,2],b4[,2])
col3 <- c(b1[,3], b2[,3], b3[,3],b4[,3])
col4 <- c(b1[,4], b2[,4], b3[,4],b4[,4])
dataset <- cbind(col1,col2,col3,col4)
col1Norm <- ((col1-min(col1))/(max(col1)-min(col1)))
col2Norm <- ((col2-min(col2))/(max(col2)-min(col2)))
col3Norm <- ((col3-min(col3))/(max(col3)-min(col3)))
col4Norm <- ((col4-min(col4))/(max(col4)-min(col4)))

if (channel_count == 8) { 
  b5 <- read.csv(file.path(data_path, "channel_5.txt"))
  b6 <- read.csv(file.path(data_path, "channel_6.txt"))
  b7 <- read.csv(file.path(data_path, "channel_7.txt"))
  b8 <- read.csv(file.path(data_path, "channel_8.txt"))
  # Grouping Kurtosis into a single dataset, to look for correlations
  col5 <- c(b5[,1], b6[,1], b7[,1],b8[,1])
  col6 <- c(b5[,2], b6[,2], b7[,2],b8[,2])
  col7 <- c(b5[,3], b6[,3], b7[,3],b8[,3])
  col8 <- c(b5[,4], b6[,4], b7[,4],b8[,4])
  dataset <- cbind(dataset,col5,col6,col7,col8)
  col5Norm <- ((col5-min(col5))/(max(col5)-min(col5)))
  col6Norm <- ((col6-min(col6))/(max(col6)-min(col6)))
  col7Norm <- ((col7-min(col7))/(max(col7)-min(col7)))
  col8Norm <- ((col8-min(col8))/(max(col8)-min(col8)))
  }
################## this dataset is terrible ######

meansNorm <- c(median(col1Norm), median(col2Norm), median(col3Norm), median(col4Norm))
means <- c(median(col1), median(col2), median(col3), median(col4))
Sx <- cov(dataset)
res <- covMcd(x = dataset)
var <- c(var(col1), var(col2), var(col3), var(col4))

b1Norm <- cbind(col1Norm[1:count],col2Norm[1:count],col3Norm[1:count],col4Norm[1:count])
b2Norm <- cbind(col1Norm[(count+1):(2*count)],col2Norm[(count+1):(2*count)],col3Norm[(count+1):(2*count)],col4Norm[(count+1):(2*count)])
b3Norm <- cbind(col1Norm[(2*count+1):(3*count)],col2Norm[(2*count+1):(3*count)],col3Norm[(2*count+1):(3*count)],col4Norm[(2*count+1):(3*count)])
b4Norm <- cbind(col1Norm[(3*count+1):(4*count)],col2Norm[(3*count+1):(4*count)],col3Norm[(3*count+1):(4*count)],col4Norm[(3*count+1):(4*count)])

df <- data.frame(V1=rep(NA, count), V2=rep(NA, count), V3=rep(NA, count), V4=rep(NA, count))
for (x in 1:count) {
    Bearing1Euclidean <- sqrt(sum((b1Norm[x,] - meansNorm)^2))
    Bearing2Euclidean <- sqrt(sum((b2Norm[x,] - meansNorm)^2))
    Bearing3Euclidean <- sqrt(sum((b3Norm[x,] - meansNorm)^2))
    Bearing4Euclidean <- sqrt(sum((b4Norm[x,] - meansNorm)^2))
    df[x,] <- c(Bearing1Euclidean, Bearing2Euclidean, Bearing3Euclidean, Bearing4Euclidean)
    }
write.csv(df, file.path(data_path, "Euclidean.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, count), V2=rep(NA, count), V3=rep(NA, count), V4=rep(NA, count))
for (x in 1:count) {
    Bearing1Manhattan <- sum(abs(b1Norm[x,] - meansNorm))
    Bearing2Manhattan <- sum(abs(b2Norm[x,] - meansNorm))
    Bearing3Manhattan <- sum(abs(b3Norm[x,] - meansNorm))
    Bearing4Manhattan <- sum(abs(b4Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Manhattan, Bearing2Manhattan, Bearing3Manhattan,Bearing4Manhattan)
}
write.csv(df, file.path(data_path, "Manhattan.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, count), V2=rep(NA, count), V3=rep(NA, count), V4=rep(NA, count))
for (x in 1:count) {
    Bearing1Chebyshev <- max(abs(b1Norm[x,] - meansNorm))
    Bearing2Chebyshev <- max(abs(b2Norm[x,] - meansNorm))
    Bearing3Chebyshev <- max(abs(b3Norm[x,] - meansNorm))
    Bearing4Chebyshev <- max(abs(b4Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Chebyshev, Bearing2Chebyshev, Bearing3Chebyshev,Bearing4Chebyshev)
}
write.csv(df, file.path(data_path, "Chebyshev.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, count), V2=rep(NA, count), V3=rep(NA, count), V4=rep(NA, count))
for (x in 1:count) {
    Bearing1Penrose <- sum(((b1[x,] - means)^2/(var*length(var))))
    Bearing2Penrose <- sum(((b2[x,] - means)^2/(var*length(var))))
    Bearing3Penrose <- sum(((b3[x,] - means)^2/(var*length(var))))
    Bearing4Penrose <- sum(((b3[x,] - means)^2/(var*length(var))))
    df[x,] <- c(Bearing1Penrose, Bearing2Penrose, Bearing3Penrose,Bearing4Penrose)
}
write.csv(df, file.path(data_path, "Penrose.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, count), V2=rep(NA, count), V3=rep(NA, count), V4=rep(NA, count))
Bearing1Mahalanobis <- mahalanobis(b1,means,Sx)
Bearing2Mahalanobis <- mahalanobis(b2,means,Sx)
Bearing3Mahalanobis <- mahalanobis(b3,means,Sx)
Bearing4Mahalanobis <- mahalanobis(b4,means,Sx)
toPrint <- cbind(Bearing1Mahalanobis,Bearing2Mahalanobis,Bearing3Mahalanobis, Bearing4Mahalanobis)
for (i in 1:count) {
    df[i,] <- c(toPrint[i,1], toPrint[i,2],toPrint[i,3] ,toPrint[i,4])
}
write.csv(df, file.path(data_path, "Mahalanobis.txt"), row.names=FALSE)
return(df)
}


