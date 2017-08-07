
RMS <- function(b) {
  sqrt(sum(b^2)/length(b))
}

####

# 
processthe.files <- function(file, i) {
  data <- read.table(file, header=FALSE, sep="\t", row.names=NULL)
  bearing <- as.vector(data[,i])
  RMSVal <- RMS(bearing)
  CF <- if (abs(max(bearing)) >= abs(min(bearing))) {
    (abs(max(bearing))/RMSVal)
  }
  else {
    (abs(min(bearing))/RMSVal)
  }
  cbind(skewness(bearing), kurtosis(bearing), RMSVal, CF)
}


tuneR_EDA <- function(dataset,sample=TRUE){
  library(moments)
  source("R/tuneR_common.r")
  if(dataset == '2nd_test') {
    if(sample) count = 20
    else count = 984
    channel_count = 4
    data_path <- dataset_path(2)
    results_path<- results_path(2)
    }
  else if(dataset == '1st_test') {
    if(sample) count = 20
    else count = 2156
    channel_count = 8
    data_path <- dataset_path(1)
    results_path<- results_path(1)
  }
  else {
    count = 20
    channel_count =  4 
    data_path <- dataset_path(2)
    results_path<- results_path(2)
    
  }
  infiles <- list.files(data_path, pattern="*.*", full.names=TRUE)

#
# this function needs the following from the tuneR_common object
# the name of the test  ,"1st_test" or "2nd_test"
# the number of files i to read from those directory, 2156, or 984 or 20
  dataset_count <- 20 ##length(infiles)

for (channel in 1:channel_count) {
    df <- data.frame(kurt=rep(NA, dataset_count), skew=rep(NA, dataset_count), rms=rep(NA, dataset_count), cf=rep(NA, dataset_count))
    for (i in 1:dataset_count) {
        result <- processthe.files(infiles[i], channel)
        df[i,] <- result
    }
    write.csv(df, file.path(results_path, paste("channel_", channel, ".txt", sep='')), row.names=FALSE)
  }
}
    
