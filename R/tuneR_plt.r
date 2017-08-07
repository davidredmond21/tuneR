
# Plot the various metrics
#
tuneR_plt<-function(dataset,sample=TRUE ,type=1, feature = "Penrose.txt" ){
  source("R/tuneR_common.r")
  library(ggplot2)
  library(reshape)
  library(viridis)
  path <- getwd()
  
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
#
## Read in the datafiles and build as suitable dataframe
#
# ADD cases for 8 channel
##  run the function with the planted variables 
  df <- tuneR_explr("2nd_test")   # this flat file has all features stacked  
  df$idx <- as.integer(row.names(df))
  colnames(df) <- c("Bearing 1", "Bearing 2", "Bearing 3", "Bearing 4", "idx")
  melted <- melt(df, id.vars="idx")
  if (type == 1) {
    plt <- ggplot(melted, aes(x=idx, y=value, group=variable, colour=variable)) + 
    geom_line() +
    xlab("Time") +
    ylab(feature)
  }
  else {
    plt <- ggplot(melted, aes(x=value, fill= variable, colour=variable)) + 
      geom_density(position="identity",  alpha=0.6, size = 1) +
      facet_wrap(~ variable ) +
      scale_x_continuous(name = "Distribution")+
      ggtitle("Density plot of extracted features" )
  }
  plt <- plt + theme_bw()
  plt <- plt + scale_color_viridis(discrete = TRUE)
  plt
}

tuneR_plt("2nd_test", TRUE, 1, "Penrose.txt")
tuneR_plt("2nd_test", TRUE, 2, "Penrose.txt")

