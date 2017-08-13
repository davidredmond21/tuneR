
# Only need to run this complete file onse to construct the datasets 
#  
#
library(moments)
library(ggplot2)
library(robustbase)
library(reshape2)
library(tibble)
library(dplyr)
library(entropy)
#  
#' Title TuneR package to explore vibration datasets 
#' This assumes the NASA data has been extracted to the local directory
#'
#' @return Sets up user specific paths 
#' @export
#'
#' @examples
#' 
machine_health_data_path <- function() {
      path <- getwd() ## "/home/david/Desktop/Advanced_R/assign/tuneR"
      }
dataset_path <- function(i) {
      dataset <- c('1st_test', '2nd_test')
      file.path(machine_health_data_path(), 'datasets', dataset[i])
   }
results_path <- function(i) {
      dataset <- c('dataset_1', 'dataset_2')
      file.path(machine_health_data_path(), 'results', dataset[i])
   }
dataset_properties <- function(i) {
      channels <- c(8, 4)
      count <- c(2156, 984)
      c(channels[i], count[i])
   }
# Prequisits
# Down load the nasa datasets and extract into your home account
# A sample is available in the dataset directory 
# Edit the absolute path names in this file
# The graphs are not stored, you can use the back scroll arrow to view
#'load_tunr : selects te list of files. it is assumed that
#'the user has downloaded the nasa datasets and extract into your home account
#'however this package will use a local dataset directory with a sample set of those files
#'
#' @param type a character string that identifies what dastaset to use , either "1st_test" or "2nd_test".
#' @param sample_size vibration datasets are large 984 to 2000 so this allows the user to select a sample size. if a sample_size of -1 is entered then the entire dataset is used
#'
#' @return returns a list of individual vibration files, the sample size, number of vibration channels, and datapath
#' @export
#'
#' @examples 
#' Result = load_tunr("1st_test", 20 )
#' Result = load_tunr("2nd_test", -1 )
#'
load_tunr <-function( type=c("1st_test","2nd_test"),sample_size) {  #select which dataset
   # Find out which dataset
   #
   arg = match.arg(type)
   dataset= arg
   #dataset = '2nd_test'
   if(dataset == '2nd_test') {
      channel_count = 4
      data_path = dataset_path(2)
   }
   if (dataset =='1st_test') {
      channel_count = 8
      data_path <- dataset_path(1)
   }
   #sample_size= 18
   total_infiles = list.files(data_path, pattern="*.*", full.names=TRUE)
   if (sample_size < 1) sample_size= length(total_infiles)
   infiles = sample(total_infiles, sample_size)
   # Put it all in a list and return
   out_list = list(infiles = infiles,
                   channel_count = channel_count,
                   data_path = data_path,
                   sample_size = sample_size,
                   dataset = arg)
   return(out_list)
}
# Read the first data file to define the number of bearings index= i
# Read the directory list or infile to define the total number of samples index 
# Execution process
# Strategy is to build a result for each bearing, 
# these can then be merged later
# Calcualte each parameter for each bearing
# inputs are the object list form the load_tunr function
# outputs a list of dataframes for individual features, and combined flat files
#
# As yet there are not options for selecting which features to choose
#
#' eda_tunr : will calculate the following statistics on the datafiles
#' RMS, Kurtosis, Entropy, Skewness, crestfactor
#' Then it aloss calculates the Mahalanobis distance of these features between the different bearings to 
#' explore of there is significant correlation
#'
#' @param Result is a list of raw vibration datafiles, the sample_size, the number of channels, and the dataset
#'
#' @return a list of datasets for each feature rms,kurtosis, entropy, creatfactor, skewness, and a flatfile will all plus the Mahalanobis   distances

#' @importFrom dplyr "mutate" "select" 
#' @importFrom moments "kurtosis","skewness"
#' @importFrom entropy "entropy","discretize"
#' @importFrom robustbase "mahalanobisD"
#' @importFrom tibble "as_tibble"
#' @examples
#' 
#' eda_tunr<-function(Result=list("filenames",channel_count,data_path,sample_size,dataset) 
#' 
##### when it see Result it will callthis function
eda_tunr<-function(Result) {
   infiles = Result$infiles
   sample_size = Result$sample_size
   channel_count = Result$channel_count
   dataset = Result$dataset
   #
   # bunch of useful functions 
   rms_o  <- function(xx){sqrt(sum(xx*xx)/length(xx) )  }  # function returns the RMS values of each vector in the dataframe
   kurt_o <- function(xx){kurtosis(xx)}
   CF     <- function(xx){max(abs(xx)/rms_o(xx) ) }  # returns the Crest Factor value for each vector in the dataframe
   ENT    <- function(xx){entropy(discretize(xx,numBins=length(xx)))}
   mah   <-  function(xx){mahalanobisD(xx,colMeans(xx),var(xx))}
   # euc   <-  function(xx){dist(xx,method="euclidean")[1:10]}
   #mann  <-function(xx){ dist(xx,method="manhattan")[1:10]}
   #mink  <-function(xx){ dist(xx,method="minkowski")[1:10]}
   
   dataset_count <- length(infiles)
   rms_vec  <-matrix(data=NA,nrow=sample_size,ncol=channel_count)
   krt_vec <- matrix(data=NA,nrow=sample_size,ncol=channel_count)
   cst_vec <- matrix(data=NA,nrow=sample_size,ncol=channel_count)
   skw_vec <- matrix(data=NA,nrow=sample_size,ncol=channel_count)
   ent_vec <- matrix(data=NA,nrow=sample_size,ncol=channel_count)
   #
   k=1
   for (i in infiles) {
      db<-read.table(i,header=FALSE,sep="\t",row.names = NULL)
      rms_vec[k,]  <- apply(db,2,rms_o)
      skw_vec[k,]  <- apply(db,2,skewness)
      krt_vec[k,]  <- apply(db,2,kurt_o) 
      cst_vec[k,]  <- apply(db,2,CF)
      ent_vec[k,]  <- apply(db,2,ENT)
      k<-k+1
   }
   #
   rms_vec%<>%as_tibble()
   krt_vec%<>%as_tibble()
   skw_vec%<>%as_tibble()
   cst_vec%<>%as_tibble()
   ent_vec%<>%as_tibble()
   #
   N = nrow(rms_vec)
   #
   rms_vec %<>% mutate("mah"=mah(rms_vec))#  
   skw_vec %<>% mutate("mah"=mah(skw_vec))#   
   krt_vec %<>% mutate("mah"=mah(krt_vec))#  
   cst_vec %<>% mutate("mah"=mah(cst_vec))#  
   ent_vec %<>% mutate("mah"=mah(ent_vec))#  
   #glimpse(ent_vec)
   #
   idx = seq(1:N)
   #
   rms_vec %<>% cbind(.,rep("rms",nrow(rms_vec)))%>% cbind(.,idx)
   skw_vec %<>% cbind(.,rep("skw",nrow(skw_vec)))%>% cbind(.,idx)
   krt_vec %<>% cbind(.,rep("krt",nrow(krt_vec)))%>% cbind(.,idx)
   cst_vec %<>% cbind(.,rep("cst",nrow(cst_vec)))%>% cbind(.,idx)
   ent_vec %<>% cbind(.,rep("ent",nrow(ent_vec)))%>% cbind(.,idx)
   #
   # need to provide some meaningful column heading 
   if(channel_count==4)   data_names = c("v1","v2","v3","v4","Mahalanobis","Summary",'idx') 
   else if(channel_count==8) 
      data_names = c("v1","v2","v3","v4","v5","v6","v7","v8","Mahalanobis","Summary",'idx') 
   colnames(rms_vec)<-data_names
   colnames(skw_vec)<-data_names
   colnames(krt_vec)<-data_names
   colnames(cst_vec)<-data_names
   colnames(ent_vec)<-data_names
   # Combine All the feature vectors into a single dataframe 
   flatten<-Reduce(function(x, y) merge(x, y, all=TRUE), list(rms_vec,krt_vec,skw_vec,cst_vec,ent_vec))
   flatten%<>%as_tibble()
   # covert my flatfile to a tibble
   #
   df = flatten
   # Output so it can be plotted
   out_tbl = list(rms_vec = rms_vec,
                  skw_vec = skw_vec,
                  krt_vec = krt_vec,
                  cst_vec = cst_vec,
                  ent_vec = ent_vec,
                  flatten = flatten)
   return(out_tbl)
}
#
# generating nice plots
#
#' plot_tunr will produce plots for the calculated features 
#' so that you can explore the structure of each of the summarized features for each of the bearing measurements
#' looks at the response over time
#' These plots looks at the RMS , Kurtosis, Entropy, CrestFactor , and Skewness with parametric modelling 
#' the value here is to show the CI on each
#'  where a large CI exists there could be outliers or indication of a faulty bearing 
#' @param x_type is the freature type to be plotted, options are 
#' "rms", "kurtosis","entropy",",crest","skewness",mahalanobis"
#' @param out_tbl is the list of datasets generated by \code{\link{eda_tunr}}
#'
#' @importFrom dplyr "mutate" "select" "arrange" "%>%"
#' @importFrom ggplot2 "ggplot"
#' @importFrom reshape2 "melt"  
#' @examples
#' obj2 = eda_tunr(load_tunr("1st_test", 20 ))
#' plot_tunr("rms",obj2)

plot_tunr <-function(x_type, out_tbl) {
   df      = out_tbl$flatten 
   rms_vec = out_tbl$rms_vec
   krt_vec = out_tbl$krt_vec
   ent_vec = out_tbl$ent_vec
   cst_vec = out_tbl$cst_vec
   skw_vec = out_tbl$skw_vec
   # exploting the structure of each of the summarized features for each of the bearing measurements
   # looks at the response over time
   if (x_type=="features") {
      df %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                  variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         ggtitle("All features for each bearings") +
         facet_wrap(~ Summary )+
         xlab("Time") +
         ylab("Features") }
   else if (x_type =="RMS") {
      ### these plots looks at the RMS , Kurtosis, Entropy, CrestFactor , and Skewness with parametric modelling 
      # the value here is to show the CI on each
      # where a large CI exists there could be outliers or indication
      # of a faulty bearing 
      rms_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_point() +
         stat_smooth(method = loess)+
         ggtitle("RMS  plot with Loess modelling limits") +
         theme_bw() +
          xlab("Time") +
         ylab("RMS") }
   else if (x_type =="Kurtosis") {
      krt_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         stat_smooth(method = loess)+
         theme_bw() +
          ggtitle("Kurtosis plot with Loess modelling limits") +
         xlab("Time") +
         ylab("Kurtosis") }
   
   else if (x_type =="Entropy") {
      ent_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         ggtitle("Entropy plot with Loess modelling limits") +
         stat_smooth(method = loess)+
         theme_bw() +    
          xlab("Time") +
         ylab("Entropy") }
   #
   else if (x_type =="Crest") {
      cst_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         ggtitle("CrestFactor plot with Loess modelling limits") +
         stat_smooth(method = loess)+
          xlab("Time") +
         ylab("Crest_Factor") }
   #
   else if (x_type =="Skewness") {
      skw_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         ggtitle("Skewness plot with Loess modelling limits") +
         stat_smooth(method = loess)+
          xlab("Time") +
         ylab("Skewness")
   }
   ##
   # looking at the density plot for Mahalanobis , etc we get to see if there are samples that are outside 
   # a normal distribution... effectively we're using IID to qualify outliers/ anomalies
   #
   else if ( x_type== "mahalanobis") {
      df %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                  variable.name ="bearings") %>% na.omit()%>%
         ggplot(., aes(x = Mahalanobis,colour= bearings))+
         geom_density() +
         stat_function(fun = dnorm, colour="forestgreen",alpha=0.6 ) +
         facet_wrap( ~ bearings) +
         ggtitle("Mahalanobis Density plot of Bearings with Normal density") 
   }
   # 
}



