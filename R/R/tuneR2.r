
# Only need to run this complete file onse to construct the datasets
#
#
#.libPaths(Sys.getenv('RLIBPATHS'))

library(moments)
library(testthat)
library(ggplot2)
library(robustbase)
library(reshape2)
library(tibble)
library(dplyr)
library(entropy)
library(roxygen2)
library(readr)
library(magrittr)
library(viridis)


# Prequisits
# Down load the nasa datasets and extract into your home account
# A sample is available in the dataset directory
# Edit the absolute path names in this file
#' Title TuneR package to explore vibration datasets
#' This assumes the NASA data has been extracted to the local directory
#'
#' @return Sets up user specific paths and list of files needed analysis
#'
#' @param type is the list of dataset files to be ingested
#' @param sample_size  the number of dataset samplles. using 0, or -1 will pick the all files available
#'
#' @export
#' @examples
#' ans=load_tunr("1st_test",10)
#' ans=load_tunr("1st_test",-1)
#'
load_tunr <-function( type=c("1st_test","2nd_test","4th_test"),sample_size) {  #select which dataset
   # Find out which dataset
   #
   arg = match.arg(type)
   dataset= arg
   if(dataset == '2nd_test') {
      channel_count = 4
      data_path=file.path(getwd(),'data',dataset)
      res_path=file.path(getwd(),'results',dataset)
   }
   if (dataset =='1st_test') {
      channel_count = 8
      data_path=file.path(getwd(),'data',dataset)
      results =file.path(getwd(),'results',dataset)
   }
   if (dataset =='4th_test') {
      channel_count = 4
      data_path=file.path(getwd(),'data',dataset)
      results =file.path(getwd(),'results',dataset)
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
                   results=results,
                   dataset = arg)
   return(out_list)
}
#' Title Mahalanobis calculation with coarse offset
#'
#' @param xx this is the dataframe
#' @param mah_offset this tries to center the mahanalobis
#'
#' @return single scalar value or vector
#' @export
#'
#' @examples
#' mah_vec= mah(df)
#' rms_vec %<>% mutate("mah"=mah(rms_vec))
mah   <-  function(xx,mah_offset=0) {
  mahalanobis(xx,colMeans(xx),var(xx))-mah_offset }
#
# Read the first data file to define the number of bearings index= i
# Read the directory list or infile to define the total number of samples index
# Execution process
# Strategy is to build a result for each bearing,
# these can then be merged later
# Calcualte each parameter for each bearing
# inputs are the object list form the load_tunr function
#  outputs a list of dataframes for individual features, and combined flat files
#' Title eda_tunr : will calculate the following statistics on the datafiles,RMS, Kurtosis, Entropy, Skewness, crestfactor Then it alos calculates the Mahalanobis distance of these features between the different bearings to explore of there is significant correlation
#'
#' @param Result is a list of raw vibration datafiles, the sample_size, the number of channels, and the dataset
#'
#' @return a list of datasets for each feature rms,kurtosis, entropy, creatfactor, skewness, and a flatfile will all plus the Mahalanobis   distances
#' @export
#' @importFrom stats "mahalanobis" "var"
#' @importFrom utils "read.table"
#' @importFrom readr "write_rds"
#' @importFrom dplyr   "as_tibble" "mutate"
#' @importFrom moments "skewness"  "kurtosis"
#' @import entropy
#' @import magrittr
#'
#' @examples
#' ans=load_tunr("1st_test",-1)
#' eda_tunr(ans)
#'
eda_tunr<-function(Result) {
   infiles = Result$infiles
   sample_size = Result$sample_size
   channel_count = Result$channel_count
   dataset = Result$dataset
   results = Result$results
   mah_offset = 4
   options(mc.cores = parallel::detectCores())

   #
   #
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
      rms_vec[k,]  <- apply(db,2,function(xx){sqrt(sum(xx*xx)/length(xx))} )
      skw_vec[k,]  <- apply(db,2,skewness)
      krt_vec[k,]  <- apply(db,2,kurtosis)
      cst_vec[k,]  <- apply(db,2,function(xx){max(abs(xx)/(sqrt(sum(xx^2))/length(xx) )) } )
      ent_vec[k,]  <- apply(db,2,function(xx){entropy(discretize(xx,numBins=length(xx))) } )
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
   write_rds(out_tbl, paste0(file.path(results), dataset,".rds"))
}
#
# generating nice plots
#' Title:  plot_tunr will produce plots for the calculated features
#' @return cool plots
#' @export
#'
#' @import dplyr
#' @importFrom ggplot2 "ggplot" "facet_grid" "stat_function"
#' @importFrom reshape2 "melt"
#' @importFrom readr "read_rds"
#' @importFrom viridis "scale_color_viridis"
#' @importFrom stats "loess" "dnorm" "na.omit"
#'
#' @examples
#' ans_df = eda_tunr(load_tunr("1st_test", 20 ))
#' plot_tunr("Mahalanobis_iid")
#' plot_tunr("Mahalanobis_ts")
#'
#' @param x_type is the freature type to be plotted, options are  "rms", "kurtosis","entropy",",crest","skewness","mahalanobis"
#' @param dataset is the name of the dataset "1st_test" or "2nd_test"
#' @param regen boolean default is false, only used in the case of re-generate the summary statistics and write out an large tibble
plot_tunr <-function(x_type,dataset="1st_test", regen=FALSE) {
   res_file=paste0(file.path(getwd(),'results',dataset),".rds")
  out_tbl=read_rds(res_file)
   if(!regen) print("reading pre-calculated results from results directory")
           else print("run re-generation of summary stats, will take some time...")
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
         facet_wrap(~ Summary ,scales='free')+
         scale_color_viridis(discrete=TRUE) +
         xlab("Time X 15min") +
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
         scale_color_viridis(discrete=TRUE) +
         ggtitle("RMS  plot with Loess modelling limits") +
         theme_bw() +
         xlab("Time X 15min") +
         ylab("RMS") }
   else if (x_type =="Kurtosis") {
      krt_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         stat_smooth(method = loess)+
         scale_color_viridis(discrete=TRUE) +
         theme_bw() +
         ggtitle("Kurtosis plot with Loess modelling limits") +
         xlab("Time X 15min") +
        ylab("Kurtosis") }

   else if (x_type =="Entropy") {
      ent_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
        ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
        ggtitle("Entropy plot with Loess modelling limits") +
        stat_smooth(method = loess)+
        scale_color_viridis(discrete=TRUE) +
        theme_bw() +
        xlab("Time X 15min") +
        ylab("Entropy") }
   #
   else if (x_type =="Crest") {
      cst_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         ggtitle("CrestFactor plot with Loess modelling limits") +
         stat_smooth(method = loess)+
         scale_color_viridis(discrete=TRUE) +
         xlab("Time X 15min") +
         ylab("Crest_Factor") }
   #
   else if (x_type =="Skewness") {
      skw_vec %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                       variable.name ="bearings") %>%
         ggplot(., aes(x=idx,y=value,colour= bearings)) + geom_line() +
         ggtitle("Skewness plot with Loess modelling limits") +
         stat_smooth(method = 'loess')+
         xlab("Time X 15min") +
         ylab("Skewness")
   }
   else if ( x_type=="Mahalanobis_ts") {
     df %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                 variable.name ="Summary") %>%
       ggplot(., aes(x=idx,y = Mahalanobis,colour= Summary ))+
       geom_line() +
       scale_color_viridis(discrete=TRUE) +
       stat_smooth(method = 'gam')+
       xlab("Time X 15min") +
       ylab("Mahalanobis") +
       ggtitle("Mahalanobis timeseries plot of Features with GAM modelling")
   }
   ##
   # looking at the density plot for Mahalanobis , etc we get to see if there are samples that are outside
   # a normal distribution... effectively we're using IID to qualify outliers/ anomalies
   #
   else if ( x_type=="Mahalanobis_iid") {
       df %>% melt(id.vars=c("Mahalanobis","idx", "Summary"),
                  variable.name ="Summary") %>% na.omit()%>%
         ggplot(., aes(x = Mahalanobis,fill=Summary, colour= Summary))+
         geom_density() +
         scale_color_viridis(discrete=TRUE) +
         theme_light()+
         stat_function(fun = dnorm, colour="purple",alpha=0.9 ) +
         facet_grid( ~ Summary, scales = 'free_x') +
         ggtitle("Mahalanobis Density plot (centered) of Features with Normal density")
   }
   #
}



