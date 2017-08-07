# get the basic paths set up
#
machine_health_data_path <- function() {
    path <- getewd()
}
dataset_path <- function(i) {
    dataset <- c('1st_test', '2nd_test')
    file.path(machine_health_data_path(), 'datasets', dataset[i])

results_path <- function(i) {
    dataset <- c('dataset_1', 'dataset_2')
    file.path(machine_health_data_path(), 'results', dataset[i])
}
# the main configuration function for tuneR_common

tuneR_common = function(obj, data_set = c('1st_test', '2nd_test')) {
# Find out which data set to use
  fit_dat = match.arg(data_set)
  # Find out which bit of the data to take
    dat_choose = switch(data_set, "1st_test" = 1, "2nd_test" = 2)
  # Get the data set to use
  curr_dat = obj %>% extract2(dat_choose)
  
  # Fit some models
  if(data_set == '2nd_test') count = 984 
    else if(fit_dat == '2nd_test') count = 984 
    else count = 20
  print(count)
  # Output so it can be plotted
  out = list(model = count,
             data = curr_dat,
             dat_type = fit_dat)
  class(out) = 'tuneR'
#  invisible(out)
}


  
  



dataset_properties <- function(obj, channels <-c( 8, 4), 
                      ((if dataset[i]=="1st_test") count=2156,
                      (else if dataset[i] == "2nd_test) count= 984,
                      count =20),
                      c(channels[i], count[i]) {
  UseMethod('dataset_properties')
                      }

