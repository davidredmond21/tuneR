
machine_health_data_path <- function() {
    path <- "/home/david/Desktop/Advanced_R/assign/tuneR"
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
choices = c("Euclidean","Manhattan", "Chebyshev", "Penrose","Mahalanobis")

