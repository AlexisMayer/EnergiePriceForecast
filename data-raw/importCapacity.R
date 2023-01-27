#' Import install capacity by Technology
#'
#' @param data_path A string indicating the path to the data file. The default is "data/install_capacity.csv"
#'
#' @return A data frame containing the install capacity of energy by technology
#'
#' @export
importCapacity = function() {
  
  # Load Demand
  data = read.csv2("data/install_capacity.csv")
  
  # Output
  return(data)
  
}
