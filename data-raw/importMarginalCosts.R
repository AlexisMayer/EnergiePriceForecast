#' Import Marginal Costs of Energy by Technology
#'
#' @param data_path A string indicating the path to the data file. The default is "data/levelized_cost.csv"
#'
#' @return A data frame containing the marginal costs of energy by technology
#'
#' @export
importMarginalCosts = function() {
  
  # Load Demand
  data = read.csv2("data/levelized_cost.csv")

  # Output
  return(data)

}
