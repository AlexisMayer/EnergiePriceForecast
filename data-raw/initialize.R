#' Split, Fit & Compute
#'
#' @param data A dataframe containing the historical data that needs to be forecasted.
#' @param len Number of forecasted points.
#' @param last_date Last date in the training set.
#' @param low The low scenario.
#' @param high The high scenario.
#' @param all A boolean indicating whether to return the scenarios or the forecasted values only.
#'
#' @return A dataframe containing the forecasted values for the input dataframe. If all is set to TRUE, the dataframe will also contain the low and high scenarios.
#' 
#' @export
initialize = function(data, len, last_date, low, high, all = TRUE) {
  
  # Split 
  h = as.numeric(len) - last_date
  train = data[1:(nrow(data) - h), ]
  test = data[(nrow(data) - h + 1):nrow(data), ]
  
  # Forecast 
  data = map(
    names(data)[-1], 
    ~ fit(
      serie = .x
      , train = train
      , test = test
      , h = h)) %>% 
    reduce(full_join)
  
  # Create column low & high
  data = map(
    names(data)[-1]
    , ~ scenario(
      data = data
      , serie = .x
      , last_date = last_date
      , low = low
      , high = high
      , all = all)) %>% 
    reduce(full_join)
  
  # Output
  return(data)
}

