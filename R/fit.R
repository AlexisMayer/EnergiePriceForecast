#' Fit models
#'
#' @param train A dataframe containing the training dataset.
#' @param test A dataframe containing the test dataset.
#' @param h The number of steps ahead to forecast.
#' @param serie The name of the column containing the time series data.
#'
#' @importFrom forecast snaive tbats
#' @importFrom dplyr full_join select
#' @return A dataframe containing the forecasted values and the actual values.
#' 
#' @export
fit = function(train, test, h, serie) {
  
  # Forecast
  pred_snaive = train[[serie]] %>% 
    ts(., delta = 1) %>% 
    snaive(h = h) %>% 
    as.data.frame() %>% 
    .[,1]

  pred_arima = train[[serie]] %>% 
    ts(., delta = 1) %>% 
    forecast::auto.arima(lambda = 0) %>% 
    forecast(h = h) %>% 
    as.data.frame() %>% 
    .[,1]
  
  pred = list(pred_snaive, pred_arima) %>% 
    reduce(cbind) %>% 
    rowMeans()
  
  # Merge
  tps = pred %>%
    cbind(test[,1], .) %>% 
    as_tibble()
  
  # Rename
  names(tps) = c("Date", serie)
  
  # Merge train
  tps = tps %>% 
    full_join(select(train, Date, serie), .) %>% 
    arrange(Date)
  
  # Output
  return(tps)
  
}
