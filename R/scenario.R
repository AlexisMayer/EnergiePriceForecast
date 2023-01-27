#' Compute low and high deviation scenarios
#'
#' @param data A dataframe containing the historical data for the specified serie
#' @param serie The name of the serie for which the low and high deviation scenarios will be computed
#' @param last_date The date of the last observation used to compute the scenarios
#' @param low The low deviation percentage (default: 5)
#' @param high The high deviation percentage (default: 5)
#' @param all If TRUE, return all columns of the dataframe, if FALSE, return only the Date, serie, serie_low and serie_high columns (default: TRUE)
#' @return A dataframe containing the historical data with additional columns for the expected values, low deviation and high deviation scenarios
#' 
#' @export
scenario = function(data, serie, last_date, low, high, all) {
  
  # Initialize
  data = data %>% 
    select(-contains(paste0(serie, "_tps")),
           -contains(paste0(serie, "_low")),
           -contains(paste0(serie, "_high"))) %>% 
    mutate(across(!!sym(serie), ~ NA, .names = '{.col}_tps')) %>% 
    mutate(across(!!sym(serie), ~ NA, .names = '{.col}_low')) %>% 
    mutate(across(!!sym(serie), ~ NA, .names = '{.col}_high'))
  
  # Find position 
  id = which(names(data) == serie)
  idl = which(names(data) == paste0(serie, "_low"))
  idh = which(names(data) ==  paste0(serie, "_high"))
  
  # Add expected values
  data[[paste0(serie,"_tps")]][1:which(data[["Date"]] == last_date)] = data[[serie]][1:which(data[["Date"]] == last_date)]
  data[[paste0(serie,"_low")]][1:which(data[["Date"]] == last_date)] = data[[serie]][1:which(data[["Date"]] == last_date)]
  data[[paste0(serie,"_high")]][1:which(data[["Date"]] == last_date)] = data[[serie]][1:which(data[["Date"]] == last_date)]
  data[[paste0(serie,"_tps")]][nrow(data)] = data[[serie]][nrow(data)]
  data[[paste0(serie,"_low")]][nrow(data)] = data[[serie]][nrow(data)] * ( 1 + low / 100 )
  data[[paste0(serie,"_high")]][nrow(data)] = data[[serie]][nrow(data)] * ( 1 + high / 100 )
  
  # Usefull function
  thales = function(y, p) { 
    A = data[["Date"]][p] - data[["Date"]][which(data[["Date"]] == last_date)]
    B = data[[y]][nrow(data)] - data[[y]][which(data[["Date"]] == last_date)]
    C = data[["Date"]][nrow(data)] - data[["Date"]][which(data[["Date"]] == last_date)]
    # Output
    return(A*B/C)
  }
  
  # Use it 
  for(p in (which(data[["Date"]] == last_date)+1):(nrow(data)-1)) {
    data[[paste0(serie,"_tps")]][p] = thales(paste0(serie,"_tps"), p) + data[[serie]][which(data[["Date"]] == last_date)]
    data[[paste0(serie,"_low")]][p] = thales(paste0(serie,"_low"), p) + data[[serie]][which(data[["Date"]] == last_date)]
    data[[paste0(serie,"_high")]][p] = thales(paste0(serie,"_high"), p) + data[[serie]][which(data[["Date"]] == last_date)]
  }
  
  # Compute theorical deviation
  data[[paste0(serie,"_low")]] = data[[paste0(serie,"_low")]] / data[[paste0(serie,"_tps")]] 
  data[[paste0(serie,"_high")]] = data[[paste0(serie,"_high")]] / data[[paste0(serie,"_tps")]] 
  
  # Report it on the true realisation
  data[[paste0(serie,"_low")]] = data[[paste0(serie,"_low")]] * data[[serie]] 
  data[[paste0(serie,"_high")]] = data[[paste0(serie,"_high")]] * data[[serie]] 
  
  # Output 
  if(all == TRUE) {
    return(select(data, 1:id, idl, idh, everything(), -contains("_tps")))
  } else {
    return(data[, c(1,id, idl,idh)])
  }
  
}

