#' Import historical demand data
#'
#' @param country Country code in ISO 3166-1 alpha-2 format (default: "FR").
#' @return A dataframe containing the historical monthly demand data for the specified country over the last 10 years.
#' 
#' @export
importDemand <- function(country = "FR") {
  
  # Chargement des packages
  # library(eurostat)

  # Récupération des données
  # data <- get_eurostat(id = "nrg_bal_c")
  
  # Process
  # data <- data %>% 
  #   filter(geo == "FR", unit == "GWH", nrg_bal == "AFC", siec == "TOTAL") %>%
  #   select("Pays" = geo, unit, "Date" = time, values) %>% 
  #   mutate_at(vars(Date), ~ as.numeric(substr(.x, 1, 4))) 
  
  # Save 
  # write.csv2(data, "data/demand.csv")
  
  # Read 
  data = read.csv2("data/demand.csv")
  
  # Output 
  return(data)
  
}

