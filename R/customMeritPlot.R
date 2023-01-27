#' Plot merit order curve
#'
#' @param data A dataframe containing the data to be plotted. It should contain columns 'position', 'MC' (Marginal costs), 'Capacity' and 'Type'
#' @param prix The equilibrium price to be plotted on the y-axis
#' @param demand The demand to be plotted on the x-axis
#' 
#' @return A plotly object of the merit order curve
#' 
#' @import plotly
#' @import dplyr
#' 
#' @export
customMeritPlot = function(data, prix, demand) {
  plot_ly(data) %>%
    add_bars(
      x = ~ position
      , y = ~ MC
      , width = ~ Capacity
      , color = ~ Type
    ) %>% 
    layout(
      shapes = list(
        list(
          type = "line"
          , x0 = 0
          , x1 = demand
          , y0 = prix
          , y1 = prix
          , line = list(color = "grey", dash = "dot"))
        , list(
          type = "line"
          , x0 = demand
          , x1 = demand
          , y0 = 0
          , y1 = prix
          , line = list(color = "grey", dash = "dot"))
      )
    ) %>% 
    layout(
      annotations = list(
        list(
          x = demand
          , y = prix
          , text = paste("Prix d'équilibre :\n", round(prix), "€/MWh")
        )
      )
    ) %>% 
    layout(
      xaxis = list(title = "Power plant capacity (MW)")
      , yaxis = list(title = "Marginal costs (€/MWh)")
      , title = "Merit order curve"
    )
}
