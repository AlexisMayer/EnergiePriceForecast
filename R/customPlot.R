#' Render a line plot
#'
#' @param data A dataframe containing the historical data for the specified serie
#' @param serie The name of the serie for which the plot will be rendered
#' @param lim A value used as a threshold for the plot, a line will be drawn on this value
#' @return A plotly object containing the line plot of the specified serie with additional lines for the low and high deviation scenarios
#' 
#' @importFrom plotly plot_ly add_lines add_trace
#' @importFrom dplyr select contains all_of
#' @importFrom tidyr gather
#' 
#' @export
customPlot = function(data, serie, lim) {
  data %>% 
    select(Date, all_of(serie)
           , contains(paste0(serie, "_low"))
           , contains(paste0(serie, "_high")))  %>% 
    gather("key", "value", -"Date") %>% 
    plot_ly(x = ~Date, y = ~value, color = ~key, height = 600) %>% 
    add_lines() %>% 
    add_trace(
      x = lim
      , type = 'scatter'
      , mode = 'lines'
      , line = list(
        color = 'black'
        , dash = "dash"
      )
      , name = ''
      , showlegend = FALSE)
}

