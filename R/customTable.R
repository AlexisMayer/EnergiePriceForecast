#' customTable - Renders a table with options for color and formatting
#'
#' @param data A dataframe containing the historical data for the specified serie
#' @param serie The name of the serie to be displayed in the table
#' @param lim The limit used to determine the color of the rows
#' @return A rendered datatable with options for color, formatting and row grouping
#' 
#' @export
customTable = function(data, serie, lim) {
  data %>% 
    select(Date, "Value" = all_of(serie)
           , contains(paste0(serie, "_low"))
           , contains(paste0(serie, "_high")))  %>% 
    mutate(Decate = paste0(substr(Date,1,3),0)) %>% 
    DT::datatable(
      extensions = 'RowGroup'
      , editable = TRUE
      , rownames = FALSE
      , options = list(
        rowGroup = list(dataSrc = 4)
        , searching = FALSE
        , scrollY = 500
        , columnDefs = list(list(visible = FALSE, targets = 4))
        , order = list(0, 'desc')
        , pageLength = nrow(data))
    ) %>% 
    # Row colors 
    formatStyle(
      target = "row"
      , columns = "Date"
      , color = styleInterval(lim, c("black","red"))
    ) %>% 
    formatStyle(columns = c(1:ncol(data)), lineHeight='30%') %>% 
    formatRound(
      columns = c(2:4)
      , digits = 0
      , interval = 3
      , mark = " "
      , dec.mark = ","
    )
}

