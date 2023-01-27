server = function(input, output, session) {
  
  # SET-UP -----------------------------------------
  shinyalert(
    type = "info",
    title = "Guide utilisateur",
    text = HTML(
      "L'ordre de mérite est un modèle utilisé pour déterminer la manière dont les différentes sources d'énergie doivent être utilisées pour répondre à la demande énergétique.
      Il est généralement utilisé pour optimiser la production d'énergie électrique en prenant en compte les coûts marginaux de différentes sources d'énergie et les capacités de production.
      
      Les onglets offre et demande vous permettent d'ajuster les coûts marginaux, les capacités de production et la demande totale en fonction de vos propres hypothèses.
      Les tableaux sont éditables en cliquant sur la cellule que vous souhaitez modifier.
      Lorsque vous appuyez sur exécuter, les prévisions sont recalculées en fonction des nouvelles valeurs d'entrées.
      Enfin, l'onglet Merit order vous permet de visualiser les résultats du modèle en fonction de votre scénario.
      
      Il est important de noter que les données affichées sont inspirées de la réalité mais ne sont en réalité que des estimations.
      Cet outil est un exemple de ce que peut permettre R-Shiny et n'a pas vocation à être utilisé dans la vie réelle.
      Si vous êtes intéressé par ce genre d'outil, n'hésitez pas à me contacter !"
    )
  )
  
  
  
  # Initialize 
  out = reactiveValues(Cost = Cost , Capacity = Capacity , Demand = Demand , cells_edit = NULL , last_date = last_date)
  
  # Back-up for reset button 
  outSave = reactiveValues(Cost = Cost, Capacity = Capacity, Demand = Demand)
  
  # Proxy for edit mode 
  proxyCost = dataTableProxy('tableCost')
  proxyCapacity = dataTableProxy('tableCapacity')
  proxyDemand = dataTableProxy('tableDemand')
  
  # Edite action
  observe({ DT::replaceData(proxyCost, out$Cost, rownames = FALSE, resetPaging = FALSE) })
  observe({ DT::replaceData(proxyCapacity, out$Capacity, rownames = FALSE, resetPaging = FALSE) })
  observe({ DT::replaceData(proxyDemand, out$Demand, rownames = FALSE, resetPaging = FALSE) })

  # Edite cells modification 
  observeEvent(input$tableCost_cell_edit, {
    # Return edited table 
    tps = input$tableCost_cell_edit
    # Find column position 
    tps$col = which(names(out$Cost) == input$serieOffre) - 1
    # Save in dataset
    out$Cost <<- editData(out$Cost, tps, rownames = FALSE)
    # Detect which cells has been edited
    if(is.null(out$cells_edit)) {
      out$cells_edit = input$tableCost_cell_edit[[1]]
    } else if(out$cells_edit > 1) {
      out$cells_edit = max(out$cells_edit, input$tableCost_cell_edit[[1]])  
    } else {
      out$cells_edit <<- NULL
    }
  })
  
  # Edite cells modification 
  observeEvent(input$tableCapacity_cell_edit, {
    # Return edited table 
    tps = input$tableCapacity_cell_edit
    # Find column position 
    tps$col = which(names(out$Capacity) == input$serieOffre) - 1
    # Save in dataset
    out$Capacity <<- editData(out$Capacity, tps, rownames = FALSE)
    # Detect which cells has been edited
    if(is.null(out$cells_edit)) {
      out$cells_edit = input$tableCapacity_cell_edit[[1]]
    } else if(out$cells_edit > 1) {
      out$cells_edit = max(out$cells_edit, input$tableCapacity_cell_edit[[1]])  
    } else {
      out$cells_edit <<- NULL
    }
  })
  
  # Edite cells modification 
  observeEvent(input$tableDemand_cell_edit, {
    # Return edited table 
    tps = input$tableDemand_cell_edit
    # Find column position 
    tps$col = which(names(out$Demand) == "demand") - 1
    # Save in dataset
    out$Demand <<- editData(out$Demand, tps, rownames = FALSE)
    # Detect which cells has been edited
    if(is.null(out$cells_edit)) {
      out$cells_edit = input$tableDemand_cell_edit[[1]]
    } else if(out$cells_edit > 1) {
      out$cells_edit = max(out$cells_edit, input$tableDemand_cell_edit[[1]])  
    } else {
      out$cells_edit <<- NULL
    }
  })
  
  # COUTS MARGINAUX ----------------------------------
  
  observeEvent({ input$runCost ; input$serieOffre }, {
      
    # Split train & test 
    if(!is.null(out$cells_edit)) {
      
      # Find all edit cells 
      cells_edit = which(!out$Cost[[input$serieOffre]] == outSave$Cost[[input$serieOffre]])
      
      # Find with cells should be keeped
      cells_keep = c(1:which(out$Cost$Date == out$last_date), cells_edit)
  
      # Remove forecast between and after cells edit 
      out$Cost[[input$serieOffre]][-cells_keep] = NA
      
      # Interpolation between last observe and cell edit 
      out$Cost[[input$serieOffre]] = zoo::na.fill(out$Cost[[input$serieOffre]], c(NA, "extend"))
      
      # Split
      h = nrow(out$Cost) - max(cells_keep)
      train = out$Cost[1:max(cells_keep), ]
      test = out$Cost[(max(cells_keep) + 1):nrow(out$Cost), ]
      
      # Make prevision & forecast
      if(h > 0) {
        
        # Forecast
        pred = fit(
            serie = input$serieOffre 
            , train = train
            , test = test
            , h = h)
    
      } else { pred = train }
      
      # Create column low & high
      pred = scenario(
          data = pred
          , serie = input$serieOffre
          , last_date = out$last_date 
          , low = input$lowCost
          , high = input$highCost
          , all = F)
      
      # Merge
      out$Cost = out$Cost %>% 
        select(-all_of(names(pred)[-1])) %>% 
        left_join(pred)
      
    } 
    
    # Render table
    output$tableCost = renderDataTable({ customTable(out$Cost, input$serieOffre, out$last_date) })
    
    # Render plot  
    output$plotCost = renderPlotly({ customPlot(out$Cost, input$serieOffre, out$last_date) })
    
    # Remove 
    out$cells_edit <<- NULL

  }, ignoreNULL = FALSE)
  
  # Reset
  observeEvent(input$resetCost, {
    # Replace serie with back-up 
    out$Cost[[input$serieOffre]] = outSave$Cost[[input$serieOffre]]
    out$Cost[[paste0(input$serieOffre, "_low")]] = outSave$Cost[[paste0(input$serieOffre, "_low")]]
    out$Cost[[paste0(input$serieOffre, "_high")]] = outSave$Cost[[paste0(input$serieOffre, "_high")]]
    # Clean 
    out$cells_edit <<- NULL
  })
  
  # CAPACITY -----------------------------------------
  
  observeEvent({ input$runCapacity }, {
    
    # Split train & test 
    if(!is.null(out$cells_edit)) {
      
      # Find all edit cells 
      cells_edit = which(!out$Capacity[[input$serieOffre]] == outSave$Capacity[[input$serieOffre]])
      
      # Find with cells should be keeped
      cells_keep = c(1:which(out$Capacity$Date == out$last_date), cells_edit)
      
      # Remove forecast between and after cells edit 
      out$Capacity[[input$serieOffre]][-cells_keep] = NA
      
      # Interpolation between last observe and cell edit 
      out$Capacity[[input$serieOffre]] = zoo::na.fill(out$Capacity[[input$serieOffre]], c(NA, "extend"))
      
      # Split
      h = nrow(out$Capacity) - max(out$cells_edit)
      train = out$Capacity[1:max(out$cells_edit), ]
      test = out$Capacity[(max(out$cells_edit) + 1):nrow(out$Capacity), ]
      
      # Make prevision & forecast
      if(h > 0) {
        # Forecast
        pred = fit(
          serie = input$serieOffre 
          , train = train
          , test = test
          , h = h)
      } else { pred = train }
      
      # Create column low & high
      pred = scenario(
        data = pred
        , serie = input$serieOffre
        , last_date = out$last_date
        , low = input$lowCapacity
        , high = input$highCapacity
        , all = F)
      
      # Merge
      out$Capacity = out$Capacity %>% 
        select(-all_of(names(pred)[-1])) %>% 
        left_join(pred)
      } 
    
    # Render table
    output$tableCapacity = renderDataTable({ customTable(out$Capacity, input$serieOffre, out$last_date) })
    
    # Render plot  
    output$plotCapacity = renderPlotly({ customPlot(out$Capacity, input$serieOffre, out$last_date) })
    
    # Remove 
    out$cells_edit <<- NULL
    
  }, ignoreNULL = FALSE)
  
  # Reset
  observeEvent(input$resetCapacity, {
    # Replace serie with back-up 
    out$Capacity[[input$serieOffre]] = outSave$Capacity[[input$serieOffre]]
    out$Capacity[[paste0(input$serieOffre, "_low")]] = outSave$Capacity[[paste0(input$serieOffre, "_low")]]
    out$Capacity[[paste0(input$serieOffre, "_high")]] = outSave$Capacity[[paste0(input$serieOffre, "_high")]]
    # Clean 
    out$cells_edit <<- NULL
  })

  # DEMAND -------------------------------------------
  
  observeEvent({ input$runDemand }, {
  
    # Split train & test 
    if(!is.null(out$cells_edit)) {

      # Find all edit cells 
      cells_edit = which(!out$Demand[["demand"]] == outSave$Demand[["demand"]])
      
      # Find with cells should be keeped
      cells_keep = c(1:which(out$Demand$Date == out$last_date), cells_edit)
      
      # Remove forecast between and after cells edit 
      out$Demand[["demand"]][-cells_keep] = NA
      
      # Interpolation between last observe and cell edit 
      out$Demand[["demand"]] = zoo::na.fill(out$Demand[["demand"]], c(NA, "extend"))
      
      # Forecast
      h = nrow(out$Demand) - max(out$cells_edit)
      train = out$Demand[1:max(out$cells_edit), ]
      test = out$Demand[(max(out$cells_edit) + 1):nrow(out$Demand), ]
      
      if(h > 0) {
        
        # Forecast
        pred = fit(
            serie = "demand"
            , train = train
            , test = test
            , h = h) 
        
      } else { pred = train }
      
      # Create column low & high
      pred = scenario(
          data = pred
          , serie = "demand"
          , last_date = out$last_date
          , low = input$lowDemand
          , high = input$highDemand
          , all = F)
      
      # Merge
      out$Demand = out$Demand %>% 
        select(-all_of(names(pred)[-1])) %>% 
        left_join(pred)
        
    }
    
    # DT table
    output$tableDemand = renderDataTable({ customTable(out$Demand, "demand", out$last_date) })
    
    # Plotly long 
    output$plotDemand = renderPlotly({ customPlot(out$Demand, "demand", out$last_date) })

    # Remove 
    out$cells_edit <<- NULL
    
  }, ignoreNULL = FALSE)
  
  # Reset
  observeEvent(input$resetDemand, {
    # Replace serie with back-up 
    out$Demand[["demand"]] = outSave$Demand[["demand"]]
    out$Demand[["demand_low"]] = outSave$Demand[["demand_low"]]
    out$Demand[["demand_high"]] = outSave$Demand[["demand_high"]]
    # Clean 
    out$cells_edit <<- NULL
  })
  
  # MERIT ORDER --------------------------------------
  
  observe({
 
    # Demand level 
    demand = out$Demand$demand[which(out$Demand == input$dateMerit)] # UNITE ?????
    
    # Message
    print(paste0(demand ,"MW demandés"))

    # Comput cost evolutions      
    cost = gather(out$Cost, "Type", "MC", -1) 

    # Comput capacity evolutions
    capacity = gather(out$Capacity, "Type", "Capacity", -1)

    # Comput marginal costs and cumulative capacity 
    tps = left_join(cost, capacity) %>% 
      filter(!str_detect(Type, "_low|_high")) %>% 
      group_by(Date) %>% 
      arrange(MC) %>% 
      mutate(position = 0.5 * (cumsum(Capacity) + cumsum(c(0, Capacity[-length(Capacity)]))),
             cum_capacity = cumsum(Capacity),
             top = ifelse(cum_capacity <= demand, 0, 1),
             top = ifelse(lag(top) != 1 & top == 1, 1, 0)) %>% 
      ungroup()
    
    # Prix d'équilibre
    prix = tps$MC[which(tps$top == 1 & tps$Date == input$dateMerit)]
    
    # Messages
    print(paste0("Max capacité : ", sum(tps$Capacity[which(tps$Date == input$dateMerit)]), "MW"))
    print(paste0("Prix d'équilibre : ", prix, "€/MWh"))
    
    # DT table
    output$tableMerit = renderDataTable({ 
      tps %>%      
        filter(Date == input$dateMerit) %>% 
        select(Date, Type,  "Marginal cost" = MC, Capacity, "Cumulative Capacity" = cum_capacity) %>% 
        DT::datatable(
          editable = FALSE
          , rownames = FALSE
          , options = list(
            searching = FALSE
            , scrollY = 500
            , order = list(0, 'desc')
            , pageLength = nrow(tps))
        ) %>% 
        formatStyle(columns = c(1:5), lineHeight='30%') %>% 
        formatRound(
          columns = c(3:5) 
          , digits = 0
          , interval = 3
          , mark = " "
          , dec.mark = ","
        )
      })
    
    # Plotly long 
    output$plotMerit = renderPlotly({ customMeritPlot(filter(tps, Date == input$dateMerit), prix, demand) })
    
    # Dowload 
    output$download = downloadHandler(
      filename = function() {"MeritOrder.xlsx"},
      content = function(file) {
        write_xlsx(
          x = list(
            "Prix" = out$Cost
            , "Capacités" = out$Capacity
            , "Demande" = out$Demand
            , "Mérit" = select(tps, Date, Type,  "Marginal cost" = MC, Capacity, "Cumulative Capacity" = cum_capacity)
          )
          , path = file)}
    )
    
  })
  
}