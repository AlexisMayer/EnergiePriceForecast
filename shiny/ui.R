ui = dashboardPage(
  
  # Set-up -------------------------------------------------------------------

  title = "DashboardPage"
  
  # Header -------------------------------------------------------------------
  
  , header = dashboardHeader()
  
  # Sidebar ------------------------------------------------------------------
  
  , sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tab"
      , menuItem("Offre", tabName = "offre")
      , menuItem("Demande", tabName = "demande")
      , menuItem("Ordre de mérite", tabName = "merit")
      )
    )
  
  # Body --------------------------------------------------------------------
  
  , body = dashboardBody(
    
    useShinyalert(),
    
    tags$head(tags$style(HTML(".sweet-alert p { text-align: justify; }"))),
    
    tabItems(
      
      # Offre ---------------------------------------------------------------
      
      tabItem(
        tabName = "offre"
        , fluidRow(
          box(
            title = "Prévision des composantes de l'offre"
            , status = "primary"
            , width = 12
            , solidHeader = TRUE
            , collapsible = TRUE
            , selectInput(
              inputId = "serieOffre"
              , label = "Choix de la variable :"
              , choices = c("Biomass", "Gas", "Coal", "Hydropower", "Nuclear", "Solar", "Wind Onshore",  "Wind Offshore")
              , selected = "Biomass"
            )))
        , fluidRow(
          tabBox(
            title = "Coûts mariginaux"
            , width = 6
            , height = 700
            , tabPanel(
              title = "Table"
              , numericInput(
                inputId = "lowCost"
                , label = "Déviation (%) scénario bas :"
                , min = 0
                , max = -100
                , value = -20)
              , numericInput(
                inputId = "highCost"
                , label = "Déviation (%) scénario haut :"
                , min = 0
                , max = 100
                , value = 20)
              , actionButton(
                inputId = "runCost"
                , label = "Actualiser"
                , class = "btn-success"
                , width = "100%")
              , actionButton(
                inputId = "resetCost"
                , label = "Reset"
                , class = "btn-warning"
                , width = "100%")
              , br()
              , br()
              , dataTableOutput(
                outputId = "tableCost"))
            , tabPanel(
              title = "Graph"
              , plotlyOutput(
                outputId = "plotCost"
                , width = "100%")
              )
            ), 
          tabBox(
            title = "Capacités installées (MW)"
            , width = 6
            , height = 700
            , tabPanel(
              title = "Table"
              , numericInput(
                inputId = "lowCapacity"
                , label = "Déviation (%) scénario bas :"
                , min = 0
                , max = -100
                , value = -20)
              , numericInput(
                inputId = "highCapacity"
                , label = "Déviation (%) scénario haut :"
                , min = 0
                , max = 100
                , value = 20)
              , actionButton(
                inputId = "runCapacity"
                , label = "Actualiser"
                , class = "btn-success"
                , width = "100%")
              , actionButton(
                inputId = "resetCapacity"
                , label = "Reset"
                , class = "btn-warning"
                , width = "100%")
              , br()
              , br()
              , dataTableOutput(
                outputId = "tableCapacity")
            )
            , tabPanel(
              title = "Graph"
              , plotlyOutput(
                outputId = "plotCapacity"
                , width = "100%")
            )
          )
        )
      )
     
      # Demande ------------------------------------------------------
      
      , tabItem(
        tabName = "demande"
        , fluidRow(
          tabBox(
            title = "Prévision de la demande"
            , height = 700
            , width = 12
            , tabPanel(
              title = "Table"
              , numericInput(
                inputId = "lowDemand"
                , label = "Déviation (%) scénario bas :"
                , min = 0
                , max = -100
                , value = -20)
              , numericInput(
                inputId = "highDemand"
                , label = "Déviation (%) scénario haut :"
                , min = 0
                , max = 100
                , value = 20)
              , actionButton(
                inputId = "runDemand"
                , label = "Actualiser"
                , class = "btn-success"
                , width = "100%")
              , actionButton(
                inputId = "resetDemand"
                , label = "Reset"
                , class = "btn-warning"
                , width = "100%")
              , br()
              , br()
              , dataTableOutput(outputId = "tableDemand"))
            , tabPanel(
              title = "Graph"
              , plotlyOutput(outputId = "plotDemand", width = "100%")
              )
            )
          )
        )
      
      # Merit Order ------------------------------------------------------
      
      , tabItem(
        tabName = "merit"
        , fluidRow(
          tabBox(
            title = "Merit Order Estimation"
            , width = 12
            , tabPanel(
              title = "Graph"
              , numericInput(
                inputId = "dateMerit"
                , label = "Année de la projection :"
                , min = 2022
                , max = 2100
                , value = 2030)
              , downloadButton(
                "download"
                , label = "Télécharger")
              , br()
              , br()
              , plotlyOutput(outputId = "plotMerit", width = "100%", height = "600px"))
            , tabPanel(
              title = "Table"
              , dataTableOutput(outputId = "tableMerit")
              )
            )
          )
        )
      )
    )
  )