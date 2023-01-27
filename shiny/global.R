# Packages 
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(DT)
library(plotly)
library(tidyverse)
library(lubridate)
library(forecast)
library(readxl)
library(writexl)

# Load data
load("../data/init.RData")

# Usefull functions
lapply(list.files("R", full.names = TRUE), function(x) source(x, encoding = "UTF-8"))

# Shiny UI/Server
source("../shiny/server.R", encoding = "UTF-8")
source("../shiny/ui.R", encoding = "UTF-8")
