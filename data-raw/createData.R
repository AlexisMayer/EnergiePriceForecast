# Initilize Rdata

# Dependances
library(tidyverse)
library(forecast)

# Usefull fonctions 
fls = list.files("R", full.names = TRUE)
lapply(fls, source)

fls = list.files("data-raw", full.names = TRUE)
fls = fls[-which(grepl("createData", fls))]
lapply(fls, source)

# Clean 
rm(fls)

# Simulate some technology costs
simCost = data.frame(
  Date = c(2010:2022),
  Nuclear = c(.3, .29, .33, .28, .31, .29, .23, .27, .3, .25, .35, .45, .35),
  Gas = c(.55, .6, .63, .67, .6, .68, .65, .78, .69, .70, .90, .110, .150),
  Coal = c(.45, .49, .4, .5, .55, .66, .6, .68, .79, .75, .100, .150, .190) * 1.5
)

# Load costs 
Cost = importMarginalCosts() %>% 
  select("Date" = Year, "Biomass" = Bioenergy, Hydropower, Solar, "Wind Onshore" = `Onshore.wind` , "Wind Offshore" = `Offshore.wind`) %>% 
  left_join(simCost) %>% 
  mutate_at(vars(-1), ~ as.numeric(.x) * 400)

# Clean 
rm(simCost)

# Load capacities
Capacity = importCapacity() %>% 
  select(Date, "Biomass" = Bioenergy, Gas, Coal, Hydropower, Nuclear, Solar, "Wind Onshore" = `Onshore.wind` , "Wind Offshore" = `Offshore.wind`) %>% 
  mutate_at(vars(-1), ~ as.numeric(.x) * 30) %>% 
  mutate_at(vars(-1), ~ ifelse(is.na(.x), lead(.x, n = 1), .x))

# Load Demand 
Demand = importDemand() %>% 
  select(Date, "demand" = values) %>% 
  mutate_at(vars(-1), as.numeric)

# Info
last_date = last(Cost$Date)

# Ajuster la taille du tableau
Cost = full_join(Cost, data.frame(Date = seq(max(Cost$Date) + 1, max(Cost$Date) + 50, by = 1)))
Capacity = full_join(Capacity, data.frame(Date = seq(max(Capacity$Date) + 1, max(Capacity$Date) + 50, by = 1)))
Demand = full_join(Demand, data.frame(Date = seq(max(Demand$Date) + 1, max(Demand$Date) + 50, by = 1)))

# Initialize forecast
Cost = initialize(
  data = Cost
  , len = max(Cost$Date) + 1
  , last_date = last_date
  , low = -20
  , high = 20
  , all = FALSE)

Capacity = initialize(
  data = Capacity
  , len = max(Capacity$Date)
  , last_date = last_date
  , low = -20
  , high = 20
  , all = FALSE)

Demand = initialize(
  data = Demand
  , len = max(Demand$Date)
  , last_date = last_date
  , low = -10
  , high = 10
  , all = FALSE)

# Clean 
rm(list = lsf.str())

# Save
save.image("data/init.RData")


