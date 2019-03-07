library(shinythemes)
library(leaflet)
library(tidyverse)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(USAboundaries)
library(DT)
library(plotly)
library(prophet)
library(shinycustomloader)
library(scales)
library(dygraphs)

options(scipen = 999)

# Load states boundaries and reorder alphabetically
us_states() %>%
  arrange(name) -> states

# Load CDC data
STD = readRDS("data/STD.rds") %>% rename(Ethnicity = `Race/Ethnicity`)

# Extract the population data only
STD %>%
  select(State, Year, Ethnicity, Age, Gender, Population) %>%
  distinct -> populations

# Create indexes for the selectors
STD %>%
  select(Disease, Gender, Age, Ethnicity, State) %>%
  map(~ unique(.) %>% sort) -> selectVars

# Function to add "All" to a vector
andAll <- function(x)
  c("All", x)




