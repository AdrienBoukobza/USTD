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
us_states() -> states

# Load CDC data
readRDS("data/STD.rds") %>%
  select(Disease,
         State,
         Year,
         Ethnicity = `Race/Ethnicity`,
         Age,
         STD_Cases,
         Gender,
         Population) %>%
  filter(Ethnicity != "Race Suppressed") -> STD

# Create indexes for the selectors
STD %>%
  select(Disease, Gender, Age, Ethnicity, State) %>%
  map(~ unique(.) %>% sort) -> selectVars

# Function to add "All" to a vector
andAll <- function(x)
  c("All", x)
