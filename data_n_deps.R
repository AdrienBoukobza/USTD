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

states <- us_states()

STD = readRDS("data/STD.rds") %>% rename(Ethnicity = `Race/Ethnicity`)

STD %>%
  select(Disease, Gender, Age, Ethnicity, State) %>%
  map(~ unique(.) %>% sort) -> selectVars

andAll <- function(x)
  c("All", x)

