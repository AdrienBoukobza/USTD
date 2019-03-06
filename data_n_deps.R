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

STD = readRDS("data/STD.rds")
varsDisease <- c(STD %>% group_by(Disease) %>% summarise() %>% unlist(use.names = FALSE), "All")
varsGender <- c(STD %>% group_by(Gender) %>% summarise() %>% unlist(use.names = FALSE),"All")
varsAge <- c(STD %>% group_by(Age_Code) %>% summarise() %>% unlist(use.names = FALSE), "All")
varsAge <- varsAge[order(unlist(varsAge),decreasing=TRUE)]
varsEthnia <- c(STD %>% group_by(`Race/Ethnicity`) %>% summarise() %>% unlist(use.names = FALSE), "All")
varscomparison <- STD %>% group_by(Disease) %>% summarise() %>% unlist(use.names = FALSE)

