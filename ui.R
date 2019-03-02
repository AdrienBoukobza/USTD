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

ui <- fluidPage(theme = shinytheme("flatly"),
            navbarPage("USTD", id = "navbar",
                       tabPanel("Home", includeMarkdown("markdown/Home.rmd")),
                       tabPanel("Interactive Map",
                                leafletOutput("map", width = "100%", height = "700"),
                                absolutePanel(id = "controls",
                                              class = "panel panel-default",
                                              fixed = FALSE,
                                              draggable = FALSE,
                                              top = 80,
                                              left = "auto",
                                              right = 30,
                                              bottom = "auto",
                                              width = 340,
                                              height = "auto",
                                              align = "center",
                                              h2("STD in the USA"),
                                              selectInput("disease", "Disease", varsDisease, selected = "All"),
                                              selectInput("gender", "Gender", varsGender, selected = "All"),
                                              sliderInput("year", "Year:",
                                                          min = 1996,
                                                          max = 2014,
                                                          value = 2014,
                                                          animate = animationOptions(interval = 1500, loop = FALSE)),
                                              selectInput("age", "Age class", varsAge, selected = "All"),
                                              actionButton("preset1", "Chlamydia in young adult female"),
                                              actionButton("preset2", "Minimum of Gonorrhea"),
                                              actionButton("preset3", "Minimum of Syphilis")),
                                fluidRow(column(12,
                                                plotOutput("curvetotal")))
                       ),
                       tabPanel("Curve Explorer",
                                selectInput("statecurve", "State", c("All",states$name)),
                                selectInput("diseasecurve", "Disease", c("All",varsDisease[1:3])),
                                withLoader(plotlyOutput("curve",width = "100%", height = "400px"), type = 'html', loader = "dnaspin")),
                       tabPanel("Risk calculator",
                                fluidRow(h2("Comparative beetween the different States", align = "center"),
                                         column(4,
                                                offset = 1,
                                                h3("Reference population :"),
                                                selectInput('OddsDisease', 'Disease', varsDisease[1:3]),
                                                selectInput('OddsState', 'State', states$name),
                                                selectInput('OddsGender', 'Gender', varsGender[1:2]),
                                                selectInput('OddsEthnia', 'Ethnia', varsEthnia[1:6]),
                                                selectInput('OddsAge', 'Age', varsAge[2:8]),
                                                selectInput('OddsYear', "Year", c(1996:2014))),
                                         column(4,
                                                h3("Compare by"),
                                                selectInput("Factor","Factor",c("Disease", "Gender", "Ethnia", "Age", "Year")),
                                                selectInput("Comparison", "Comparison", varscomparison))),
                                DTOutput("contingence"),
                                fluidRow(column (5,
                                                 offset =1,
                                                 actionButton("MapRRbutton", "Rendering map with RR")),
                                         column (5,
                                                 offset =1,
                                                 actionButton("MapORbutton", "Rendering map with OR"))),
                                leafletOutput("map2", width = "100%", height = "700")),
                       tabPanel("Data Explorer", DTOutput ("mapTable")),
                       tabPanel("About", includeMarkdown("markdown/About.Rmd")),
                       tabPanel("Licence", includeMarkdown("markdown/Licence.Rmd"))
            )
  )