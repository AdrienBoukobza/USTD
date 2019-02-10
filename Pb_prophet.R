library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library (readr)
library (dplyr)
library (geojsonio)
library (DT)
library (mapview)
library (stringr)
library (shinyjs)
library(plotly)
library(ggplot2)
library(shinythemes)
library(DiagrammeR)
library(prophet)
library(tidyr)
library(shinycustomloader)

STD = readRDS("data/STD.rds")

varsDisease = c(
  "Chlamydia" = "Chlamydia",
  "Gonorrhea" = "Gonorrhea",
  "Primary and Secondary Syphilis" = "Primary and Secondary Syphilis",
  "All" = "All"
)

states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")


ui <- fluidPage(theme = shinytheme("flatly"),
                
                shinyjs::useShinyjs(),
                
                navbarPage("DescarteSTD",
                           
                           tabPanel ("Curve Explorer",
                                     selectInput("statecurve", "State", states$name),
                                     selectInput("diseasecurve", "Disease", varsDisease[1:3]),
                                     withLoader(plotlyOutput("curve",width = "100%", height = "400px"), type = 'html', loader = "dnaspin")
                           )
                           
                )
)

server <- function(input, output, session) {
  
  output$curve = renderPlotly({
    
    plot = STD %>% filter(State == input$statecurve) %>% filter(Disease == input$diseasecurve) %>%
      group_by(Year) %>% summarise(sum(STD_Cases))
    
    colnames(plot) = c("ds", "y")
    plot$ds = paste (plot$ds, "-01-01", sep = "")
    newplot = prophet(plot)
    
    future = make_future_dataframe(newplot, periods= 3650)
    forecast <- predict(newplot, future)
    tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    plot(newplot, forecast, xlabel= "Year", ylabel = "Number of cases")
  })
  
    }
# Run the application 
shinyApp(ui = ui, server = server)