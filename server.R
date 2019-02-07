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

ui <- fluidPage(theme = shinytheme("flatly"),
                
                shinyjs::useShinyjs(),
                
                navbarPage("DescarteSTD",
                           tabPanel ("Interactive Map",
                                     leafletOutput("map", width = "100%", height = "700"),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                                   draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                   width = 340, height = "auto", align = "center",
                                                   h2("STD in the USA"),
                                                   
                                                   selectInput ("disease", "Disease", varsDisease),
                                                   selectInput ("gender", "Gender", varsGender),
                                                   sliderInput("year", "Year:",
                                                               min = 1996, max = 2014,
                                                               value = 1996, animate =
                                                                 animationOptions(interval = 1500, loop = FALSE)),
                                                   selectInput ("age", "Age class", varsAge)
                                                   
                                     ),
                                     DiagrammeROutput("timeline", width = 1000, height= 300),
                                     plotlyOutput("curvetotal", width = "975", height = 300),
                                     plotlyOutput("curvefilter", width = "975", height = 300)
                                     
                           ),
                           
                           tabPanel("Clinics explorer"),
                           
                           tabPanel ("Curve Explorer",
                                     selectInput("statecurve", "State", states$name),
                                     selectInput("diseasecurve", "Disease", varsDisease[1:3]),
                                     withLoader(plotlyOutput("curve",width = "100%", height = "400px"), type = 'html', loader = "dnaspin")
                           ),
                           
                           tabPanel("Risk calculator",
                                    fluidRow(
                                      column(3,
                                             h4("Comparative beetween two inhabitants of the United States")
                                      ),
                                      column(4, offset = 1,
                                             selectInput('OddsGender', 'First Gender', varsGender[1:2]),
                                             selectInput('OddsAge', 'First Age', varsAge[1:7]),
                                             selectInput('OddsDisease', 'First Disease', varsDisease[1:3]),
                                             selectInput('OddsState', 'First State', states$name),
                                             selectInput('OddsEthnia', 'First Ethnia', varsEthnia),
                                             selectInput('OddsYear', "First Year", c(1996:2014)),
                                             checkboxInput("exclude", "Exclude",value = TRUE),
                                             selectInput("excluded", "Excluded", c("Gender", "Age", "Disease", "State", "Ethnia", "Year"))
                                      ),
                                      column(4,
                                             selectInput('OddsGender2', 'Second Gender',varsGender[1:2]),
                                             selectInput('OddsAge2', 'Second Age', varsAge[1:7]),
                                             selectInput('OddsDisease2', 'Second Disease', varsDisease[1:3]),
                                             selectInput('OddsState2', 'Second State', states$name),
                                             selectInput('OddsEthnia2', 'Second Ethnia', varsEthnia),
                                             selectInput('OddsYear2', "Second Year", c(1996:2014)),
                                             checkboxInput("exclude2", "Exclude",value = TRUE),
                                             selectInput("excluded2", "Excluded", c("Gender", "Age", "Disease", "State", "Ethnia", "Year"))
                                             
                                      )
                                    ),
                                    DTOutput ("contingence"),
                                    
                                    fluidRow(
                                      column (4, offset =1,
                                              actionButton("RRbutton", "Calculate RR"),
                                              textInput("RR", label = "Risk Ratio", value = "Waiting for Data"),
                                              actionButton("MapRRbutton", "Rendering map with RR")
                                      ),
                                      
                                      
                                      column (5, offset =1,
                                              actionButton("ORbutton", "Calculate OR"),
                                              textInput("OR", label = "Odds Ratio", value = "Waiting for Data"),
                                              actionButton("MapORbutton", "Rendering map with OR")
                                      )
                                    ),
                                    leafletOutput("map2", width = "100%", height = "700")
                                    
                           ),
                           
                           tabPanel("Data Explorer",
                                    DTOutput ("mapTable")
                           )
                           
                )
)

server <- function(input, output, session) {
  
  output$map = renderLeaflet ({
    states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
    m <- leaflet(states, options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addTiles() %>% 
      setMaxBounds( lng1 = -0
                    , lat1 = 80
                    , lng2 = -180
                    , lat2 = 10 )
  })
  
  observe({
    
    #Creating a copy to work on
    
    STD1 = STD
    
    #Filtering data, preventing the analyzer to filter the "All" Option.
    
    if(input$disease != "All")
    {
      STD1 = STD1 %>% filter(Disease == input$disease)
    }
    
    if(input$gender != "All")
    {
      STD1 = STD1 %>% filter(Gender == input$gender)
    }
    
    if(input$age != "All")
    {
      STD1 = STD1 %>% filter(Age_Code == input$age)
    }
    
    STD1 = STD1 %>% filter (Year == input$year)
    
    # Creating two new DT : one to summarise the cases and one to summarise the population
    STD1Cases = STD1 %>% group_by(State) %>% summarise(STD_Cases = sum(STD_Cases))
    STD1Pop = STD1 %>% group_by(State) %>% summarise (Population = sum(Population))
    
    #Setting the old work as the new base for the cases
    STD1 = STD1Cases
    rm(STD1Cases)
    
    #Creating the rates in the working table
    STD1 = STD1 %>% mutate (RateCalc = STD1$STD_Cases * 1000 / STD1Pop$Population)
    
    #Completing the table with the populations
    STD1 = STD1 %>% mutate(Population = STD1Pop$Population)
    
    #Adding the states data
    states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
    
    #Establishing which are the missing states
    difference = tibble(setdiff(states$name,STD1$State),
                        STD_Cases = 0,
                        RateCalc = 0,
                        Population = 0
                        )
    
    
    colnames(difference) = c("State","STD_Cases", "RateCalc" , "Population")
    
    #Fusionning the two data and ordering to use with the map
    STD1 = rbind(STD1,difference)
    STD1 = STD1[order(match(STD1$State, states$name)),]
    
    
    ## Preparing the legend
    maxSTD1 = max(STD1$RateCalc)
    
    stepLegend = (maxSTD1 / 9)
    legendRow = c(0)
    
    for (i in 1:9)
    {
      legendRow = c(legendRow, 0 + stepLegend*i)
    }
    
    legendRow[10] = legendRow[10] + 0.05
    
    bins <- legendRow
    pal <- colorBin("YlOrRd", domain = STD1$RateCalc, bins = bins)
    
    #Preparing the labels
    
    if (input$disease!= "All") {meancountrypop = STD %>%  filter(Disease == input$disease)}
    meancountrypop = STD %>%  filter (Year == input$year) %>% 
      group_by(Disease) %>% summarise (sum(Population))

    meancountrypop = as.numeric(meancountrypop[1,2])
    
    if (input$disease!= "All") {meancountrycases = STD %>%  filter(Disease == input$disease)}
    meancountrycases = STD  %>% filter (Year == input$year) %>% 
      group_by(Disease) %>% summarise (sum(STD_Cases))
    
    meancountrycases = as.numeric(meancountrycases[1,2])
    
    meancountry = (meancountrycases *1000 )/ meancountrypop
    
    for (i in 1:51){
      meancountry = c(meancountry, meancountry[1])}
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g cases in the state <br/>Average: %g <br/> Population: %g <br/> Remind, country mean: %g",
      states$name, STD1$STD_Cases, STD1$RateCalc,STD1$Population, meancountry
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy ("map")  %>% addPolygons(data = states,
                                          fillColor = ~pal(STD1$RateCalc),
                                          weight = 1,
                                          opacity = 0.7,
                                          color = "grey",
                                          dashArray = "3",
                                          fillOpacity = 0.7,
                                          highlight = highlightOptions(
                                            weight = 5,
                                            color = "#666",
                                            dashArray = "",
                                            fillOpacity = 0.85,
                                            bringToFront = TRUE),
                                          label = labels,
                                          labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%  clearControls() %>% addLegend(pal = pal, values = STD1$RateCalc, opacity = 0.85,
                                                                                                    title = "for 1 000 inhabitants of a class of age",
                                                                                                    position = "bottomright")
  })
  
  output$mapTable = renderDT({
    
    STD$Disease = as.factor(STD$Disease)
    STD$State = as.factor (STD$State)
    STD$`Race/Ethnicity` = as.factor(STD$`Race/Ethnicity`)
    STD$Age = as.factor (STD$Age)
    STD$Age_Code = as.factor (STD$Age_Code)
    STD$Gender = as.factor(STD$Gender)
    STD$Gender_Code = as.factor(STD$Gender_Code)
    STD
    
    
    }, extensions = c("Buttons", "ColReorder", 'KeyTable'), 
                             filter = "top",  
                             options = list(keys = TRUE, colReorder = TRUE,pageLength = 20, dom = "Bfrtip", buttons = c("copy", "csv", "pdf",I('colvis')), autoWidth = TRUE,
                                            columnDefs = list(list(width = '200px', targets = "_all"))))
  
  
  contingenceTB = reactive ({
  
  
  #Building the contingence Table
  
  #Defining which is the value to exclude here
  
  deactivate = 0
  deactivate2 = 0
  
  if (input$excluded == "Gender" && input$exclude == TRUE) {deactivate = 1}
  if (input$excluded == "Age"&& input$exclude == TRUE) {deactivate = 2}
  if (input$excluded == "Disease"&& input$exclude == TRUE) {deactivate = 3}
  if (input$excluded == "State"&& input$exclude == TRUE) {deactivate = 4}
  if (input$excluded == "Ethnia"&& input$exclude == TRUE) {deactivate = 5}
  if(input$excluded == "Year" && input$exclude == TRUE ){deactivate = 6}
  
  if (input$excluded2 == "Gender" && input$exclude2 == TRUE) {deactivate2 = 1}
  if (input$excluded2 == "Age"&& input$exclude2 == TRUE) {deactivate2 = 2}
  if (input$excluded2 == "Disease"&& input$exclude2 == TRUE) {deactivate2 = 3}
  if (input$excluded2 == "State"&& input$exclude2 == TRUE) {deactivate2 = 4}
  if (input$excluded2 == "Ethnia"&& input$exclude2 == TRUE) {deactivate2 = 5}
  if(input$excluded2 == "Year" && input$exclude2 == TRUE ){deactivate2 = 6}
  
  toggleState("OddsGender", input$exclude != TRUE | deactivate != 1)
  toggleState("OddsGender2", input$exclude2 != TRUE | deactivate2 != 1)
  toggleState("OddsAge", input$exclude != TRUE | deactivate!= 2)
  toggleState("OddsAge2", input$exclude2 != TRUE | deactivate2 != 2)
  toggleState("OddsDisease", input$exclude != TRUE | deactivate != 3)
  toggleState("OddsDisease2", input$exclude2 != TRUE | deactivate2 != 3)
  toggleState("OddsState", input$exclude != TRUE | deactivate != 4)
  toggleState("OddsState2", input$exclude2 != TRUE | deactivate2!= 4)
  toggleState("OddsEthnia", input$exclude != TRUE | deactivate != 5)
  toggleState("OddsEthnia2", input$exclude2 != TRUE | deactivate2!= 5)
  toggleState("OddsYear", input$exclude != TRUE | deactivate != 6)
  toggleState("OddsYear2", input$exclude2 != TRUE | deactivate2!= 6)
  
  
  #Defining the first condition  
  
  temp = STD
  
  if (deactivate == 1){} 
  else {temp = temp %>% filter(Gender == input$OddsGender)}
  
  if (deactivate == 2){}
  else {temp= temp %>% filter(Age_Code == input$OddsAge)}
  
  if(deactivate == 3){}
  else {temp = temp %>% filter (Disease == input$OddsDisease)}
  
  if (deactivate == 4){}
  else {temp = temp %>% filter (State == input$OddsState)}
  
  if (deactivate == 5){}
  else {temp = temp %>% filter (`Race/Ethnicity` == input$OddsEthnia)}
  
  if(deactivate ==6){}
  else {temp = temp %>% filter (Year == input$OddsYear)}
  
  if (deactivate == 6){
    temp = temp %>% group_by (Disease) %>% summarise(sum(STD_Cases))
  }
  
  if(input$exclude == FALSE) {
    temp = temp %>% group_by(Disease) %>% summarise (sum(STD_Cases))
    deactivate =7 #To avoid the error with unknown column
  }
  
  if(deactivate < 6){
    temp = temp %>% group_by (`Year`) %>% summarise(sum(STD_Cases))
  }
  
  
  
  #Creating the first value on the first line
  
  firstline = c()
  if(is.na (as.numeric(temp[1,2])))
  {firstline =c(firstline, 0)}
  else  
  {firstline = c(firstline, as.numeric(temp[1,2]))}
  
  #Defining the second condition
  
  temp2 = STD
  
  if (deactivate2 == 1){} 
  else {temp2 = temp2 %>% filter(Gender == input$OddsGender2)}
  
  if (deactivate2 == 2){}
  else {temp2= temp2 %>% filter(Age_Code == input$OddsAge2)}
  
  if(deactivate2 == 3){}
  else {temp2 = temp2 %>% filter (Disease == input$OddsDisease2)}
  
  if (deactivate2 == 4){}
  else {temp2 = temp2 %>% filter (State == input$OddsState2)}
  
  if (deactivate2 == 5){}
  else {temp2 = temp2 %>% filter (`Race/Ethnicity` == input$OddsEthnia2)}
  
  if(deactivate2 ==6){}
  else {temp2 = temp2 %>% filter (Year == input$OddsYear2)}
  
  if (deactivate2 == 6){
    temp2 = temp2 %>% group_by (Disease) %>% summarise(sum(STD_Cases))
  }
  
  if(input$exclude2 == FALSE) {
    temp2 = temp2 %>% group_by(Disease) %>% summarise (sum(STD_Cases))
    deactivate2 =7 #To avoid the error with unknown column
  }
  
  if(deactivate2 < 6){
    temp2 = temp2 %>% group_by (`Year`) %>% summarise(sum(STD_Cases))
  }
  
  #Creating the first value on second line
  secondline = c()
  if(is.na (as.numeric(temp2[1,2])))
  {secondline =c(secondline, 0)}
  else  
  {secondline = c(secondline, as.numeric(temp2[1,2]))}
  
  #Creating the total values
  total = STD %>% group_by(State) %>% summarise (sum(Population))
  total1 = total %>% filter(State == input$OddsState)
  total2 = total %>% filter(State == input$OddsState2)
  
  total1 = as.numeric(total1[1,2])
  total2 = as.numeric(total2[1,2])
  
  if(input$exclude == TRUE && deactivate == 4) {
    total1 = sum(total$`sum(Population)`)
  }
  
  if(input$exclude2 == TRUE && deactivate2 == 4) {
    total2 = sum(total$`sum(Population)`)
  }
  
  
  #Creating the table itself
  contingenceTB = data.frame ("Titles" = c("First Condition", "Second Condition", "Total"),
                              "Diseased" = c(firstline[1], secondline[1], firstline[1]+secondline[1]), 
                              "Non.Diseased" =c(total1 - firstline[1], total2 - secondline[1], total1-firstline[1]+ total2 - secondline[1]),
                              "Total" = c(as.numeric(total1), total2, total1+total2))
})
  
  output$contingence = renderDT({
    contingenceTB ()
  })

  onclick ("RRbutton", {
    
    contingence = contingenceTB ()
    
    if(contingence [1,4] !=0 && contingence[2,2] != 0)
    {
      RR = as.numeric((contingence[1,2] / contingence [1,4]) / (contingence[2,2] / contingence [2,4]))
      updateTextInput(session, "RR",label = "Risk Ratio", value = RR)
      
    }else {updateTextInput(session, "RR",label = "Risk Ratio", value = "Condition isn't applicable")}
  })
  
  onclick ("ORbutton", {
    
    contingence = contingenceTB ()
    
    if(contingence [1,3] !=0 && contingence[2,2] != 0)
    {
      OR = as.numeric((contingence[1,2] / contingence [1,3]) / (contingence[2,2] / contingence [2,3]))
      updateTextInput(session, "OR",label = "Odds Ratio", value = OR)
      
    }else {updateTextInput(session, "OR",label = "Risk Ratio", value = "Condition isn't applicable")}
  })
  
  onclick("MapRRbutton",{
    allstateRR = STD %>% filter(Gender == input$OddsGender2) %>% 
      filter(Age_Code == input$OddsAge2) %>% 
      filter(Disease == input$OddsDisease2) %>% 
      filter(`Race/Ethnicity` == input$OddsEthnia2) %>% 
      filter (Year == input$OddsYear2)
    
    difference = tibble(Disease = "",
      State = setdiff(states$name,allstateRR$State),
      Year = 0,
      `Race/Ethnicity` = "",
      Age ="",
      Age_Code = "",
      STD_Cases = 0,
      Population = 0,
      Gender = "",
      Gender_Code = ""
    )
    
    #Fusionning the two data and ordering to use with the map
    allstateRR = rbind(allstateRR,difference)
    allstateRR = allstateRR[order(match(allstateRR$State, states$name)),]
    
    total = STD %>% group_by(State) %>% summarise (sum(Population))
    difference = tibble(State = setdiff(states$name,total$State),
                        `sum(Population)` = 0
    )
    total = rbind(total, difference)
    
    contingence = contingenceTB()
    allstateRR = allstateRR %>%  mutate (RR = ifelse(allstateRR$Population != 0 & allstateRR$STD_Cases != 0,
                                      as.numeric((contingence[1,2] / contingence [1,4]) / (STD_Cases / total$`sum(Population)`)),
                                      0))
    
    ## Preparing the legend
    maxRR = max(as.numeric(allstateRR$RR))
    if (maxRR !=0){
    stepLegend = (maxRR / 9)
    legendRow = 0
    
    for (i in 1:9)
    {
      legendRow = c(legendRow, 0 + stepLegend*i)
    }
    
    legendRow[10] = legendRow[10] + 0.05
    
    }else{legendRow = 1:10}
    
    bins <- legendRow
    pal <- colorBin("YlGnBu", domain = allstateRR$RR, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Chances to have the first condition compared to second one : %g ",
      states$name, allstateRR$RR
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy ("map2")  %>% addPolygons(data = states,
                                          fillColor = ~pal(allstateRR$RR),
                                          weight = 1,
                                          opacity = 0.7,
                                          color = "grey",
                                          dashArray = "3",
                                          fillOpacity = 0.7,
                                          highlight = highlightOptions(
                                            weight = 5,
                                            color = "#666",
                                            dashArray = "",
                                            fillOpacity = 0.85,
                                            bringToFront = TRUE),
                                          label = labels,
                                          labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%  clearControls() %>% addLegend(pal = pal, values = allstateRR$RR, opacity = 0.85,
                                                                                                    title = "Risk Ratio",
                                                                                                    position = "bottomright")


    
    
  }) 
  
  onclick("MapORbutton",{
    
    allstateOR = STD %>% filter(Gender == input$OddsGender2) %>% 
      filter(Age_Code == input$OddsAge2) %>% 
      filter(Disease == input$OddsDisease2) %>% 
      filter(`Race/Ethnicity` == input$OddsEthnia2) %>% 
      filter (Year == input$OddsYear2)
    
    difference = tibble(Disease = "",
                        State = setdiff(states$name,allstateOR$State),
                        Year = 0,
                        `Race/Ethnicity` = "",
                        Age ="",
                        Age_Code = "",
                        STD_Cases = 0,
                        Population = 0,
                        Gender = "",
                        Gender_Code = ""
    )
    
    #Fusionning the two data and ordering to use with the map
    allstateOR = rbind(allstateOR,difference)
    allstateOR = allstateOR[order(match(allstateOR$State, states$name)),]
    
    total = STD %>% group_by(State) %>% summarise (sum(Population))
    difference = tibble(State = setdiff(states$name,total$State),
                        `sum(Population)` = 0
    )
    total = rbind(total, difference)
    
    contingence = contingenceTB()
    allstateOR = allstateOR %>%  mutate (OR = ifelse(allstateOR$Population != 0 & allstateOR$STD_Cases != 0,
                                                     as.numeric((contingence[1,2] / contingence [1,3]) / (STD_Cases / (total$`sum(Population)`- STD_Cases))),
                                                     0))
    
    ## Preparing the legend
    maxOR = max(as.numeric(allstateOR$OR))
    if (maxOR !=0){
      stepLegend = (maxOR / 9)
      legendRow = 0
      
      for (i in 1:9)
      {
        legendRow = c(legendRow, 0 + stepLegend*i)
      }
      
      legendRow[10] = legendRow[10] + 0.05
      
    }else{legendRow = 1:10}
    
    bins <- legendRow
    pal <- colorBin("Oranges", domain = allstateOR$OR, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Odds Ratio: %g ",
      states$name, allstateOR$OR
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy ("map2")  %>% addPolygons(data = states,
                                           fillColor = ~pal(allstateOR$OR),
                                           weight = 1,
                                           opacity = 0.7,
                                           color = "grey",
                                           dashArray = "3",
                                           fillOpacity = 0.7,
                                           highlight = highlightOptions(
                                             weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.85,
                                             bringToFront = TRUE),
                                           label = labels,
                                           labelOptions = labelOptions(
                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                             textsize = "15px",
                                             direction = "auto")) %>%  clearControls() %>% addLegend(pal = pal, values = allstateOR$OR, opacity = 0.85,
                                                                                                     title = "Odds Ratio",
                                                                                                     position = "bottomright")
    
    
    
  })
  
  output$map2 = renderLeaflet ({
    states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
    m <- leaflet(states, options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addTiles() %>% 
      setMaxBounds( lng1 = -90
                    , lat1 = 70
                    , lng2 = -100
                    , lat2 = 10 )
  })
  
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
  
  output$curvetotal = renderPlotly({
    plot2 = STD %>% group_by(Year) %>% summarise(sum(STD_Cases))
    colnames(plot2) = c("Year", "STD_Cases")
    ggplot(plot2, aes(x = Year, y = STD_Cases))+
      geom_line(color = "blue")+
      xlab ("Year")+
      ylab ("Number of cases")+
      labs(title = "Evolution of total number of cases")
  })
  
  output$curvefilter = renderPlotly({
    
    plot3 = STD

    if(input$age != "All") {plot3 = plot3 %>% filter(Age_Code == input$age)}
    
    if(input$gender != "All") {plot3 = plot3 %>% filter(Gender == input$gender)}
    
    if(input$disease != "All") {plot3 = plot3 %>% filter(Disease == input$disease)}
    
    plot3  = plot3 %>% group_by(Year) %>% summarise (sum(STD_Cases))
    colnames(plot3) = c("Year", "STD_Cases")
    
    ggplot(plot3, aes(x= Year, y = STD_Cases))+
      geom_line(color = "red")+
      xlab("Year")+
      ylab ("Number of cases")+
      labs (title = "Evolution of number of cases with our filters")
    
  })

  output$timeline = renderDiagrammeR({
     
    stringtimeline = "gantt
dateFormat  YYYY-MM-DD
     title Some informations
     
     section Disease
     Minimum of Syphilis:"
    
    if (input$year >= 2000 & input$year <=2001){
      stringtimeline = paste0(stringtimeline,"active,Disease_1,2000-01-01, 2001-12-31
                               Minimum of Gonorrhea:")
    }else{
      stringtimeline = paste0(stringtimeline, "done,Disease_1,2000-01-01, 2001-12-31
                               Minimum of Gonorrhea:")
    }
    
    if (input$year == 2009){
      stringtimeline = paste0(stringtimeline,"active,Disease_2,2009-01-01, 2009-12-31
                          section President 
                          Bill Clinton:")
    }else{
      stringtimeline = paste0(stringtimeline, "done,Disease_2,2009-01-01, 2009-12-31 
                          section President
                          Bill Clinton:")
    }
    
    if (input$year >= 1996 & input$year <=2001){
      stringtimeline = paste0(stringtimeline,"active,Clinton ,1996-01-20, 2001-01-20
                          George W Bush:")
    }else{
      stringtimeline = paste0(stringtimeline, "done,Clinton , 1996-01-20, 2001-01-20
                          George W Bush:")
    }
    
    if (input$year >= 2001 & input$year <=2009){
      stringtimeline = paste0(stringtimeline,"active,Bush,2001-01-20, 2009-01-20
                          Barack Obama:")
    }else{
      stringtimeline = paste0(stringtimeline, "done,Bush,2001-01-20, 2009-01-20
                          Barack Obama:")
    }
    
    if (input$year >= 2009 & input$year <=2014){
      stringtimeline = paste0(stringtimeline,"active,Obama,2009-01-20, 2014-01-20")
    }else{
      stringtimeline = paste0(stringtimeline, "done,Obama,2009-01-20, 2014-01-20")
    }
    
    timeline = mermaid(stringtimeline)
    
    timeline$x$config = list(ganttConfig = list(
      axisFormatter = list(list(
        "%Y"
        ,htmlwidgets::JS(
          'function(d){ return d.getDay() == 1 }'
        )
      ))
    ))
    timeline
  })
}
# Run the application 
shinyApp(ui = ui, server = server)