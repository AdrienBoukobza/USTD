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

ui <- function (){
  fluidPage(theme = shinytheme("flatly"),
                
                shinyjs::useShinyjs(),
                
                navbarPage("USTD",
                           
                           tabPanel ("Home",
                                     verbatimTextOutput("hometext")
                                     ),
                           
                           tabPanel ("Interactive Map",
                                     leafletOutput("map", width = "100%", height = "700"),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                                   draggable = TRUE, top = 80, left = "auto", right = 30, bottom = "auto",
                                                   width = 340, height = "auto", align = "center",
                                                   h2("STD in the USA"),
                                                   
                                                   selectInput ("disease", "Disease", varsDisease, selected = "All"),
                                                   selectInput ("gender", "Gender", varsGender, selected = "All"),
                                                   sliderInput("year", "Year:",
                                                               min = 1996, max = 2014,
                                                               value = 2014, animate =
                                                                 animationOptions(interval = 1500, loop = FALSE)),
                                                   selectInput ("age", "Age class", varsAge, selected = "All"),
                                                   actionButton("preset1", "Chlamydia in young adult female"),
                                                   actionButton("preset2", "Minimum of Gonorrhea"),
                                                   actionButton("preset3", "Minimum of Syphilis")


                                                   
                                     ),
                                     
                                     absolutePanel(id = "curvepannel", class = "panel panel-default", fixed = FALSE,
                                                   draggable = FALSE, top = 300, left = 50, right = "auto", bottom = "auto",
                                                   width = 500, height = 400, align = "center",
                                                   h2("Curve of the STD cases in the USA"),
                                                   plotlyOutput("curvetotal", height = 400)
                                     ),
                                                   
                                     DiagrammeROutput("timeline", width = "100%", height= 200)
                                     
                                     
                                     
                           ),
                           
                           tabPanel ("Curve Explorer",
                                     selectInput("statecurve", "State", states$name),
                                     selectInput("diseasecurve", "Disease", varsDisease[1:3]),
                                     withLoader(plotlyOutput("curve",width = "100%", height = "400px"), type = 'html', loader = "dnaspin")
                           ),
                           
                           tabPanel("Risk calculator",
                                    fluidRow(
                                      
                                             h2("Comparative beetween the different States", align = "center"),
                                      
                                      column(4,offset = 1,
                                             selectInput('OddsDisease', 'Disease', varsDisease[1:3]),
                                             selectInput('OddsState', 'State', states$name),
                                             selectInput("OddsState2", 'State to compare', states$name)
                                      ),
                                      
                                      column(4,
                                             selectInput('OddsGender', 'Gender', varsGender[1:2]),
                                             selectInput('OddsEthnia', 'Ethnia', varsEthnia)
                                             ),
                                      
                                      column(3,
                                             selectInput('OddsAge', 'Age', varsAge[1:7]),
                                             selectInput('OddsYear', "Year", c(1996:2014))
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
                           ),
                           tabPanel("About",
                                    verbatimTextOutput("abouttext")
                                    ),
                           tabPanel("Licence",
                                    verbatimTextOutput("licencetext"),
                                    verbatimTextOutput('licencetext2')
                                    )
                           
                )
)
}
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
    
    stepLegend = 7
    legendRow = c(0)
    
    for (i in 1:9)
    {
      legendRow = c(legendRow, 0 + stepLegend*i)
    }
    
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
    #using the factor in data table
    
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
                             options = list(keys = TRUE, colReorder = TRUE,pageLength = 12, dom = "Bfrtip", buttons = c("copy", "csv", "pdf",I('colvis')), autoWidth = TRUE,
                                            columnDefs = list(list(width = '200px', targets = "_all"))))
  
  
  contingenceTB = reactive ({
  
  
  #Building the contingence Table
  
  #Defining the first condition  
  
  temp = STD %>% 
    filter(Disease == input$OddsDisease) %>% 
    filter(Age_Code == input$OddsAge) %>% 
    filter(Gender == input$OddsGender) %>% 
    filter(State == input$OddsState) %>% 
    filter(`Race/Ethnicity` == input$OddsEthnia) %>% 
    filter(Year == input$OddsYear)
  
  temp2 = STD %>% 
    filter(Disease == input$OddsDisease) %>% 
    filter(Age_Code == input$OddsAge) %>% 
    filter(Gender == input$OddsGender) %>% 
    filter(State == input$OddsState2) %>% 
    filter(`Race/Ethnicity` == input$OddsEthnia) %>% 
    filter(Year == input$OddsYear)
    
  
  #Creating the first value on the first line

  firstline = c()
  if(is.na (as.numeric(temp[1,7])))
  {firstline =c(firstline, 0)}
  else
  {firstline = c(firstline, as.numeric(temp[1,7]))}


  #Creating the first value on second line
  secondline = c()
  if(is.na (as.numeric(temp2[1,7])))
  {secondline =c(secondline, 0)}
  else
  {secondline = c(secondline, as.numeric(temp2[1,7]))}

  #Creating the total values
  total = STD %>% group_by(State) %>% summarise (sum(Population))
  total1 = total %>% filter(State == input$OddsState)
  total2 = total %>% filter(State == input$OddsState2)

  total1 = as.numeric(total1[1,2])
  total2 = as.numeric(total2[1,2])


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
    allstateRR = STD %>% filter(Gender == input$OddsGender) %>% 
      filter(Age_Code == input$OddsAge) %>% 
      filter(Disease == input$OddsDisease) %>% 
      filter(`Race/Ethnicity` == input$OddsEthnia) %>% 
      filter (Year == input$OddsYear)
    
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
    legendRow = 0
    stepLegend = 1.5
    
    for (i in 1:9)
    {
      legendRow = c(legendRow, 0 + stepLegend*i)
    }
    
    
    bins <- legendRow
    pal <- colorBin("YlGnBu", domain = allstateRR$RR, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Multiplication chances: %g ",
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
    
    allstateOR = STD %>% filter(Gender == input$OddsGender) %>% 
      filter(Age_Code == input$OddsAge) %>% 
      filter(Disease == input$OddsDisease) %>% 
      filter(`Race/Ethnicity` == input$OddsEthnia) %>% 
      filter (Year == input$OddsYear)
    
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
      stepLegend = 1.5
      legendRow = 0
      
      for (i in 1:9)
      {
        legendRow = c(legendRow, 0 + stepLegend*i)
      }
      
      legendRow[10] = legendRow[10] + 0.05
    
    bins <- legendRow
    pal <- colorBin("Oranges", domain = allstateOR$OR, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Multiplicator chances: %g ",
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
      setMaxBounds( lng1 = -0
                    , lat1 = 80
                    , lng2 = -180
                    , lat2 = 10 )
  })
  
  output$curve = renderPlotly({
    
    plot = STD %>% filter(State == input$statecurve) %>% filter(Disease == input$diseasecurve) %>%
      group_by(Year) %>% summarise(sum(STD_Cases))
    
    colnames(plot) = c("ds", "y")
    plot$ds = paste (plot$ds, "-01-01", sep = "")
    newplot = prophet(plot)
    
    future = make_future_dataframe(newplot, periods= 10, freq='year')
    forecast <- predict(newplot, future)
    
    new_data <- forecast %>%   
      filter(ds >= as.Date('2014-12-31')) %>%  
      mutate(ds = as.Date(ds),  
             y = yhat)
    
    to_plot <- plot %>% mutate(ds = as.Date(ds)) %>%  
      bind_rows(new_data)
    
    to_plot2 = to_plot %>%  filter(is.na (trend))
    
    to_plot = to_plot %>%  filter(ds >"2014-01-01")
    
    to_plot2 = rbind(to_plot2, to_plot)
    
    p <- ggplot(data = to_plot2, aes(x=ds, y = y, ymin = yhat_lower, ymax = yhat_upper))  +  
      geom_ribbon(alpha = 0.2) +
      geom_line(color = "blue")+ 
      xlab ("Year")+
      ylab ("Number of cases")+
      labs(title = "Prevision of evolution of number of cases")
  })
  
  output$curvetotal = renderPlotly({
    plot2 = STD %>% group_by(Year) %>% summarise(sum(STD_Cases))
    colnames(plot2) = c("Year", "STD_Cases")
    
    plot3 = STD
    
    if(input$age != "All") {plot3 = plot3 %>% filter(Age_Code == input$age)}
    
    if(input$gender != "All") {plot3 = plot3 %>% filter(Gender == input$gender)}
    
    if(input$disease != "All") {plot3 = plot3 %>% filter(Disease == input$disease)}
    
    plot3  = plot3 %>% group_by(Year) %>% summarise (sum(STD_Cases))
    colnames(plot3) = c("Year", "STD_Cases2")
    
    plot2 = left_join(plot2,plot3)
    
    ggplot(plot2, aes(x = plot2$Year))+
      geom_line(aes(y = log(plot2$STD_Cases)),color = "blue")+
      geom_line(aes(y= log(plot2$STD_Cases2)), color = "red")+
      xlab ("Year")+
      ylab ("Log of number of cases")+
      labs(title = "Evolution of total number of cases and filtered")
  })

  output$timeline = renderDiagrammeR({
     
    stringtimeline = "gantt
dateFormat  YYYY-MM-DD
     
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

  output$abouttext = renderText ("Us : We are two students in the French University Paris Descartes.
                                 \nThis project is a part of the evaluation of our first year of master about biomedical computer science.
                                 \nAll our data come from the US CDC site, using the WONDER tool.")
  
  output$hometext = renderText ("The recent raise of STD in the USA is a very hot topic because it could represent a major public health issue within few years. That's why we decided to examine the data about this subject.
                                  \nFirst, to have a global overview of the importance of STD in the USA we chose to make a map showing the density of STD in each state on 1000 inhabitants.
                                  \nThe second point, is a simple computed prevision to see how the number of cases should evolve in few years in each state and for each disease.
                                  \nCAUTION: These results are only mathematical previsions of the cases, and don't have a real predictive value.
                                  \nThe third point allows the user to compare the chances to have a specific disease for a specific population in each state.
                                  \nCAUTION: The risk calculator only works when the two diseased cases are different from 0 and the mapping option only works when the first disease condition is different from 0.
                                  \nRisk ratio is the probability of the outcome of an event in an exposed group compared to a non exposed one.
                                  \nOdds Ratio is quite based on the Risk ratio but also uses the inverse probability of an event to calculate a value.
                                  \nFinally, the user can consult the data and copy it for his own study.")
  
  output$licencetext = renderText("CDC WONDER is a public service developed and operated by the Centers for Disease Control and Prevention, an agency of United States federal government. The public web site at http://wonder.cdc.gov is in the public domain, and only provides access to public use data and information. You may access the information freely, and use, copy, distribute or publish this information without additional or explicit permission. Please do provide a citation to credit the authors and/or data providers. When referring to a written article or document, please cite the item as you would any other document on the world wide web.

All of CDC WONDER's datasets are covered by the following policy:
These data are provided for the purpose of statistical reporting and analysis only. The CDC/ATSDR Policy on Releasing and Sharing Data prohibits linking these data with other data sets or information for the purpose of identifying an individual. If the identity of a individual described in a data set is discovered inadvertently, make no disclosure or other use of this information and report the discovery to:
                                  
                                  Associate Director for Science
                                  Office of Science Policy and Technology Transfer, CDC
                                  Mail Stop D50
                                  Phone: 404-639-7240")
  output$licencetext2 = renderText({"USTD, Visualizing shiny program of STD.
                                   
    Copyright (C) 2019  Adrien Boukobza & François-Xavier Durand

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    Also add information on how to contact you by electronic and paper mail."})
  
  onclick("preset1",{
    updateSelectInput(session, "disease", selected = "Chlamydia")
    updateSelectInput(session, "gender", selected = "Female")
    updateSelectInput(session, "age", selected = "20-24")
    updateSliderInput(session, "year", value = 2014)
  })
  
  onclick("preset2",{
    updateSelectInput(session, "disease", selected = "Gonorrhea")
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "age", selected = "All")
    updateSliderInput(session, "year", value = 2009)
  })
  
  onclick("preset3",{
    updateSelectInput(session, "disease", selected = "Primary and Secondary Syphilis")
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "age", selected = "All")
    updateSliderInput(session, "year", value = 2001)
  })
  }
# Run the application 
shinyApp(ui = ui, server = server)