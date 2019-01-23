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

STD = readRDS("data/STD.rds")

affinefunction = function (a=a, b=b, x=x){
  result = a*x+b
  return (result)
} 


ui <- fluidPage(theme = shinytheme("flatly"),
  
  shinyjs::useShinyjs(),
  
  navbarPage("DescarteSTD",
             tabPanel ("Interactive Map",
                       leafletOutput("map", width = "100%", height = "700"),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                     width = 340, height = "auto", align = "center",
                                     h2("STD in the USA"),
                                     
                                     selectInput ("disease", "Disease", varsDisease),
                                     selectInput ("gender", "Gender", varsGender),
                                     sliderInput("year", "Year:",
                                                 min = 1996, max = 2014,
                                                 value = 1996, animate =
                                                   animationOptions(interval = 1200, loop = FALSE)),
                                     selectInput ("age", "Age class", varsAge)
                                     
                       ),
                       mermaid("
gantt
dateFormat  YYYY-MM-DD
title Some informations

section Disease
Minimum of Syphilis           :active,        Disease_1,    2000-01-01, 2001-12-31
Minimum of Gonorrhea          :active,        Disease_2,    2009-01-01, 2009-12-31

section President
Bill Clinton                  :crit, done,    Clinton ,   1993-01-20, 2001-01-20
George W Bush                 :crit, done,    Bush,       2001-01-20, 2009-01-20
Barack Obama                  :crit, done,    Obama,      2009-01-20, 2017-01-20
")
                       
             ),
             
             tabPanel("Clinics explorer"),
             
             tabPanel ("Curve Explorer",
                       selectInput("statecurve", "State", states$name),
                       selectInput("diseasecurve", "Disease", varsDisease[1:3]),
                       sliderInput("yearcurve", "Projection Year:",
                                   min = 2015, max = 2100,
                                   value = 2015, animate =
                                     animationOptions(interval = 1200, loop = FALSE)),
                       plotlyOutput("curve",width = "100%", height = "400px")
                       ),
             
             tabPanel("Odds Ratio",
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
                    )
                    
             ),
             
             tabPanel("Data Explorer",
                      DTOutput ("mapTable")
             )
             
  )
)

server <- function(input, output, session) {
  
  output$map = renderLeaflet ({
    states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addTiles()
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
    
    
    #Creating two lists : one with all the names of the states and one with our selection's
    
    states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
    
    temp = states$name
    temp2 = STD1$State
    
    #Initializing the 3 variables used in the loop
    i=1 #Representing the position in the states list
    j=1 #Representing the position in our selection list
    temp3 = c() #Generating a vector with all of the informations in our list
    temp4 = c() #Generating a vector with the rates
    temp5 = c()
    
    while (i < 53)
    {
      if(!is.na(temp [i] == temp2[j])) { #Avoid the error with NA values
        if (temp [i] == temp2[j])
        {
          temp3 = c(temp3,STD1[j,"RateCalc"]) #Saving the value in a vector
          temp4 = c(temp4, STD1[j, "STD_Cases"]) #Saving the rates
          temp5 = c(temp5, STD1Pop[j,"Population"]) #Saving the population
          i = i+1 #Next step on the state list
          j = j+1 #Next step on our selection list
        }
      }
      if(temp [i] != temp2[j] |is.na(temp [i] != temp2[j]))
      {
        temp3 = c(temp3, 0) #If the state isn't in our filtered list, it has 0 people with the disease
        temp4 = c(temp4, 0)
        temp5 = c(temp5, 0)
        i =i+1 #Next step of the state list
      }
      
    }
    
    temp3 =unlist(temp3, use.names=FALSE) 
    
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
    pal <- colorBin("YlOrRd", domain = temp3, bins = bins)
    
    #Preparing the labels

    meancountrypop = STD %>% filter(Disease == input$disease) %>% filter (Year == input$year) %>% 
      group_by(Disease) %>% summarise (sum(Population))
    meancountrypop = as.numeric(meancountrypop[1,2])
    
    meancountrycases = STD %>% filter(Disease == input$disease) %>% filter (Year == input$year) %>% 
      group_by(Disease) %>% summarise (sum(STD_Cases))
    meancountrycases = as.numeric(meancountrycases[1,2])
    
    meancountry = (meancountrycases *1000 )/ meancountrypop
    
    for (i in 1:51){
      meancountry = c(meancountry, meancountry[1])}

    labels <- sprintf(
      "<strong>%s</strong><br/>%g cases in the state <br/>Average: %g <br/> Population: %g <br/> Remind, country mean: %g",
      states$name, temp4, temp3,temp5, meancountry
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy ("map")  %>% addPolygons(data = states,
      fillColor = ~pal(temp3),
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
        direction = "auto")) %>%  clearControls() %>% addLegend(pal = pal, values = temp3, opacity = 0.85,
                                                                title = "for 1 000 inhabitants of a class of age",
                                                                position = "bottomright")
      })
    
  output$mapTable = renderDT(STD, extensions = c("Buttons", "ColReorder", 'KeyTable'), 
                             filter = "top",  
                             options = list(keys = TRUE, colReorder = TRUE,pageLength = 20, dom = "Bfrtip", buttons = c("copy", "csv", "pdf",I('colvis'))))
  
  output$contingence = renderDT({
    
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
    total = STD2 %>% group_by(State) %>% summarise (sum(Population))
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
    
    write.csv(contingenceTB, "data/Contingence.csv")
    contingenceTB
    
  })
    
onclick ("RRbutton", {

  contingence <- read_csv("data/Contingence.csv",
                          col_types = cols(X1 = col_skip()))
  
  if(contingence [1,4] !=0 && contingence[2,2] != 0)
    {
    RR = as.numeric((contingence[1,2] / contingence [1,4]) / (contingence[2,2] / contingence [2,4]))
    updateTextInput(session, "RR",label = "Risk Ratio", value = RR)
  
    }else {updateTextInput(session, "RR",label = "Risk Ratio", value = "Condition isn't applicable")}
})

onclick ("ORbutton", {
  
  contingence <- read_csv("data/Contingence.csv",
                          col_types = cols(X1 = col_skip()))
  
  if(contingence [1,3] !=0 && contingence[2,2] != 0)
  {
    OR = as.numeric((contingence[1,2] / contingence [1,3]) / (contingence[2,2] / contingence [2,3]))
    updateTextInput(session, "OR",label = "Odds Ratio", value = OR)
    
  }else {updateTextInput(session, "OR",label = "Risk Ratio", value = "Condition isn't applicable")}
})

onclick("MapRRbutton",{
  
  contingence <- read_csv("data/Contingence.csv",
                          col_types = cols(X1 = col_skip()))
}) 

output$curve = renderPlotly({
  
  plot = STD %>% filter(State == input$statecurve) %>% filter(Disease == input$diseasecurve) %>%
    group_by(Year) %>% summarise(sum(STD_Cases))
  colnames(plot) = c("Year", "Cases")
  
  a = (plot[19,2]- plot[1,2])/18
  b = plot[19,2] - 2014*a
  x = input$yearcurve
  i = 0
  temp = c()
  temp2 = c()
  
  for (i in 2015:x){
    temp = c(temp,i)
    if (affinefunction (a,b,i) >= 0){temp2 = c(temp2, affinefunction(a,b,i))}
    else {temp2 = c(temp2,0)}
  }
  newRows = data.frame(Year = unlist(temp), Cases = unlist(temp2))
  plot = rbind (plot, newRows)
  
  ggplot(plot, aes(x = plot$Year, y = plot$Cases, fill = plot$Cases)) +
    geom_bar(stat = "identity") +
    xlab ("Year")+
    ylab ("Number of cases")+
    ggtitle ("Representation of the evolution of the cases in few next years")+
    labs (fill = "Number of cases" )

})

}
# Run the application 
shinyApp(ui = ui, server = server)

