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

STD = readRDS("STD.rds")

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  navbarPage("DescarteSTD",
             tabPanel ("Interactive Map",
                       leafletOutput("map", width = "100%", height = "700"),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                     width = 340, height = "auto", 
                                     h2("STD in the USA"),
                                     
                                     selectInput ("disease", "Disease", varsDisease),
                                     selectInput ("gender", "Gender", varsGender),
                                     sliderInput("year", "Year:",
                                                 min = 1996, max = 2014,
                                                 value = 2000, animate =
                                                   animationOptions(interval = 2000, loop = TRUE)),
                                     selectInput ("age", "Age class", varsAge)
                                     
                       ),
                       actionButton("savemap", "Save map")
             ),
             
             tabPanel("Clinics explorer"),
             
             tabPanel ("Curve Explorer"),
             
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
                               checkboxInput("exclude", "Exclude",value = TRUE),
                               selectInput("excluded", "Excluded", c("Gender", "Age", "Disease", "State", "Ethnia"))
                        ),
                        column(4,
                               selectInput('OddsGender2', 'Second Gender',varsGender[1:2]),
                               selectInput('OddsAge2', 'Second Age', varsAge[1:7]),
                               selectInput('OddsDisease2', 'Second Disease', varsDisease[1:3]),
                               selectInput('OddsState2', 'Second State', states$name),
                               selectInput('OddsEthnia2', 'Second Ethnia', varsEthnia)
                        )
                      ),
                      DTOutput ("contingence") 
             ),
             
             tabPanel("Data Explorer",
                      DTOutput ("mapTable")
             )
             
  )
)

server <- function(input, output, session) {
  
  output$map = renderLeaflet ({
    
    #Creating a copy to work on
    
    allDiseaseWork = allDisease
    
    #Filtering data, preventing the analyzer to filter the "All" Option.
    
    if(input$disease == "All") {}
    else
    {
      allDiseaseWork = allDiseaseWork %>% filter(Disease == input$disease)
    }
    
    if(input$gender == "All"){}
    else
    {
      allDiseaseWork = allDiseaseWork %>% filter(Gender == input$gender)
    }
    
    if(input$age == "All"){}
    else
    {
      allDiseaseWork = allDiseaseWork %>% filter(Age_Code == input$age)
    }
    
    allDiseaseWork = allDiseaseWork %>% filter (Year == input$year)
    
    # Creating two new DT : one to summarise the cases and one to summarise the population
    allDiseaseWorkCases = allDiseaseWork %>% group_by(State) %>% summarise(STD_Cases = sum(STD_Cases))
    allDiseaseWorkPop = allDiseaseWork %>% group_by(State) %>% summarise (Population = sum(Population))
    
    #Setting the old work as the new base for the cases
    allDiseaseWork = allDiseaseWorkCases
    rm(allDiseaseWorkCases)
    
    #Creating the rates in the working table
    allDiseaseWork = allDiseaseWork %>% mutate (RateCalc = allDiseaseWork$STD_Cases * 1000 / allDiseaseWorkPop$Population)
    rm(allDiseaseWorkPop)
    
    
    #Creating two lists : one with all the names of the states and one with our selection's
    
    temp = states$name
    temp2 = allDiseaseWork$State
    
    #Initializing the 3 variables used in the loop
    i=1 #Representing the position in the states list
    j=1 #Representing the position in our selection list
    temp3 = c() #Generating a vector with all of the informations in our list
    temp4 = c() #Generating a vector with the rates
    while (i < 53)
    {
      if(!is.na(temp [i] == temp2[j])) { #Avoid the error with NA values
        if (temp [i] == temp2[j])
        {
          temp3 = c(temp3,allDiseaseWork[j,"RateCalc"]) #Saving the value in a vector
          temp4 = c(temp4, allDiseaseWork[j, "STD_Cases"])
          i = i+1 #Next step on the state list
          j = j+1 #Next step on our selection list
        }
      }
      if(temp [i] != temp2[j] |is.na(temp [i] != temp2[j])) 
      {
        temp3 = c(temp3, 0) #If the state isn't in our filtered list, it has 0 people with the disease
        temp4 = c(temp4, 0)
        i =i+1 #Next step of the state list
      }
      
    }
    
    temp3 =unlist(temp3, use.names=FALSE)
    
    states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
    
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    ## Preparing the legend
    minallDiseaseWork =  min(allDiseaseWork$RateCalc)
    maxallDiseaseWork = max(allDiseaseWork$RateCalc)
    
    stepLegend = (maxallDiseaseWork - minallDiseaseWork) / 9
    legendRow = c(0)
    
    for (i in 1:9)
    {
      legendRow = c(legendRow, minallDiseaseWork + stepLegend*i)    
    }
    
    bins <- legendRow
    pal <- colorBin("YlOrRd", domain = temp3, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g cases in the state",
      states$name, temp4
    ) %>% lapply(htmltools::HTML)
    
    m =  m %>% addTiles () %>% addPolygons(
      fillColor = ~pal(temp3),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% 
      addLegend(pal = pal, values = ~temp3, opacity = 0.7, title = "for 1 000 inhabitants of a class of age",
                position = "bottomright")
    
  })
  
  output$mapTable = renderDT(allDisease, extensions = c("Buttons", "ColReorder", 'KeyTable'), 
                             filter = "top",  
                             options = list(keys = TRUE, colReorder = TRUE,pageLength = 20, dom = "Bfrtip", buttons = c("copy", "csv", "pdf",I('colvis'))))
  
  observeEvent(input$savemap, {
    
    temp = Sys.time()
    temp =str_replace_all(temp, "[[:punct:]]", "")
    temp = str_replace_all (temp, " ", "")
    
    mapshot (m, file = paste0("map",temp,".png"))
  })
  output$contingence = renderDT({
    
    #Building the contingence Table
    
    # Defining which is the value to exclude here
    
    deactivate = 0
    
    if (input$excluded == "Gender") {deactivate = 1}
    if (input$excluded == "Age") {deactivate = 2}
    if (input$excluded == "Disease") {deactivate = 3}
    if (input$excluded == "State") {deactivate = 4}
    if (input$excluded == "Ethnia") {deactivate = 5}
    
    toggleState("OddsGender", input$exclude != TRUE | deactivate != 1)
    toggleState("OddsGender2", input$exclude != TRUE | deactivate != 1)
    toggleState("OddsAge", input$exclude != TRUE | deactivate!= 2)
    toggleState("OddsAge2", input$exclude != TRUE | deactivate != 2)
    toggleState("OddsDisease", input$exclude != TRUE | deactivate != 3)
    toggleState("OddsDisease2", input$exclude != TRUE | deactivate != 3)
    toggleState("OddsState", input$exclude != TRUE | deactivate != 4)
    toggleState("OddsState2", input$exclude != TRUE | deactivate!= 4)
    toggleState("OddsEthnia", input$exclude != TRUE | deactivate != 5)
    toggleState("OddsEthnia2", input$exclude != TRUE | deactivate!= 5)
    
    #Defining the first condition  
    
    # temp =allDiseaseEthnia %>% filter(Gender == input$OddsGender) %>%  filter(Age_Code == input$OddsAge) %>% 
    #      filter(Disease == input$OddsDisease) %>% 
    #      filter(State == input$OddsState) %>%
    #      filter(`Race/Ethnicity` == input$OddsEthnia)
    
    temp = allDiseaseEthnia
    
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
    
    
    
    
    #Creating the first vector for the first line
    
    firstline = c()
    if(is.na (as.numeric(temp[1,7])))
    {firstline =c(firstline, 0)}
    else  
    {firstline = c(firstline, as.numeric(temp[1,7]))}
    
    #Defining the second condition
    temp2 =allDiseaseEthnia %>% filter(Gender == input$OddsGender2) %>%  filter(Age_Code == input$OddsAge2) %>% 
      filter(Disease == input$OddsDisease2) %>% 
      filter(State == input$OddsState2) %>%
      filter(`Race/Ethnicity` == input$OddsEthnia2)
    
    #Creating the second vector for the second line
    secondline = c()
    if(is.na (as.numeric(temp2[1,7])))
    {secondline =c(secondline, 0)}
    else  
    {secondline = c(secondline, as.numeric(temp2[1,7]))}
    
    #Creating the total values
    total1 = allDiseaseEthnia %>% group_by(State) %>% summarise (sum(Population))
    # total1 = total1 %>% filter(State == )
    
    
    #Creating the table itself
    contingenceTB = data.frame ("Titles" = c("First Condition", "Second Condition", "Total"),
                                "Diseased" = c(firstline[1], secondline[1], firstline[1]+secondline[1]), 
                                "Non.Diseased" =c("", "", ""),
                                "Total" = c("", "", ""))
    contingenceTB
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

