source("data_n_deps.R")

server <- function(input, output, session)
{
  output$map = renderLeaflet(
    {
      m <- leaflet(states, options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
        setView(-96, 37.8, 4) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
                                                                 id = "mapbox.light",
                                                                 accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addTiles() %>%
        setMaxBounds(lng1 = -0, lat1 = 80, lng2 = -180, lat2 = 10)
    })

  observe(
    {
      #Creating a copy to work on
      if (input$navbar =="Interactive Map")
        STD1 <- STD
      else
        STD1 <- STD

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
      
      if (input$disease == "All")
        STD1Pop = STD1 %>% filter(Disease == "Chlamydia") %>% group_by(State) %>% summarise(Population = sum(Population)) 

      #Setting the old work as the new base for the cases
      STD1 = STD1Cases
      rm(STD1Cases)

      #Creating the rates in the working table
      STD1 = STD1 %>% mutate (RateCalc = STD1$STD_Cases * 1000 / STD1Pop$Population)

      #Completing the table with the populations
      STD1 = STD1 %>% mutate(Population = STD1Pop$Population)

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
      legendRow = seq(0,54,6)

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

      meancountry  = rep(meancountry, 52)

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
                                        position = "bottomleft")
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

  observe({
    if(input$Factor == "Disease")
      updateSelectInput(session, "Comparison", choices = STD %>% group_by(Disease) %>% summarise() %>% unlist(use.names = FALSE))
    if(input$Factor == "Gender")
      updateSelectInput(session, "Comparison", choices = STD %>% group_by(Gender) %>% summarise() %>% unlist(use.names = FALSE))
    if(input$Factor == "Ethnia")
      updateSelectInput(session, "Comparison", choices = STD %>% group_by(`Race/Ethnicity`) %>% summarise() %>% unlist(use.names = FALSE))
    if(input$Factor == "Year")
      updateSelectInput(session, "Comparison", choices = 1996:2014)
    if(input$Factor == "Age")
      updateSelectInput(session, "Comparison", choices = STD %>% group_by(Age_Code) %>% summarise() %>% unlist(use.names = FALSE))
  })

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

    temp2 = STD
    
    #Defining the second condition
    
    if(input$Factor == "Disease")
      temp2 <- temp2 %>% filter(Disease == input$Comparison)
    else
      temp2 <- temp2 %>% filter(Disease == input$OddsDisease)
    
    if(input$Factor == "Age")
      temp2 <- temp2 %>% filter(Age_Code == input$Comparison)
    else
      temp2 <- temp2 %>% filter(Age_Code ==input$OddsAge)
    
    if(input$Factor == "Gender")
      temp2 <- temp2 %>% filter(Gender == input$Comparison)
    else
      temp2 <- temp2 %>% filter(Gender == input$OddsGender)
    
    if(input$Factor == "Ethnia")
      temp2 <- temp2 %>% filter(`Race/Ethnicity` == input$Comparison)
    else
      temp2 <- temp2 %>% filter(`Race/Ethnicity`== input$OddsEthnia)
    
    if(input$Factor == "Year")
      temp2 <- temp2 %>% filter(Year == input$Comparison)
    else
      temp2 <- temp2 %>% filter(Year == input$OddsYear)
    
    if(is.na (as.numeric(temp[1,7])))
    {
      temp[1,7] <- 0
      temp[1,1] <- input$OddsDisease
      temp[1,2] <- input$OddsState
      temp[1,3] <- input$OddsYear
      temp[1,4] <- input$OddsEthnia
      temp[1,6] <- input$OddsAge
      temp[1,9] <- input$OddsGender
      temp[1,8] <- 0
    }
   
      temp <- rbind(temp,temp2)
      STDCondition <- temp [1,7]
      PopulationCondition <- temp [1,8]

      
      #Generating the RR
      temp <- temp %>% mutate(nonDiseased = Population - STD_Cases)
      temp<- temp %>% mutate(STDonPopulation = STD_Cases / Population)
      temp <- temp %>% mutate(ReferenceSTDonPop = as.numeric(STDCondition /PopulationCondition ))
      temp <- temp %>% mutate (RR = as.numeric(ReferenceSTDonPop/STDonPopulation))
      
      NonDiseased <- temp [1,11]                        
      #Generating the OR
      temp <- temp %>% mutate(STDonNon = STD_Cases / nonDiseased)
      temp <- temp %>% mutate (ReferenceSTDonNon = as.numeric(STDCondition / NonDiseased))
      temp <- temp %>% mutate(OR = as.numeric (ReferenceSTDonNon / STDonNon))
       
  })

  output$contingence = renderDT({
    contingenceTB ()
  })

  observeEvent(input$MapRRbutton,
               {
            allstateRR = contingenceTB ()
            difference = tibble(Disease = "",
                                State = setdiff(states$name,allstateRR$State),
                                Year = 0,
                                `Race/Ethnicity` = "",
                                Age ="",
                                Age_Code = "",
                                STD_Cases = 0,
                                Population = 0,
                                Gender = "",
                                Gender_Code = "",
                                nonDiseased = "",
                                STDonPopulation = "",
                                ReferenceSTDonPop = "",
                                RR = 0,
                                STDonNon = "",
                                ReferenceSTDonNon = "",
                                OR = 0
            )

            #Fusionning the two data and ordering to use with the map
            allstateRR = rbind(allstateRR,difference)
            allstateRR = allstateRR[order(match(allstateRR$State, states$name)),]

            total = STD %>% group_by(State) %>% summarise (sum(Population))
            difference = tibble(State = setdiff(states$name,total$State),
                                `sum(Population)` = 0
            )
            total = rbind(total, difference)

            ## Preparing the legend
            legendRow = seq(0,54,6)

            bins <- legendRow
            pal <- colorBin("YlGnBu", domain = allstateRR$RR[2:53], bins = bins)

            labels <- sprintf(
                              "<strong>%s</strong><br/>Multiplication chances: %g ",
                              states$name, allstateRR$RR[2:53]
                              ) %>% lapply(htmltools::HTML)

            leafletProxy ("map2")  %>% addPolygons(data = states,
                                                   fillColor = ~pal(allstateRR$RR[2:53]),
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
                                                                          direction = "auto")) %>%  clearControls() %>% addLegend(pal = pal, values = allstateRR$RR[2:53], opacity = 0.85,
                                              title = "Risk Ratio",
                                              position = "bottomright")




  })

  observeEvent(input$MapORbutton,
               {
            allstateOR = contingenceTB()
            difference = tibble(Disease = "",
                                State = setdiff(states$name,allstateOR$State),
                                Year = 0,
                                `Race/Ethnicity` = "",
                                Age ="",
                                Age_Code = "",
                                STD_Cases = 0,
                                Population = 0,
                                Gender = "",
                                Gender_Code = "",
                                nonDiseased = "",
                                STDonPopulation = "",
                                ReferenceSTDonPop = "",
                                RR = 0,
                                STDonNon = "",
                                ReferenceSTDonNon = "",
                                OR = 0
            )

            #Fusionning the two data and ordering to use with the map
            allstateOR = rbind(allstateOR,difference)
            allstateOR = allstateOR[order(match(allstateOR$State, states$name)),]

            total = STD %>% group_by(State) %>% summarise (sum(Population))
            difference = tibble(State = setdiff(states$name,total$State),
                                `sum(Population)` = 0
            )
            total = rbind(total, difference)

            ## Preparing the legend
            legendRow = seq(0,54,6)

            bins <- legendRow
            pal <- colorBin("Oranges", domain = allstateOR$OR[2:53], bins = bins)

            labels <- sprintf(
                              "<strong>%s</strong><br/>Multiplicator chances: %g ",
                              states$name, allstateOR$OR[2:53]
                              ) %>% lapply(htmltools::HTML)

            leafletProxy ("map2")  %>% addPolygons(data = states,
                                                   fillColor = ~pal(allstateOR$OR[2:53]),
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
                                                                          direction = "auto")) %>%  clearControls() %>% addLegend(pal = pal, values = allstateOR$OR[2:53], opacity = 0.85,
                                              title = "Odds Ratio",
                                              position = "bottomright")



  })

  output$map2 = renderLeaflet ({
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

    plot <- STD
    
    if(input$statecurve != "All")
      plot = plot %>% filter(State == input$statecurve)
      
    if (input$diseasecurve != "All")
      plot = plot %>% filter(Disease == input$diseasecurve)
    
    plot = plot %>% group_by(Year) %>% summarise(sum(STD_Cases))

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

  output$curvetotal = renderPlot({
    plot2 = STD %>% group_by(Year) %>% summarise(sum(STD_Cases))
    colnames(plot2) = c("Year", "STD_Cases")

    plot3 = STD

    if(input$age != "All") {plot3 = plot3 %>% filter(Age_Code == input$age)}

    if(input$gender != "All") {plot3 = plot3 %>% filter(Gender == input$gender)}

    if(input$disease != "All") {plot3 = plot3 %>% filter(Disease == input$disease)}

    plot3  = plot3 %>% group_by(Year) %>% summarise (sum(STD_Cases))
    colnames(plot3) = c("Year", "STD_Cases2")

    plot2 = left_join(plot2,plot3)

    ggplot(plot2)+
      geom_line(mapping = aes(x = Year, y = log(STD_Cases),color = "All cases"), size = 1)+
      geom_line(mapping = aes(x = Year, y= log(STD_Cases2),color = "Filtered cases"),  size = 1)+
      xlab ("Year")+
      ylab ("Log of number of cases")+
      labs(title = "Evolution of total number of cases and filtered")+
      theme(legend.position = "right")+
      guides(color=guide_legend("Number of cases"))
  })

  observeEvent(input$preset1,
          {
            updateSelectInput(session, "disease", selected = "Chlamydia")
            updateSelectInput(session, "gender", selected = "Female")
            updateSelectInput(session, "age", selected = "20-24")
            updateSliderInput(session, "year", value = 2014)
          })

  observeEvent(input$preset2,
          {
            updateSelectInput(session, "disease", selected = "Gonorrhea")
            updateSelectInput(session, "gender", selected = "All")
            updateSelectInput(session, "age", selected = "All")
            updateSliderInput(session, "year", value = 2009)
          })

  observeEvent(input$preset3,
          {
            updateSelectInput(session, "disease", selected = "Primary and Secondary Syphilis")
            updateSelectInput(session, "gender", selected = "All")
            updateSelectInput(session, "age", selected = "All")
            updateSliderInput(session, "year", value = 2001)
          })
}


