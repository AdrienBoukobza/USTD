source("data_n_deps.R")

server <- function(input, output, session)
{
  # Create the first map leaftlet object
  output$map <- renderLeaflet(
  {
    leaflet(data = states, options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
      setView(-96, 37.8, 4) %>%
      addTiles %>%
      setMaxBounds(lng1 = -0, lat1 = 80, lng2 = -180, lat2 = 10)
  })

  # Reactive subpopulation object, common to map and prevalence curve
  subpop <- reactive(
  {
    STD %>%
      filter(Age     == input$age     | input$age     == "All",
             Gender  == input$gender  | input$gender  == "All",
             Disease == input$disease | input$disease == "All")
  })

  populations <- reactive(
  {
    # Extract the population data only
    STD %>%
      select(State, Year, Ethnicity, Age, Gender, Population) %>%
      distinct %>%
      filter(Age     == input$age     | input$age     == "All",
             Gender  == input$gender  | input$gender  == "All")
  })

  rates <- reactive(
  {
    req(subpop(), populations())

    populations() %>%
      group_by(State, Year) %>%
      summarise(Population = sum(Population)) %>%
      left_join(subpop() %>%
                  group_by(State, Year) %>%
                  summarise(STD_Cases = sum(STD_Cases))) %>%
      mutate(Rate = 1000 * STD_Cases / Population)
  })

  legende <- reactive(
  {
    req(rates())

    max(rates()$Rate, na.rm = T) -> maxrate

    colorBin("YlOrRd", domain = c(0, maxrate), 9)
  })

  # Update the map according to the UI
  observe(
  {
    req(rates(), legende(), input$navbar)

    states %>%
      left_join(rates() %>% filter(Year == input$year), by = c("name" = "State")) -> mapdata

    # Compute mean country rate
    (1000 *
      (STD %>%
        filter(Disease == input$disease | input$disease == "All",
               Year    == input$year) %>%
        summarise(STD_Cases = sum(STD_Cases)) %>%
        pull(STD_Cases)) /
      (populations() %>%
        filter(Year == input$year) %>%
        pull(Population) %>%
        sum)) %>%
    rep(52) -> meancountry

    labels <- sprintf("<strong>%s</strong><br/>%g cases in the state <br/>%g cases / 1000 hab<br/>Population: %.10g<br/>Country mean: %g",
                     mapdata$name, mapdata$STD_Cases, mapdata$Rate, mapdata$Population, meancountry) %>%
      lapply(htmltools::HTML)

    leafletProxy("map") %>%
      addPolygons(data        = states,
                  fillColor   = ~ legende()(mapdata$Rate),
                  weight      = 1,
                  opacity     = 0.7,
                  color       = "grey",
                  dashArray   = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight       = 5,
                                               color        = "#666",
                                               dashArray    = "",
                                               fillOpacity  = 0.85,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style     = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize  = "15px",
                                              direction = "auto")) %>%
    clearControls() %>%
    addLegend(pal      = legende(),
              values   = mapdata$Rate,
              opacity  = 0.85,
              title    = "for 1 000 inhabitants of the selected subpopulation",
              position = "bottomleft")
  })

  # Render the prevalence curve
  output$curvetotal <- renderPlot(
  {
    req(subpop())

    # Select patients from UI
    subpop() %>%
    # Summarise by year
    group_by(Year) %>%
    summarise(STD_Cases = sum(STD_Cases, na.rm = T)) %>%
    mutate(group = "Filtered cases") %>%
    # Add total population
    bind_rows(STD %>%
              group_by(Year) %>%
              summarise(STD_Cases = sum(STD_Cases)) %>%
              mutate(group = "All cases")) %>%

    # Plot
    ggplot() +
      aes(x = Year, y = STD_Cases, color = group) +
      geom_line() +
      scale_y_log10() +
      xlab ("Year") +
      ylab ("Number of cases") +
      labs(title = "Evolution of total number of cases and filtered") +
      theme(legend.position = "right") +
      guides(color = guide_legend(""))
  })

  output$curveincidence <- renderPlot(
    {
      subpop() %>%
        group_by(Year) %>%
        summarise(STD_Cases = sum(STD_Cases, na.rm = T)) %>%
        mutate(STD_Cases = STD_Cases - lag(STD_Cases)) %>%

        # Plot
        ggplot() +
        aes(x = Year, y = STD_Cases) +
        geom_line() +
        xlab ("Year") +
        ylab ("Incidence of filtered cases") +
        labs(title = "Evolution of incidence of filtered cases") +
        theme(legend.position = "right") +
        guides(color = guide_legend(""))
    })

  ## Create the risk table TODO: fix (celui-là je m'en occuperai)
  #output$contingence <- renderDT(
  #{
  #  #Building the contingence Table
  #  #Defining the first condition
  #  temp <- STD %>%
  #    filter(Disease   == input$OddsDisease,
  #           Age       == input$OddsAge,
  #           Gender    == input$OddsGender,
  #           State     == input$OddsState,
  #           Ethnicity == input$OddsEthnicity,
  #           Year      == input$OddsYear)

  #  temp2 <- STD

  #  #Defining the second condition

  #  # if (input$Factor == "Disease")
  #  #   temp2 <- temp2 %>% filter(Disease == input$Comparison)
  #  # else
  #  #   temp2 <- temp2 %>% filter(Disease == input$OddsDisease)

  #  # if (input$Factor == "Age")
  #  #   temp2 <- temp2 %>% filter(Age == input$Comparison)
  #  # else
  #  #   temp2 <- temp2 %>% filter(Age == input$OddsAge)

  #  # if (input$Factor == "Gender")
  #  #   temp2 <- temp2 %>% filter(Gender == input$Comparison)
  #  # else
  #  #   temp2 <- temp2 %>% filter(Gender == input$OddsGender)

  #  # if (input$Factor == "Ethnicity")
  #  #   temp2 <- temp2 %>% filter(Ethnicity == input$Comparison)
  #  # else
  #  #   temp2 <- temp2 %>% filter(Ethnicity== input$OddsEthnicity)

  #  # if (input$Factor == "Year")
  #  #   temp2 <- temp2 %>% filter(Year == input$Comparison)
  #  # else
  #  #   temp2 <- temp2 %>% filter(Year == input$OddsYear)

  #  if (is.na(as.numeric(temp[1, 7])))
  #  {
  #    temp[1, 7] <- 0
  #    temp[1, 1] <- input$OddsDisease
  #    temp[1, 2] <- input$OddsState
  #    temp[1, 3] <- input$OddsYear
  #    temp[1, 4] <- input$OddsEthnicity
  #    temp[1, 6] <- input$OddsAge
  #    temp[1, 9] <- input$OddsGender
  #    temp[1, 8] <- 0
  #  }

  #  temp <- rbind(temp, temp2)
  #  STDCondition <- temp[1, 7]
  #  PopulationCondition <- temp[1, 8]

  #  #Generating the RR
  #  temp <- temp %>% mutate(nonDiseased = Population - STD_Cases)
  #  temp <- temp %>% mutate(STDonPopulation = STD_Cases / Population)
  #  temp <- temp %>% mutate(ReferenceSTDonPop = as.numeric(STDCondition / PopulationCondition))
  #  temp <- temp %>% mutate (RR = as.numeric(ReferenceSTDonPop / STDonPopulation))

  #  NonDiseased <- temp[1, 11]
  #  #Generating the OR
  #  temp <- temp %>% mutate(STDonNon = STD_Cases / nonDiseased)
  #  temp <- temp %>% mutate (ReferenceSTDonNon = as.numeric(STDCondition / NonDiseased))
  #  temp <- temp %>% mutate(OR = as.numeric (ReferenceSTDonNon / STDonNon))

  #  temp
  #})

  ## Create the risk map object
  #output$map2 <- renderLeaflet(
  #{
  #  leaflet(options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
  #    setView(-96, 37.8, 4) %>%
  #    addTiles %>%
  #    setMaxBounds(lng1 = -0,
  #                 lat1 = 80,
  #                 lng2 = -180,
  #                 lat2 = 10)
  #})

  ## Update risk map for RR TODO: fix
  #observeEvent(input$MapRRbutton,
  #{
  #  allstateRR <- contingenceTB ()
  #  difference <- tibble(Disease           = "",
  #                       State             = setdiff(states$name, allstateRR$State),
  #                       Year              = 0,
  #                       Ethnicity         = "",
  #                       Age               = "",
  #                       Age_Code          = "",
  #                       STD_Cases         = 0,
  #                       Population        = 0,
  #                       Gender            = "",
  #                       Gender_Code       = "",
  #                       nonDiseased       = "",
  #                       STDonPopulation   = "",
  #                       ReferenceSTDonPop = "",
  #                       RR                = 0,
  #                       STDonNon          = "",
  #                       ReferenceSTDonNon = "",
  #                       OR                = 0)

  #  #Fusionning the two data and ordering to use with the map
  #  allstateRR <- rbind(allstateRR, difference)
  #  allstateRR <- allstateRR[order(match(allstateRR$State, states$name)), ]

  #  total <- STD %>% group_by(State) %>% summarise(sum(Population))
  #  difference <- tibble(State = setdiff(states$name, total$State), `sum(Population)` = 0)
  #  total <- rbind(total, difference)

  #  ## Preparing the legend
  #  legendRow <- seq(0, 54, 6)

  #  bins <- legendRow
  #  pal <- colorBin("YlGnBu", domain = allstateRR$RR[2:53], bins = bins)

  #  labels <- sprintf("<strong>%s</strong><br/>Multiplication chances: %g ",
  #                    states$name, allstateRR$RR[2:53]) %>%
  #    lapply(htmltools::HTML)

  #  leafletProxy("map2") %>%
  #    addPolygons(data        = states,
  #                fillColor   = ~pal(allstateRR$RR[2:53]),
  #                weight      = 1,
  #                opacity     = 0.7,
  #                color       = "grey",
  #                dashArray   = "3",
  #                fillOpacity = 0.7,
  #                highlight = highlightOptions(weight       = 5,
  #                                             color        = "#666",
  #                                             dashArray    = "",
  #                                             fillOpacity  = 0.85,
  #                                             bringToFront = TRUE),
  #                label = labels,
  #                labelOptions = labelOptions(style     = list("font-weight" = "normal", padding = "3px 8px"),
  #                                            textsize  = "15px",
  #                                            direction = "auto")) %>%
  #  clearControls() %>%
  #  addLegend(pal      = pal,
  #            values   = allstateRR$RR[2:53],
  #            opacity  = 0.85,
  #            title    = "Risk Ratio",
  #            position = "bottomright")
  #})

  # Render the forecast curve
  output$curve <- renderDygraph(
  {
    # Create training object
    STD %>%
      # Filter input
      filter(State   == input$statecurve   | input$statecurve   == "All",
             Disease == input$diseasecurve | input$diseasecurve == "All") %>%
      # Prepare for prophet
      group_by(Year) %>%
      summarise(y = sum(STD_Cases)) %>%
      mutate(Year = str_c(Year, "-01-01") %>% as.POSIXct) %>%
      rename(ds = Year) %>%
      # Augment the data with cubic spline interpolation
      spline(n = 100, method = "natural") %>%
      # Rebuild an usable dataframe
      as.data.frame %>%
      rename(ds = x) %>%
      mutate(ds = ds %>% as.POSIXct(origin = "1970-01-01")) -> train

    # Train the model
    train %>%
      prophet -> model

    # Predict the values
    model %>%
      predict(make_future_dataframe(., periods = 10, freq = "year")) %>%
      # Plot the result
      dyplot.prophet(model, .)
  })

  # Create the raw data DT
  output$mapTable <- renderDT(
  {
    STD %>%
      mutate_if(is.character, factor)
  },
  extensions = c("Buttons", "ColReorder", 'KeyTable'),
  filter = "top",
  options = list(keys       = TRUE,
                 colReorder = TRUE,
                 pageLength = 12,
                 dom        = "Bfrtip",
                 buttons    = c("copy", "csv", "pdf", I('colvis')),
                 autoWidth  = TRUE,
                 columnDefs = list(list(width = '200px', targets = "_all"))))

  # Create the presets
  observeEvent(input$preset1,
  {
    updateSelectInput(session, "disease", selected = "Chlamydia")
    updateSelectInput(session, "gender" , selected = "Female")
    updateSelectInput(session, "age"    , selected = "20-24 years")
    updateSliderInput(session, "year"   , value    = 2014)
  })

  observeEvent(input$preset2,
  {
    updateSelectInput(session, "disease", selected = "Gonorrhea")
    updateSelectInput(session, "gender" , selected = "All")
    updateSelectInput(session, "age"    , selected = "All")
    updateSliderInput(session, "year"   , value    = 2009)
  })

  observeEvent(input$preset3,
  {
    updateSelectInput(session, "disease", selected = "Primary and Secondary Syphilis")
    updateSelectInput(session, "gender" , selected = "All")
    updateSelectInput(session, "age"    , selected = "All")
    updateSliderInput(session, "year"   , value    = 2001)
  })
}
