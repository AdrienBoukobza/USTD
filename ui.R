source("data_n_deps.R")

Home <- tabPanel("Home", includeMarkdown("markdown/Home.md"))

STDMap <- tabPanel("Interactive Map",
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
                                 selectInput("disease", "Disease", selectVars$Disease %>% andAll, selected = "All"),
                                 selectInput("gender", "Gender", selectVars$Gender %>% andAll, selected = "All"),
                                 sliderInput("year", "Year:",
                                             min = 1996,
                                             max = 2014,
                                             value = 2014,
                                             animate = animationOptions(interval = 1500, loop = FALSE)),
                                 selectInput("age", "Age class", selectVars$Age %>% andAll, selected = "All"),
                                 actionButton("preset1", "Chlamydia in young adult female"),
                                 actionButton("preset2", "Minimum of Gonorrhea"),
                                 actionButton("preset3", "Minimum of Syphilis")),
                   fluidRow(column(12, plotOutput("curvetotal"))),
                   fluidRow(column(6, plotOutput("curveincidence")),
                            column(6, plotOutput("curveincidence2"))))

CurveExplorer <- tabPanel("Curve Explorer",
                          selectInput("statecurve", "State", selectVars$State %>% andAll),
                          selectInput("diseasecurve", "Disease", selectVars$Disease %>% andAll),
                          withLoader(dygraphOutput("curve", width = "70%", height = "600px"), type = 'html', loader = "dnaspin"))

RiskCalculator <- tabPanel("Risk calculator",
                           fluidRow(h2("Comparative beetween the different States", align = "center"),
                                    column(4,
                                           offset = 1,
                                           h3("Reference population :"),
                                           selectInput('OddsDisease', 'Disease', selectVars$Disease),
                                           selectInput('OddsState', 'State', selectVars$State),
                                           selectInput('OddsGender', 'Gender', selectVars$Gender),
                                           selectInput('OddsEthnicity', 'Ethnicity', selectVars$Ethnicity),
                                           selectInput('OddsAge', 'Age', selectVars$Age),
                                           selectInput('OddsYear', "Year", c(1996:2014))),
                                    column(4,
                                           h3("Compare by"),
                                           selectInput("Factor", "Factor", c("Disease", "Gender", "Ethnicity", "Age", "Year")))),
                           DTOutput("contingence"),
                           fluidRow(column(1,
                                           offset = 5,
                                           actionButton("MapRRbutton", "Rendering map with RR"))),
                           leafletOutput("map2", width = "100%", height = "700"))

DataExplorer <- tabPanel("Data Explorer", DTOutput("mapTable"))

About <- tabPanel("About", includeMarkdown("markdown/About.md"))

License <- tabPanel("License", includeMarkdown("markdown/License.md"))

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("USTD", id = "navbar",
                           Home,
                           STDMap,
                           CurveExplorer,
                           RiskCalculator,
                           DataExplorer,
                           About,
                           License))
