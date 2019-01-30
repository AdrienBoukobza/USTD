summary (allDisease)
str (allDisease)
table (allDisease$`Gender Code`)
table (allDisease$Disease)
table(allDisease$State)
table (allDisease$Age)
table (allDisease$`Age Code`)
table (allDisease$`STD Cases`)
table (allDisease$Population)

temp = c(which(allDisease$"Gender Code" == "U"),
         which(allDisease$"Age" == "Unknown"),
         which (allDisease$"Population" == NA))
if (length(temp) != 0) #Vérifier que le vecteur n'est pas vide
{allDisease = allDisease[-c(temp),]}


########
library (geojsonio)
library(geojson)

states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))

#######

allDiseasesave = allDisease


allDisease = allDiseasesave

allDisease = allDisease %>% filter(Disease == "Primary and Secondary Syphilis") %>%
             # filter (`Gender Code`== "F") %>%
             filter (Year == "1996") %>% 
             filter(`Age Code` == "0-14")

allDisease %>% mutate (RateCalc = `STD Cases` * 100 / Population)

####

temp = states$name
temp2 = allDisease$State

i=1
j=1

temp3 = c()

while (i < 53)
{
  if (temp [i] == temp2[j]|!is.na(temp [i] == temp2[j]))
  {
    temp3 = c(temp3,allDisease[j,"STD_Cases"])
    i = i+1
    j = j+1
  }
  if(temp [i] != temp2[j] |is.na(temp [i] != temp2[j]))
  {
    temp3 = c(temp3, 0)
    i =i+1
  }
  
}

temp3 =unlist(temp3, use.names=FALSE)

states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")
m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = temp3, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g cases in the state",
  states$name, temp3
) %>% lapply(htmltools::HTML)

m %>% addPolygons(
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
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>% 
  addLegend(pal = pal, values = ~temp3, opacity = 0.7, title = NULL,
  position = "bottomright")

########
allDisease= allDisease %>%  group_by(State) %>%  summarise(sum(`Population`))

allDisease = allDisease %>% mutate (Rateonpop = `STD Cases`*100/Population)

##

summary (AllDiseaseEthnia)
summary (AllDiseaseEthnia$Gender)
table (AllDiseaseEthnia$Gender)


#####

# Creating two new DT : one to summarise the cases and one to summarise the population

STD1 = STD1 %>% filter (Gender == "Male") %>%  filter (Disease == "Primary and Secondary Syphilis") %>% 
  filter (Year == 2000) %>% 
  filter (Age_Code == "0-14")

STD1Cases = STD1 %>% group_by(State) %>% summarise(STD_Cases = sum(STD_Cases))
STD1Pop = STD1 %>% group_by(State) %>% summarise (Population = sum(Population))

#Setting the old work as the new base for the cases
STD1 = STD1Cases
rm(STD1Cases)

#Creating the rates in the working table
STD1 = STD1 %>% mutate (RateCalc = STD1$STD_Cases * 1000 / STD1Pop$Population)
rm(STD1Pop)


#Creating two lists : one with all the names of the states and one with our selection's

states <- geojsonio::geojson_read("geojson/us-states.json", what = "sp")

temp = states$name
temp2 = STD1$State

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
      temp3 = c(temp3,STD1[j,"RateCalc"]) #Saving the value in a vector
      temp4 = c(temp4, STD1[j, "STD_Cases"])
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
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- ggplot(test, aes(Year, "sum(STD_Cases)")) +
    geom_point(aes(colour = "sum(STD_Cases)")))
######


plot = STD %>% filter(State == "Alabama") %>% filter(Disease == "Chlamydia") %>%
  group_by(Year) %>% summarise(sum(STD_Cases))
colnames(plot) = c("Year", "Cases")

a = (plot[19,2]- plot[1,2])/18
b = plot[19,2] - 2014*a
x = input$yearcurve

temp = c()
temp2 = c()
for (i in 2015:2019){
  temp = c(temp,i)
  temp2 = c(temp2, affinefunction(a,b,i))
}
temp2 = unlist(temp2)
temp = unlist(temp)
newRows = data.frame(Year = temp, Cases = temp2)
plot = rbind (plot, newRows)


#######


meancountrypop = STD %>% filter(Disease == "Chlamydia") %>% filter (Year == "1996") %>% 
  group_by(Disease) %>% summarise (sum(Population))
meancountrypop = as.numeric(meancountrypop[1,2])

meancountrycases = STD %>% filter(Disease == "Chlamydia") %>% filter (Year == "1996") %>% 
  group_by(Disease) %>% summarise (sum(STD_Cases))
meancountrycases = as.numeric(meancountrycases[1,2])

meancountry = (meancountrycases *1000 )/ meancountrypop

for (i in 1:51){
  meancountry = c(meancountry, meancountry[1])}

#####

plot = STD %>% filter(State == "Alabama") %>% filter(Disease == "Chlamydia") %>%
  group_by(Year) %>% summarise(sum(STD_Cases))
colnames(plot) = c("ds", "y")
plot$ds = paste (plot$ds, "-01-01", sep = "")
newplot = prophet(plot)

future = make_future_dataframe(newplot, periods= 3650)
forecast <- predict(newplot, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(newplot, forecast)
dyplot.prophet(newplot, forecast)

######
mermaid( 
"gantt
dateFormat  YYYY-MM-DD
title Adding GANTT diagram functionality to mermaid
section A section
Completed task            :done,    des1, 2014-01-06,2014-01-08
Active task               :active,  des2, 2014-01-09, 3d
Future task               :         des3, after des2, 5d
Future task2               :         des4, after des3, 5d
section Critical tasks
Completed task in the critical line :crit, done, 2014-01-06,24h
Implement parser and jison          :crit, done, after des1, 2d
Create tests for parser             :crit, active, 3d
Future task in critical line        :crit, 5d
Create tests for renderer           :2d
Add to mermaid                      :1d"

)
mermaid("theme": "default")



######
  
  
 mermaid( "gantt
dateFormat  YYYY-MM-DD
title Some informations

section Disease
Minimum of Syphilis           :done,        Disease_1,    2000-01-01, 2001-12-31
Minimum of Gonorrhea          :done,        Disease_2,    2009-01-01, 2009-12-31

section President
Bill Clinton                  :done,    Clinton ,   1993-01-20, 2001-01-20
George W Bush                 :done,    Bush,       2001-01-20, 2009-01-20
Barack Obama                  :done,    Obama,      2009-01-20, 2017-01-20
", width = "800")

######################
library (DiagrammeR)
stringtimeline = "gantt
dateFormat  YYYY-MM-DD
title Some informations

section Disease
Minimum of Syphilis:"

randomnumber = 2010
if (randomnumber >= 2000 & randomnumber <=2001){
  stringtimeline = paste0(stringtimeline,"active,Disease_1,2000-01-01, 2001-12-31
Minimum of Gonorrhea:")
}else{
  stringtimeline = paste0(stringtimeline, "done,Disease_1,2000-01-01, 2001-12-31
Minimum of Gonorrhea:")
}

if (randomnumber == 2009){
  stringtimeline = paste0(stringtimeline,"active,Disease_2,2009-01-01, 2009-12-31
                          section President 
                          Bill Clinton:")
}else{
  stringtimeline = paste0(stringtimeline, "done,Disease_2,2009-01-01, 2009-12-31 
                          section President
                          Bill Clinton:")
}

if (randomnumber >= 1993 & randomnumber <=2001){
  stringtimeline = paste0(stringtimeline,"active,Clinton ,1993-01-20, 2001-01-20
                          George W Bush:")
}else{
  stringtimeline = paste0(stringtimeline, "done,Clinton , 1993-01-20, 2001-01-20
                          George W Bush:")
}

if (randomnumber >= 2001 & randomnumber <=2009){
  stringtimeline = paste0(stringtimeline,"active,Bush,2001-01-20, 2009-01-20
                          Barack Obama:")
}else{
  stringtimeline = paste0(stringtimeline, "done,Bush,2001-01-20, 2009-01-20
                          Barack Obama:")
}

if (randomnumber >= 2009 & randomnumber <=2017){
  stringtimeline = paste0(stringtimeline,"active,Obama,2009-01-20, 2017-01-20")
}else{
  stringtimeline = paste0(stringtimeline, "done,Obama,2009-01-20, 2017-01-20")
}

mermaid(stringtimeline)

################
timeline = mermaid(stringtimeline)

timeline$x$config = list(ganttConfig = list(
  axisFormatter = list(list(
    "%Y"
    ,htmlwidgets::JS(
      'function(d){ return d.getDay() == 1 }'
    )
  ))
))

###############
df <- tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
df %>% complete(group, nesting(item_id, item_name))

# You can also choose to fill in missing values
df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0))
# }

df = tibble (letters [1:10], c(1:10))
df[8,1] = NA
vecteur = letters[1:10]
complete (df, nesting(vecteur))
