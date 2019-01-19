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
