# USTD

The recent surge of STDs in the USA is a very hot topic and could represent a major public health issue within a short timespan. It prompted us to provide means to explore the data about this subject.

### Interactive map
 
The first tool we provide is an **explorable map** of the diseases burden over the USA.  
Each state is shaded according to the density of the disease of interest in the selected population.

### Curve explorer

The second tool is a **simple forecast** of the number of cases for any combination of disease and state.  
The forecast is provided using the `prophet` package from facebook research (https://facebook.github.io/prophet/)

### Risk calculator

The third tool is a **risk comparator** to calculate the risk ratios between a reference population and ones differing on one parameter.  
The risks are also displayed on a map, between the reference population and the same population in all other states.

### Data explorer

The **raw data** used in this application is also provided.
