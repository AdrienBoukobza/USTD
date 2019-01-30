library(tidyr)

library(dplyr, warn.conflicts = FALSE)
df <- tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
df
df %>% complete(group, nesting(item_name))

# You can also choose to fill in missing values



df = df %>% mutate(Rate_Calc='')


difference = tibble( Disease = "",
                     setdiff(states$name,df$State),
                     Year = "0",
                     "Race/Ethnicity" ="",
                     Age = "",
                     Age_Code = "",
                     STD_Cases = 0,
                     Population = 0,
                     Gender = "",
                     Gender_Code = "",
                     Rate_Calc = 0)
                     
  
  setdiff(states$name,df$State)

colnames(difference) = c("Disease", "State", "Year", "Race/Ethnicity", "Age", "Age_Code", "STD_Cases", "Population", "Gender",
                         "Gender_Code", "Rate_Calc")

test = rbind(df,difference)
test = test[order(match(test$State, states$name)),]

test$state = factor(test$State, levels = c(states$name))
test = test[order(test$state),]

############
STD1 = STD
STD1 = STD1 %>% filter(Disease == "Primary and Secondary Syphilis") %>% filter(Year == "1996") %>% 
  filter(Gender == "Male") %>% filter(Age_Code == "0-14")


# Creating two new DT : one to summarise the cases and one to summarise the population
STD1Cases = STD1 %>% group_by(State) %>% summarise(STD_Cases = sum(STD_Cases))
STD1Pop = STD1 %>% group_by(State) %>% summarise (Population = sum(Population))

#Setting the old work as the new base for the cases
STD1 = STD1Cases
rm(STD1Cases)

#Creating the rates in the working table
STD1 = STD1 %>% mutate (RateCalc = STD1$STD_Cases * 1000 / STD1Pop$Population)
STD1 = STD1 %>% mutate(Population = STD1Pop$Population)
