library(tidyverse)

#Read in data 
gapminder_data <- read_csv("data/gapminder_data.csv")

#What is the mean life expectancy? 
#summarize() gets rid of the extra data 

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

#exercise: What is the mean population in the gapminder dataset? 
gapminder_data %>% 
  summarize(averagePop = mean(pop))

gapminder_data_pop_summarized<- gapminder_data %>% summarize(averagePop = mean(pop))

#what is the mean population AND the mean lifeExp? 
gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp), 
            averagePop = mean(pop))

#what is the mean life expectancy for the most recent year? 
#filter filters dataset only focusing on data that meets the condition statement

#max() to find most recent year
gapminder_data %>% 
  summarize(maxYear = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(averageLifeExp = mean(lifeExp))
# having these two pipes helps stack, so that way you don't need to recall the previous stacked dataset.
# It just assumes you are continuing based on what was on the left side

# exercise, using the filter function, we want to filter for the most recent year using the max function.
# Basically it helps so that you don't need to do an extra step.
gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize(averageLifeExp = mean(lifeExp))


#exercise, What is the mean GDP per capita for the first or earliest year? 
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(average_GDP = mean(gdpPercap))

# other helpful functions: > (greater than), < (less than), != (does not equal to)

#What is the mean life expectancy for EACH year? 
#group_by() takes dataframe and uses a column that you assign, to group data into logical groups
#this is a super helpful function!!! 
gapminder_data %>% 
  group_by(year) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#What is mean life expectancy for each continent? 
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp))


#what is the mean life expectancy and mean GDP per capita for each continent 
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanlifeExp = mean(lifeExp), meanGDP = mean(gdpPercap))


#What is the GDP (not per capita)? 
#mutate() adds a new column to our dataset rather than creating a separate smaller dataframe like in summarize()) 

gapminder_data %>% 
  mutate(GDP = gdpPercap * pop)

#make a new column for population in millions
gapminder_data %>% 
  mutate(pop_in_millions = pop / 1000000)

gapminder_data_popmil <- gapminder_data %>% 
  mutate(GDP = gdpPercap*pop, pop_in_millions = pop / 1000000)

#select() choses a subset of columns from a dataset. sometimes you don't want to look at unnecessary columns
gapminder_data %>% 
  select(year,pop)

gapminder_data %>% 
  select(-continent)
#select(-x,...) selects everything EXCEPT the one that you inputed in the negative select function

#create a tibble with only country, continent, year, and lifeExp
gapminder_data %>% 
  select(-pop, -gdpPercap)

#select helper function: starts_with(), contains(), ...
gapminder_data %>% 
  select(year, starts_with("c"))

#Vectors is a 'list' of values of the SAME type (characters, numbers,etc). Each column of tibble is a vector. 
#to specify vector: c()

my_vec <- c()
#value does not have rows and columns
#to assign multiple things into a vector 
my_vec <-c("dog", "cat", "horse")
# in environment, you have a vector my_vec, and it is a character and you have 3 characters
num_vec <-c(1,2,3,4)
# in environmnet, you have a numeric vector listed there. 

proof <- gapminder_data %>% 
  pull(year)
#since each column in dataset is a vector, in the values, it is listed as a numeric vector because it is all years

#ex: your_data %>% filter(id %in% c("id1", "id2", "id3")) 
#basically allows you to filter IDs that are INside the vector id1, id2, id3


#pivot_longer and pivot_wider (shaping functions), they take datasets and make them wider or longer. in one dataset, they have 1 observation perdata. Wide data you don't have 1 observation per row (instead they're split up by year into different columns)
# this is helpful to move data into the different types is often in the wide format rather than longer
# another way to think about it, a more condensed version of data, and really highlights what you're more interested in. 
# pivot_wider transfers data from long format INTO wide format, we need to specify where we're getting the names from and the values from 
# pivot_longer gives wider data and helps you turn it into longer data. basically making more columnns for yourself and separate out the cases

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#exercise pivot_longer but populate values with gdpPercap
gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = gdpPercap)

#pivot_longer
gapminder_data %>% 
  pivot_longer(cols = c(pop, lifeExp, gdpPercap), 
               names_to = "measurement_type", 
               values_to = "measurement")
#if you think about pivot_longer like a tree, each BRANCH to a new level basically can be written with all of its labels/suffixes
#pivot_wider just removes those extraneous suffixes and condenses that information into its most important values. 

#Is there a relationship between GDP and CO2 emissions

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) 

#read in CO2 data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))
#skip tells you to skip the first line
#col_names allows you to rename columns

#recode helps you modify the name within your column. Super helpful. 
co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, 
              values_from = value) %>% 
  #if you want to combine tables, you want the width/length to match 
  filter(year == 2005) %>% 
  select(-year)

#now we have data for CO2 that is similar to gapminder dataset, 1 country per row for each table, so if we wanted to join the datasets together, we can.

#inner_join takes all of the observations that are in BOTH datasets, and joins the other columns together, but removes the columns that are NOT shared
#outer_join includes all countries for both datasets even if they are not shared. the countries not in a dataset would have N/A

inner_join(gapminder_data_2007, co2_emissions, by = "country")


  
  
