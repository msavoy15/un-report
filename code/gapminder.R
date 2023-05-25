library(tidyverse)

gapminder_1997 <- read_csv("data/gapminder_1997.csv")
name <- "Megan"
age <- 26
name <- "Megan Savoy"
sentence <- "Megan is a graduate student"
print(sentence)

read_csv()
Sys.Date()
getwd()
sum(10,4)
round(3.76)
?round()
round(3.751, digits=2)

# aes makes the aesthetics appropriate for the type of data. For example, x axis will be based on the object name I've assigned it to

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) + 
  labs (x = "GDP Per Capita") +
  aes (y = lifeExp) + 
  labs (y = "Life Expectancy") +
  geom_point() +
  #geom_text(aes(label = country)) +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + 
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) + 
  labs(size = "population (in millions)") 
  #aes(shape = continent) 
  #aes(shape = name) might be helpful to visualize the different populations for isotopes or animals
  #geom_text(aes(label = country), size = 4, hjust = 1.1) this way makes it so the name is not written over the point

gapminder_data <- read_csv("data/gapminder_data.csv")
view(gapminder_data)
head(gapminder_data)
dim(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent, group = country) +
  geom_line() +
  scale_color_brewer(palette = "Set1")

ggplot(data = gapminder_1997, mapping = aes(x = continent, y = lifeExp) + 
         labs(x = "Continent", y = "Life expectancy", 
              title = "Life expectancy by Continent")) +
  geom_violin(aes(fill = continent, color = continent)) +
  theme_bw()
  #geom_jitter(aes(size = pop/1000000)) + 
  #labs(size = "population (in millions)")

  
