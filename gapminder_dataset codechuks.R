library(tidyverse)
library(patchwork)

gapminder <- read.csv("gapminder_data.csv")

View(gapminder)

dim(gapminder) # To identify the number of observations and variables in the dataset


glimpse(gapminder) # glimpse shows the variables name, types and the first few instansis of the dataset

head(gapminder) # head will show the first six variables of the dataset

tail(gapminder) # tail will shw the last six variables of the dataset


gapminder$gdp # if you want to look at a pparticular variable on the gapminder dataset

attach(gapminder) # But if we assign attach to the gapminder dataset we don't need to add  
                  # the dataset with the variable anymore to look at a particular variable

names(gapminder)
summary(hdi_index)
unique(gdp)
mean(hdi_index, na.rm = TRUE)
class(life_exp)
length(gapminder)
table(continent)
View(sort(table(gdp), decreasing = TRUE))

# Variable with NA's

View(gapminder[is.na(gdp),])
View(gapminder[is.na(hdi_index),])
View(gapminder[is.na(co2_consump),])

# identifying hdi_index and gdp per capital variables with NA's and replacing the NA"s with zero (0)

gapminder %>% 
  select(country,continent,year,life_exp,hdi_index,gdp) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hdi_index = replace_na(hdi_index, 0)) %>% 
  mutate(gdp = replace_na(gdp, 0)) %>% 
  arrange(continent) %>% 
View()


gapminder %>% 
  select(country,continent,year,life_exp,hdi_index,gdp) %>% 
  mutate(life_exp = replace_na(life_exp, 0)) %>% 
  mutate(hdi_index = replace_na(hdi_index, 0)) %>% 
  mutate(gdp = replace_na(gdp, 0)) %>% 
  arrange(continent) %>% 
  View()


# Creating a new dataframe from my existinfg dataset

gapminder_dataset <- gapminder %>% 
  select(country,continent,year,life_exp,hdi_index,gdp) %>% 
  mutate(life_exp = replace_na(life_exp, 0)) %>% 
  mutate(hdi_index = replace_na(hdi_index, 0)) %>% 
  mutate(gdp = replace_na(gdp, 0)) %>% 
  arrange(continent)
  

  

# Checking fr duplicate dtate 

gapminder_dataset[duplicated(gapminder_dataset),] # apperantly there are no duplicated data

duplicated(gapminder_dataset)

gapminder_dataset %>% 
  distinct() %>% 
  View()


View(gapminder_dataset)

gapminder_dataset %>% 
  select(country,continent,year,life_exp,hdi_index,gdp) %>% 
  group_by(continent) %>% 
  summarise(Average = mean(hdi_index)) %>% 
  mutate(Average = round(Average, digits = 3)) %>% 
  arrange(continent) %>% 
  View()

# Average Life Expectancy For North,East,South And West Africa
gapminder_dataset %>% 
  select(country,continent,year,life_exp,hdi_index,gdp) %>% 
  filter(country %in% c("Nigeria", "South Africa",
                        "Egypt","Kenya")) %>% 
  group_by(country) %>% 
  summarise(Average_Life_Expectancy = mean(life_exp)) %>% 
  ggplot(aes(country, Average_Life_Expectancy, col = country))+
  geom_point(size = 3)+
  theme_linedraw()+
  theme(panel.grid.minor = element_blank())+
  labs(title = "Average Life Expectancy For North,East,South And West Africa")

# Human Development Index (HDI) by Continent
gapminder_dataset %>% 
  select(country,continent,year,life_exp,hdi_index,gdp) %>% 
  group_by(continent) %>% 
  summarise(Average = mean(hdi_index)) %>% 
  mutate(Average = round(Average, digits = 3)) %>% 
  arrange(continent) %>% 
  ggplot(aes(continent, Average, col = continent))+
  geom_point(size = 3, alpha = 5)+
  geom_hline(yintercept = mean(gapminder_dataset$hdi_index),
             colour = "grey",
             size = 1)+
  theme_linedraw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Human Development Index (HDI) by Continent")

View(gapminder_dataset) 


  # Reshaping the dataset from long to wild 

gapminder_df <- gapminder_dataset %>% 
  pivot_wider(names_from = year, values_from = c("life_exp","hdi_index","gdp"))

View(gapminder_df)
