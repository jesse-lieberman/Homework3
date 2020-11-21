#Jesse Lieberman
#Prof Xavi
#Business Analytics
#November 20,2020

rm(list = ls())
graphics.off()
gc()
#installing packages 
install.packages("coronavirus")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(coronavirus)
library(ggplot2)
library(dplyr)
library(tidyr)


#QUESTION 1

#part A

#this is the coronavirus dataframe. It contains 7 columns and 150720 rows
coronavirus
dim(coronavirus)

#converting the coronavirus$date column to dates using as.Date
coronavirus$date = as.Date(coronavirus$date)

#PART B
#using the head function to return the first 100 elements in the data frame
head(coronavirus, 100)


#PART C

#There are 7 columns in the coronavirus data frame. 
# The 1st column is the date
# The 2nd column is the province of a country, for example "Alberta is a 
# Canadian Province
# The 3rd column is the country/region. Some countries do not have provinces, such 
# as the US. These values are left blank
# the 4th column is the latitude of the country/province. If the country has no
# provinces, the latitude will be the same.
# The 5th column is the longitude of the country/province. If the country has no
# provinces, the longitude will be the same.
# The 6th column has 3 values: confirmed, death, and recovered. This represents
# the type of cases referred to in the last column.
# The 7th column stores the number of cases as specified from column 6


#QUESTION 2

#PART A

# summary_total_cases_confirmed stores each country in order by number of 
#confirmed cases it has
summary_total_cases_confirmed = coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases   = sum(cases)) %>%
  arrange(-total_cases)

#This variable stores the top 20 countries in terms of number of cases
top_20_countries_by_cases = head(summary_total_cases_confirmed, 20)

#creating a summary of top_20_countries_by_cases
summary(top_20_countries_by_cases)

# PART B

# Stores the first 5 countries and number of confirmed cases
top_5_country_data = head(summary_total_cases_confirmed, 5)

# Creating a vertical bar graph using ggplot. Each bar has a different color 
# for easier visualization. I used the fill aesthetic. 
#Countries are on the x axis and total confirmed cases are on the y-axis
top_5_vertical_bargraph   = ggplot(top_5_country_data, aes(x = country, 
                                y = total_cases, fill = country)) + 
  geom_bar(stat = "identity")
print(top_5_vertical_bargraph)


#PART C

# Maps the same data by horizontally.
top_5_horizontal_bargraph = ggplot(top_5_country_data, aes(x = total_cases,
                                           y = country, fill = country)) + 
  geom_bar(stat = "identity")
print(top_5_horizontal_bargraph)
# PART D

#prints the horizontal plot, but with the Title
print(top_5_horizontal_bargraph + ggtitle("Top 5 countries by total cases"))


#Question 3:

#Part A:

#creating the data frame recent_cases. This data frame has 4 columns. The 1st 
# column is the data. The 2nd-4th columns are the number of confirmed cases, 
# deaths, and recovered cases respectively. The data frame is arranged by date
# from the date with the most confirmed cases at the top of the data frame.
recent_cases = coronavirus %>%
                  select(date, type, cases) %>%
                  group_by(date, type) %>%
                  summarise(total_cases = sum(cases)) %>%
                  pivot_wider(names_from = type,
                              values_from = total_cases) %>%
                  arrange(-confirmed)
# PART B

# Creating a line graph, which plots the number of confirmed cases on y-axis
# by the date  on the x-axis
line_graph_recent_cases = ggplot(recent_cases, aes(x = date, y = confirmed, 
                                      color = "red")) + 
        geom_line(stat = "identity")
print(line_graph_recent_cases + ggtitle("Worldwide confirmed cases by date"))

