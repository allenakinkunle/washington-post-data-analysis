# The Washington Post is compiling a database of every
# fatal shooting in the United States by a police officer
# in the line of duty in 2015 and 2016. The data is published
# on Github https://github.com/washingtonpost/data-police-shootings

# Author: Allen Akinkunle
# Email: hello@allenkunle.me
# Twitter: @allenakinkunle
# Date: 8th July, 2016

# Import libraries
library(dplyr)
library(magrittr)
library(plotly)

# Import the dataset, remove unnecessary columns
shooting_data <- 
  read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv") %>%
  select(age, state)

# Print the number of NAs in each column of the dataset
sapply(shooting_data, function(x) sum(is.na(x)))

# Replace missing age values with 0
shooting_data$age[is.na(shooting_data$age)] <- 0


#############################################################
# 1. Plot a distribution of killings by age
#############################################################
# There are 37 missing values in the Age column. Recode as 0
shooting_data$cat_age <- 
  cut(shooting_data$age, 
      breaks = c(-Inf, 1, 18, 30, 45, 60, Inf),  
      labels = c("Unknown", "Under 18", "18-29", "30-44", "45-59", "60 above"), 
      right = FALSE)

killings_by_age_group_plot <- shooting_data %>%
  group_by(cat_age) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = cat_age, y = count)) +
  geom_bar(stat = "identity", fill = "#f29999") +
  labs(x = "Age Group", y = "Number of Police Killings", title = "Distribution of Police Killings by age group in the United States (Jan 2015 - July 2016)") +
  theme(
    plot.title = element_text(color = "#008A3C", size = 13, face = "bold", family = "Helvetica"),
    axis.title.x = element_text(color = "#008A3C", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#008A3C", size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)
  )

ggplotly(killings_by_age_group_plot)
plotly_POST(killings_by_age_group_plot, "Number of Police Killings by Age Group in the United States (Jan 2015 - July 2016)")

#############################################################
# 2. Plot a choropleth map for killings by state
#############################################################

shooting_data$state_name <- ifelse(is.na(match(shooting_data$state, state.abb)), 
                                   "District of Columbia", state.name[index])

state_population_data <- read.csv("state_population_data.csv")

killings_by_state_plot <- shooting_data %>%
  group_by(state_name, state) %>%
  summarise(count = n()) %>%
  merge(statedata, by.x = "state_name", by.y = "name") %>%
  mutate(
    shooting_per_million = round(count / ((population / 1000000)), digits = 2),
    hover = paste(state_name, '<br>', shooting_per_million, 'per million people', 
                  '<br>', count, 'shootings since January 2015')
  )

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 1)

# specify some map projection/options
g <- list(scope = 'usa')

plot_ly(killings_by_state_plot, z = shooting_per_million, text = hover, locations = state, type = 'choropleth',
        locationmode = 'USA-states', color = shooting_per_million, colors = 'Reds',
        marker = list(line = l), colorbar = list(title = "Police killings per million people", 
                                                 lenmode="pixels", titleside="right", xpad=0, ypad=0)) %>%
  layout(title = 'Police killings per million people in United States (Jan 2015 - July 2016))', geo = g)
