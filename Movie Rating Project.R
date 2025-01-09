library(dplyr)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)

# Load dataset
movie_dataset = read.csv("movies.csv")

# Take out irrelevant variables
movie_dataset = movie_dataset %>% 
  select(-c(index,MovieID,Release.Date,Rating.Count,Gross,Title))

# Change variable names
names(movie_dataset)[names(movie_dataset) == "MPAA.Rating"] <- "certificate"
names(movie_dataset)[names(movie_dataset) == "Genre"] <- "genre"
names(movie_dataset)[names(movie_dataset) == "Rating"] <- "rating"
names(movie_dataset)[names(movie_dataset) == "Budget"] <- "budget"
names(movie_dataset)[names(movie_dataset) == "Runtime"] <- "run_time"

# Filter unwanted data
movie_dataset = movie_dataset %>%
  filter(genre != "Mystery")

movie_dataset = movie_dataset %>%
  filter(genre != "Western")

movie_dataset = movie_dataset %>%
  filter(genre != "War")

movie_dataset = movie_dataset %>%
  filter(genre != "History")

# Set a color palette & background color
my_colors <- c("#5271ff", "#78c2ad", "#f0a986", "#a2a6d3", "#ed6a5a", "#f4f1de", "#6d6875", "#f19066", "#81b29a", "#d0b7d6", "#d7acac", "#bbded6")# Create the box plot with customizations
par(bg = "#fdfcf9")

# Boxplots for categorical variables (certificate and genre)
boxplot(rating ~ certificate, data = movie_dataset,
        main="Rating Distribution by Certificate",
        xlab="Certificate", ylab="Rating",
        col=my_colors,           # Set custom colors
        border="black",         # Set border color for boxes
        las = 1
)

boxplot(rating ~ genre, data = movie_dataset,
        main="Rating Distribution by Genre",
        xlab="Genre", ylab="Rating",
        col=my_colors,           # Set custom colors
        border="black",         # Set border color for boxes
        las = 1
)

# Scatterplot for budget
plot(movie_dataset$budget, movie_dataset$rating,
        main="Rating Distribution by Budget",
        xlab="Budget ($)", ylab="Rating",
        las = 1
)

# Boxplot for budget (w/ breaks)
breaks <- c(0, 40000000, 80000000, 120000000, 160000000, 400000000)

movie_dataset$budget <- cut(movie_dataset$budget, breaks = breaks, labels = c("0-40M", "40M-80M", "80M-120M", "120M-160M", "160M+"), include.lowest = TRUE)

boxplot(rating ~ budget, data = movie_dataset,
        main="Rating Distribution by Budget",
        xlab="Budget ($)", ylab="Rating",
        col=my_colors,           # Set custom colors
        border="black",         # Set border color for boxes
        las = 1
)

# Scatterplot for run_time
plot(movie_dataset$run_time, movie_dataset$rating,
     main="Rating Distribution by Run Time",
     xlab="Run Time (min)", ylab="Rating",
     las = 1
)

# Boxplot for run_time (w/ breaks)
breaks_2 <- c(75, 100, 125, 150, 175, 202)

movie_dataset$run_time <- cut(movie_dataset$run_time, breaks = breaks_2, labels = c("75-100", "100-125", "125-150", "150-175", "175+"), include.lowest = TRUE)

boxplot(rating ~ run_time, data = movie_dataset,
        main="Rating Distribution by Run Time",
        xlab="Run Time (min)", ylab="Rating",
        col=my_colors,           # Set custom colors
        border="black",         # Set border color for boxes
        las = 1
)

# Linear Regression
regression = lm(rating ~ run_time + genre + budget + certificate, movie_dataset)
summary(regression)

# Decision Tree
movie_train = movie_dataset[1:360, ]
movie_test = movie_dataset[361:483, ]

m.rpart = rpart(rating ~ budget + run_time + genre + certificate, data = movie_train)
summary(m.rpart)

rpart.plot(m.rpart, digits = 2, fallen.leaves = TRUE, type = 3, extra = 101)
