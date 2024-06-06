#Ahmet Burak Yuksel 21803003 - Lab Project

#1. Use the Data that you collect from internet or R already define datasets.
data(iris)
iris

#2. Data Preparation
#Setting some NA values
set.seed(123)
iris_with_na <- iris
iris_with_na[sample(1:150, 20), "Sepal.Length"] <- NA
iris_with_na[sample(1:150, 15), "Petal.Length"] <- NA
head(iris_with_na)
iris_with_na
#Removing NA datas.
data <- iris_with_na
if (sum(is.na(data)) == 0) {
  print("No missing values in the dataset.")
} else {
  print("There are missing values in the dataset.")
}
data$Sepal.Length[is.na(data$Sepal.Length)] <- mean(data$Sepal.Length, na.rm = TRUE)
data <- na.omit(data)
data

#3. Sketch 2 different graph that shows a relation between the columns of the dataset by using the ggplot2
library(ggplot2)
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Scatter Plot", x = "Sepal Length", y = "Sepal Width")

ggplot(data, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_line() +
  labs(title = "Line Plot", x = "Sepal Length", y = "Petal Length")

ggplot(data, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  labs(title = "Histogram of Sepal Length", x = "Sepal Length", y = "Frequency")

#4. Use dplyr module functionality in your homework. Pipe, summarise(), mutate() and groupby()…
library(dplyr)
result <- data %>%
  group_by(Species) %>%
  summarise(
    avg_sepal_length = mean(Sepal.Length, na.rm = TRUE),
    avg_sepal_width = mean(Sepal.Width, na.rm = TRUE),
    avg_petal_length = mean(Petal.Length, na.rm = TRUE),
    avg_petal_width = mean(Petal.Width, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    sepal_area = avg_sepal_length * avg_sepal_width,
    petal_area = avg_petal_length * avg_petal_width
  )
print(result)