#Install and load the dplyr package
#install.packages("dplyr")
library(dplyr)
library(readxl)
library(ggplot2)

# Create data frame for higher education and faculty
data <- read.csv("faculty.csv",sep = ";",header = FALSE)
data

dfFaculty <- data.frame(Year = data$V1, Grad = data$V2)

summary(dfFaculty)

library(psych)

desc_stats_fac <- describe(dfFaculty$Grad)
desc_stats_fac

# Create data frame for master
data <- read.csv("master.csv",sep = ";",header = FALSE)
data

dfMaster <- data.frame(Year = data$V1, Grad = data$V2)

summary(dfMaster)

library(psych)

desc_stats <- describe(dfMaster$Grad)
desc_stats

library(ggplot2)



# Create a line plot
ggplot(dfMaster, aes(x = Year, y = Grad)) +
  geom_line(color = "blue") +
  labs(
       x = "Year",
       y = "Graduates") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))

# Convert Grad column to numeric
dfFaculty$Grad <- as.numeric(dfFaculty$Grad)

# Create a bar plot
library(ggplot2)
library(scales)  # for number_format

ggplot(dfFaculty, aes(x = Year, y = Grad)) +
  geom_bar(stat = "identity", fill = "red", color = "black", size = 0.5) +
  labs(
    x = "Year",
    y = "Graduates"
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, max(dfFaculty$Grad), by = 0.5e6)  # Adjust breaks for 0.5 million increments
  )


#------------------------------------------------------
econ103 <- read_xlsx("econ_103.xlsx")
econ103

econ103$GPA <- as.numeric(econ103$GPA)
econ103

dfecon103 <- data.frame(Semester = econ103$Semester, Gpa = econ103$GPA)

summary(dfecon103)

library(psych)

desc_stats_econ103 <- describe(dfecon103$Gpa)
desc_stats_econ103
#------------------------------------------------------
eng101 <- read_xlsx("eng_101.xlsx")
eng101

eng101$GPA <- as.numeric(eng101$GPA)
eng101

dfeng101 <- data.frame(Semester = eng101$Semester, Gpa = eng101$GPA)

summary(dfeng101)

library(psych)

desc_stats_eng101 <- describe(dfeng101$Gpa)
desc_stats_eng101
#------------------------------------------------------
math101 <- read_xlsx("math_101.xlsx")
math101

math101$GPA <- as.numeric(math101$GPA)
math101

dfmath101 <- data.frame(Semester = math101$Semester, Gpa = math101$GPA)

summary(dfmath101)

library(psych)

desc_stats_math101 <- describe(dfmath101$Gpa)
desc_stats_math101

#-----------------------------------------------------------
# Convert Gpa columns to numeric
dfmath101$Gpa <- as.numeric(dfmath101$Gpa)
dfeng101$Gpa <- as.numeric(dfeng101$Gpa)
dfecon103$Gpa <- as.numeric(dfecon103$Gpa)

# Combine data frames into one
combined_df <- rbind(
  data.frame(Data = "Math101", dfmath101),
  data.frame(Data = "Eng101", dfeng101),
  data.frame(Data = "Econ103", dfecon103)
)

# Create a line chart with dots for three different data frames
library(ggplot2)

library(ggplot2)

ggplot(combined_df, aes(x = Semester, y = Gpa, color = Data, shape = Data, group = Data)) +
  geom_line() +
  geom_point(size = 3) +  # Adjust size of the dots
  labs(
    x = "",
    y = "GPA"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "")) +
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize line colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels



