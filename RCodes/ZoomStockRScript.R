# Set the working directory
setwd("/Users/abyrax/Desktop")

# Read the CSV data with correct format
data <- read.csv("ZoomStocks.csv", sep=';', header=FALSE, skip=2)
df <- data.frame(Date = as.Date(data$V1, format = "%Y-%m-%d"), Avg = data$V2)

# Display the data
print(df)

# Summary statistics
summary(df)

# Descriptive statistics using psych package
library(psych)
desc_stats <- describe(df$Avg)
print(desc_stats)

# Filter data for the years
data_2019 <- df[df$Date >= as.Date("2019-01-01") & df$Date <= as.Date("2019-12-31"), ]
data_2020 <- df[df$Date >= as.Date("2020-01-01") & df$Date <= as.Date("2020-12-31"), ]
data_2021 <- df[df$Date >= as.Date("2021-01-01") & df$Date <= as.Date("2021-12-31"), ]
data_2022 <- df[df$Date >= as.Date("2022-01-01") & df$Date <= as.Date("2022-12-31"), ]
data_2023 <- df[df$Date >= as.Date("2023-01-01") & df$Date <= as.Date("2023-12-31"), ]

# Display the filtered data for years
print(data_2019)
summary(data_2019)
desc_2019 <- describe(data_2019$Avg)
print(desc_2019)
print(data_2020)
summary(data_2020)
desc_2020 <- describe(data_2020$Avg)
print(desc_2020)
print(data_2021)
summary(data_2021)
desc_2021 <- describe(data_2021$Avg)
print(desc_2021)
print(data_2022)
summary(data_2022)
desc_2022 <- describe(data_2022$Avg)
print(desc_2022)
print(data_2023)
summary(data_2023)
desc_2023 <- describe(data_2023$Avg)
print(desc_2023)

# Convert Date column to Date type
df$Date <- as.Date(df$Date)

# Create a scatter plot with color based on a categorical column (e.g., 'category')
ggplot(df, aes(x = Date, y = Avg, color = factor(Avg))) +
  geom_point() +
  labs(x = "",
       y = "Average Value") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # Display month names as 3-month periods
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),  # Rotate text for better visibility
        legend.position = "none")  # Remove the legend
# Create a line plot
ggplot(df, aes(x = Date, y = Avg)) +
  geom_line(color = "blue") +
  labs(x = "Date",
       y = "Open Price") +
  theme_minimal()

# Scatter Plot
ggplot(df, aes(x = Date, y = Avg)) +
  geom_point() +
  labs(x = "Date",
       y = "Average Stock Price") +
  theme_minimal()

##########

# Create a data frame with the provided data
faculty_data <- data.frame(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
  graduate_numbers = c(3230876, 3298665, 3426866, 3522998, 3777545, 4035843, 4130514)
)

# Display the data
cat("Faculty Data:\n")
print(faculty_data)

# Descriptive statistics
summary_stats <- summary(faculty_data$graduate_numbers)
cat("\nSummary Statistics for Graduate Numbers:\n", summary_stats, "\n")

# Alternatively, you can use individual functions for specific statistics
mean_value <- mean(faculty_data$graduate_numbers)
sd_value <- sd(faculty_data$graduate_numbers)
median_value <- median(faculty_data$graduate_numbers)
min_value <- min(faculty_data$graduate_numbers)
max_value <- max(faculty_data$graduate_numbers)
q1 <- quantile(faculty_data$graduate_numbers, 0.25)
q3 <- quantile(faculty_data$graduate_numbers, 0.75)
skewness_value <- e1071::skewness(faculty_data$graduate_numbers)
kurtosis_value <- e1071::kurtosis(faculty_data$graduate_numbers)
n_length <- length(faculty_data$graduate_numbers)

# Print individual statistics
cat("Sample Size (using length):", n_length, "\n")
cat("\nMean Graduate Numbers:", mean_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Median Graduate Numbers:", median_value, "\n")
cat("Minimum Graduate Numbers:", min_value, "\n")
cat("Maximum Graduate Numbers:", max_value, "\n")
cat("1st Quartile (Q1):", q1, "\n")
cat("3rd Quartile (Q3):", q3, "\n")
cat("Skewness:", skewness_value, "\n")
cat("Kurtosis:", kurtosis_value, "\n")

master_data <- data.frame(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
  master_graduates = c(77153, 149656, 166713, 179600, 173715, 256496, 252416)
)

# Display the data
cat("Master Graduates Data:\n")
print(master_data)

# Descriptive statistics
summary_stats <- summary(master_data$master_graduates)
cat("\nSummary Statistics for Master Graduates:\n", summary_stats, "\n")

# Alternatively, you can use individual functions for specific statistics
mean_value <- mean(master_data$master_graduates)
sd_value <- sd(master_data$master_graduates)
median_value <- median(master_data$master_graduates)
min_value <- min(master_data$master_graduates)
max_value <- max(master_data$master_graduates)
q1 <- quantile(master_data$master_graduates, 0.25)
q3 <- quantile(master_data$master_graduates, 0.75)
skewness_value <- e1071::skewness(master_data$master_graduates)
kurtosis_value <- e1071::kurtosis(master_data$master_graduates)
n_length <- length(master_data$master_graduates)

# Print individual statistics
cat("Sample Size (using length):", n_length, "\n")
cat("\nMean Master Graduates:", mean_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Median Master Graduates:", median_value, "\n")
cat("Minimum Master Graduates:", min_value, "\n")
cat("Maximum Master Graduates:", max_value, "\n")
cat("1st Quartile (Q1):", q1, "\n")
cat("3rd Quartile (Q3):", q3, "\n")
cat("Skewness:", skewness_value, "\n")
cat("Kurtosis:", kurtosis_value, "\n")