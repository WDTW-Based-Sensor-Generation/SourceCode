# Install and load necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library(readxl)
library(ggplot2)
library(dplyr)

# Read the Excel file
data <- read_excel("unique_ids1TestSet.xlsx")

# Filter data based on specified boundaries
filtered_data <- data %>%
  filter(Latitude >= 33.2 & Latitude < 33.420,
         Longitude > -88.800 & Longitude < -88.770,
         Altitude >= 100 & Altitude <= 1000)


# Set tick label size and bold font
par(cex.axis = 2, font.axis = 4)

# Create a scatterplot matrix with the filtered data
pairs(filtered_data[, c("Longitude", "Latitude", "Altitude")],
      main = "Scatterplot Matrix",
      labels = c("LONG", "LAT", "ALT"),
      cex.labels = 2,
      font.labels = 4)

# Calculate the correlation matrix
correlation_matrix <- cor(data[, c("Latitude", "Longitude", "Altitude")])

# Print the correlation matrix with descriptive row and column names
colnames(correlation_matrix) <- c("Latitude", "Longitude", "Altitude")
rownames(correlation_matrix) <- c("Latitude", "Longitude", "Altitude")
print(correlation_matrix)
