#Install required packages
install.packages("tidyverse")
install.packages("caret")
install.packages("broom")
install.packages("car")
install.packages("ggplot2")

# Load necessary libraries
library(tidyverse)
library(caret)
library(broom)
library(car)
library(ggplot2)

# Load the dataset
cancer_data <- read.csv("C:\\Users\\Ferah Shan\\Downloads\\cancer_reg.csv")

# Check for missing values
cat("Total missing values: ", sum(is.na(df)), "\n")

# Boxplot Visualization
ggplot(cancer_data, aes(x = "", y = TARGET_deathRate)) +
  geom_boxplot() +
  labs(title = "Boxplot of Death Rate", y = "Death Rate")

# Interquartile Range (IQR) Method
Q1 <- quantile(cancer_data$TARGET_deathRate, 0.25, na.rm = TRUE)
Q3 <- quantile(cancer_data$TARGET_deathRate, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers using IQR method
outliers_iqr <- cancer_data %>% filter(TARGET_deathRate < lower_bound | TARGET_deathRate > upper_bound)

# Print outliers based on IQR method
print(outliers_iqr)

# Remove outliers based on IQR method
cancer_data_clean_iqr <- cancer_data %>%
  filter(TARGET_deathRate >= lower_bound & TARGET_deathRate <= upper_bound)

# Verify the number of rows removed
cat("Number of rows before removing outliers (IQR method):", nrow(cancer_data), "\n")
cat("Number of rows after removing outliers (IQR method):", nrow(cancer_data_clean_iqr), "\n")

# Check the structure of the dataset
str(cancer_data_clean_iqr)

#Select the required variables and impute mean where there is missing value
selected_vars <- cancer_data_clean_iqr %>%
  select(avgAnnCount, avgDeathsPerYear, incidenceRate, medIncome, popEst2015, povertyPercent, 
         studyPerCap, MedianAge, PctHS25_Over, PctBachDeg25_Over, PctEmployed16_Over, 
         PctUnemployed16_Over, PctPrivateCoverage, PctPublicCoverage, PctWhite, PctBlack, 
         PctAsian, PctOtherRace, PctMarriedHouseholds, BirthRate, TARGET_deathRate)

selected_vars <- selected_vars %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Build the initial model
model <- lm(TARGET_deathRate ~ ., data = selected_vars)

# Evaluate the model
summary(model)

# Convert categorical variables to factors
cancer_data <- cancer_data_clean_iqr %>%
  mutate(across(where(is.character), as.factor))

# Check the number of data points in the death rate column
length(cancer_data$TARGET_deathRate)

# Display the mortality_rate column
head(cancer_data$TARGET_deathRate)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(cancer_data$TARGET_deathRate, p = 0.7, list = FALSE)
train_data <- cancer_data[train_index, ]
test_data <- cancer_data[-train_index, ]

# Diagnostics
# Linearity plot
plot(model, which = 1)

# Independence (Durbin-Watson test)
durbinWatsonTest(model)

# Heteroskedasticity plot
plot(model, which = 3)

# Heteroskedasticity test
ncvTest(model)

# Normality plot
plot(model, which = 2)

# Normality test
shapiro.test(residuals(model))

# Multicollinearity (VIF)
vif(model)

# Output the model equation and diagnostics
list(
  equation = model,
  diagnostics = list(
    linearity = plot(model, which = 1),
    independence = durbinWatsonTest(model),
    heteroskedasticity = list(
      plot = plot(model, which = 3),
      test = ncvTest(model)
    ),
    normality = list(
      plot = plot(model, which = 2),
      test = shapiro.test(residuals(model))
    ),
    multicollinearity = vif(model),
    outliers = plot(model, which = 4)
  )
)
