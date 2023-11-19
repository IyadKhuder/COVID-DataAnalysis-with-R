# Basic ETL
# Remove previously-stored variables  
rm(list=ls())

# Load necessary libraries for data analysis and visualization
library(Hmisc) # Provides functions like describe(data)
library(ggplot2) # A versatile plotting package
library(dplyr) # For data manipulation and selection


# Define the URL of the dataset
data_url <- "dataset/COVID19_line_list_data.csv"

# Read the dataset from the specified URL
raw_data <- read.csv(data_url)

# Display descriptive statistics for the raw data
describe(raw_data)

# Clean up the 'death' column
# Some rows in the "death" field have the date of death instead of 1.
# If there's a date, it confirms death, so create a new Boolean field 'death_B'.
raw_data$death_B <- as.integer(raw_data$death != 0)

# Create a 'survival' field that is the opposite of the 'death_B' field
raw_data$survival <- as.integer(raw_data$death_B == 0)

# List of unnecessary fields to remove
unnecessary_fields <- c("summary", "death", "X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6", 
                        "symptom_onset", "If_onset_approximated", "symptom", "source", "link")

# Remove unnecessary fields from the dataset
filtered_data <- raw_data %>%
  select(-one_of(unnecessary_fields))


# Calculate and display the death rate
death_rate <- sum(filtered_data$death_B) / nrow(filtered_data)
cat("Death Rate:", death_rate, "\n")

# Analyze AGE
# Claim: People who die are older
dead <- subset(filtered_data, death_B == 1)
alive <- subset(filtered_data, death_B == 0)

# Calculate the mean age for both dead and alive individuals
mean_age_dead <- mean(dead$age, na.rm = TRUE)
mean_age_alive <- mean(alive$age, na.rm = TRUE)

cat("Mean Age (Dead):", mean_age_dead, "\n")
# Mean Age (Dead): 68.58621 

cat("Mean Age (Alive):", mean_age_alive, "\n")
# Mean Age (Alive): 48.07229 

# Perform a t-test to check the statistical significance of the age difference
t_test_age <- t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
print(t_test_age)
# OUTPUT: t = -10.839, df = 72.234, p-value < 2.2e-16

# Normally, if p-value < 0.05, we reject the Null-Hypothesis
# here, p-value is infinitely small!
# A very small p-value denounces the Null-Hypothesis.
# Conclusion: The age is statistically significant, and the claim is proved true.

# Visualization: Boxplot of Age by Survival Status
ggplot(filtered_data, aes(x = factor(survival), y = age, fill = factor(survival))) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Survival Status",
       x = "Survival Status",
       y = "Age",
       fill = "Survival Status") +
  theme_minimal()


# Analyze GENDER
# Check the significance of gender (male/female) on the rate of death among patients
men <- subset(filtered_data, gender == "male")
women <- subset(filtered_data, gender == "female")

# Calculate the probability of death for each gender category
prob_death_men <- mean(men$death_B, na.rm = TRUE)
prob_death_women <- mean(women$death_B, na.rm = TRUE)

cat("Probability of Death (Men):", prob_death_men, "\n")
# Probability of Death (Men): 0.08461538

cat("Probability of Death (Women):", prob_death_women, "\n")
# Probability of Death (Women): 0.03664921 

# Perform a t-test to check the statistical significance of the gender difference
t_test_gender <- t.test(men$death_B, women$death_B, alternative="two.sided", conf.level = 0.99)
print(t_test_gender)
# OUTPUT: t = 3.084, df = 894.06, p-value = 0.002105

# Conclusion: With a 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so, gender is statistically significant.


# Visualization: Barplot of Probability of Death by Gender
gender_probs <- data.frame(Gender = c("Men", "Women"), Probability_of_Death = c(prob_death_men, prob_death_women))
ggplot(gender_probs, aes(x = Gender, y = Probability_of_Death, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Probability of Death by Gender",
       x = "Gender",
       y = "Probability of Death",
       fill = "Gender") +
  theme_minimal()


# ############# Distribution of cases across different countries #############

# Filter out rows where the "case_in_country" variable is non-numerical | dplyr package.
filtered_data <- filtered_data %>%
  filter(!is.na(as.numeric(case_in_country)))

# Now, For each unique value of the field `country`, 
# keep just the row, which contains the maximal value of  the field `case_in_country` 
# for that specific country, in such a way that 
# I eliminate the gradual increments of  `case_in_country`values for each country 
# and only maintain the last attained scores.

country_data <- filtered_data %>%
  group_by(country) %>%
  arrange(desc(case_in_country)) %>%
  slice(1) %>%
  ungroup()

# Visualization: Barplot of Number of Cases by Country
ggplot(country_data, aes(x = reorder(country, -case_in_country), y = case_in_country, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of COVID-19 Cases by Country",
       x = "Country",
       y = "Number of Cases",
       fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



























