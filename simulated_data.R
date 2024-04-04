# Load required libraries
library(dplyr)
library(tidyr)

# Define variables
num_samples <- 1000
age_groups <- c('18-24', '25-34', '35-44', '45-54')
genders <- c('Male', 'Female')
income_groups <- c('Low', 'Medium', 'High')
education_levels <- c("High School", "Bachelor's Degree", "Master's Degree")
political_party_support <- c('Yes', 'No')

# Generate  data
set.seed(123)
data <- data.frame(
  Age_Group = sample(age_groups, num_samples, replace = TRUE),
  Gender = sample(genders, num_samples, replace = TRUE),
  Income_Group = sample(income_groups, num_samples, replace = TRUE),
  Highest_Education = sample(education_levels, num_samples, replace = TRUE),
  Political_Party_Support = sample(political_party_support, num_samples, replace = TRUE)
)

# Test 1: Average support percentage for each demographic category
test1 <- data %>%
  group_by(Age_Group, Gender, Income_Group, Highest_Education) %>%
  summarise(Avg_Support_Percentage = mean(Political_Party_Support == 'Yes'))

# Test 2: Support percentage by age group
test2 <- data %>%
  group_by(Age_Group) %>%
  summarise(Support_Percentage = mean(Political_Party_Support == 'Yes'))

# Test 3: Support percentage by gender
test3 <- data %>%
  group_by(Gender) %>%
  summarise(Support_Percentage = mean(Political_Party_Support == 'Yes'))

# Test 4: Support percentage by income group
test4 <- data %>%
  group_by(Income_Group) %>%
  summarise(Support_Percentage = mean(Political_Party_Support == 'Yes'))

# Test 5: Support percentage by education level
test5 <- data %>%
  group_by(Highest_Education) %>%
  summarise(Support_Percentage = mean(Political_Party_Support == 'Yes'))

# Test 6: Number of samples for each demographic category
test6 <- data %>%
  group_by(Age_Group, Gender, Income_Group, Highest_Education) %>%
  summarise(Count = n())

# Test 7: Total support percentage
test7 <- data %>%
  summarise(Total_Support_Percentage = mean(Political_Party_Support == 'Yes'))

# Test 8: Cross-tabulation between age group and political party support
test8 <- prop.table(table(data$Age_Group, data$Political_Party_Support), margin = 1)

# Test 9: Cross-tabulation between gender and political party support
test9 <- prop.table(table(data$Gender, data$Political_Party_Support), margin = 1)

# Test 10: Cross-tabulation between income group and political party support
test10 <- prop.table(table(data$Income_Group, data$Political_Party_Support), margin = 1)

