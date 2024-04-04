# Load required libraries
library(ggplot2)
library(rstanarm)

# Generate sample data
set.seed(123)
data <- data.frame(
  Age_Group = sample(c('18-24', '25-34', '35-44', '45-54'), 1000, replace = TRUE),
  Gender = sample(c('Male', 'Female'), 1000, replace = TRUE),
  Income_Group = sample(c('Low', 'Medium', 'High'), 1000, replace = TRUE),
  Highest_Education = sample(c("High School", "Bachelor's Degree", "Master's Degree"), 1000, replace = TRUE),
  Political_Party_Support = sample(c('Yes', 'No'), 1000, replace = TRUE)
)cc

# Plot stacked bar chart for political party support by demographic categories
ggplot(data, aes(x = Age_Group, fill = Political_Party_Support)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Gender + Income_Group + Highest_Education, scales = "free_x") +
  labs(title = "Political Party Support by Demographic Categories", x = "Age Group", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red"))

# Build Bayesian logistic regression model using rstanarm
model <- stan_glm(Political_Party_Support ~ Age_Group + Gender + Income_Group + Highest_Education,
                  data = data, family = binomial(link = "logit"))

# Summarize the model
summary(model)

