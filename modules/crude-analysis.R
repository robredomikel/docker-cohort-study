library(MatchIt)
library(optmatch)
library(Matching)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(plotly)

############### HELP FUNCTIONS

# Function to generate Q-Q plots for two groups
qq_plot <- function(data, covariate_name, group_column) {
  # Filter data for the specific covariate
  subset_data <- data[data$Covariate == covariate_name, ]
  
  # Create a QQ plot comparing groups
  ggplot(subset_data, aes(sample = Value, colour = !!sym(group_column))) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("Q-Q Plot for", covariate_name),
         subtitle = "Comparing Quantiles between Groups",
         x = "Treatment cases",
         y = "Controls") +
    scale_fill_continuous(type = "gradient") +
    theme_minimal()
}

###########################################
# CRUDE ANALYSIS
###########################################

df <- read.csv('final_data_file.csv')
df <- df[c(1:2, 6, 4, 3, 5 , 7, 8:ncol(df))]

# df$main_language <- as.numeric(as.factor(df$main_language))
# df$creation_year <- as.numeric(as.factor(df$creation_year))

View(df)
cases_df <- df[ df$MS.NonMS =='MS',] 
cases_endvelocity <- abs(cases_df$velocity_mean_end)

controls_df <- df[ df$MS.NonMS =='~MS',]
controls_endvelocity <- abs(controls_df$velocity_mean_end)

## ASSESSING NORMALITY

# Through histograms
hist(cases_endvelocity, col='steelblue', main='CASES')
hist(controls_endvelocity, col = 'red', main='CONTROLS')

# Definitely non-normal

# Through interquantile plots
par(mfrow=c(1,2))
qqnorm(cases_endvelocity, main='CASES')
qqline(cases_endvelocity)

qqnorm(controls_endvelocity, main='CONTROLS')
qqline(controls_endvelocity)
par(mfrow=c(1,1))
# Definitely non-normal

# Through Shapiro-Wilk test:
shapiro.test(cases_endvelocity) # For cases
shapiro.test(controls_endvelocity) # For controls
# Massively non-normal.


############################################
# UNMATCHED DATA

## INDEPENDENT TWO-WAY SAMPLE T-TEST

# Ho: Adoption of Docker has impact on the development velocity.
# Ha: Adoption of Docker does not have impact on the development velocity.

mean(cases_endvelocity)
mean (controls_endvelocity) 
sd(cases_endvelocity)
sd(controls_endvelocity)

# Mmm somewhat similar variances

# Assuming equal variance
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE)
# Trying the left sided sample t-test
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE, alternative = 'two.sided')
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE, alternative = 'less')
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE, alternative = 'greater')

# Assuming different variance (Welch's t-test)
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE)
# Trying the left sided sample t-test
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE, alternative = 'two.sided')
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE, alternative = 'less')
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE, alternative = 'greater')



## Wilcoxon rank sum test (Non parametric) ~ two sample, hence MANN-WHITNEY
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "greater")
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "two.sided")
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "less")

# With correction
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "greater", correct = T)
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "two.sided", correct = T)
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "less", correct = T)

# We reject in both cases the null-hypothesis

############################################
# MATCHED DATA

# Observational boxplots of the groups.
data_long <- pivot_longer(df, cols = c(velocity_mean_start:n_contributors), names_to = "Covariate", values_to = "Value")

ggplot(data_long, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

# NOTE: The box plots provide a look of match between both cases and controls, therefore
# we should look now how main_language as categorical value looks like and see if matching is needed.

# Paired BARPLOT for the main_language confounder in case we get matching
# Count the occurrences
df_language <- df %>%
  group_by(main_language, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(df_language, aes(x = main_language, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Language",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()

# Same based on creation year
df_creation <- df %>%
  group_by(creation_year, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(df_creation, aes(x = creation_year, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Creation Year",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()

# NOTE: Based on the displayed observational plots, the matching will be based on 
# the main language variable as it's the one that shows the most difference between the study groups.

################# PROPENSITY SCORE MATCHING

##### 1:1 tolerance
m.propensity_tol_1 <- matchit(as.numeric(as.factor(df$MS.NonMS)) - 1 ~ as.numeric(as.factor(main_language)), data = df, method = "nearest")
summary(m.propensity_tol_1) # 1:1 tolerance
data_m.propensity_1 <- match.data(m.propensity_tol_1)
data_long_m_propensity_1 <- pivot_longer(data_m.propensity_1, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_m_propensity_1$Covariate), function(cov) {
  qq_plot(data_long_m_propensity_1, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_m_propensity_1, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (1:1 tolerance - Propensity Score Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

###### 3:1 tolerance
m.propensity_tol_3 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "nearest", ratio = 3)
summary(m.propensity_tol_3) # 1:3 tolerance (3 controls per treatment)
data_m.propensity_3 <- match.data(m.propensity_tol_3)
data_long_m_propensity_3 <- pivot_longer(data_m.propensity_3, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_m_propensity_3$Covariate), function(cov) {
  qq_plot(data_long_m_propensity_3, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_m_propensity_3, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (3:1 tolerance - Propensity Score Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

####### 4:1 tolerance
m.propensity_tol_4 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "nearest", ratio = 4)
summary(m.propensity_tol_4) # 1:4 tolerance (4 controls per treatment)
data_m.propensity_4 <- match.data(m.propensity_tol_4)
data_long_m_propensity_4 <- pivot_longer(data_m.propensity_4, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_m_propensity_4$Covariate), function(cov) {
  qq_plot(data_long_m_propensity_4, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_m_propensity_4, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (4:1 tolerance - Propensity Score Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

# NOTES: 

#################  Exact Matching (Does not allow for ratio)

m.exact_1 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "exact")
summary(m.exact_1)
data_m.exact_1 <- as.data.frame(match.data(m.exact_1))
data_long_exact_1 <- pivot_longer(data_m.exact_1, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_exact_1$Covariate), function(cov) {
  qq_plot(data_long_exact_1, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_exact_1, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (Exact Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

# 1. Get row names of the matched data
matched_rows <- rownames(data_m.exact_1)

# 2. Get row names of the original data
original_rows <- rownames(df)

# 3. Find rows that were discarded (those in original not in matched)
discarded_rows <- setdiff(original_rows, matched_rows)

# 4. Extract discarded rows from the original dataframe
discarded_data <- df[discarded_rows, ]

# Check the first few rows of the discarded data
head(discarded_data)

################# Optimal Matching

m.optimal_1 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "optimal", ratio = 1)
summary(m.optimal_1) # 1:1 tolerance
data_m.optimal_1 <- match.data(m.optimal_1)
data_long_optimal_1 <- pivot_longer(data_m.optimal_1, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_optimal_1$Covariate), function(cov) {
  qq_plot(data_long_optimal_1, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_optimal_1, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (1:1 tolerance - Optimal Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

###### 3:1 tolerance
m.optimal_3 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "optimal", ratio = 3)
summary(m.optimal_3) # 1:3 tolerance (3 controls per treatment)
data_m.optimal_3 <- match.data(m.optimal_3)
data_long_optimal_3 <- pivot_longer(data_m.optimal_3, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_optimal_3$Covariate), function(cov) {
  qq_plot(data_long_optimal_3, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_optimal_3, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (3:1 tolerance - Optimal Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate

####### 4:1 tolerance
m.optimal_4 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "optimal", ratio = 4)
summary(m.optimal_4) # 1:4 tolerance (4 controls per treatment)
data_m.optimal_4 <- match.data(m.optimal_4)
data_long_optimal_4 <- pivot_longer(data_m.optimal_4, cols = velocity_mean_start:n_contributors, names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_optimal_4$Covariate), function(cov) {
  qq_plot(data_long_optimal_4, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
ggplot(data_long_optimal_4, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (4:1 tolerance - Optimal Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate



################# Covariate Matching



################# Mahalanobis Distance Matching



################# Coarsened Exact Matching


