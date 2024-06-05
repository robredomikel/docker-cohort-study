library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(ggfortify)
library(ggplot2)
library(tidyr)  # For pivoting the data
library(gridExtra) 
library(dplyr)
library(car)
library(rgl)

###########################################
# DESCRIPTIVE ANALYSIS
###########################################

### ANALYSIS OF THE ASSUMPTIONS FOR ANCOVA

model <- lm(velocity_mean_end ~  factor(MS.NonMS) + velocity_mean_start +
              size + n_languages + factor(creation_year) + n_commits + n_issues +
              n_contributors, data = data_m.exact_1)
summary(model)
par(mfrow = c(2, 2))
plot(model)
avPlots(model)
crPlots(model)

# 1. Linearity assumption (Residuals vs Fitted values)
# The blue line should go approximately horizontal and next to 0. If there is
# Some pronounced pattern then means  there is no perfect linearity
autoplot(model)
# NOTE: The mean of the residuals deviate clearly negatively from the reference linear line...
ggscatter(
  data_m.exact_1, x = "velocity_mean_start", y = "velocity_mean_end",
  facet.by  = c("creation_year", "MS.NonMS"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 2)


# 2. Homogeneity of Variance (Scale-location)
# The line should go horizontal and not follow any trend, therefore showing homogeneity, if not then that means that the residuals have non-constant variance (heterocedasticity)
# Try to 1) Remove outliers, 2) log or square transform of the dependent variable.
levene_test(data_m.exact_1, model$residuals ~ factor(MS.NonMS))

# 3. Normality of residuals (Normal Q-Q plot)
# The residuals should follow the diagonal line.
shapiro.test(model$residuals) 
# We reject the H0 (residuals normally distributed) and therefore assume that there is no statistical significance to say
# that the data is normally distributed.

# 4. Homogeneity of regression slopes
anova.test <- anova_test(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start +
                           size + n_languages + factor(creation_year) + n_commits + n_issues +
                           n_contributors, data = data_m.exact_1)
anova.test

# NOTES: From the initially obtained results it looks like the assumption for the
# ANCOVA execution does not hold, we should move into regression options.

# Distribution of the model variables
adjustment_df <- data_m.exact_1[c(2, 4:11)]
adjustment_df$MS.NonMS <- as.factor(adjustment_df$MS.NonMS)
adjustment_df$creation_year <- as.factor(adjustment_df$creation_year)
View(adjustment_df)

df_long <- pivot_longer(adjustment_df, cols = everything(), names_to = "Variable", values_to = "Value")

# Create a list of plots
plots <- lapply(unique(df_long$Variable), function(var) {
  ggplot(df_long[df_long$Variable == var, ], aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
    geom_density(col = "red") +
    ggtitle(paste("Distribution of", var))
})

# Arrange the plots in a 2 by 4 grid
do.call(grid.arrange, c(plots, ncol = 4, nrow = 2))


