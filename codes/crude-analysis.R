library(MatchIt)
library(optmatch)
library(Matching)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(plotly)
library(nortest)
library(fBasics)
library("ggpubr")
library("yarr")
library(xtable)
library(lsr)

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
    scale_color_manual(values = c("dodgerblue4", "gray63")) +
    theme_minimal()
}

###########################################
# CRUDE ANALYSIS
###########################################

raw_df <- read.csv('/Users/mrobredo23/OULU/docker_cohort-24/data/final_data_file.csv')
View(raw_df)
raw_df <- subset(raw_df, select = -c(trimmed_languages))
raw_df$velocity_mean_end <- raw_df$velocity_mean_end + 0.01
raw_df$velocity_mean_start <- raw_df$velocity_mean_start + 0.01
df <- raw_df[c(2:ncol(raw_df))] # For the modelling purposes
View(df)
df$main_language <- as.numeric(as.factor(df$main_language))
df$creation_year <- as.numeric(as.factor(df$creation_year))
df$MS.NonMS <- as.numeric(as.factor(df$MS.NonMS))

plot(df$velocity_mean_end, df$velocity_mean_start)

# Conditional plot of the power of the independent variable
coplot(velocity_mean_end~velocity_mean_start|MS.NonMS, col = "red", rows = 1, data = df)

View(df)
cases_df <- raw_df[raw_df$MS.NonMS =='MS',] 
cases_endvelocity <- abs(cases_df$velocity_mean_end)

controls_df <- raw_df[raw_df$MS.NonMS =='~MS',]
controls_endvelocity <- abs(controls_df$velocity_mean_end)

################# ASSESSING NORMALITY #####################

# Through histograms
par(mfrow=c(1,2))
hist(cases_endvelocity, col="dodgerblue4", main='CASES')
hist(controls_endvelocity, col = "gray63", main='CONTROLS')
# Definitely non-normal

# Through interquantile plots
qqnorm(cases_endvelocity, main='CASES', col='red')
qqline(cases_endvelocity)

qqnorm(controls_endvelocity, main='CONTROLS', col='red')
qqline(controls_endvelocity)
par(mfrow=c(1,1))
# Definitely non-normal

normality_plot(cases_endvelocity, controls_endvelocity, "MS projects", 
               "non-MS projects", "non-MS projects")

# Through Shapiro-Wilk test: (Given our small sample sizes... it matches the case scenario)
shapiro.test(cases_endvelocity) # For cases
shapiro.test(controls_endvelocity) # For controls
# Massively non-normal.
# We reject the H0 (data normally distributed) and therefore assume that there is no statistical significance to say
# that the data is normally distributed.

# Through other normality tests. 

# Anderson-darling (given that it doesn't require sample size)
ad.test(cases_endvelocity)
ad.test(controls_endvelocity)
# Ho: The data population follows a normal distribution
# Ha: Otherwise
# Tresult: p-value < 2.2e-16 & p-value < 2.2e-16 (We reject Ho)

# D'Agostino's K-squared test:
normalTest(cases_endvelocity, method = c("da")) # has power only against the alternatives that the distribution is skewed and/or kurtic.
normalTest(controls_endvelocity, method = c("da"))
# Ho: The data does not significantly differ from the normal distribution (Approx. normal)
# Ha: Otherwise
# Tresult: < 2.2e-16 (We reject the null hypothesis) in both

#####################################################################

################# CRUDE ANALYSIS: UNMATCHED DATA ####################

## INDEPENDENT TWO-WAY SAMPLE T-TEST

# Ho: Adoption of Docker has impact on the development velocity.
# Ha: Adoption of Docker does not have impact on the development velocity.

# 1. Data is numeric (CHECK)

# 2. Observations are independent between each other. (CHECK)
# We assume it as we both populations are filled by projects that are independent from each other by the nature of data.


# 3. Sample means are normally distributed (CHECK - They aren't)
# NOTE: According to the CLT a population whose sample of n=30 is normally distributed, it will be approximately distributed.
par(mfrow=c(2,1))
cases_sample <- sample(cases_endvelocity, size = 30)
hist(cases_sample, main="Sample mean for Cases end velocity", freq=F)
curve(dnorm(x, mean = mean(cases_sample), sd=sd(cases_sample)), add = T, col="red")
controls_sample <- sample(controls_endvelocity, size = 30)
hist(controls_sample, main="Sample mean for Controls end velocity", freq=F)
curve(dnorm(x, mean = mean(controls_sample), sd=sd(controls_sample)), add = T, col="red")
par(mfrow=c(1,1))
# 4. The variances between the groups are equal (CHECK - They aren't)
(sd(cases_endvelocity))^2
(sd(controls_endvelocity))^2

# NOTES: There is enough evidence that show we do not fulfill the assumptions to run a Independent two-way samples t-test.

##### HYPOTHESIS TESTING
# Left tile hypothesis (less)
# Ho: The velocity is higher in cases than in controls (for us would be less because we are looking for a smaller mean velocity in cases)
# Ha: Otherwise

# Two sided hypothesis
# Ho: The velocity median in both groups is equal
# Ha: The velocity median in both groups is different 

# Assuming equal variance
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE)
# Trying the left sided sample t-test
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE, alternative = 'two.sided')
t.test(cases_endvelocity, controls_endvelocity, var.equal = TRUE, alternative = 'less')
# LOGIC

# Assuming different variance (Welch's t-test)
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE)
# Trying the left sided sample t-test
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE, alternative = 'two.sided')
t.test(cases_endvelocity, controls_endvelocity, var.equal = FALSE, alternative = 'less')

##### NON-PARAMETRIC HYPOTHESIS TESTING 
library(gridExtra)

# VERSION WITH VIOLIN PLOTS
plot1 <- ggplot(data = df, aes(x=MS.NonMS, y=velocity_mean_end), 
                color = "MS.NonMS", palette = c("dodgerblue4", "gray63"),
                ylab = "Velocity Means End", xlab = "Groups") + geom_violin() + coord_flip()
plot2 <- ggplot(data = df, aes(x=MS.NonMS, y=velocity_mean_start), 
                color = "MS.NonMS", palette = c("dodgerblue4", "gray63"),
                ylab = "Velocity Means End", xlab = "Groups") + geom_violin() + coord_flip()
grid.arrange(plot1, plot2, ncol = 2)

# VERSION WITH NORMAL BOXPLOTS 
df_renamed <- df
df_renamed <- df_renamed %>%
  mutate(MS.NonMS = fct_recode(MS.NonMS, "Non-MS" =  "~MS"))

plot1 <- ggboxplot(data = df_renamed, x = "MS.NonMS", y = "velocity_mean_end", 
          color = "MS.NonMS", palette = c("lavenderblush3", "lightcyan4", "black"),
          ylab = "Velocity Means End", xlab = "") + coord_flip() + 
  ggtitle("") + theme(legend.position="none") + theme(legend.title=element_blank()) +
  geom_boxplot(aes(fill = `MS.NonMS`, color = "black"), outlier.shape = 21,
               outlier.fill = 'transparent', outlier.alpha = 1, size = 0.8) +
  font("xlab", size = 20, face = "bold") + font("xy.text", size = 16)

plot2 <- ggboxplot(data = df_renamed, x = "MS.NonMS", y = "velocity_mean_start", 
          color = "MS.NonMS", palette = c("lavenderblush3", "lightcyan4", "black"),
          ylab = "Velocity Means Start", xlab = "") + coord_flip() +
  ggtitle("") + theme(legend.position="none") + theme(legend.title=element_blank()) +
  geom_boxplot(aes(fill = `MS.NonMS`, color = "black"), outlier.shape = 21,
               outlier.fill = "transparent", outlier.alpha = 1, size = 0.8) +
  font("xlab", size = 20, face = "bold") + font("xy.text", size = 16)
grid.arrange(plot1, plot2, ncol = 2)

## Wilcoxon rank sum test (Non parametric) ~ two sample, hence MANN-WHITNEY
wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "two.sided", conf.int = T) # p-value = 0.5987 
# Ho: Both distributions are equal (median difference is zero - two sided)
# Ha: Both distributions are unequal (two-sided)

mw_result <- wilcox.test(cases_endvelocity, controls_endvelocity, alternative = "less", conf.int = T) # p-value = 0.7013
# Ho: The median of the first group is smaller than the median of the second group (The time for projects to get issues closed is shorter with MS pros than with Mono pros)
# Ha: The median of the first group is greater than the median of the second group. (The time for projects to get issues closed is longer with MS pros than with Mono pros))


#####################################################################

################# CRUDE ANALYSIS: MATCHED DATA ####################

# The df containing the data from the matching process is "matched_data_df"
cases_matched_df <- matched_data_df[matched_data_df$MS.NonMS =='MS',] 
cases_matched_endvelocity <- abs(cases_df$velocity_mean_end)

controls_matched_df <- matched_data_df[matched_data_df$MS.NonMS =='~MS',]
controls_matched_endvelocity <- abs(controls_df$velocity_mean_end)

## Wilcoxon rank sum test (Non parametric) ~ two sample, hence MANN-WHITNEY
wilcox.test(cases_matched_endvelocity, controls_matched_endvelocity, alternative = "two.sided", conf.int = T) # p-value = 0.5987 
# Ho: Both distributions are equal (median difference is zero - two sided)
# Ha: Both distributions are unequal (two-sided)

wilcox.test(cases_matched_endvelocity, controls_matched_endvelocity, alternative = "less", 
            conf.int = T, conf.level = 0.95) # p-value = 0.7013
# Ho: The median of the first group is greater than the median of the second group (The time for projects to get issues closed is longer with MS pros than with Mono pros)
# Ha: The median of the first group is less than the median of the second group. (The time for projects to get issues closed is shorter with MS pros than with Mono pros))


#####################################################################

################# CRUDE ANALYSIS: TRANSFORMED DATA ####################

# Assuming different variance (Welch's t-test)
t.test(cases_endvelocity_cube, controls_endvelocity_cube, var.equal = FALSE)
# Trying the left sided sample t-test
t.test(cases_endvelocity_cube, controls_endvelocity_cube, var.equal = FALSE, alternative = 'two.sided')
t.test(cases_endvelocity_cube, controls_endvelocity_cube, var.equal = FALSE, alternative = 'less', conf.level = 0.95)

# QUANTIFICATION OF THE DIFFERENCE
cohensD(x=cases_endvelocity_cube, controls_endvelocity_cube, method = "pooled")
