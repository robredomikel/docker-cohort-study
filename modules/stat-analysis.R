###########################################
# STATISTICAL ANALYSISANALYSIS
###########################################

library(car)
library(corrplot)
library(RColorBrewer)
library(rstatix)
library(bestglm)

###########################################
# 1.WITHOUT MATCHING

###########################################
# 1.1 WITHOUT TRANSFORMATION

###########################################
# 1.1.1 GLM (Gamma)
gamma_model <- glm(formula = velocity_mean_end ~ MS.NonMS + velocity_mean_start + size + n_languages + 
                     creation_year + n_commits + n_issues + n_contributors + main_language, data=df, family = Gamma(link = "log")) 
vif_values <- vif(gamma_model)
vif_values # No multicollinearity so far.
cor_matrix <- cor(df[header_names])
corrplot(cor_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

# Assessment of the model results
shapiro.test(gamma_model$residuals) # p-value < 2.2e-16 (Not normally distributed, not reliable model)
summary(gamma_model)
mean(gamma_model$residuals^2)
plot(gamma_model$fitted.values, gamma_model$residuals)

# Set to comparison the impact of independent variable.
gamma_model_h1 <- glm(formula = velocity_mean_end ~ velocity_mean_start + size + n_languages + 
                     creation_year + n_commits + n_issues + n_contributors + main_language, data=df, family = Gamma(link = "log")) 
anova(gamma_model, gamma_model_h1, test="F") # p-value = 0.9903 (No statistical difference)

# Impact of the interaction
gamma_model_intr <- glm(formula = velocity_mean_end ~ MS.NonMS*velocity_mean_start + size*n_contributors + n_languages + 
                     creation_year + n_commits + n_issues + main_language, data=df, family = Gamma(link = "log")) 
summary(gamma_model_intr)


###########################################
# 1.1.2 GLM (Inverse Gaussian)
invgauss_model <- glm(formula = velocity_mean_end ~ MS.NonMS + velocity_mean_start + size + n_languages + 
                     creation_year + n_commits + n_issues + n_contributors + main_language, data=df, family = inverse.gaussian(link = "inverse")) 

# With the complexity of the data... Doesn't work (Check it out later)


###########################################
# 1.2 WITH TRANSFORMATION 

df_log <- df
df_log$velocity_mean_end <- log(df_log$velocity_mean_end)
df_cube <- df
df_cube$velocity_mean_end <- (df_cube$velocity_mean_end)^(1/3)

normality_plot(df_log$velocity_mean_end, df_cube$velocity_mean_end, "LOG", 
               "CUBE", "cube transformation")

###########################################
# 1.2.1 ANCOVA (We use the cube)


model_aov <- aov(velocity_mean_end ~ MS.NonMS + velocity_mean_start + size*n_contributors + n_languages + 
                   creation_year + n_commits + n_issues + main_language, data = df_cube)
summary(model_aov)

# 1. Linearity assumption (Residuals vs Fitted values)
# The blue line should go approximately horizontal and next to 0. If there is
# Some pronounced pattern then means  there is no perfect linearity
autoplot(model_aov)
# After fitting the ancova model, there's no clear pattern within the residuals appart from some outliers, which confirms the
# linear relationship among the dependent and the independent variables.
corrplot(cor(df_cube[header_names]))

# 2. Homogeneity of Variance (Scale-location)
leveneTest(velocity_mean_end ~ as.factor(MS.NonMS), data = df_cube)
# p-value=0.8354 (We don't have enough evidence to reject the null hypothesis, therefore 
# the difference among the variances of the two groups is not statistically significant.

# 3. Normality of residuals (Normal Q-Q plot)
# The residuals should follow the diagonal line.
shapiro.test(model$residuals) 

# 4. Homogeneity of regression slopes
anova.test <- anova_test(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start +
                           size + n_languages + factor(creation_year) + n_commits + n_issues +
                           n_contributors, data = df_cube)
anova.test


###########################################
# 1.2.2 Linear Regression (We use the cube)
model_lm <- lm(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                   factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube)
summary(model_lm)

model_lm_0 <- lm(velocity_mean_end ~ velocity_mean_start + size*n_contributors + n_languages + 
                 factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube)

# We test the impact of the 
anova(model_lm_0,model_lm) # 0.7451 We cannot reject the null hypothesis, therefore the implication of the variance in the
# addition of the independent variable is not statistically signigicant to say that it improvers the model.

autoplot(model_lm)


######################################################################################
######################################################################################
######################################################################################


###########################################
# 2.WITH MATCHING (exact matching)

###########################################
# 2.1 WITHOUT TRANSFORMATION

###########################################
# 2.1.1 GLM (Gamma)
gamma_model <- glm(formula = velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size + n_contributors + n_languages + 
                     factor(creation_year) + n_commits + n_issues + factor(main_language), 
                   data=data.exactMatching, family = Gamma(link = "log"), weights = data.exactMatching$weights) 
summary(gamma_model)
vif_values <- vif(gamma_model, type = "predictor")  # Since we have interactions with the weights
vif_values # No multicollinearity so far.

model_covariates <- c("MS.NonMS", "velocity_mean_start", "size", "n_contributors",
                      "n_languages", "creation_year", "n_commits", "n_issues", "main_language")

numeric_df <- data.exactMatching
numeric_df$MS.NonMS <- as.numeric(numeric_df$MS.NonMS)
numeric_df$main_language <- as.numeric(numeric_df$main_language)
numeric_df$creation_year <- as.numeric(numeric_df$creation_year)

cor_matrix <- cor(numeric_df[model_covariates])
corrplot(cor_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

# Assessment of the model results
shapiro.test(gamma_model$residuals) # p-value < 3.639e-14 (Not normally distributed, not reliable model)
summary(gamma_model)
plot(gamma_model$fitted.values, gamma_model$residuals) # There's no clear trend, could be considered.
autoplot(gamma_model)

# Set to comparison the impact of independent variable.
gamma_model_h0 <- glm(formula = velocity_mean_end ~ velocity_mean_start + size + n_languages + 
                        creation_year + n_commits + n_issues + n_contributors + main_language, 
                      data=data.exactMatching, family = Gamma(link = "log"), weights = data.exactMatching$weights) 
anova(gamma_model_h0, gamma_model, test="F") # p-value = 0.477 (No statistical difference)

# Impact of the interaction
gamma_model_intr <- glm(formula = velocity_mean_end ~ MS.NonMS + velocity_mean_start + size*n_contributors + n_languages + 
                          creation_year + n_commits + n_issues + main_language, 
                        data=data.exactMatching, family = Gamma(link = "log"), weights = data.exactMatching$weights) 
anova(gamma_model, gamma_model_intr, test = "F") # 0.9076
summary(gamma_model_intr)


###########################################
# 2.1.2 GLM (Inverse Gaussian)
invgauss_model <- glm(formula = velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size + n_contributors + n_languages + 
                        factor(creation_year) + n_commits + n_issues + factor(main_language), 
                      data=data.exactMatching, family = inverse.gaussian(link = "inverse"), weights = data.exactMatching$weights) 

vif_values <- vif(invgauss_model, type = "predictor")  # Since we have interactions with the weights
vif_values # No multicollinearity so far.

summary(invgauss_model)
autoplot(invgauss_model)

# Testing the impact of Docker
invgauss_model_h0 <- glm(formula = velocity_mean_end ~ velocity_mean_start + size + n_contributors + n_languages + 
                        factor(creation_year) + n_commits + n_issues + factor(main_language), 
                      data=data.exactMatching, family = inverse.gaussian(link = "inverse"), weights = data.exactMatching$weights)
anova(invgauss_model_h0, invgauss_model, test = "F") # P-value: 0.4786

# Impact of the interaction
invgauss_model_intr <- glm(formula = velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                           factor(creation_year) + n_commits + n_issues + factor(main_language), 
                         data=data.exactMatching, family = inverse.gaussian(link = "inverse"), weights = data.exactMatching$weights)
anova(invgauss_model, invgauss_model_intr, test = "F") # 0.9535
summary(gamma_model_intr)

###########################################
# 2.2 WITH TRANSFORMATION 

data.exactMatching_log <- data.exactMatching
data.exactMatching_log$velocity_mean_end <- log(data.exactMatching_log$velocity_mean_end)
data.exactMatching_cube <- data.exactMatching
data.exactMatching_cube$velocity_mean_end <- (data.exactMatching_cube$velocity_mean_end)^(1/3)

normality_plot(data.exactMatching_log$velocity_mean_end, data.exactMatching_cube$velocity_mean_end, "LOG", 
               "CUBE", "cube transformation")

shapiro.test(data.exactMatching_log$velocity_mean_end)
shapiro.test(data.exactMatching_cube$velocity_mean_end) # Almost normally distributed

###########################################
# 2.2.1 ANCOVA (We use the cube)


model_aov <- aov(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                   factor(creation_year) + n_commits + n_issues + factor(main_language), 
                 data = data.exactMatching_cube, weights = data.exactMatching_cube$weights)
summary(model_aov)

# 1. Linearity assumption (Residuals vs Fitted values)
# The blue line should go approximately horizontal and next to 0. If there is
# Some pronounced pattern then means  there is no perfect linearity
autoplot(model_aov)
# After fitting the ancova model, there's no clear pattern within the residuals appart from some outliers, which confirms the
# linear relationship among the dependent and the independent variables.
corrplot(cor(numeric_df[header_names]))

# 2. Homogeneity of Variance (Scale-location)
leveneTest(velocity_mean_end ~ factor(MS.NonMS), data = data.exactMatching_cube)
# p-value=0.4523 (We don't have enough evidence to reject the null hypothesis, therefore 
# the difference among the variances of the two groups is not statistically significant.

# 3. Normality of residuals (Normal Q-Q plot)
# The residuals should follow the diagonal line.
shapiro.test(model_aov$residuals) 
qqnorm(model_aov$residuals)
qqline(model_aov$residuals)

# 4. Homogeneity of regression slopes
anova.test <- anova_test(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start +
                           size + n_languages + factor(creation_year) + n_commits + n_issues +
                           n_contributors, data = data.exactMatching_cube, weights=data.exactMatching_cube$weights)
anova.test


###########################################
# 2.2.2 Linear Regression (We use the cube)
model_lm <- lm(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                 factor(creation_year) + n_commits + n_issues + factor(main_language), 
               data = data.exactMatching_cube, weights = data.exactMatching_cube$weights)
summary(model_lm)

model_lm_0 <- lm(velocity_mean_end ~ velocity_mean_start + size*n_contributors + n_languages + 
                   factor(creation_year) + n_commits + n_issues + factor(main_language), 
                 data = data.exactMatching_cube, weights = data.exactMatching_cube$weights)

# We test the impact of the 
anova(model_lm_0,model_lm) # 0.8763 We cannot reject the null hypothesis, therefore the implication of the variance in the
# addition of the independent variable is not statistically signigicant to say that it improvers the model.

autoplot(model_lm)


######################################################################################
######################################################################################
######################################################################################

###########################################
# 3.WITH MATCHING (full optimal matching - logistic regression)

###########################################
# 3.1 WITHOUT TRANSFORMATION

###########################################
# 3.1.1 GLM (Gamma)
gamma_model <- glm(formula = velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size + n_languages + 
                     factor(creation_year) + n_commits + n_issues + n_contributors + factor(main_language), 
                   data=data.fullMatching, family = Gamma(link = "log"), weights = data.fullMatching$weights) 

# Model optimization through "AIC" criteria
X <- data.matrix(data.fullMatching[model_covariates])
y <- data.fullMatching$velocity_mean_end
xy_matrix <- as.data.frame(cbind(X,y))
bestAIC <- bestglm(xy_matrix, IC="AIC", weights = data.fullMatching$weights, family = Gamma)

# The process directly removes all the possible covariates leaving only "veloity_mean_start"
# ASK IF WE ARE INTERESTED ON THIS, BUT THE MODEL OPTIMIZATION WILL PROVIDE THIS RESULT FOR ALL
# THE MODEL TYPES WE HAVE CONSIDERED

vif_values <- vif(gamma_model)
vif_values # No multicollinearity so far.

numeric_df <- data.fullMatching
numeric_df$MS.NonMS <- as.numeric(numeric_df$MS.NonMS)
numeric_df$main_language <- as.numeric(numeric_df$main_language)
numeric_df$creation_year <- as.numeric(numeric_df$creation_year)

cor_matrix <- cor(numeric_df[model_covariates])
corrplot(cor_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

# Assessment of the model results
shapiro.test(gamma_model$residuals) # p-value < 2.2e-16 (Not normally distributed, not reliable model)
summary(gamma_model)
mean(gamma_model$residuals^2)
plot(gamma_model$fitted.values, gamma_model$residuals)

# Set to comparison the impact of independent variable.
gamma_model_h1 <- glm(formula = velocity_mean_end ~ velocity_mean_start + size + n_languages + 
                        creation_year + n_commits + n_issues + n_contributors + main_language, data=df, family = Gamma(link = "log")) 
anova(gamma_model, gamma_model_h1, test="F") # p-value = 0.9903 (No statistical difference)

# Impact of the interaction
gamma_model_intr <- glm(formula = velocity_mean_end ~ MS.NonMS*velocity_mean_start + size*n_contributors + n_languages + 
                          creation_year + n_commits + n_issues + main_language, data=df, family = Gamma(link = "log")) 
summary(gamma_model_intr)


###########################################
# 3.1.2 GLM (Inverse Gaussian)
invgauss_model <- glm(formula = velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size + n_languages + 
                        factor(creation_year) + n_commits + n_issues + n_contributors + factor(main_language), 
                      data=data.fullMatching, family = inverse.gaussian(link = "log"),
                      weights = data.fullMatching$weights) 

# Algorithm doesn't converge (SAME ISSUE AS WITH GAMMA)



###########################################
# 3.2 WITH TRANSFORMATION 

df_log <- data.fullMatching
df_log$velocity_mean_end <- log(df_log$velocity_mean_end)
df_cube <- data.fullMatching
df_cube$velocity_mean_end <- (df_cube$velocity_mean_end)^(1/3)

normality_plot(df_log$velocity_mean_end, df_cube$velocity_mean_end, "LOG", 
               "CUBE", "cube transformation")

###########################################
# 3.2.1 ANCOVA (We use the cube)


model_aov <- aov(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                   factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube,
                 weights=df_cube$weights)
summary(model_aov)

# 1. Linearity assumption (Residuals vs Fitted values)
# The blue line should go approximately horizontal and next to 0. If there is
# Some pronounced pattern then means  there is no perfect linearity
autoplot(model_aov)
# After fitting the ancova model, there's no clear pattern within the residuals appart from some outliers, which confirms the
# linear relationship among the dependent and the independent variables.

df_numeric <- df_cube
df_numeric$MS.NonMS <- as.numeric(df_numeric$MS.NonMS)
df_numeric$main_language <- as.numeric(df_numeric$main_language)
df_numeric$creation_year <- as.numeric(df_numeric$creation_year)

corrplot(cor(df_numeric[covariate_names]))

# 2. Homogeneity of Variance (Scale-location)
leveneTest(velocity_mean_end ~ as.factor(MS.NonMS), data = df_numeric)
# p-value=0.8354 (We don't have enough evidence to reject the null hypothesis, therefore 
# the difference among the variances of the two groups is not statistically significant.

# 3. Normality of residuals (Normal Q-Q plot)
# The residuals should follow the diagonal line.
shapiro.test(model_aov$residuals) # p-value = 0.0001362 
# There is statistically significant evidence to say that the residuals are not normally distributed.

# 3. Homogeneity of regression slopes
anova.test <- anova_test(velocity_mean_end ~ factor(MS.NonMS)*velocity_mean_start +
                           factor(MS.NonMS)*size + factor(MS.NonMS)*n_languages +
                           factor(MS.NonMS)*factor(creation_year) + factor(MS.NonMS)*n_commits +
                           factor(MS.NonMS)*n_issues + factor(MS.NonMS)*n_contributors + 
                           factor(MS.NonMS)*factor(main_language), data = df_cube)
anova.test


###########################################
# 3.2.2 Linear Regression (We use the cube)
model_lm <- lm(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                 factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube)
summary(model_lm)

model_lm_0 <- lm(velocity_mean_end ~ velocity_mean_start + size*n_contributors + n_languages + 
                   factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube)

# We test the impact of the 
anova(model_lm_0,model_lm) # 0.7451 We cannot reject the null hypothesis, therefore the implication of the variance in the
# addition of the independent variable is not statistically signigicant to say that it improvers the model.

autoplot(model_lm)







