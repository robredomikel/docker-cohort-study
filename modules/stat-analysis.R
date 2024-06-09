###########################################
# STATISTICAL ANALYSISANALYSIS
###########################################

library(car)
library(corrplot)
library(RColorBrewer)
library(glmulti)
library(rstatix)

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
# 2.1.2 GLM (Inverse Gaussian)
invgauss_model <- glm(formula = velocity_mean_end ~ MS.NonMS + velocity_mean_start + size + n_languages + 
                        creation_year + n_commits + n_issues + n_contributors + main_language, data=df, family = inverse.gaussian(link = "inverse")) 

# With the complexity of the data... Doesn't work (Check it out later)


###########################################
# 2.2 WITH TRANSFORMATION 

df_log <- df
df_log$velocity_mean_end <- log(df_log$velocity_mean_end)
df_cube <- df
df_cube$velocity_mean_end <- (df_cube$velocity_mean_end)^(1/3)

normality_plot(df_log$velocity_mean_end, df_cube$velocity_mean_end, "LOG", 
               "CUBE", "cube transformation")

###########################################
# 2.2.1 ANCOVA (We use the cube)


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
# 2.2.2 Linear Regression (We use the cube)
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
# 2.WITH MATCHING (full optimal matching)

###########################################
# 2.1 WITHOUT TRANSFORMATION

###########################################
# 2.1.1 GLM (Gamma)
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
# 2.1.2 GLM (Inverse Gaussian)
invgauss_model <- glm(formula = velocity_mean_end ~ MS.NonMS + velocity_mean_start + size + n_languages + 
                        creation_year + n_commits + n_issues + n_contributors + main_language, data=df, family = inverse.gaussian(link = "inverse")) 

# With the complexity of the data... Doesn't work (Check it out later)


###########################################
# 2.2 WITH TRANSFORMATION 

df_log <- df
df_log$velocity_mean_end <- log(df_log$velocity_mean_end)
df_cube <- df
df_cube$velocity_mean_end <- (df_cube$velocity_mean_end)^(1/3)

normality_plot(df_log$velocity_mean_end, df_cube$velocity_mean_end, "LOG", 
               "CUBE", "cube transformation")

###########################################
# 2.2.1 ANCOVA (We use the cube)


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
# 2.2.2 Linear Regression (We use the cube)
model_lm <- lm(velocity_mean_end ~ factor(MS.NonMS) + velocity_mean_start + size*n_contributors + n_languages + 
                 factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube)
summary(model_lm)

model_lm_0 <- lm(velocity_mean_end ~ velocity_mean_start + size*n_contributors + n_languages + 
                   factor(creation_year) + n_commits + n_issues + factor(main_language), data = df_cube)

# We test the impact of the 
anova(model_lm_0,model_lm) # 0.7451 We cannot reject the null hypothesis, therefore the implication of the variance in the
# addition of the independent variable is not statistically signigicant to say that it improvers the model.

autoplot(model_lm)







