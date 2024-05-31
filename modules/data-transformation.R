library(forecast)

###########################################
# DATA TRANSFORMATION
###########################################

###########################################
# INITIAL DISTRIBUTION

raw_df <- read.csv('/Users/mrobredo23/OULU/docker_cohort-24/data/final_data_file.csv')
raw_df <- subset(raw_df, select = -c(trimmed_languages))
df <- raw_df[c(2:ncol(raw_df))] # For the modelling purposes
cases_df <- df[df$MS.NonMS =='MS',] 
cases_endvelocity <- abs(cases_df$velocity_mean_end)
controls_df <- df[df$MS.NonMS =='~MS',]
controls_endvelocity <- abs(controls_df$velocity_mean_end)

normality_plot <- function(group1, group2, name1, name2, trans_technique) {
  
  par(mfrow=c(2,2))
  hist(group1, main=paste(name1), freq=F)
  lines(density(group1), col="red")
  hist(group2, main=paste(name2), freq=F)
  lines(density(group2), col="red")
  
  group2[is.infinite(group2)] <- NA
  group2 <- na.omit(group2)
  
  qqnorm(group1, main=paste(name1), col='blue')
  qqline(group1)
  qqnorm(group2, main=paste(trans_technique), col='blue')
  qqline(group2)
  par(mfrow=c(1,1))
  }

###########################################
# LOG TRANSFORMATION
cases_endvelocity_log <- log(cases_endvelocity)
controls_endvelocity_log <- log(controls_endvelocity)


normality_plot(cases_endvelocity, cases_endvelocity_log, "Original cases", 
               "log cases", "log transformation")
normality_plot(controls_endvelocity, controls_endvelocity_log, "Original controls", 
               "log controls", "log transformation")

# Normality test 
cases_endvelocity_log[is.infinite(cases_endvelocity_log)] <- NA
cases_endvelocity_log <- na.omit(cases_endvelocity_log)
controls_endvelocity_log[is.infinite(controls_endvelocity_log)] <- NA
controls_endvelocity_log <- na.omit(controls_endvelocity_log)
shapiro.test(cases_endvelocity_log)
shapiro.test(controls_endvelocity_log)



###########################################
# SQUARE TRANSFORMATION
cases_endvelocity_sqrt <- sqrt(cases_endvelocity)
controls_endvelocity_sqrt <- sqrt(controls_endvelocity)


normality_plot(cases_endvelocity, cases_endvelocity_sqrt, "Original cases", 
               "SQRT cases", "sqrt transformation")
normality_plot(controls_endvelocity, controls_endvelocity_sqrt, "Original controls", 
               "SQRT controls", "sqrt transformation")

# Normality test 
shapiro.test(cases_endvelocity_sqrt)
shapiro.test(controls_endvelocity_sqrt)

###########################################
# CUBE ROOT TRANSFORMATION
cases_endvelocity_cube <- (cases_endvelocity)^(1/3)
controls_endvelocity_cube <- (controls_endvelocity)^(1/3)


normality_plot(cases_endvelocity, cases_endvelocity_cube, "Original cases", 
               "Root cube cases", "cube transformation")
normality_plot(controls_endvelocity, controls_endvelocity_cube, "Original controls", 
               "Root cube controls", "cube transformation")

# Normality test 
shapiro.test(cases_endvelocity_cube)
shapiro.test(controls_endvelocity_cube)


###########################################
# RECIPROCAL TRANSFORMATION (Strongest transformation)
cases_endvelocity_recip <- 1/cases_endvelocity
controls_endvelocity_recip <- 1/controls_endvelocity


normality_plot(cases_endvelocity, cases_endvelocity_recip, "Original cases", 
               "reciprocal cases", "reciprocal transformation")
normality_plot(controls_endvelocity, controls_endvelocity_recip, "Original controls", 
               "reciprocal controls", "reciprocal transformation")

# Normality test 
shapiro.test(cases_endvelocity_recip)
shapiro.test(controls_endvelocity_recip)

# Really bad results!

###########################################
# REVERSE SCORE TRANSFORMATION (We don't do it, we don't have negative skew)

###########################################
# try BOX-COX TRANSFORMATION
cases_endvelocity_boxcox <- BoxCox(cases_endvelocity, lambda = "auto")
controls_endvelocity_boxcox <- BoxCox(controls_endvelocity, lambda = "auto")


normality_plot(cases_endvelocity, cases_endvelocity_boxcox, "Original cases", 
               "boxcox cases", "boxcox transformation")
normality_plot(controls_endvelocity, controls_endvelocity_boxcox, "Original controls", 
               "boxcox controls", "boxcox transformation")

# Normality test 
shapiro.test(cases_endvelocity_boxcox)
shapiro.test(controls_endvelocity_boxcox)


###########################################
## The LOG, SRQT & CUBE transformations provide reasonable transformations but further needs to be checked.
# We will check if the CLT holds for the transformed populations.

###########################################
# LOG TRANSFORMATION CLT



###########################################
# SQUARE TRANSFORMATION

###########################################
# CUBE ROOT TRANSFORMATION

