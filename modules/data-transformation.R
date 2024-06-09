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
  hist(group1, main=paste(name1), breaks=40, ylim = c(0, max(density(group1)$y)), freq=F,
       cex.main=1.7, cex.lab=1.5, cex.axis=1.5, xlab="End velocity mean")
  lines(density(group1), col="red", lwd=2)
  hist(group2, main=paste(name2), breaks=40, ylim = c(0, max(density(group2)$y)), freq=F,
       cex.main=1.7, cex.lab=1.5, cex.axis=1.5, xlab="End velocity mean")
  lines(density(group2), col="red", lwd=2)
  
  group2[is.infinite(group2)] <- NA
  group2 <- na.omit(group2)
  
  qqnorm(group1, main=paste(name1, "QQ plot"), col='blue',
         cex.main=1.7, cex.lab=1.5, cex.axis=1.5)
  qqline(group1)
  qqnorm(group2, main=paste(trans_technique, "QQ plot"), col='blue',
         cex.main=1.7, cex.lab=1.5, cex.axis=1.5)
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

# NOTE: According to the CLT a population whose sample of n=30 is normally distributed, it will be approximately distributed.

###########################################
# LOG TRANSFORMATION CLT
cases_sample_log <- sample(cases_endvelocity_log, size = 30)
controls_sample_log <- sample(controls_endvelocity_log, size = 30)

normality_plot(cases_endvelocity_log, cases_sample_log, "Log pop cases", 
               "log sample cases", "boxcox transformation")
normality_plot(controls_endvelocity_log, controls_sample_log, "Log pop controls", 
               "log sample controls", "boxcox transformation")

# Normality test 
shapiro.test(cases_sample_log) # p-value = 0.1566
shapiro.test(controls_sample_log) # p-value = 0.9076


###########################################
# SQUARE TRANSFORMATION
cases_sample_sqrt <- sample(cases_endvelocity_sqrt, size = 30)
controls_sample_sqrt <- sample(controls_endvelocity_sqrt, size = 30)

normality_plot(cases_endvelocity_sqrt, cases_sample_sqrt, "sqrt pop cases", 
               "sqrt sample cases", "sqrt transformation")
normality_plot(controls_endvelocity_sqrt, controls_sample_sqrt, "Log pop controls", 
               "sqrt sample controls", "sqrt transformation")

# Normality test 
shapiro.test(cases_sample_sqrt) # p-value = 0.0007245
shapiro.test(controls_sample_sqrt) # p-value = 0.0007245

###########################################
# CUBE ROOT TRANSFORMATION

cases_sample_cube <- sample(cases_endvelocity_cube, size = 30)
controls_sample_cube <- sample(controls_endvelocity_cube, size = 30)

normality_plot(cases_endvelocity_cube, cases_sample_cube, "cube pop cases", 
               "cube sample cases", "cube transformation")
normality_plot(controls_endvelocity_cube, controls_sample_cube, "Log pop controls", 
               "cube sample controls", "cube transformation")


# Normality test 
shapiro.test(cases_sample_cube) # p-value = 0.3315
shapiro.test(controls_sample_cube) # p-value = 0.5641


#### CONCLUSION: The Log and Cube transformations present potential options for running the parametric tests.

