library(ggplot2)
library(lsr)
library(pwr)
library(WebPower)
library(parameters)
library(effsize)
library(smd)

###########################################
# WITHOUT DATA TRANSFORMATION
###########################################

# We use a MonteCarlo simulation based t-test in order to run the power analysis
# based on non-normal data.
control_skewness <- skewness(controls_endvelocity, type = "2") # Calculation log normal distribution
control_kurtosis <- kurtosis(controls_endvelocity, type = "2")
cases_skewness <- skewness(cases_endvelocity, type = "2") # Calculation log normal distribution
cases_kurtosis <- kurtosis(cases_endvelocity, type = "2")

# Power
power_analysis <- wp.mc.t(n=c(length(cases_endvelocity), length(controls_endvelocity)),
                          mu0 = mean(cases_endvelocity), mu1 = mean(controls_endvelocity), 
                          sd = c(sd(cases_endvelocity), sd(controls_endvelocity)),
                          skewness = c(3.615, 5.333), kurtosis = c(17.748, 43.208),
                          alpha = 0.05, type = "two.sample", alternative = "two.sided")

# Effect size is calculated with SMD due to the non-existence of non-parametric effect
# size calculations. From the data transformation stage we know that the best possible transformation
# is the "cube transformation". Therefore, that's the one we are going to use.
SMD(cases_endvelocity_cube, controls_endvelocity_cube, bias.cor = T, var.equal = F,
    na.rm = F)
# bias-corrected SMD 
# 0.06829907

# Further we will consider the sample from the CLT to use cohen's D
cohens_d <- cohen.d(cases_sample_cube, controls_sample_cube)
#Cohen's d
#d estimate: 0.1751957 (negligible)
#95 percent confidence interval:
#     lower      upper 
#-0.3426361  0.6930275

###########################################
# DATA TRANSFORMATION
###########################################

# We initially consider those transformations of the dependent variable that demosntrated approximated normality.


## LOG TRANSFORMED DATA

# means and std of the samples:
cases_endvelocity_log_mean <- mean(cases_endvelocity_log)
cases_endvelocity_log_std <- sd(cases_endvelocity_log)

controls_endvelocity_log_mean <- mean(controls_endvelocity_log)
controls_endvelocity_log_std <- sd(controls_endvelocity_log)


vplot <- ggplot(df, aes(x=df$MS.NonMS, y=log(df$velocity_mean_end))) + 
  geom_violin(trim=FALSE) + stat_summary(fun=mean, geom="point", shape=23, size=2, color="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))

stat_summary(fun.y=mean, geom="point", shape=23, size=2)

# Distance effect by Cohen's D statistic
cohensD(cases_endvelocity_log, controls_endvelocity_log) # 0.07120195 Small effect size
# Means that the relationship between variables is quite small.

# Power-test
pwr.t2n.test(n1=58, n2=207, d= 0.07120195, sig.level = 0.1, alternative = 'two.sided')
# power = 0.1385217

# t-test
t.test(x = cases_endvelocity_log, y = controls_endvelocity_log, alternative = 'two.sided')
