library(ggplot2)
library(lsr)
library(pwr)

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
