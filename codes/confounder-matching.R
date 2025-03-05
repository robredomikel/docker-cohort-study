library(TOSTER)
library(MatchIt)
library(optmatch)
library(Matching) 
library(dplyr)

############################################
# MATCHED DATA

# OPTION WITH BASE R from rejected paper
# Necessary BOXPLOTs for confounders with nonSQ and SQ
# par(mfrow=c(2,3), mar=c(6,1,6,1), oma=c(0.5,1,0.5,1))
par(mfrow=c(2,3), mar=c(3,1,3,1), oma=c(1,1,1,1))
par(mfrow=c(2,3))
boxplot(n_commits ~ MS.NonMS, horizontal = T, data = df, main= '# commits', xlab = '',
        ylab = '', col = c('lavenderblush3', 'lightcyan4'), boxwex = 0.90, cex.main=2.5, cex.axis=2, outcex=1.5, lwd=1.5, yaxt= "n")
boxplot(n_contributors ~ MS.NonMS, horizontal = T, data = df, main= '# developers',
        xlab = '', ylab = '', col = c('lavenderblush3', 'lightcyan4'), boxwex = 0.90, cex.main=2.5, cex.axis=2, cex.lab = 2, outcex=1.5, lwd=1.5, yaxt= "n")
boxplot(n_issues ~ MS.NonMS, horizontal = T, data = df, main= '# issues',
        xlab = '', ylab = '', col = c('lavenderblush3', 'lightcyan4'), boxwex = 0.90, cex.main=2.5, cex.axis=2, cex.lab = 2, outcex=1.5, lwd=1.5,yaxt= "n")
boxplot(n_languages ~ MS.NonMS, horizontal = T, data = df, main= '# programming languages',
        xlab = '', ylab = '', col = c('lavenderblush3', 'lightcyan4'), boxwex = 0.90, cex.main=2.5, cex.axis=2, cex.lab = 2, outcex=1.5, lwd=1.5,yaxt= "n")
boxplot(size ~ MS.NonMS, horizontal = T, data = df, main= 'Project size',
        xlab = '', ylab = '', col = c('lavenderblush3', 'lightcyan4'), boxwex = 0.90, cex.main=2.5, cex.axis=2, cex.lab = 2, outcex=1.5, lwd=1.5, yaxt= "n")
boxplot(velocity_mean_start ~ MS.NonMS, horizontal = T, data = df, main= "Start velocity",
        xlab = '', ylab = '', col = c('lavenderblush3', 'lightcyan4'), boxwex = 0.90, cex.main=2.5, cex.axis=2, cex.lab = 2, outcex=1.5, lwd=1.5, yaxt= "n")

# OPTION WITH GGPLOT2
# Observational boxplots of the groups.
data_long <- pivot_longer(df_renamed, cols = c(`Start Velocity`, `Size of the project`, `# Programming languages`, 
                                       `# Commits`, `# Issues`, `# Developers`), 
                          names_to = "Covariate", values_to = "Value", names_repair = name_reshape(names))

bp1 <- ggplot(data_long, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  coord_flip() + # Make the boxplots horizontal
  theme(axis.text.x = element_blank(), # Hide x-axis text labels
        axis.ticks.x = element_blank() # Optionally hide x-axis ticks as well
  ) + # Rotate x-axis labels for clarity. # axis.text.x = element_text(angle = 45, hjust = 1)
  labs(title = "Comparison of Covariates between Treatment and Control Groups",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate
bp1 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# NOTE: The box plots provide a look of match between both cases and controls, therefore
# we should look now how main_language as categorical value looks like and see if matching is needed.

# Paired BARPLOT for the main_language confounder in case we get matching
# Count the occurrences
df_language <- df %>%
  group_by(main_language, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp1 <- ggplot(df_language, aes(x = main_language, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Language",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp1 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# Same based on creation year
library(forcats)
df_creation <- df
df_creation <- df_creation %>%
  mutate(MS.NonMS = fct_recode(MS.NonMS, "Non-MS" =  "~MS"))
df_creation <- df_creation %>%
  group_by(creation_year, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')


barp2 <- ggplot(df_creation, aes(x = creation_year, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Creation Year",
       x = "Creation Year",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp2 + scale_fill_manual(values = c("dodgerblue4", "gray63")) + font("xlab", size = 16, face = "bold") +
  font("ylab", size = 16, face = "bold") + font("title", size = 18, face = "bold") +
  font("xy.text", size = 12) + font("legend.text", size = 12) + font("legend.title", size = 12)

# Same based on number of languages
df_numlang <- df %>%
  group_by(n_languages, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

# N_languages
histo_plot <- ggplot(df_numlang, aes(x = n_languages, y= Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.9) +  # Automatically counts; no 'y' aesthetic needed
  labs(title = "Distribution of Categories by # of languages",
       x = "# of languages",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = c("dodgerblue4", "gray63"))
histo_plot


# NOTE: Based on the displayed observational plots, the matching will be based on 
# the main language variable as it's the one that shows the most difference between the study groups.

################# PROPENSITY SCORE MATCHING

##### 1:1 tolerance
m.propensity_tol_1 <- matchit(as.numeric(as.factor(df$MS.NonMS)) - 1 ~ as.numeric(as.factor(main_language)), data = df, method = "nearest")
summary(m.propensity_tol_1) # 1:1 tolerance
data_m.propensity_1 <- match.data(m.propensity_tol_1)
data_long_m_propensity_1 <- pivot_longer(data_m.propensity_1, 
                                         cols = c(velocity_mean_start, size, n_commits:
                                                    n_contributors), names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_m_propensity_1$Covariate), function(cov) {
  qq_plot(data_long_m_propensity_1, cov, "MS.NonMS")
}) #. + scale_fill_manual(values = c("dodgerblue4", "gray63"))

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
bp2 <- ggplot(data_long_m_propensity_1, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (1:1 tolerance - Propensity Score Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate
bp2 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

###### 3:1 tolerance
m.propensity_tol_3 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "nearest", ratio = 3)
summary(m.propensity_tol_3) # 1:3 tolerance (3 controls per treatment)
data_m.propensity_3 <- match.data(m.propensity_tol_3)
data_long_m_propensity_3 <- pivot_longer(data_m.propensity_3, cols = c(velocity_mean_start, size, n_commits:n_contributors), names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_m_propensity_3$Covariate), function(cov) {
  qq_plot(data_long_m_propensity_3, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
bp3 <- ggplot(data_long_m_propensity_3, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (3:1 tolerance - Propensity Score Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate
bp3 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

####### 4:1 tolerance
m.propensity_tol_4 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "nearest", ratio = 4)
summary(m.propensity_tol_4) # 1:4 tolerance (4 controls per treatment)
data_m.propensity_4 <- match.data(m.propensity_tol_4)
data_long_m_propensity_4 <- pivot_longer(data_m.propensity_4, cols = c(velocity_mean_start, size:n_contributors), names_to = "Covariate", values_to = "Value")

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


######################################################################################
######################################################################################
######################################################################################


################# ################# ################# ################# 
#################  Exact Matching (Does not allow for ratio)

m.exact_1 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "exact")
summary(m.exact_1)
# Sample Sizes:
#.              Control Treated
# All            207.        58
# Matched (ESS)  139.44      57
# Matched        198.        57
# Unmatched        9.         1
# Discarded        0.         0
data_m.exact_1 <- as.data.frame(match.data(m.exact_1))
data_long_exact_1 <- pivot_longer(data_m.exact_1, cols = c(velocity_mean_start, size, n_commits:n_contributors), names_to = "Covariate", values_to = "Value")

# Q-Q plot
plots <- lapply(unique(data_long_exact_1$Covariate), function(cov) {
  qq_plot(data_long_exact_1, cov, "MS.NonMS")
})

wrap_plots(plots, ncol = 2) # Q-Q plots for matching through different variables (We consider all variables, do not pivot only with one)

# Boxplots
bp_exact <- ggplot(data_long_exact_1, aes(x = Covariate, y = Value, fill = MS.NonMS)) +
  geom_boxplot() + # Draw boxplots
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Comparison of Covariates between Treatment and Control Groups (Exact Matching)",
       x = "Covariate",
       y = "Value") +
  facet_wrap(~ Covariate, scales = "free") # Create a separate plot for each covariate
bp_exact + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# 1. Get row names of the matched data
matched_rows <- rownames(data_m.exact_1)

# 2. Get row names of the original data
original_rows <- rownames(df)

# 3. Find rows that were discarded (those in original not in matched)
discarded_rows <- setdiff(original_rows, matched_rows)

# 4. Extract discarded rows from the original dataframe
discarded_data <- raw_df[discarded_rows, ]
non_discarded_data <- raw_df[matched_rows, ]
matched_data_df <- non_discarded_data
# Check the first few rows of the discarded data
head(discarded_data)

# Plotting the main language and creation year

# Same based on creation year
df_creation <- non_discarded_data %>%
  group_by(creation_year, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp3 <- ggplot(df_creation, aes(x = creation_year, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Creation Year",
       x = "Creation year",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp3 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

df_mainlang <- non_discarded_data %>%
  group_by(main_language, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp4 <- ggplot(df_mainlang, aes(x = main_language, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Main Language",
       x = "Main Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp4 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

df_numlang <- non_discarded_data %>%
  group_by(n_languages, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

# N_languages
histo_plot <- ggplot(df_numlang, aes(x = n_languages, y= Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.9) +  # Automatically counts; no 'y' aesthetic needed
  labs(title = "Distribution of Categories by # of languages",
       x = "# of languages",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = c("dodgerblue4", "gray63"))
histo_plot

# NOTE: Perhaps match additionally based on # of languages?

######################################################################################
######################################################################################
######################################################################################

######### Matching Quality assessment (EXACT MATCHING) #########

#### Numerically

# STANDARDIZED MEAN DIFFERENCES & KS TWO SAMPLE TEST

confounders <- c("velocity_mean_start", "main_language", "size", "n_languages", "creation_year", "n_commits", "n_issues", "n_contributors")
smd_results <- list()
ks_results <- list()

for (i in seq_along(confounders)){
  
  formula <- as.formula(paste(confounders[i], "~ MS.NonMS"))
  result <- smd_calc(formula = formula, data=data.exactMatching, paired = F,
                     smd_ci = c("nct"), bias_correction = F)
  # The data comes from the matching process performed in the weighting-matching stage in the R notebook
  test_result <- ks.test(formula=formula, data=data.exactMatching)
  
  smd_results[[confounders[i]]] <- result
  ks_results[[confounders[i]]] <- test_result
  
}

# NOTES:

# ON SMD: The impact in terms of the Cohen's D estimate shows that in most of the cases (except # of issues & size)
#         the mean number of the confounder has a standard deviation lower for MS than the mean value for ~MS 

#.            Variable    Estimate        SE   Lower_CI   Upper_CI Conf_Level
#1 velocity_mean_start -0.26261520 0.1721281 -0.5965534 0.07265977       0.95
#2                size  0.06595153 0.1426121 -0.2117071 0.34345630       0.95
#3         n_languages -0.18073330 0.1661096 -0.5032995 0.14261510       0.95
#4       creation_year -0.19870380 0.1561734 -0.5021545 0.10538490       0.95
#5           n_commits -0.07545033 0.1398714 -0.3476618 0.19692860       0.95
#6            n_issues  0.02181209 0.1553502 -0.2803865 0.32394110       0.95
#7      n_contributors -0.17831610 0.1653184 -0.4993677 0.14348910       0.95

# ON KS-TEST: None of the tests provided a significant p-value therefore we fail to reject the null hypothesis,
#             this in turn means that there is no significant difference between the distribution of the two samples.

#.            Variable        D p_value
#1 velocity_mean_start 0.101540  0.7515
#2                size 0.138760  0.3616
#3         n_languages 0.065125  0.9919
#4       creation_year 0.098086  0.7881
#5           n_commits 0.145930  0.3026
#6            n_issues 0.138760  0.3616
#7      n_contributors 0.119090  0.5568

#### Graphically

header_names <- c("MS.NonMS", "velocity_mean_start", "size", "n_languages", "n_commits", "n_issues", "n_contributors")
proper_names <- c("MS.NonMS", "Velocity Start", "Project size", "# Languages", "# Commits", "# Issues", "# Developers")
cont_subset_df <- subset(data.exactMatching, select = header_names)
cont_subset_df$MS.NonMS <- as.factor(ifelse(cont_subset_df$MS.NonMS=="MS",1,0))

# Kernel density distribution
par(mfrow=c(2, 3)) 
# header_names[2:length(header_names)]
for (i in seq(from=2, to=length(header_names))){
  
  # NOTE THAT THE THE TREATMENT VARIABLE HAS BEEN MODIFIED TO NUMERICAL IN weighted-mathching script
  cases <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 1] # MS
  controls <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 0] # ~MS
  cases_density <- density(cases)
  controls_density <- density(controls)
  max_density <- max(c(cases_density$y, controls_density$y))
  
  hist(controls, freq = F, main = paste('Histogram of', proper_names[i]),
       ylab="", col = rgb(0.062745, 0.305882, 0.545098, 0.5), 
       xlab = proper_names[i], probability = T,  xlim=range(cont_subset_df[i]),
       ylim = c(0, max_density * 1.1), cex.main=2, cex.axis=1.5, cex.lab=1.75)
  lines(density(controls), col = "dodgerblue4", lwd=2)
  
  hist(cases, freq = F, col = rgb(1.000000, 0.549020, 0.000000, 0.5), probability = T, add=T)
  lines(density(cases), col = "darkorange", lwd=2)
  
  # legend("topright", legend = c("NonMS", "MS"), fill = c(rgb(0.062745, 0.305882, 0.545098, 0.5), rgb(1.000000, 0.549020, 0.000000, 0.5)))
}

par(mfrow=c(1, 1))

# Cumulative distribution function
par(mfrow=c(2, 3)) 
# header_names[2:length(header_names)]
for (i in seq(from=2, to=length(header_names))){
  
  cases <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 1] # MS
  controls <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 0] #~MS 
  cases_cdf <- ecdf(cases)
  controls_cdf <- ecdf(controls)
  xlim <- range(cont_subset_df[, header_names[i]])
  
  plot(controls_cdf, main = paste('CDF of', proper_names[i]), xlab = proper_names[i], ylab = '', 
       col = rgb(1.000000, 0.549020, 0.000000, 1), xlim = xlim, verticals = TRUE, do.points = FALSE, lwd=5,
       cex.main=2, cex.axis=1.5, cex.lab=1.75)
  lines(cases_cdf, col = rgb(0.062745, 0.305882, 0.545098, 1), verticals = TRUE, do.points = FALSE, lwd=5)
  # Add legend
  legend("bottomright", legend = c("~MS", "MS"), col = c(rgb(0.062745, 0.305882, 0.545098, 0.5), rgb(1.000000, 0.549020, 0.000000, 0.5)), lty = 1)
}

par(mfrow=c(1, 1))

# Paired BARPLOT for the main_language confounder in case we get matching
# Count the occurrences
df_language <- data.exactMatching %>%
  group_by(main_language, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp1 <- ggplot(df_language, aes(x = main_language, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Language",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp1 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# Same based on creation year
df_creation <- data.exactMatching %>%
  group_by(creation_year, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp2 <- ggplot(df_creation, aes(x = creation_year, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Creation Year",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp2 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# Same based on number of languages
df_numlang <- data.exactMatching %>%
  group_by(n_languages, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

# N_languages
histo_plot <- ggplot(df_numlang, aes(x = n_languages, y= Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Automatically counts; no 'y' aesthetic needed
  labs(title = "Distribution of Categories by # of languages",
       x = "# of languages",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = c("dodgerblue4", "gray63"))
histo_plot


######################################################################################
######################################################################################
######################################################################################

######### Matching Quality assessment (full optimal matching) #########

#### Numerically

# STANDARDIZED MEAN DIFFERENCES & KS TWO SAMPLE TEST

confounders <- c("velocity_mean_start", "main_language", "size", "n_languages", "creation_year", "n_commits", "n_issues", "n_contributors")
smd_results <- list()
ks_results <- list()

for (i in seq_along(confounders)){
  
  formula <- as.formula(paste(confounders[i], "~ MS.NonMS"))
  result <- smd_calc(formula = formula, data=data.fullMatching , paired = F,
                     smd_ci = c("nct"), bias_correction = F)
  # The data comes from the matching process performed in the weighting-matching stage in the R notebook
  test_result <- ks.test(formula=formula, data=data.fullMatching )
  
  smd_results[[confounders[i]]] <- result
  ks_results[[confounders[i]]] <- test_result
  
}

# NOTES:

# SMD (Cohen's D)
#                          estimate        SE   lower.ci   upper.ci conf.level
# velocity_mean_start   -0.2512653 0.1698625 -0.5808954 0.07960394       0.95
# main_language.         0.2449892 0.150642 -0.0484602 0.5377505       0.95
# size                   0.07136731 0.1412351 -0.2036376 0.3462091       0.95 
# n_languages.           -0.1549179 0.1619849 -0.4696147 0.1603927       0.95
# creation_year.         -0.2289281 0.1546348 -0.5293833 0.07224687       0.95
# n_commits              -0.07089686 0.1382338 -0.3399512 0.1983108       0.95
# n_issues                0.02508334 0.1533155 -0.2732012 0.3232903       0.95
# n_contrbutors           -0.1721107 0.1637694 -0.4902063 0.1467031       0.95

# ON KS-TEST: None of the tests provided a significant p-value therefore we fail to reject the null hypothesis,
#             this in turn means that there is no significant difference between the distribution of the two samples.

#.            Variable        
# velocity_mean_start D = 0.10312, p-value = 0.7211
# main_language.      D = 0.13068, p-value = 0.4215
# size                D = 0.14193, p-value = 0.321
# n_languages.        D = 0.056305, p-value = 0.9988
# creation_year.      D = 0.1067, p-value = 0.6808
# n_commits           D = 0.13568, p-value = 0.3747
# n_issues            D = 0.1341, p-value = 0.3891
# n_contrbutors       D = 0.11553, p-value = 0.581


#### Graphically

header_names <- c("MS.NonMS", "velocity_mean_start", "size", "n_languages", "n_commits", "n_issues", "n_contributors")
cont_subset_df <- subset(data.fullMatching, select = header_names)

# Kernel density distribution
par(mfrow=c(2, 3)) 
# header_names[2:length(header_names)]
for (i in seq(from=2, to=length(header_names))){
  
  
  cases <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 2] # MS
  controls <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 1] # ~MS
  cases_density <- density(cases)
  controls_density <- density(controls)
  max_density <- max(c(cases_density$y, controls_density$y))
  
  hist(controls, freq = F, main = paste('Histogram of', header_names[i]),
       ylab="Kernel density", col = rgb(0.062745, 0.305882, 0.545098, 0.5), 
       xlab = header_names[i], probability = T,  xlim=range(cont_subset_df[i]),
       ylim = c(0, max_density * 1.1))
  lines(density(controls), col = "dodgerblue4", lwd=2)
  
  hist(cases, freq = F, col = rgb(1.000000, 0.549020, 0.000000, 0.5), probability = T, add=T)
  lines(density(cases), col = "darkorange", lwd=2)
  
  legend("topright", legend = c("~MS", "MS"), fill = c(rgb(0.062745, 0.305882, 0.545098, 0.5), rgb(1.000000, 0.549020, 0.000000, 0.5)))
}

par(mfrow=c(1, 1))

# Cumulative distribution function
par(mfrow=c(2, 3)) 
# header_names[2:length(header_names)]
for (i in seq(from=2, to=length(header_names))){
  
  cases <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 2] # MS
  controls <- cont_subset_df[,header_names[i]][cont_subset_df[,header_names[1]] == 1] #~MS 
  cases_cdf <- ecdf(cases)
  controls_cdf <- ecdf(controls)
  xlim <- range(cont_subset_df[, header_names[i]])
  
  plot(controls_cdf, main = paste('CDF of', header_names[i]), xlab = header_names[i], ylab = 'Cumulative Probability', 
       col = rgb(1.000000, 0.549020, 0.000000, 0.5), xlim = xlim, verticals = TRUE, do.points = FALSE, lwd=2)
  lines(cases_cdf, col = rgb(0.062745, 0.305882, 0.545098, 0.5), verticals = TRUE, do.points = FALSE, lwd=2)
  # Add legend
  legend("bottomright", legend = c("~MS", "MS"), col = c(rgb(0.062745, 0.305882, 0.545098, 0.5), rgb(1.000000, 0.549020, 0.000000, 0.5)), lty = 1)
}

par(mfrow=c(1, 1))

# Categorical variables
# Paired BARPLOT for the main_language confounder in case we get matching
# Count the occurrences
df_language <- data.fullMatching %>%
  group_by(main_language, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp1 <- ggplot(df_language, aes(x = main_language, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Language",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp1 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# Same based on creation year
df_creation <- data.fullMatching %>%
  group_by(creation_year, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

barp2 <- ggplot(df_creation, aes(x = creation_year, y = Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Distribution of Categories by Creation Year",
       x = "Programming Language",
       y = "Count",
       fill = "Category") +
  theme_minimal()
barp2 + scale_fill_manual(values = c("dodgerblue4", "gray63"))

# Same based on number of languages
df_numlang <- data.fullMatching %>%
  group_by(n_languages, MS.NonMS) %>%
  summarise(Count = n(), .groups = 'drop')

# N_languages
histo_plot <- ggplot(df_numlang, aes(x = n_languages, y= Count, fill = as.factor(MS.NonMS))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Automatically counts; no 'y' aesthetic needed
  labs(title = "Distribution of Categories by # of languages",
       x = "# of languages",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = c("dodgerblue4", "gray63"))
histo_plot

#################################################################### 
#################################################################### 
#################################################################### 


################# Optimal Matching

m.optimal_1 <- matchit(as.factor(df$MS.NonMS) ~ as.factor(main_language), data = df, method = "optimal", ratio = 1)
summary(m.optimal_1) # 1:1 tolerance
data_m.optimal_1 <- match.data(m.optimal_1)
data_long_optimal_1 <- pivot_longer(data_m.optimal_1, cols = c(velocity_mean_start, size:n_contributors), names_to = "Covariate", values_to = "Value")

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


# 1. Get row names of the matched data
matched_rows_optimal <- rownames(data_m.optimal_1)

# 2. Get row names of the original data
original_rows_optimal <- rownames(df)

# 3. Find rows that were discarded (those in original not in matched)
discarded_rows_optimal <- setdiff(original_rows_optimal, matched_rows_optimal)

# 4. Extract discarded rows from the original dataframe
discarded_data_optimal <- raw_df[discarded_rows_optimal, ]
non_discarded_data_optimal <- raw_df[matched_rows_optimal, ]

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


