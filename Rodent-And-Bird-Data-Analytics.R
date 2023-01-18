library(ggplot2)
library(dplyr)
library(binom)
library(car)

## Rodent Weight Data - Chaetodipus baileyi ------------------------------------

rodentWeightData <- read.csv("rodent_weights.csv", stringsAsFactors = TRUE)
summary(rodentWeightData)

chaetodipus_only_data <- filter(rodentWeightData, 
                                     species_name == "Chaetodipus_baileyi")

# Question 1 & 2

chaetodipus_male_only_data <-filter(chaetodipus_only_data, sex == "M")

ggplot(chaetodipus_male_only_data, aes(x = weight)) + 
  geom_histogram() +
  xlab("Weight (male Chaetodipus baileyi)") +
  ylab("Frequency") +
  theme_minimal()

qqnorm(chaetodipus_male_only_data$weight)
qqline(chaetodipus_male_only_data$weight)

# Question 3

sd(chaetodipus_male_only_data$weight)^2
var(chaetodipus_male_only_data$weight)
## [1] 68.3791

# Question 4 - 8

ggplot(chaetodipus_only_data, aes(x = weight)) +
  geom_histogram() +
  facet_wrap(~ sex, ncol = 1)

chaetodipus_by_sex <- group_by(chaetodipus_only_data, sex)
summarise(chaetodipus_by_sex, 
          group_mean = mean(weight, na.rm = TRUE),
          group_sd = sd(weight, na.rm = TRUE), 
          group_var = var(weight, na.rm = TRUE))
##   sex   group_mean group_sd group_var
## <fct>         <dbl>    <dbl>     <dbl>
## 1 F           30.2     5.27      27.8
## 2 M           33.8     8.27      68.4

leveneTest(data = chaetodipus_only_data, weight ~ sex, center = mean)
## Levene's Test for Homogeneity of Variance (center = mean)
##         Df F value    Pr(>F)    
## group    1  329.26 < 2.2e-16 ***
##      2803 

## This data does not meet the assumption of equal variance for the two-sample
## t-test. Therefore, we will use Welch's t-test to test the difference in 
## male and female weight 

t.test(weight ~ sex, data = chaetodipus_only_data, var.equal = FALSE)
## Welch Two Sample t-test
## 
## data:  weight by sex
## t = -13.19, df = 1878, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group F and group M 
##                         is not equal to 0
## 95 percent confidence interval:
##   -4.141742 -3.069486
## sample estimates:
##   mean in group F mean in group M 
##          30.21088        33.81650 

t.test(weight ~ sex, data = chaetodipus_only_data, var.equal = TRUE)
## Two Sample t-test
## 
## data:  weight by sex
## t = -14.072, df = 2803, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group F 
## and group M is not equal to 0
## 95 percent confidence interval:
##   -4.108033 -3.103195
## sample estimates:
##   mean in group F mean in group M 
## 30.21088        33.81650 

length(chaetodipus_male_only_data$sex) -1
## [1] 1187
length(chaetodipus_only_data$sex)-length(chaetodipus_male_only_data$sex)-1
## [1] 1616
length(chaetodipus_only_data$sex)

# Bird Abundance Data ----------------------------------------------------------

abundancePlotData <- read.csv("abundance_by_plot.csv", stringsAsFactors = TRUE)

# Question 9
ggplot(abundancePlotData, aes(x = plot_type, y = abundance)) +
  geom_jitter(position = position_jitter(0.05)) +
  theme_minimal() +
  xlab("Plot Type") +
  ylab("Bird Abundance")

ggplot(abundancePlotData, aes(x = plot_type, y = abundance)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Plot Type") +
  ylab("Bird Abundance")

abundance_by_plot <- group_by(abundancePlotData, plot_id)


ggplot(abundance_by_plot, aes(x = plot_type, y = abundance)) +
  geom_jitter(position = position_jitter(0.05)) +
  theme_minimal() +
  xlab("Plot Type") +
  ylab("Bird Abundance")

summarise(abundance_by_plot, group_abundance = abundance)

# Question 10
## Null: the mean bird abundance is the same for each plot types (the variance
##        among groups is zero)
## Alternate: at least one of the plot types had a difference mean bird 
##            abundance 

# Question 11

ggplot(abundancePlotData, aes(x = log(abundance))) +
  geom_histogram() +
  facet_wrap(~ plot_type, ncol = 1) +
  xlab("Bird Abundance") +
  ylab("Frequency")

## UNTRANSFORMED
group_by_plot_type <- group_by(abundancePlotData, plot_type)
summarise(group_by_plot_type, groups_mean = mean(log(abundance), na.rm = TRUE),
          group_sd = sd(log(abundance), na.rm = TRUE), 
          group_var = var(log(abundance), na.rm = TRUE))
## A tibble: 3 × 4
## plot_type        groups_mean group_sd group_var
## <fct>                  <dbl>    <dbl>     <dbl>
## 1 Control                 3.67     5.65      32.0
## 2 Krat Exclosure          5.66     7.78      60.6
## 3 Rodent Exclosure        4.58     6.24      39.0

# data appears to not be normal and not equal variance -- violate 
## assumptions of ANOVA 

# TRANSFORMED
group_by_plot_type <- group_by(abundancePlotData, plot_type)
summarise(group_by_plot_type, groups_mean = mean(log(abundance), na.rm = TRUE),
          group_sd = sd(log(abundance), na.rm = TRUE), 
          group_var = var(log(abundance), na.rm = TRUE))
## A tibble: 3 × 4
## plot_type        groups_mean group_sd group_var
## <fct>                    <dbl>    <dbl>     <dbl>
## 1 Control                0.706    0.961     0.924
## 2 Krat Exclosure         1.06     1.13      1.28 
## 3 Rodent Exclosure       0.959    0.984     0.968

# UNTRANSFORMED
kruskal.test(abundance ~ plot_type, data = abundancePlotData)
## Kruskal-Wallis rank sum test
## 
## data:  abundance by plot_type
## Kruskal-Wallis chi-squared = 2.0403, df = 2, p-value = 0.3605

abundanceANOVA <- lm(abundance ~ plot_type, data = abundancePlotData)
anova(abundanceANOVA)
## Analysis of Variance Table
## 
## Response: abundance
##            Df Sum Sq Mean Sq F value Pr(>F)
## plot_type  2   64.5  32.262  0.7219 0.4885
## Residuals 93 4156.1  44.689 
## df_error = 93

## df_total = 95

## df_control = 29

## df_krat = 34

## df_rodent = 30

# TRANFORMED 
log_abundanceANOVA <- lm(log(abundance) ~ plot_type, data = abundancePlotData)
anova(log_abundanceANOVA)
## Analysis of Variance Table
## 
## Response: log(abundance)
##          Df Sum Sq Mean Sq F value Pr(>F)
## plot_type  2  2.078  1.0391  0.9736 0.3815
## Residuals 93 99.263  1.0674 

# Question 12

summary(abundancePlotData)


