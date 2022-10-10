#############
# Data Analysis
#############

rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Set & check working directory
getwd()# "/Users/ciaraoflaherty/"
setwd("~/StatsI_Fall2022/problemSets/PS01")

# Packages
library(tidyverse)
# install.packages("ggplot2")
library(ggplot2)

#########
# QUESTION 1
#########

# Import data
iqdata <- read.csv("iq.csv")

# Initial exploration
summary(iqdata) # prints min, quartiles, median, and mean
str(iqdata) # each observation is in integers
# x_bar <- mean(iqdata$iq)

## Checking our assumptions ?
## 1- Determine if data is normally distributed
pdf("qqnorm_plot_iq.pdf")
qqnorm(iqdata$iq)
qqline(iqdata$iq,
       distribution = qnorm)
dev.off()
# the data falls along the line in the middle but comes off the line a bit 
# at the ends ("heavy tails") especially the higher values - our sample data 
# will probably have more extreme values than would be expected if it were
# truly normally distributed 
# source: https://data.library.virginia.edu/understanding-q-q-plots/
# appears as if the data may be 'uniformly' distributed

## assess the appearance of the sample plotted by density
pdf("density_plot_iq.pdf")
plot(density(iqdata$iq),
     main = "Pdf of iq",
     xlab = "IQ")
dev.off()
# looks somewhat normal, very sharp slopes on both sides, 1 clear peak and 1
# smaller one. both tails are quite narrow, therefore we wouldn't expect many
# extreme values for iq to appear, whether extremely high or extremely low.
# I would thus expect the vast majority of the values for iq to lie between
# around 85 and 115, from initial observation of the sample density plot.

##############
# Assessing ability to infer
##############

## find the standard deviation of the sample
sd_samp <- sd(iqdata$iq)

## using the central limit theorem to help us calculate confidence intervals 
# CLT states the the mean of the sampling distribution of the sample means is
# same as the population mean \mu, and that the standard error is equal to the
# population variance, or the population sd \sigma divided by the sqrt
# of the sample size

# therefore, thanks to the CLT we don't need to bootstrap - rather we can 
# infer the parameters. there is some doubt about the reliability of the 
# CLT method (https://www.scirp.org/journal/paperinformation.aspx?paperid=76758)
# but for our purposes here i.e. a fairly normal distribution, the error 
# should not be too great. Because the population is assumed normal, 
# even though the sample size is <30, the theorem should still hold true
# source: https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_probability/
# BS704_Probability12.html

## constructing Cofidence Intervals of 90%

# "The functions qnorm()and pnorm()convert from units of standard deviations 
# (= standard error in this case) to percentiles (= probabilities) 
# (and vice-versa) for a normal distribution." "qnorm(p,mean,sd)"
# https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/9_-_t-distribution_
# confidence_intervals__z_scores.pdf Page 2

CI_lower <- qnorm(0.05, 
                  mean = mean(iqdata$iq), 
                  sd = (sd(iqdata$iq)/sqrt(length(iqdata$iq))) # standard error
                  # or population variance equation = denominator
)

CI_upper <- qnorm(0.95,
                  mean = mean(iqdata$iq),
                  sd = (sd(iqdata$iq)/sqrt(length(iqdata$iq)))
)
# sd of the sample here is the best estimate of sigma (CLT)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))
# values returned are:
#   Lower    Upper
# 94.13283 102.7472
# so 90% of the sample means of our data would be expected to lie between the 
# above values of \bar_{x}
# i.e. between 94.13283 and 102.7472


## CIs using a t-distribution (n<30)
# manually
se <- sd(iqdata$iq)/sqrt(length(iqdata$iq)) 
    # making standard error vector
t_score <- qt(.05, df = length(iqdata$iq)-1, lower.tail = FALSE)
    # qt(x,df)  -inverse probability cumulative density
    # lower.tail = FALSE - assumes importance of the area to the right of the 
    # lower tail - one-sided
CI_lower_t <- mean(iqdata$iq) - (se * t_score)
CI_upper_t <- mean(iqdata$iq) + (se * t_score)
    # formula: http://statisticslectures.com/topics/ciindependentsamplest/

# call
  CI_lower_t # 93.95993
  CI_upper_t # 102.9201
  # slightly larger difference between lower and upper confidence interval 
  # markers
  
# Question 1 Part 1 Answer:
## Checking manual work with R's t.test function
t.test(iqdata$iq, conf.level = 0.9, 
       alternative = "two.sided"
       )
# returns: CI_Low -> 93.95993 CI_Upp -> 102.92007, p value -> 2.2e-16 
# i.e. \alpha < 0.05 ergo we reject H0 that the true population mean is 
# equal to 0.
# the 'more correct' confidence interval is the one using the t-distribution

### Part 2 
# - the null hypothesis for this test is that the differences of the
# two means (the sample mean and the national average) is less than or equal 
# to 0, i.e. is not greather than 0
# - H0: x bar - 100 =/<  0; HA: x bar - 100 > 0 OR
# H0: x bar =/< 100; HA: x bar > 100

# one sample t test 
iqdata %>%
  select(iq) %>%
  t.test(mu=100, alternative = "greater", var.equal = FALSE) 
  # H0: x bar </= 100, HA: x bar > 100
  # also cannot assume that the variance of the 2 samples are equal and 
  # cannot test - do not have the national sample
  # p value - 0.7215, which > 0.05 , therefore fail to reject H0 that mu =/< 100
  # code derived from https://www.youtube.com/watch?v=fO2X-8FXY6k 

# therefore we don't have evidence to conclude that the average IQ in the 
# school is greater than the national average IQ of 100

#########
# QUESTION 2
#########
## Researchers are curious about what affects the amount of money 
## communities spend on addressing homelessness.

# respondent variable/y -> amount of money spent on homelessness 

rm(list=ls())
### Load packages
# library(tidyverse)


### Import data
expen <- read.delim("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt")


### Explore data
summary(expen)
head(expen)
str(expen)

# respondent variable in column 'Y' - amount spent per capita
# input variables: X1- State income per capita, X2 - No. per 100,000 financially
# insecure, X3 - number of ppl per 1000 living in an urban area, Region 

### Restructure Data
# Name columns/input variables
names(expen)[1:6] <- c("State", "YSpent", "Income", "No.FinInsec", 
                       "No.UrbRes", "Region")
print(expen)

# Apply region names to factor levels
expen$Region <- as.factor(expen[[6]])
levels(expen$Region) <- c("Northeast", "North Central", "South",
                                 "West")
print(expen)

# Plot relationships

# Y & X1, X2, X3, individual plots
pdf("plots_PS01_part1.pdf")
    plot(expen$Income, expen$YSpent, 
         xlab = "Personal Income per capita", 
         ylab= "Housing Assistance Expenditure per capita")
    plot(expen$No.FinInsec, expen$YSpent, 
         xlab = "Number of Finanically Insecure residents per 100,000", 
         ylab= "Housing Assistance Expenditure per capita")
    plot(expen$No.UrbRes, expen$YSpent, 
         xlab = "Number of persons per 1000 resident in Urban Areas", 
         ylab= "Housing Assistance Expenditure per capita")
dev.off()

# expen %>%
 # ggplot(aes(x = Income,
  #           y = YSpent)) +
 # geom_point()+
 # geom_smooth()+
 # labs(x="Personal Income per capita",
  #     y="Housing Assistance Expenditure per capita",
  #     title="Housing Assistance Expenditure and Income")+ 
 # theme_minimal()
#ggsave("Expend_Incom1.jpg", plot = last_plot())

expen %>%
  ggplot(aes(x = Income,
             y = YSpent)) +
  geom_point(aes(colour=No.FinInsec, size=No.UrbRes))+
  geom_smooth()+
  labs(x="Personal Income per capita",
       y="Housing Assistance Expenditure per capita",
       title="Housing Assistance Expenditure and Income")+ 
  theme_minimal()
ggsave("Expend_Incom_Fin_Urb.pdf", plot = last_plot())


# Y & Region
# boxplot base R
pdf("Expen_Region_fix.pdf")
plot(expen$Region, expen$YSpent,
     xlab="Region",
     ylab="Spent")
dev.off()


# Y & X1
expen %>%
  ggplot(aes(x = Income,
             y = YSpent)) +
  geom_point()+
  geom_smooth()+
  labs(x="Personal Income per capita",
       y="Housing Assistance Expenditure per capita",
       title="Housing Assistance Expenditure and Income")+ 
  theme_minimal()
ggsave("Expend_Incom_solo.pdf", plot = last_plot())

# Y & X1 + Region
expen %>%
  ggplot(aes(x = Income,
             y = YSpent)) +
  geom_point(aes(colour=Region))+
  # geom_smooth()+
  labs(x="Personal Income per capita",
       y="Housing Assistance Expenditure per capita",
       title="Housing Assistance Expenditure and Income")+ 
  theme_minimal()
ggsave("Expend_Incom_Reg.pdf", plot = last_plot())








