### PS04

setwd("~/StatsI_Fall2022/problemsets/PS03/template")

## Question 1 
data1 <- read.csv("https://raw.githubusercontent.com/oflaheci/StatsI_Fall2022/main/datasets/incumbents_subset.csv")

summary(data1) # some binary variables

# part 1 - how the difference in campaign spending affects 
# incumbent's vote share

# testing assumptions
hist(data1$voteshare) # distribution relatively normal
plot(voteshare~difflog, data=data1) # very roughly linear

# run regression
spend_lm <- lm(voteshare~difflog, data = data1)
summary(spend_lm) # small p-value, low se, 1 unit increase in 
# difflog = 0.041666 increase in incumbent's voteshare

# test homoscedasticity
par(mfrow=c(2,2))
plot(spend_lm)# residual means mostly centred around zero and horizontal
# no extreme outliers. linear regression still valid. 

# part 2 - scatterplot with regression line
library(ggplot2)

ggplot(data1, aes(y=voteshare, x=difflog))+
  geom_point(size=2, shape=1)+
  geom_smooth(method=lm)

ggsave("sc_plot1.png",
       device = "png",
       dpi = 300)
dev.off()

  # error bars expand at the ends
  # run just as geom_smooth() gives an S-curve line

# part 3 save the residuals in a separate object 

resids <- residuals(spend_lm)

# part 4 write prediction equation - not code

# Y = 0.579031 + 0.041666X

## Question 2
# how the difference between incumbent and challenger’s spending and the 
# vote share of the presidential candidate of the incumbent’s party are 
# related. 

# testing assumptions
hist(data1$presvote) # distribution normal
plot(presvote~difflog, data=data1) # again very roughly linear

# run regression
spend_pres_lm <- lm(presvote~difflog, data = data1)
summary(spend_pres_lm) # small p-value, low se, 1 unit increase in 
# difflog = 0.023837 increase in incumbent party's presidential voteshare

# test homoscedasticity
par(mfrow=c(2,2))
plot(spend_pres_lm) # residual means mostly centred around zero and horizontal
# no extreme outliers. linear regression still valid. 

# part 2 - scatterplot with regression line

ggplot(data1, aes(y=presvote, x=difflog))+
  geom_point(size=2, shape=1)+
  geom_smooth(method=lm)

ggsave("sc_plot2.png",
       device = "png",
       dpi = 300)
dev.off()

# part 3 save the residuals in a separate object 

resids2 <- residuals(spend_pres_lm)

# part 4 - prediction equation

# Y = 0.507583 + 0.023837X 

## Question 3
# how the vote share of the presidential candidate of the incumbent’s party 
# is associated with the incumbent’s electoral success.

# part 1
# testing assumptions
hist(data1$voteshare) # distribution normal
plot(voteshare~presvote, data=data1) # some suggestion of a linear relation

# run regression
votesh_presv_lm <- lm(voteshare~presvote, data = data1)
summary(votesh_presv_lm) # small p-value, low se, 1 unit increase in 
# presvote = 0.388018 increase in incumbent's voteshare

# test homoscedasticity
par(mfrow=c(2,2))
plot(votesh_presv_lm)# residual means mostly centred around zero. no so 
# horizontal more sloped. 

# part 2 
ggplot(data1, aes(y=voteshare, x=presvote))+
  geom_point(size=2, shape=1)+
  geom_smooth(method=lm) # error bands

ggsave("sc_plot3.png",
       device = "png",
       dpi = 300)
dev.off()

# part 3
# Y = 0.441330 + 0.388018X 

## Question 4
# part 1
hist(resids) # distribution normal
plot(resids~resids2) # linear-ish 
# 'if what's left to explain of voteshare can be explained by "the capacity of 
# presvote(?) to explain anything over and above difflog" (residuals from Q2)
# "Hence, the estimated slope (using OLS regression) will be the same in the 
# model with 𝑠𝑜𝑚𝑎=𝛽0+𝛽𝑤𝑡9𝑤𝑡9+𝛽ℎ𝑡9ℎ𝑡9 as in the 
# model 𝑟𝑒𝑠𝑖𝑑.𝑠𝑜𝑚𝑎=𝛽ℎ𝑡9𝑟𝑒𝑠𝑖𝑑.ℎ𝑡9" - 
# https://stats.stackexchange.com/questions/7322/what-does-plotting-residuals-from-one-regression-against-the-residuals-from-anot

# telling us if after removing the effect of difflog (i.e. the linear model)
# from presvote and voteshare, if there's a relationship b/t voteshare and 
# presvote. hence, the multiple regression will show some of the same results
# indicating the interaction effects of the input variables. 

residd_lm <- lm(resids~resids2)
summary(residd_lm) # small increase in resids 2 for 1 in resids

# part 2
ggplot(data1, aes(y=resids2, x=resids))+
  geom_point(size=2, shape=1)+
  geom_smooth(method=lm) 

ggsave("sc_plot4.png",
       device = "png",
       dpi = 300)
dev.off()

# part 3 
# Y = -4.860e-18 + 2.569e-01X  

## Question 5 

## part 1 - multiple regression model
mrmodel <- lm(voteshare ~ difflog + presvote, data = data1)
summary(mrmodel)

# part 2
# Y = 0.4486442 + 0.0355431X1 + 0.0117637X2

# part 3 

summary(residd_lm)

# residual standard error. one diff in df
# the residuals values.





