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

resids <- spend_lm$residuals

# part 4 write prediction equation - not code

# Y = 0.579031 + 0.041666X

predict(spend_lm, newdata = #ndat1)







