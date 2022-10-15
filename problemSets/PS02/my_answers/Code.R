###### 
#PS01 Answers
######

library(tidyverse)

## Question 1
# part a
# calculate chi-square test statistic 
# chi^2 = sum of (diff b/t observed and expected value squared)/(expected value)

# step one prepare data in R

PolTre <- matrix(c(14, 7, 6, 7, 7, 1), 2, 3) # assign frequencies to a matrix
dimnames(PolTre) = list(
  class = c("Upper", "Lower"),
  treatment = c("Not Stopped", "Bribe Requested", "Warning")
) # name rows and columns
PolTre
addmargins(PolTre)


# step two find proportion of category 'class' representation in total sample
# step three calculate and assign expected values

# the expected value of each cell is the column total x sample proportion of 
# the class i.e. row total over sample total

# if cell1 
sum(PolTre[,1])*sum(PolTre[1,])/sum(PolTre)

expv <- matrix(, nrow=2, ncol = 3)
for (r in 1:2){
  expv[r,1] <- sum(PolTre[,1])*sum(PolTre[r,])/sum(PolTre)
  expv[r,2] <- sum(PolTre[,2])*sum(PolTre[r,])/sum(PolTre)
  expv[r,3] <- sum(PolTre[,3])*sum(PolTre[r,])/sum(PolTre)
}
expv # expected values matrix

# step four calculate chi-square values for each cell
# i.e. take expv value from PolTre value for each cell, square, then over
# expv value

# chi square formula in R
# chsq <- ((x-y)^2)/y where x is observed value and y is expected value

# for cell 1
(((PolTre[1,1])-(expv[1,1]))^2) / (expv[1,1])

exporg <- data.frame(cbind(PolTre, expv))
exporg
cs <- matrix(, nrow=2, ncol = 3)
for (r in 1:2){
  cs[r,1] <- (((exporg[r,1])-(exporg[r,4]))^2) / (exporg[r,4])
  cs[r,2] <- (((exporg[r,2])-(exporg[r,5]))^2) / (exporg[r,5])
  cs[r,3] <- (((exporg[r,3])-(exporg[r,6]))^2) / (exporg[r,6])
}
cs

# step five sum all values of chi-square
sum(cs)
# [1] 3.791168
# on tables, for df=2 ((r-1)(c-1)), p-value is between 0.975 and 0.2
# therefore fail to reject the null hypothesis that 
# these two samples have no dependency relationship 'statistical independence'

# checking work
chisq.test(PolTre, y=NULL)
# Pearson's Chi-squared test
# X-squared = 3.7912, df = 2, p-value = 0.1502
# same chi square and p range, same conclusion at alpha = 0.1

## part b
# calculate the (right-tail) p-value
pchisq(sum(cs),2, lower.tail = FALSE) # [1] 0.1502306
# fail to reject H0


## part c
srs <- matrix(, nrow=2, ncol = 3)
for (r in 1:2){
  srs[r,1] <- (((exporg[r,1])-(exporg[r,4]))^2) / (exporg[r,4])
  srs[r,2] <- (((exporg[r,2])-(exporg[r,5]))^2) / (exporg[r,5])
  srs[r,3] <- (((exporg[r,3])-(exporg[r,6]))^2) / (exporg[r,6])
}
srs

# standardised residuals formula in R
# x - y / sqrt (y(1-rowtotal/total)(1-coltotal/total))

srs <- matrix(, nrow=2, ncol = 3)
rownames(srs) <- c("Upper Class", "Lower Class")
colnames(srs) <- c("Not Stopped", "Bribe Requested",
                   "Given Warning")
for (r in 1:2){
  srs[r,1] <- ((exporg[r,1])-(exporg[r,4])) / 
    (sqrt(((exporg[r,4])*
           (1-(sum(PolTre[r,])/
                 sum(PolTre)))*(1-sum(PolTre[,1])/sum(PolTre))
    ))
    )
  srs[r,2] <- ((exporg[r,2])-(exporg[r,5])) / 
    (sqrt(((exporg[r,5])*
           (1-(sum(PolTre[r,])/
                 sum(PolTre)))*(1-sum(PolTre[,2])/sum(PolTre))
    ))
    )
  srs[r,3] <- ((exporg[r,3])-(exporg[r,6])) / 
    (sqrt(((exporg[r,6])*
           (1-(sum(PolTre[r,])/
                 sum(PolTre)))*(1-sum(PolTre[,3])/sum(PolTre))
    ))
    )
}
srs

# ##           Not Stopped Bribe Requested Given Warning
# Upper Class   0.3220306       -1.641957      1.523026
# Lower Class  -0.3220306        1.641957     -1.523026

# test
((PolTre[2,2])-(expv[2,2])) / 
  sqrt((expv[2,2])*(1-(sum(PolTre[2,])/sum(PolTre)))*
       (1-sum(PolTre[,2])/sum(PolTre)))
((PolTre[1,2])-(expv[1,2])) / 
  sqrt((expv[1,2])*(1-(sum(PolTre[1,])/sum(PolTre)))*
         (1-sum(PolTre[,2])/sum(PolTre)))


# Question 2
fempol <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summary(fempol)
head(fempol)

# part b
lm1 <- lm(water ~ reserved, data = fempol)
summary(lm1)

# expected value for water projects when reserved policy is in place (=1 ) is
# 14.738 beta 0
# difference in group specific estimations (beta 1) is 'reserved' 9.252

# t-value: 6.446, p-value: 4.22e-10, reject the null hypothesis

# ANOVA

# anova(lm1)
# RegSS <- sum((lm1$fitted.values-mean(fempol$water))^2)
# RegSS
# SSE <- sum((fempol$water-lm1$fitted.values)^2)
# SSE

# fstat <- (RegSS/1)/(SSE/320)
# fstat

# pf(fstat,1,320, lower.tail = FALSE)




