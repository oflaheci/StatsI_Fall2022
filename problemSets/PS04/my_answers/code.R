### PS04 ###

install.packages(car)
library(car)
data(Prestige)
help(Prestige)
install.packages(stargazer)
library(stargazer)

## Question 1
head(Prestige)
levels(Prestige$type)

# part 1
professional <- ifelse(Prestige$type == "prof", 1, 0)
# professional <- na.omit(professional) # 98 obs - ones included equal:
print(professional)

# part 2 
# partial f-test for interaction
prestige_reg1 <- lm(prestige ~ income + professional, 
                    data=Prestige)
# Yi = Intercept + Beta1Income(Xi) + Beta2(Y/N)
stargazer(prestige_reg1, type = "text")
stargazer(prestige_reg1, title = "Regression Results")
# on average, for every one unit (dollar) increase in income, there is a 0.001
# unit increase in Prestige score. for every 1-unit increase in 'professional' 
# i.e. going from non-professional to professional, there is a 22.757 increase
# in prestige score. professionals would expect to have this much more prestige on average.

prestige_reg2 <- lm(prestige ~ income + professional +
                    income : professional,
                    data=Prestige)
# Yi = Intercept + Beta1(Xi ie Income) + Beta2(Y/N) + Beta3Int(Y/N)(Xi) -> part 3

anova(prestige_reg1, prestige_reg2) # F-stat 16.796, reject H0. p v small.

# at least one of the predictors(interaction effects) in the reg2 model is non-0
# therefore consider separate slope models

inter_lm <- lm(prestige ~ income * professional, data = Prestige) # same as reg2 diff code
stargazer(inter_lm, type = "text")
stargazer(inter_lm, title = "Regression Results")

# part 3
# Yi = Intercept + Beta1(Xi ie Income) + Beta2(Y/N) + Beta3Int(Y/N)(Xi)
# Yi = 21.142 + 0.003(Xi) + 37.781(Di) + (-0.002(Di)(Xi))

# part 4 and part 5
# on average, for every one unit (dollar) increase in income, for non-professionals
# there is a 0.003 unit increase in Prestige score. for every 1-unit increase in 
# 'professional' i.e. going from non-professional to professional, there is a 37.781-0.002=37.779 increase
# in prestige score. professionals would expect to have this much more prestige on average (i.e. holding income constant).

# but the professionalism gap isn't the same for every income amount - the interaction
# is statistically significant at 0.001. professionals
# the prestige returns for income for non-professionals are 0.003, whereas prestige returns 
# for professionals are 0.003 + - 0.002 = 0.001. 
# therefore the gap in prestige between professionals and non-professionals decreases as income increases
# and the slopes of the regression lines are different

# the income coefficient is the prestige increase associated with a 1-dollar increase in income for non-professionals, on average 0.003 points 
# the professional coefficient indicates part of the effect of being a professional on prestige. when considered
# with the interaction term, professionals are expected to have on average, holding income constant, 37.779 more prestige points than non-professionals

# part 6
# Yi = 21.142 + 0.003(Xi) + 37.781(Di) + (-0.002(Di)(Xi))
# for profs = 58.923 + 0.003(Xi) - 0.002(Xi) = 58.923 + 0.001(Xi)

# Yi+1000 where (Xi +1000) = 21.142 + 0.003(Xi+1000) + 37.781(Di) + (-0.002(Di)(Xi+1000))
# = 21.142 + 0.003(Xi+1000) + 37.781(1) - 0.002(1)(Xi+1000)
# = 21.142 + 0.003(Xi) + 0.003(1000) + 37.781 - 0.002(Xi) - 0.002(1000)
# = 21.142 + 0.003(Xi) + 3 + 37.781 - 0.002(Xi) - 2
-2+21.142+3+37.781 # 59.923
# = 59.923 + 0.003(Xi) - 0.002(Xi)
# = 59.923 + 0.001(Xi)

# Yi+1000 - Yi = [59.923 + 0.001(Xi)] - [58.923 + 0.001(Xi)] = 1

# around 1 prestige point

# test
21.142 + 0.003*(6000) + 37.781*(1) - 0.002*(1)*(6000) # 64.923
21.142 + 0.003*(7000) + 37.781*(1)- 0.002*(1)*(7000) # 65.923

# part 7 
# Yi = 21.142 + 0.003(Xi) + 37.781(Di) + (-0.002(Di)(Xi))
# for profs = 58.923 + 0.001(Xi)
# for non profs = 21.142 + 0.003(Xi)
# change in y hat to prof = 37.781 - 0.002(Xi)
37.781 - 0.002*(6000) # 25.781 point change in y hat


## Question 2
# part 1 & 2

Estimates <- c(0.042,0.042,0.302)
Standard_Errors <- c(0.016, 0.013, 0.011)
coefficients <- c('lawn sign', 'adjacent', 'constant') 
# tscores <- (Estimates-0)/(Standard_Errors)
# p_values <- 2*pt(abs(tscores) , 131-3, lower.tail = F)
model_results <- data.frame(coefficients, Estimates, Standard_Errors)
print(model_results)

tscoresmod <- (model_results$Estimates-0)/(model_results$Standard_Errors)
p_valuesmod <- 2*pt(abs(tscoresmod) , 131-3, lower.tail = F) # n = 131, k = 3
model_results <- data.frame(model_results, p_valuesmod)


print(model_results)
#    precinct Estimates Standard_Errors  p_valuesmod
# 1 lawn sign     0.042           0.016 9.720020e-03
# 2  adjacent     0.042           0.013 1.569460e-03
# 3  constant     0.302           0.011 1.738775e-55

# part 3
# constant coefficient - the y-intercept of the model
# i.e. the value of voteshare for when not in a precinct with lawn signs
# in this model vote share for the opponent in such a precinct is ~30%

# part 4 
# R-squared = 0.094, the coefficient of determination
# low R-squared. the importance of lawn signs in explaining the variation
# in voteshare is minimal. the above model has the ability to explain 
# less than a tenth of the variation in voteshare in this state.

R.squared <- 0.094
F.test <- (R.squared/(3 - 1)) / ((1 - R.squared)/(131 - 3)) 
df1 <- 3 - 1
df2 <- 131-3
F.pvalue <- df(F.test, df1, df2)
F.pvalue # 0.001634304

















