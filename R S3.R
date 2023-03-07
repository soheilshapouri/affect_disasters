###Setup

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")


library("dplyr")
library("ggpubr")
library(MASS)
library(lsr)
library(car)
library(tidyverse)


df <- read.csv("C:\\Users\\17063\\Downloads\\Data S2.csv", header = T)


### Trying to use two-group t-test (failed):


#Valance ratings:
d_V <- df[, c("DisasterGroup","Valence_Scaled") ]

x_V <- d_V[d_V$DisasterGroup == "Natural ",]$Valence_Scaled
y_V <- d_V[d_V$DisasterGroup == "Technological",]$Valence_Scaled

# Normality test: 
ggqqplot(x_V)
shapiro.test(x_V)
# -> valence of Natural group: not normal

ggqqplot(y_V)
shapiro.test(y_V)
# -> valence of technological group: normal

leveneTest(Valence_Scaled ~ DisasterGroup, data = df, center = mean)

# -> enequal variance

# Not t-test (normality and equal variance are not met), also
# not Welch test (normality is not met)


#Arousal ratings:
d_A <- df[, c("DisasterGroup", "Arousal_Scaled") ]

x_A <- d_A[d_A$DisasterGroup == "Natural ",]$Arousal_Scaled
y_A <- d_A[d_A$DisasterGroup == "Technological",]$Arousal_Scaled

# Normality Plot:
ggqqplot(x_A)
shapiro.test(x_A)
# -> Natural group: normal

ggqqplot(y_A)
shapiro.test(y_A)
# -> Technological group: NOT normal

leveneTest(Arousal_Scaled ~ DisasterGroup, data = df, center = mean)
# -> equal variance.


# -> As assumptions of student t-test and Welch test are not met, 
#use linear regression by checking the normality of errors. 

####### Linear regression:

#### Valance
g <- lm(Valence_Scaled ~ factor(DisasterGroup), data = d_V)
summary(g)

par(mfrow = c(2,2))
plot(g, which = 1:4)
#Cook's distance shows the existence of influential outliers. 

shapiro.test(g$residuals)
# residuals are Not normal.

# Removing influential outliers based on Cook's distance:
for (q in 1:30) {
  d_V <- d_V[!(cooks.distance(g)==max(cooks.distance(g))),]
  g <- lm(Valence_Scaled ~ DisasterGroup, data = d_V)
  par(mfrow=c(1,1))
  plot(g, which = 4)
  shapiro.test(g$residuals)
  # 30 removed
}


###### Transformation:
par(mfrow = c(1,1))
boxcox(g, plotit = T,  lambda = seq(0.3, 1.1, 0.05))
#Box-Cox shows 0.8 is the appropriate value for teh tranformation 

gt <- lm(Valence_Scaled^0.8 ~ DisasterGroup, data = d_V)
summary(gt)

par(mfrow = c(2,2))
plot(gt, which = 1:4)

shapiro.test(gt$residuals)
ggqqplot(gt$residuals)

#After transformation, residuals are normal 

############################ Arousal

g2 <- lm(Arousal_Scaled ~ DisasterGroup, data = d_A)
summary(g2)

par(mfrow = c(2,2))
plot(g2, which = 1:4)

shapiro.test(g2$residuals)
# -> Normal

######The End######