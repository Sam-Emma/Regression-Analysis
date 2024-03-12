install.packages("lavaan")
install.packages("OpenMx")
install.packages("semPlot")
install.packages("GGally")
install.packages("corrplot")
install.packages("olsrr")
install.packages("jtools")
install.packages("moments")
install.packages("lmtest")
install.packages('ggfortify')
library(ggfortify)
library(olsrr)
library(jtools)
library(moments)
library(lmtest)
library(ggplot2)
library(lavaan)
library(semPlot)
library(OpenMx)
library(GGally)
library(corrplot)
library('dplyr')
data <- read.csv("../market profit.csv")

model_prcsi <- '
BP ~ PRCSI + ET + ET*PRCSI
'
model_prdsi <- '
BP ~ PRDSI + ET + ET*PRDSI
'
model_rsi <- '
BP ~ RSI + ET + ET*RSI
'
model_all <- '
BP ~ PRCSI + PRDSI + RSI
'

corr = cor(data)
corrplot(corr, method = 'square') 

ggcorr(data, nbreaks = 6, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))
ggpairs(data)

#Modelling
Model <- lm(model_all, data = data)
summary(Model)
summ(Model, confint = TRUE, ci.width = 0.95)
summ(Model, scale = TRUE, transform.response = TRUE)

par(mfrow=c(2,2))
#Regression Analysis plots
plot(Model)
autoplot(Model)
skewness(Model$residuals)
kurtosis(Model$residuals)



fit_prcsi = cfa(model_prcsi, data = data)
summary(fit_prcsi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
capture.output(
  summary(fit_prcsi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE),
  file = "../prcsi.csv")
semPaths(fit_prcsi, whatLabels="std", style="lisrel", exoCov = T,
         edge.label.cex=1.5, curvePivot = TRUE, sizeMan = 10, sizeInt = 10, 
         residuals=F, shapeMan = "circle",edge.width = 1.7) 


fit_prdsi = cfa(model_prdsi, data = data)
summary(fit_prdsi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
capture.output(
  summary(fit_prdsi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE),
  file = "../prdsi.csv")
semPaths(fit_prdsi, whatLabels="std", style="lisrel", exoCov = T,
         edge.label.cex=1.5, curvePivot = TRUE, sizeMan = 10, sizeInt = 10, 
         residuals=F, shapeMan = "circle",edge.width = 1.7) 


fit_rsi = cfa(model_rsi, data = data)
summary(fit_rsi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
capture.output(
  summary(fit_rsi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE),
  file = "../rsi.csv")
semPaths(fit_rsi, whatLabels="std", style="lisrel", exoCov = T,
         edge.label.cex=1.5, curvePivot = TRUE, sizeMan = 10, sizeInt = 10, 
         residuals=F, shapeMan = "circle",edge.width = 1.7) 
