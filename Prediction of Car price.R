# Consider only the below columns and prepare a prediction model for predicting Price.
# Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

corolla <- read.csv("C:/Users/Srikanth B V/Desktop/Data Science Assignments/Session by Session/05. Simple Linear regression & Multiple linear regression/Multiple+linear+Assignments/ToyotaCorolla.csv")
View(corolla)
attach(corolla)
summary(corolla)
mydata <- subset(corolla, select = c(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight))
# Use only the required variables

View(mydata)
head(mydata)
cor(mydata) #Check for correlation within variables

?pairs

# Scatterplot Matrices
pairs(mydata, upper.panel=panel.smooth, lower.panel = NULL, main="scatter plot with corr coeff")

model1_corolla <- lm(Price ~ ., data = mydata)
summary(model1_corolla) 
# R2 value is 0.8638 which is a good model for prediction
# cc & Doors hold p value >0.05. Hence checking for significance.

summary(lm(Price ~ cc + Doors, data = mydata))

install.packages("car")
library(car)

# Identify influencing vaues using diagnostics plot
vif(model1_corolla) # All the vif<10. Hence no collinearity between variables
avPlots(model1_corolla) # Doors does not show any variation. Doors contributing very less for Price
influence.measures(model1_corolla)
influenceIndexPlot(model1_corolla)
influencePlot(model1_corolla) # 81, 602, 222, 961 are influencing elements


# Model removing influencing elements
model2_corolla <- lm(Price ~ .-Doors, data = mydata[-c(81,602,222,961)])
summary(model2_corolla)

plot(model2_corolla)
# Residual plots = if the line is horizontally straight then the errors are constant. i,e not fluctuating
# QQ Plot = plotting the errors and are normally distributed.
# Standardised errors plot and not fluctuating.
# Residual vs leverage plot. Any data falls beyond +/- 0.5 is influential

mean(model2_corolla$residuals)
hist(model2_corolla$residuals)
sum(model2_corolla$residuals)


confint(model2_corolla, level=0.95)
predict(model2_corolla, interval="predict")

predict(model2_corolla, data.frame("Age_08_04"=23,"KM"=46986,"HP"=90,"cc"=2000,"Gears"=5,"Quarterly_Tax"=210,"Weight"=1165))

# Co-ef. are significant and co-ef.of Determination value. i.e R^2 value for model2 is is 0.86 which is good.
# sum of errors is 1.493845e-10 i.e approx 0 and errors are normally distributed.
# Hence models is best fit to predict the price with 95% confidence interval.
