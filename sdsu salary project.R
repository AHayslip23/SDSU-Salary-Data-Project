#simple linear regression 
salary <- read.csv('sdsu salary data from class .csv') 
View(salary)

salary = salary[2:3]

library(caTools)
set.seed(123)

split = sample.split(salary$Estimated.Salary, SplitRatio = 0.70)
train = subset(salary, split == T) 
test = subset(salary, split == F)

regressor = lm(formula = Estimated.Salary ~ Grade, 
                data = train)

summary(regressor)
#there is a decent correaltion between the job grades and the salary that were assigned 
#75.3% correaltion is not bad 

y_pred = predict(regressor, newdata = test)
y_pred

library(ggplot2)

ggplot() + 
  geom_point(aes(x=train$Grade, y=train$Estimated.Salary),
             color = 'red') + 
  geom_line(aes(x=train$Grade, y=predict(regressor, newdata = train)),
            color = 'black') + 
  ggtitle('Salary vs Job Grade Training set') + 
  xlab('Job Grade') + 
  ylab('Salary')

ggplot() + 
  geom_point(aes(x=test$Grade, y=test$Estimated.Salary),
             color = 'blue') + 
  geom_line(aes(x=train$Grade, y=predict(regressor, newdata = train)),
            color = 'black') + 
  ggtitle('Salary vs Job Grade Test set') + 
  xlab('Job Grade') + 
  ylab('Salary')

#not as many outliers with the test set observation 

#polynomial regression 
lin_reg <- lm(formula = Estimated.Salary ~ Grade,
              data = salary)
summary(lin_reg)

salary$Grade2 = salary$Grade^2
salary$Grade3 = salary$Grade^3
salary$Grade4 = salary$Grade^4

poly_reg <- lm(formula = Estimated.Salary ~ .,
               data = salary)
summary(poly_reg)
#holy shit 96.6% accurate what 

ggplot() + 
  geom_point(aes(x = salary$Grade, y=salary$Estimated.Salary),
             color = 'orange') + 
  geom_line(aes(x = salary$Grade, y=predict(lin_reg, newdata = salary)),
            color = 'black') + 
  ggtitle('Salary vs Job Grade') + 
  xlab('Grade') + 
  ylab('Salary')

ggplot() + 
  geom_point(aes(x = salary$Grade, y=salary$Estimated.Salary),
             color = 'orange') + 
  geom_line(aes(x = salary$Grade, y=predict(poly_reg, newdata = salary)),
            color = 'black') + 
  ggtitle('Salary vs Job Grade') + 
  xlab('Grade') + 
  ylab('Salary')

y_pred <- predict(lin_reg, data.frame(Grade = 9.5))
y_pred
#the model predicts that a job with a grade in between 9 and 1 should earn $72,353.29

y_pred = predict(poly_reg, data.frame(Grade = 9.5,
                                      Grade2 = 9.5^2,
                                      Grade3= 9.5^3,
                                      Grade4= 9.5^4))
y_pred
#the polyomial regression model predicts that the job between job grades 9 and 10 should take home a salary of 63009.58
#the salary grade of 9 is between 53,000 and 57,000 
#the salary grade of 10 is between 59,000 and 70,000 

#use polynomial model, linear model overpays by almost 10K

library(e1071)
salary = salary[1:2]
View(salary)

regressor <- svm(formula = Estimated.Salary ~ Grade, 
                 data = salary, 
                 type = 'eps-regression')

y_pred = predict(regressor, data.frame(Grade=9.5))
print(y_pred)
#predicts salary of $61,462.34
#the salary grade of 9 is between 53,000 and 57,000 
#the salary grade of 10 is between 59,000 and 70,000 
#the model works for a job that is in between job grade 9 and 10 

ggplot() + 
  geom_point(aes(x=salary$Grade, y=salary$Estimated.Salary), 
             color = 'red') +
  geom_line(aes(x=salary$Grade, y=predict(regressor, newdata = salary)),
            color = 'black') + 
  ggtitle('SVM Salary vs Grade') + 
  xlab('Grade') + 
  ylab('Salary')

#performs fairly well 

#decision tree 
library(rpart)
classifier = rpart(formula = Estimated.Salary ~ Grade, 
                   data = salary)

summary(classifier)

y_pred = predict(classifier, data.frame(Grade=9.5))
print(y_pred)
#the decision tree model overfits the salary range by 20,000
#predicts that the salary is 91786

ggplot() +
  geom_point(aes(x=salary$Grade, y=salary$Estimated.Salary),
             color = 'red') + 
  geom_line(aes(x=salary$Grade, y=predict(classifier, newdata = salary)),
            color = 'blue') + 
  ggtitle('Decision Tree Salary vs Grade') + 
  xlab('Grade') + 
  ylab('Salary')
#not the most accurate, lots of outliers towards the end

classifier = rpart(formula  = Estimated.Salary ~ Grade,
                   data = salary, 
                   control = rpart.control(minsplit = 2))

y_pred = predict(classifier, data.frame(Grade=9.5))
print(y_pred)
#y_pred is way lower than last time to the point where the employee is underpaid 
#64281.67 
#seems more accurate 

library(ggplot2)
x_grid = seq(min(salary$Grade), max(salary$Grade), 0.01)
ggplot() +
  geom_point(aes(x = salary$Grade, y = salary$Estimated.Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(classifier, newdata = data.frame(Grade = x_grid))),
            colour = 'blue') +
  ggtitle('Decision Tree Regression Model') +
  xlab('Grade') +
  ylab('Salary')

summary(classifier)
#not a terribly accurate model our prediction for 9.5 

#random forest 
library(randomForest)
set.seed(1234)

regressor = randomForest(x = salary[1], 
                         y = salary$Estimated.Salary, 
                         ntree = 500)
regressor

y_pred = predict(regressor, newdata = data.frame(Grade = 9.5))
y_pred
#the preidcted salary is 55452.67 
#employee is slightly underpaid, but close to  

library(ggplot2)
x_grid = seq(min(salary$Grade), max(salary$Grade), 0.01)
ggplot() +
  geom_point(aes(x = salary$Grade, y = salary$Estimated.Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Grade = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Grade') +
  ylab('Salary')
#model actually turn out to be really good 

#job grade 10 pays between 59K and 70K 
#9.5 by model predictions 
#polynomial 72,000 #overpay slightly 
#SVM 61,000 #in the pay range 
#Decision Tree 64,000 #in the pay range 
#Random Forest 54,000 #underpay 
#go with SVM model for salary offer, it is the least amount of money that is within what the business would pay in that pay grade











  