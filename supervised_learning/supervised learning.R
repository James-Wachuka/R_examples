library(magrittr)
datasets::cars
cars%>%head
library(ggplot2)
cars %>% ggplot(aes(x = speed, y = dist)) +geom_point() +geom_smooth(method = "lm")
cars %>% lm(dist ~ speed, data = .) %>% summary
cars %>% lm(dist ~ speed, data = .) %>% coefficients
cars %>% lm(dist ~ speed, data = .) %>% confint
predict_dist<-function(speed,theta_1)
  data.frame(speed=speed,dist=theta_1 * speed,
             theta = as.factor(theta_1))
cars %>% ggplot(aes(x=speed,y=dist,colour=theta))+
  geom_point(colour="black")+
  geom_line(data=predict_dist(cars$speed,2))+
  geom_line(data=predict_dist(cars$speed,3))+
  geom_line(data=predict_dist(cars$speed,4))+
  scale_color_discrete(name=expression(theta[1]))
library(ggrepel)
library(ggthemes)
thetas <-seq(0,5,length.out=50)
fitting_error<-Vectorize(function(theta)sum((theta*cars$speed-cars$dist)**2))
data.frame(thetas=thetas,errors=fitting_error(thetas))%>%
  ggplot(aes(x=thetas,y=errors))+
  geom_line()+
  xlab(expression(theta[1]))+ylab("")

cars%>%lm(dist~speed-1,data=.)%>%coefficients
cars%>%ggplot(aes(x=speed,y=dist))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x-1)

# logistic regression
library(mlbench)
data('BreastCancer')
BreastCancer%>%head
BreastCancer%>%
  ggplot(aes(x=Cl.thickness,y=Class))+
  geom_jitter(height = 0.05,width=0.3,alpha=0.4)
BreastCancer%>%
  mutate(Cl.thickness.numeric=as.numeric(as.character(Cl.thickness)))%>%
  mutate(IsMalignant=ifelse(Class=='benign',0,1))%>%
  ggplot(aes(x=Cl.thickness.numeric,y=IsMalignant))+
  geom_jitter(height = 0.05,width=0.3,alpha=0.4)+
  geom_smooth(method='glm',method.args=list(family='binomial'))
library(dplyr)


# Evaluating classification models
formatted_data<-BreastCancer%>%
  mutate(Cl.thickness.numeric=as.numeric(as.character(Cl.thickness)),
         Cell.size.numeric=as.numeric(as.character(Cell.size)))%>%
  mutate(IsMalignant=ifelse(Class=='benign',0,1))
fitted_model<-formatted_data %>%
  glm(IsMalignant ~ Cl.thickness.numeric + Cell.size.numeric,data=.)
predict(fitted_model,formatted_data,type='response')%>% head

classify <-function(probability) ifelse(probability<0.5,0,1)
classified_malignant<-classify(predict(fitted_model,formatted_data))
table(formatted_data$IsMalignant,classified_malignant)

table(formatted_data$IsMalignant,classified_malignant,dnn=c('Data','Predictions'))

classify<-function(probability)
  ifelse(probability<0.5,'benign','malignant')
classified<-classify(predict(fitted_model,formatted_data))
table(formatted_data$Class,classified,dnn=c('Data','Predictions'))

# Accuracy
confusion_matrix<-table(formatted_data$Class,classified,
                        dnn=c('Data','Predictions'))
(accuracy<-sum(diag(confusion_matrix))/sum(confusion_matrix))

accuracy<-function(confusion_matrix)sum(diag(confusion_matrix))/sum(confusion_matrix)
replicate(8,accuracy(table(BreastCancer$Class,sample(BreastCancer$Class))))

#sensitivity and specificity
specificity<-function(confusion_matrix)confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[1,2])
sensitivity<-function(confusion_matrix)confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])
false_omission_rate<-function(confusion_matrix)confusion_matrix[2,1]/sum(confusion_matrix[,1])
negative_predicted_value<-function(confusion_matrix)confusion_matrix[1,1]/sum(confusion_matrix[,1])
positive_predicted_value<-function(confusion_matrix)confusion_matrix[2,2]/sum(confusion_matrix[,2])
false_discovery_rate<-function(confusion_matrix)confusion_matrix[1,2]/sum(confusion_matrix[,2])
prediction_summary<-function(confusion_matrix)
  c('Accuracy'=accuracy(confusion_matrix),
    'Specificity'=specificity(confusion_matrix),
    'Sensitivity'=sensitivity(confusion_matrix),
    'false_omission_rate'=false_omission_rate(confusion_matrix),
    'negative_predicted_value'=negative_predicted_value(confusion_matrix),
    'positive_predicted_value'=positive_predicted_value(confusion_matrix),
    'false_discovery_rate'=false_discovery_rate(confusion_matrix))
random_predicion_summary<-function()
  prediction_summary(table(BreastCancer$Class,
                           sample(BreastCancer$Class)))
replicate(3,random_predicion_summary())

# exercise 1
rmse<-function(x,t)sqrt(mean(sum((t-x)^2)))
line<-cars%>%lm(dist~speed,data=.)
poly<-cars%>%lm(dist~speed+I(speed^2),data=.)
rmse(predict(line,cars),cars$dist)
rmse(predict(poly,cars),cars$dist)

sampled_cars<-cars%>%mutate(training=sample(0:1,nrow(cars),replace=TRUE))
sampled_cars%>%head

training_data<-sampled_cars%>%filter(training==1)
test_data<-sampled_cars%>%filter(training==0)
#training_data<-cars[1:25,]
#test_data<-cars[26:50,]
line<-training_data%>%lm(dist~speed,data = .)
poly<-training_data%>%lm(dist~speed+I((speed^2)+(speed^3)+(speed^4)+(speed^5)),data = .)

rmse(predict(line,test_data),test_data$dist)
rmse(predict(poly,test_data),test_data$dist)

sampled_breast_data<-formatted_data%>%mutate(training=sample(0:1,nrow(formatted_data),replace=TRUE))
sampled_breast<-sampled_breast_data%>% mutate(train_test=ifelse(training==0,'test','training'))
#training_cancer<-sampled_breast%>%filter(training==1)
sampled_breast_data%>%head
fitted_model<-sampled_breast%>%
  glm(training~,data=.)
predict(fitted_model,sampled_breast,type='response')%>%head
classify <-function(probability) ifelse(probability<0.5,0,1)
classified_training<-classify(predict(fitted_model,sampled_breast))
table(sampled_breast$training,classified_training)

table(sampled_breast$training,classified_training,dnn=c('Data','Predictions'))

classify<-function(probability)
  ifelse(probability<0.5,'test','training')
classified<-classify(predict(fitted_model,sampled_breast_data))
table(sampled_breast$train_test,classified,dnn=c('Data','Predictions'))
random_predicion_summary<-function()
  prediction_summary(table(sampled_breast$training,
                           sample(sampled_breast$training)))
replicate(3,random_predicion_summary())
prediction_summary(table(sampled_breast$train_test,
                         (classified)))

library(purrr)
#permute rows
permute_rows<-function(df)df[sample(1:nrow(df)),]
#group data
group_data<-function(df,n){
  groups<-rep(1:n,each=nrow(df)/n)
  split(df,groups)
}
# cross validation
cross_val_grps<-function(grouped_df){
  result<-vector(mode='list',length = length(grouped_df))
  for(i in seq_along(grouped_df)){
    result[[i]]<-grouped_df[-i]%>%do.call('rbind',.)
    
  }
  result
}
# cross valiadtion splits
cross_validation_split<-function(grouped_df){
  result<-vector(mode='list',length=length(grouped_df))
  for(i in seq_along(grouped_df)){
    training<-grouped_df[-i]%>%do.call('rbind',.)
    test<-grouped_df[[i]]
    result[[i]]<-list(training=training,test=test)
    
  }
  result
}

cars%>%
  permute_rows%>%
  group_data(5)%>%
  cross_validation_split

cars%>%
  permute_rows%>%
  group_data(5)%>%
  cross_val_grps%>%
  map(.%>%lm(dist~speed,data=.)%>%.$coefficients)%>%
  do.call('rbind',.)

prediction_acc<-function(test_and_training){
  length=length(test_and_training)
  for (i in seq_along(test_and_training)){
  training<-test_and_training[[i]]$training
  test<-test_and_training[[i]]$test
  model<-training%>%glm(IsMalignant~.,data=.)
  predictions<-test%>%predict(model,data=.)
  targets<-test$IsMalignant
  #result[i]<-prediction_summary(confusion_matrix)
  result<-prediction_summary(targets,re      
  
  
  }
  result
}

formatted_data%>%
  permute_rows%>%
  group_data(3)%>%
  cross_validation_split%>%
  prediction_acc

formatted_data2<-BreastCancer%>%
  mutate(Cl.thickness.numeric=as.numeric(as.character(Cl.thickness)),
         Cell.size.numeric=as.numeric(as.character(Cell.size)))

prediction_summary(table(breast$training,
                         sample(sampled_breast$training)))
