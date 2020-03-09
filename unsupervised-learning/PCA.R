---
author: "Author James"
title: "Unsupervised learning"
output:
  html_notebook: 
    theme: journal
---



*Supervised* learning is about making predictions of a target variable from a set of expalanatory variables. For *unsupervised learning* we want to find out unknown structures in data.Unknown structures can tell us more about the data that we want to analyze.

#### Dimensionality Reduction
Is a technique for mapping high dimesnion data into fewer dimensions for purpose of visualizing and analyzing the data.This method is applicable especially where data has numerous colummns not necessarily observations, where observations have many vraiables and there is little information in a column eg genetic data. There is little information in a sinle marker but combined we can tell more about an individual.

##### Principal Component Analysis
Maps one vector space of your data to another without reducing the dimensionality of the data. The data is represented in a way (coordinate system) such that the most important information  is the first coordinate  and so on. For this yoyr data has to be in numerical form hence there is need to represent categorical data in binary vector for each level.

A better to understand PCA is by looking at it as linear transformation where it changes the basis of the vector spaces such that the data with more more covariance is along the first basis vector and son on. The basis on the new vector is called component and hence  the term *principal component analysis*  refers to looking at the first few important components.

##### The iris data.
This data has features tha is *sepal.width*,*sepal.length*,*petal.width*,and *petal.length* that are used to classify flower species particularly *Setosa, Verginica, Versicolor*

We can try and visualize these features to understand their structure.

First things first we'll load the required libraries
```{r}
library(magrittr)
library(ggplot2)
library(dplyr)
```


```{r}
# iris data
iris%>%head
```



```{r}
# plotting some of the measurements against each other
iris %>% ggplot() +
 geom_point(aes(x = Sepal.Length, y = Sepal.Width, colour = Species,))
```
*plot of sepal width versus the sepal length*


```{r}
iris %>% ggplot() +
 geom_point(aes(x = Petal.Length, y = Petal.Width, colour = Species,))
```
*plot of petal width versus the petal length*


The Setosa specie stands out in both plots. Also *Versicolor* and *virginica* species overlap in both plots.

To implement *PCA* we can elimninate the species class because it is non-numerical. And transform the data using *prcomp* function

```{r}
pca <- iris %>% select(-Species) %>% prcomp
pca
```
*The standard dev tells us how much variance is in each component and the rotation tells us what the linear transformation is*

```{r}
pca%>%plot
```
*This plot tells us how much covariance there is in each component. If the there is not much of covariance in the first few components, then the transformation did not perform well, and it will not be of any good to you.*

```{r}
#To map the data to the new space spanned by the principal components, we use the predict() function
mapped_iris <- pca %>% predict(iris)
mapped_iris %>% head
```

This data can be used with the data that was not used to create the *PCA*. Lets try to plot the first two components against ecah other.

```{r}
mapped_iris %>% 
 as.data.frame %>% # to convert the matrix to a data frame to work with ggplot
 cbind(Species = iris$Species) %>% # identifying the factor data for coloring the plot according to species
 ggplot() +
 geom_point(aes(x = PC1, y = PC2, colour = Species))
```
*plot of PC1 against PC2*

we'll now look at the *HouseVotes84* dataset from the *mlbench* package
```{r}
#load the required library
library(mlbench)
data(HouseVotes84)
HouseVotes84 %>% head
```

Here we'll try to discover the  differences in voting patterns between the republicans and the democrats. So we'lldo PCA on the data since there is very little we can discover from the individual columns.

Key to note is that the data is not in numerical form(it actually in trinary form y,n,NA). So our first step would be to transform it to map the votes into zeros and ones.

We need first to transform the factors  y,n and NA into numerical otherwise *PCA* will not work. To do so we are going to use a function *apply()* that will transform our row and columns as as follows votes cast as y = 1 votes cast as n = 0 and votes that were  not cast as 0. *apply()* function takes parameters (1,2) which basically means that it will operate in both dimensions i.e columns and rows

```{r}
vote_patterns <- HouseVotes84%>% # saving the transformed data into variable vote_patterns
 select(-Class) %>%
 apply(c(1,2), . %>% { ifelse(as.character(.) == "n", 0, 1) }) %>%
 apply(c(1,2), . %>% { ifelse(is.na(.), 0.5, .) })
```


```{r}
# to perform the PCA
pca <- vote_patterns %>% prcomp
```

Now we can map the vote patterns onto the principal components and plot the first against the second

```{r}
mapped_votes <- pca %>% predict(vote_patterns)
mapped_votes %>%
 as.data.frame %>%
 cbind(Class = HouseVotes84$Class) %>%
 ggplot() +
 geom_point(aes(x = PC1, y = PC2, colour = Class))
```
*plot of PC1 against PC2*
from this plot it is clear that there is a separation in the voting patterns atleast on the first principal component


##### EXERCISE 1
From the above implementation We transformed our data to to take *NA* values as 0.5.No we intend to create binary columns for each vote cast eg V1.YES,V1.NO and V1.NOTWe can then see the effect of this transformation on *PCA*. We are going to use the *mutate()* to create the new columns and *select()* function to remove the old columns

```{r}
vote_new<-HouseVotes84%>%
  mutate(v1.YES=if_else(V1=='y',1,0,missing = 0),V1.NO=if_else(V1=='y',0,1,missing = 0),V1.NOT=if_else(V1=='y',0,0,missing = 1))%>%
   mutate(v2.YES=if_else(V2=='y',1,0,missing = 0),V2.NO=if_else(V2=='y',0,1,missing = 0),V2.NOT=if_else(V2=='y',0,0,missing = 1))%>%
   mutate(v3.YES=if_else(V3=='y',1,0,missing = 0),V3.NO=if_else(V3=='y',0,1,missing = 0),V3.NOT=if_else(V3=='y',0,0,missing = 1))%>%
   mutate(v4.YES=if_else(V4=='y',1,0,missing = 0),V4.NO=if_else(V4=='y',0,1,missing = 0),V4.NOT=if_else(V4=='y',0,0,missing = 1))%>%
   mutate(v5.YES=if_else(V5=='y',1,0,missing = 0),V5.NO=if_else(V5=='y',0,1,missing = 0),V5.NOT=if_else(V5=='y',0,0,missing = 1))%>%
   mutate(v6.YES=if_else(V6=='y',1,0,missing = 0),V6.NO=if_else(V6=='y',0,1,missing = 0),V6.NOT=if_else(V6=='y',0,0,missing = 1))%>%
   mutate(v7.YES=if_else(V7=='y',1,0,missing = 0),V7.NO=if_else(V7=='y',0,1,missing = 0),V7.NOT=if_else(V7=='y',0,0,missing = 1))%>%
   mutate(v8.YES=if_else(V8=='y',1,0,missing = 0),V8.NO=if_else(V8=='y',0,1,missing = 0),V8.NOT=if_else(V8=='y',0,0,missing = 1))%>%
   mutate(v9.YES=if_else(V9=='y',1,0,missing = 0),V9.NO=if_else(V9=='y',0,1,missing = 0),V9.NOT=if_else(V9=='y',0,0,missing = 1))%>%
   mutate(v10.YES=if_else(V10=='y',1,0,missing = 0),V10.NO=if_else(V10=='y',0,1,missing = 0),V10.NOT=if_else(V10=='y',0,0,missing = 1))%>%
   mutate(v11.YES=if_else(V11=='y',1,0,missing = 0),V11.NO=if_else(V11=='y',0,1,missing = 0),V11.NOT=if_else(V11=='y',0,0,missing = 1))%>%
   mutate(v12.YES=if_else(V12=='y',1,0,missing = 0),V12.NO=if_else(V12=='y',0,1,missing = 0),V12.NOT=if_else(V12=='y',0,0,missing = 1))%>%
   mutate(v13.YES=if_else(V13=='y',1,0,missing = 0),V13.NO=if_else(V13=='y',0,1,missing = 0),V13.NOT=if_else(V13=='y',0,0,missing = 1))%>%
   mutate(v14.YES=if_else(V14=='y',1,0,missing = 0),V14.NO=if_else(V14=='y',0,1,missing = 0),V14.NOT=if_else(V14=='y',0,0,missing = 1))%>%
   mutate(v15.YES=if_else(V15=='y',1,0,missing = 0),V15.NO=if_else(V15=='y',0,1,missing = 0),V15.NOT=if_else(V15=='y',0,0,missing = 1))%>%
   mutate(v16.YES=if_else(V16=='y',1,0,missing = 0),V16.NO=if_else(V16=='y',0,1,missing = 0),V16.NOT=if_else(V16=='y',0,0,missing = 1))%>%
  select(-Class,-V1,-V2,-V3,-V4,-V5,-V6,-V7,-V8,-V9,-V10,-V11,-V12,-V13,-V14,-V15,-V16)
```
*from above operation we have created three binary columns for each vote cast. YES - for a vote cast as yes NO-for a vote cast as no and Not-for a vote that was not cast.*

```{r}
# to perform the PCA
pca <- vote_new %>%prcomp
```

Now we can map the vote patterns onto the principal components and plot the first against the second

```{r}
mapped_votes2 <- pca %>% predict(vote_new)
mapped_votes2 %>%
 as.data.frame %>%
 cbind(Class = HouseVotes84$Class) %>%
 ggplot() +
 geom_point(aes(x = PC1, y = PC2, colour = Class))
```
*The new plot of PC1 versus PC2. As seen the plot has not changed after using three binary columns for each vote cast*
