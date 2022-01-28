The objective of this project is to analyze "a posteriori" the results of the 2020 US Presidential Elections, by using supervised learning techniques that can be grouped in two main families: Classification and Regression.  

In **classification** the approach is going to be to predict the winner of each US county. However, the motivation in **regression** is going to be to predict in a rough way the winner of the presidential elections. by predicting the percentage of votes for each candidate in each county and then for each state.   
 
# Data: 
The datasets chosen are several and can be found in the following links:  
- https://www.openintro.org/data/?data=county_complete  
- https://github.com/tonmcg/US_County_Level_Election_Results_08-20



## Libraries:
```{r}
library(tidyverse)
library(stringr)
library(htmltab)
library(ggplot2)

library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)

library(usmap)
library(ggpubr)
```

## Datasets:
```{r}
all = read.csv("county_complete.csv", header = T, sep = ",")
all = all %>% select(1,2,3,which(grepl("2019",colnames(all))))

colnames(all)[3] = "county"
```


```{r}
d1 = read.csv("2020_US_County_Level_Presidential_Results.csv")

d1 = d1 %>% mutate(votes_others=total_votes - (votes_gop+votes_dem)) %>% select(1,2,3,4,5,6,11)
colnames(d1) = c("state", "fips_code", "county", "Trump_2020", "Biden_2020", "Total_2020", "Other_2020")
```


```{r}
data = inner_join(d1, all, by = c("county","state"))
```

- Most populous city in each State:
```{r}
cities_t = usmap_transform(citypop)
```
*(Notice that it is included a location variable in order to slightly upgrade the accuracy in Regression)*.   
- Final data:  
```{r}
all_2 = full_join(all, cities_t)

cities_counties = all_2 %>% select(state, county, lon, lat)
data = inner_join(data, cities_counties)
```


- Votes data:  
```{r}
# with perc:
votes = data %>% select(1,4,5,6,7)

data = data %>% mutate(Biden_2020 = Biden_2020/Total_2020) %>% select(-c(4,6,7))
plot_data = data
```



# Data Preprocessing:  

Before starting with the models we need to explore and modify the dataset in order to have a nice and clean input.


## Discard correlated variables:  

Once we have the data loaded, we are going to save some categorical values as profiles and discard some of them that are not interesting for the project:

```{r}
states_county = data$state
fips_county = data$fips_code
names_county = data$county

data$state = NULL
data$fips_code = NULL
data$county = NULL
data$fips = NULL
```

```{r}
data$households_speak_asian_or_pac_isl_2019 = NULL
data$households_speak_other_2019 = NULL
data$households_speak_other_indo_euro_lang_2019 = NULL
data$households_speak_spanish_2019 = NULL
data$mean_household_income_2019 = NULL
data$median_individual_income_age_25plus_2019 = NULL
data$household_has_broadband_2019 = NULL
data$poverty_65_and_over_2019 = NULL
data$poverty_under_18_2019 = NULL
data$uninsured_under_6_2019 = NULL
data$uninsured_under_19_2019 = NULL
data$uninsured_65_and_older_2019 = NULL
data$white_not_hispanic_2019 = NULL
data$age_under_5_2019 = NULL
data$persons_per_household_2019 = NULL
data$households_2019 = NULL
data$age_over_85_2019 = NULL
data$median_household_income_2019  = NULL
data$poverty_2019 = NULL
data$median_individual_income_2019 = NULL
```

Using the **mice** package we could look for **collinearities** and discard them:  
```{r echo=FALSE}
library(mice)
```

```{r}
mice:::find.collinear(data)
data$housing_two_unit_structures_2019 = NULL
```

Finally, we can see the correlation matrix:  
```{r fig.height = 10, fig.width = 10}
library(GGally)
ggcorr(data, label = T)
```

## Missing Values, NAs:  
We check whether if the data have missing values (NAs) or not, and where.   

```{r echo=FALSE}
# full_data = data
library(VIM)
```

Next, using the **VIM** package we can see in a graph the distribution of NAs and the missing values for each variable. We see that in one variable the percentage of missing data is around 50%. This value is high but te choice is to input the values using **mice**.  
```{r}
# plot of NAs:
na_plot = aggr(data, col=c('#69b3a2','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of NAs","Pattern"))
```

```{r results='hide'}
# model:
mice = mice(data, method = 'rf')
mice.com = mice::complete(mice)
```

```{r}
# variables that have NAs:
data$mean_work_travel_2019 = mice.com$mean_work_travel_2019

# plot without NAs
na_plot = aggr(data, col=c('#69b3a2','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of NAs","Pattern"))
```

Once we have imputed the missing values we can take a look if they are reasonable. We can see that the imputed vales are similar to the actual values. Sufficient for our needs. Notice that the density for each imputed variable is showed in magenta while the density of the actual values is in blue.  
```{r}
densityplot(mice)
```




## Insights:  
It is interesting to take a look at some variables and see how they interact with the percentage of vote for Biden in each state.  Notice that each point is a county in that state.  

Moreover, we define some thresholds (50 for X axis and 0.50 for Y axis) to divide the plane in 4 grids.

- Up: Biden wins the county.
- Right: Percentage of target is above 50%.

```{r}
plot_data$win = factor(ifelse(plot_data$Biden_2020>0.5, "Biden", "Trump"))
```

### Ethnic:

We see interesting insights while taking a look at the ethnic of the voters. Biden is more popular between Black people. However, we see that in some states Hispanic vote is biased for Trump. On the other hand, white people tend to prefer Trump. In fact, in some states like Mississippi and Alabama is clearly an almost perfect linear relation.  

```{r}
# White people:
ggplot(plot_data, aes(x=white_2019, y=Biden_2020)) +
  geom_point(alpha=0.8, aes(group=state, colour=win, fill=win),  show.legend = FALSE) + facet_wrap(~state) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = 50 , linetype="dashed", color="red") + ggtitle("Distribution of white people % and Biden % of vote") + scale_colour_manual(values = c("#377eb8", "#e41a1c"), name = "win")

# Black people:
ggplot(plot_data, aes(x=black_2019, y=Biden_2020)) +
  geom_point(alpha=0.8, aes(group=state, colour=win, fill=win),  show.legend = FALSE) + facet_wrap(~state) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = 50 , linetype="dashed", color="red") + ggtitle("Distribution of black people % and Biden % of vote") + scale_colour_manual(values = c("#377eb8", "#e41a1c"), name = "win")

# Hispanic:
ggplot(plot_data, aes(x=hispanic_2019, y=Biden_2020)) +
  geom_point(alpha=0.8, aes(group=state, colour=win, fill=win),  show.legend = FALSE) + facet_wrap(~state) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = 50 , linetype="dashed", color="red") + ggtitle("Distribution of hispanic people % and Biden % of vote") + scale_colour_manual(values = c("#377eb8", "#e41a1c"), name = "win")
```

### Social:
We see that clearly in most of the states there is a linear relation between the percentage of bachelors and the percentage of Biden vote.  
```{r}
# bachelors:
ggplot(plot_data, aes(x=bachelors_2019, y=Biden_2020)) +
  geom_point(alpha=0.8, aes(group=state, colour=win, fill=win),  show.legend = FALSE) + facet_wrap(~state) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = 50 , linetype="dashed", color="red") + ggtitle("Distribution of bachelors % and Biden % of vote") + scale_colour_manual(values = c("#377eb8", "#e41a1c"), name = "win")
```

### Economy:
We see that in most of the counties there is a linear trend that indicates that if the per capita income is higher, then Biden percentage of votes is higher. However, for example in Georgia or Arizona there is not a relation.  
```{r}
# per_capita:
ggplot(plot_data, aes(x=per_capita_income_2019, y=Biden_2020)) +
  geom_point(alpha=0.8, aes(group=state, colour=win, fill=win),  show.legend = FALSE) + facet_wrap(~state) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = median(plot_data$per_capita_income_2019), linetype="dashed", color="red") + ggtitle("Distribution of per capita income and Biden % of vote") + scale_colour_manual(values = c("#377eb8", "#e41a1c"), name = "win")
```


 

# Classification:  
The first supervised learning technique that we will apply is **Classification**. Inside this group we can find two different approaches: 

- **Probabilistic learning**: Focus is to predict the **probability** of each category and then assigning a class using the Bayes´Rule.

- **Traditional classification**: Aim is to predict the **class** (in this case the winner), *Biden* or *Trump*, in each county.  

## Naive approach:
First we are going to consider a Naive approach before computing any model in order to understand the proportions of each class and to improve later the accuracy.  

```{r}
formattable(prop.table(table(ifelse(data$Biden_2020>0.5, "Biden", "Trump"))))
```

We see that a naive approach would be assigning Trump as a winner in each county and the error would be 16%. Obviously, 84% would be our initial point when maximizing the accuracy. 

- Libraries:
```{r}
library(tidyverse)
library(SnowballC)
library(caret)
library(e1071)
library(naivebayes)
library(Rcpp)
library(MASS)
```

Before starting with the models we need to create a new binary variable, **Win**, that is going to represent whether if the county is won by Biden or Trump. For doing that we simply consider if the votes that Biden received in each county are higher than Trump received, i.e., if Biden percentage is higher than 0.5.  
```{r}
data.class = data
data.class$Win = factor(ifelse(votes$Biden_2020 < votes$Trump_2020, "Trump","Biden"))

levels(data.class$Win)

data.class$Biden_2020 = NULL
```

- Split Train/Test:  
Once we have done that, it is time to divide the data into train and test. In this case, we would use 80% as training and 20% for test.  
```{r}
# split between training and testing sets
idx = createDataPartition(data.class$Win, p = 0.8, list = FALSE)

class.train = data.class[idx,]
class.test = data.class[-idx,]

table(class.train$Win)
```

Also, in order to tune the different hyperparameters we are going to use a *repeated 5 fold cross-validation* of 10 times. This would be sufficient in order to compute the optimal parameter in each model.  
```{r}
control = trainControl(method = "repeatedcv", 
                     repeats = 5,
                     number = 10)
```

- Subsampling techniques:
```{r}
control$sampling = "up"
```


## Naive Bayes classifiers:
First classification models are going to be the **Naive Bayes Classifiers**.  


### Standard version (Gaussian)

```{r}
# model:
NB_fit = train(x = class.train,
                y = class.train$Win,
                method = "naive_bayes",
                trControl = control,
                tuneGrid = data.frame(laplace = seq(0,1,0.2),
                                      usekernel = FALSE,
                                      adjust = FALSE))
NB_fit
```

```{r}
# acc. plot:
plot(NB_fit)
```

- Probability:
Notice probabilities are extreme.
```{r}
NB_prob = predict(NB_fit, newdata=class.test, type = "prob")
rownames(NB_prob)[1:6] = names_county[-idx][1:6]
formattable(head(NB_prob))
```

- Class:
```{r}
NB_pred = predict(NB_fit, newdata = class.test)
head(NB_pred)
```

```{r}
# confusion matrix:
NB_cf = confusionMatrix(NB_pred,class.test$Win)
NB_cf
```

```{r}
# Accuracy:
NB_acc = NB_cf$overall[1]
NB_acc

# Kappa:
NB_kappa = NB_cf$overall[2]
NB_kappa
```

### Multinomial version

Commented due to errors *"In log(params) : Se han producido NaNs"*.  

```{r}
# # model:
# MNB_fit = multinomial_naive_bayes(as.matrix(class.train[,-ncol(class.train)]), class.train$Win, laplace=.6)
```

- Probability:
```{r}
# MNB_prob = predict(MNB_fit, newdata=as.matrix(class.test[,-ncol(class.test)]), type = "prob")
# rownames(MNB_prob)[1:6] = names_county[-idx][1:6]
# formattable(head(MNB_prob))
```

- Class:
```{r}
# MNB_pred = predict(MNB_fit, as.matrix(class.test[,-ncol(class.train)]))
# head(NB_pred)
```

```{r}
# # confusion matrix
# MNB_cf = confusionMatrix(MNB_pred, class.test$Win)
# MNB_cf
```

```{r}
# # Accuracy:
# MNB_acc = MNB_cf$overall[1]
# MNB_acc
# 
# # Kappa:
# MNB_kappa = MNB_cf$overall[2]
# MNB_kappa
```

### Bernoulli version

It produces NAs.  
```{r}
# model:
# BNB_fit = bernoulli_naive_bayes(as.matrix(class.train[,-ncol(class.train)]), class.train$Win, laplace=.3)
```

- Probability:
```{r}
# BNB_prob = predict(BNB_fit, newdata=as.matrix(class.test[,-ncol(class.train)]))
# head(BNB_prob)
```

- Class:
```{r}
# BNB_pred = as.factor(ifelse(BNB_prob > 0.5,"Biden","Trump"))
# head(BNB_pred)
```


```{r}
# confusion matrix
# BNB_cf = confusionMatrix(BNB_pred, class.test$Win)
```

```{r}
# Accuracy:
# BNB_acc = BNB_cf$overall[1]
# BNB_acc
# 
# # Kappa:
# BNB_kappa = BNB_cf$overall[2]
# BNB_kappa
```

### LDA

```{r}
# model:
LDA_fit = train(Win~.,
                data = class.train,
                method = "lda",
                trControl = control,
                preProcess = c("center","scale"))
LDA_fit
```

- Probability:  
```{r}
LDA_prob = predict(LDA_fit, newdata=class.test, "prob")
rownames(LDA_prob)[1:6] = names_county[-idx][1:6]
formattable(head(LDA_prob))
```

- Class:
```{r}
LDA_pred = predict(LDA_fit, newdata=class.test, "raw")
head(LDA_pred)
```

```{r}
# confusion matrix:
LDA_cf = confusionMatrix(LDA_pred,class.test$Win)
LDA_cf
```

```{r}
# Accuracy:
LDA_acc = LDA_cf$overall[1]
LDA_acc

# Kappa:
LDA_kappa = LDA_cf$overall[2]
LDA_kappa
```

### QDA

```{r}
# model:
QDA_fit = train(Win~.,
                data = class.train,
                method = "qda",
                trControl = control,
                preProcess = c("center","scale"))
QDA_fit
```

- Probability:
```{r}
QDA_prob = predict(QDA_fit, newdata=class.test, "prob")
rownames(QDA_prob)[1:6] = names_county[-idx][1:6]
formattable(head(QDA_prob))
```

- Class:
```{r}
QDA_pred = predict(QDA_fit, newdata=class.test, "raw")
head(QDA_pred)
```


```{r}
# confusion matrix:
QDA_cf = confusionMatrix(QDA_pred,class.test$Win)
QDA_cf
```

```{r}
# Accuracy:
QDA_acc = QDA_cf$overall[1]
QDA_acc

# Kappa:
QDA_kappa = QDA_cf$overall[2]
QDA_kappa
```


## Decision Trees:

```{r}
library(rpart)
library(rpart.plot)
```

```{r}
DT_fit = train(Win~., 
                   data = class.train, 
                   method = "rpart",
                   control=rpart.control(minsplit = 5, maxdepth = 5),
                   trControl = control,
                   tuneLength=10)
DT_fit
```


```{r}
rpart.plot(DT_fit$finalModel)
```

- Probability:
```{r}
DT_prob = predict(DT_fit, class.test, type = "prob")
rownames(DT_prob)[1:6] = names_county[-idx][1:6]
formattable(head(DT_prob))
```

- Class:
```{r}
DT_pred = predict(DT_fit, class.test, type = "raw")
head(DT_pred)
```

```{r}
# confusion matrix:
DT_cf = confusionMatrix(DT_pred,class.test$Win)
DT_cf
```

```{r}
# Accuracy:
DT_acc = DT_cf$overall[1]
DT_acc

# Kappa:
DT_kappa = DT_cf$overall[2]
DT_kappa
```

## Random Forest:

```{r}
RF_fit = train(Win ~., 
                  method = "rf", 
                  data = class.train,
                  preProcess = c("center", "scale"),
                  ntree = 100,
                  tuneGrid = expand.grid(mtry=c(1,3,5,7)), 
                  trControl = control)

RF_fit
```

- Probability:
```{r}
RF_prob = predict(RF_fit, newdata=class.test, "prob")
rownames(RF_prob)[1:6] = names_county[-idx][1:6]
formattable(head(RF_prob))
```

- Class:
```{r}
RF_pred = predict(RF_fit, newdata=class.test, "raw")
head(RF_pred)
```

```{r}
# confusion matrix:
RF_cf = confusionMatrix(RF_pred,class.test$Win)
RF_cf
```

```{r}
# acc. plot:
plot(RF_fit)
```

```{r}
# Accuracy:
RF_acc = RF_cf$overall[1]
RF_acc

# Kappa:
RF_kappa = RF_cf$overall[2]
RF_kappa
```


## KNN:

```{r}
library("class")
```

```{r}
# model and evaluate:
KNN_fit = train(Win~.,
                data = class.train,
                method = "knn",
                trControl = control,
                preProcess = c("center","scale"),
                tuneGrid = data.frame(k=seq(1,20,2)))
KNN_fit
```

```{r}
# acc. plot:
plot(KNN_fit)
```

- Probability:
```{r}
KNN_prob = predict(KNN_fit, newdata=class.test, "prob")
rownames(KNN_prob)[1:6] = names_county[-idx][1:6]
formattable(head(KNN_prob))
```

- Class:
```{r}
KNN_pred = predict(KNN_fit, newdata=class.test, "raw")
head(KNN_pred)
```

```{r}
# confusion matrix:
KNN_cf = confusionMatrix(KNN_pred,class.test$Win)
KNN_cf
```

```{r}
# Accuracy:
KNN_acc = KNN_cf$overall[1]
KNN_acc

# Kappa:
KNN_kappa = KNN_cf$overall[2]
KNN_kappa
```


## Logistic Regression:

```{r}
logit_fit = glm(Win~., family=binomial(link="logit"), data=class.train)
summary(logit_fit)
```

- Probability:
```{r}
logit_prob = predict(logit_fit,newdata=class.test, type='response')
head(logit_prob)
```

- Class:
```{r}
logit_pred = as.factor(ifelse(logit_prob > 0.5,"Trump","Biden"))
head(logit_pred)
```

```{r}
logit_cf = confusionMatrix(logit_pred, class.test$Win)
logit_cf
```

```{r}
# Accuracy:
logit_acc = logit_cf$overall[1]
logit_acc

# Kappa:
logit_kappa = logit_cf$overall[2]
logit_kappa
```

## Penalized Logistic Regression:

```{r}
library(glmnet)
```

```{r}
plogit_fit = glmnet(as.matrix(class.train[,-ncol(class.train)]),class.train$Win, family=c("binomial"), alpha=0, lambda=0.01)
plogit_fit
```

- Probability:
```{r}
plogit_prob = predict(plogit_fit,as.matrix(class.test[,-ncol(class.test)]), type="response")
rownames(plogit_prob)[1:6] = names_county[-idx][1:6]
formattable(head(plogit_prob))
```

- Class:
```{r}
plogit_pred = as.factor(ifelse(plogit_prob > 0.5,"Trump","Biden")) # Bayes Rule
head(plogit_pred)
```

```{r}
plogit_cf = confusionMatrix(plogit_pred, class.test$Win)
plogit_cf
```

```{r}
# Accuracy:
plogit_acc = plogit_cf$overall[1]
plogit_acc

# Kappa:
plogit_kappa = plogit_cf$overall[2]
plogit_kappa
```


## SVM:

Notice that it is commented due to time cost.  
```{r}
# SVM_grid = expand.grid(cost = c(1, 2),
#                          Loss = c(0.2, 0.5),
#                          weight = c(0.5, 1))
# 
# SVM_fit = train(Win~.,
#                 data = class.train,
#                 method = "svmLinearWeights2",
#                 trControl = control,
#                 tuneGrid = SVM_grid)
# SVM_fit
# ```
# 
# - Class:
# ```{r}
# SVM_pred = predict(SVM_fit,newdata=class.test, type='raw')
# head(SVM_pred)
# ```
# 
# ```{r}
# SVM_cf = confusionMatrix(SVM_pred, class.test$Win)
# SVM_cf
# ```
# 
# ```{r}
# # Accuracy:
# SVM_acc = SVM_cf$overall[1]
# SVM_acc
# 
# # Kappa:
# SVM_kappa = SVM_cf$overall[2]
# SVM_kappa
```


## Gradient Boosting:

```{r}
library(gbm)
```

```{r}
GBM_grid = expand.grid(
  interaction.depth = seq(1,10,2),
  n.trees = seq(1,10,2),
  shrinkage = seq(0,1,0.5),
  n.minobsinnode = 1
)
```

```{r results='hide'}
GBM_fit = train(Win ~., 
                data=class.train,
                method="gbm",
                distribution ="bernoulli",
                trControl=control,
                tuneGrid=GBM_grid)
```


```{r}
GBM_fit
```

- Probability:
```{r}
GBM_prob = predict(GBM_fit, newdata=class.test, n.trees=250, type="prob")
rownames(GBM_prob)[1:6] = names_county[-idx][1:6]
formattable(head(GBM_prob))
```

- Class:
```{r}
GBM_pred = predict(GBM_fit, newdata=class.test, n.trees=250, type="raw")
head(GBM_pred)
```

```{r}
GBM_cf = confusionMatrix(GBM_pred, class.test$Win)
GBM_cf
```

```{r}
# Accuracy:
GBM_acc = GBM_cf$overall[1]
GBM_acc

# Kappa:
GBM_kappa = GBM_cf$overall[2]
GBM_kappa
```

```{r}
GBM_imp = varImp(GBM_fit, scale = F)
plot(GBM_imp, scales = list(y = list(cex = .95)))
```


## Extreme Gradient Boosting:

Commented due to time cost.  
```{r}
# XGBM_grid = expand.grid(
#   nrounds = c(500,1000),
#   eta = c(0.01, 0.001),
#   max_depth = c(2, 4),
#   gamma = 1,
#   colsample_bytree = c(0.2, 0.4),
#   min_child_weight = c(1,5),
#   subsample = 1
# )
```

```{r}
# XGBM_fit = train(Win ~.,
#                 data=class.train,
#                 method="xgbTree",
#                 trControl=control,
#                 tuneGrid=XGBM_grid)
# 
# XGBM_fit
```

- Probability:
```{r}
# XGBM_prob = predict(XGBM_fit, newdata=class.test, n.trees=250, type="prob")
# rownames(XGBM_prob)[1:6] = names_county[-idx][1:6]
# formattable(head(XGBM_prob))
```

- Class:
```{r}
# XGBM_pred = predict(XGBM_fit, newdata=class.test, n.trees=250, type="raw")
# head(XGBM_pred)
```

```{r}
# XGBM_cf = confusionMatrix(XGBM_pred, class.test$Win)
# XGBM_cf
```

```{r}
# # # Accuracy:
# XGBM_acc = XGBM_cf$overall[1]
# XGBM_acc
# 
# # Kappa:
# XGBM_kappa = XGBM_cf$overall[2]
# XGBM_kappa
```

```{r}
# XGBM_imp = varImp(XGBM_fit, scale = F)
# plot(XGBM_imp, scales = list(y = list(cex = .95)))
```


## Cost Sensitive Learning:
A brief comment about risk learning is that in this particular case it is not useful because we are dealing with elections data. However, in order to put on practice **Cost Sensitive Learning** we would make an example with an invented context.  

We would imagine that the runner-up party in the 2016 elections (Democrat Party) want to win the 2020 elections. For this reason, his objective is to reduce the extra cost spent in electoral campaign in each county.   

For this reason, we assume that the extra money spend in a county that is more probable to Biden to win is 0, in a county that is probable to be lost by Biden, we assume that Biden wants to win it so the cost in campaign will be higher. Then, we came up with this cost matrix:  


| Prediction/Reference | Biden | Trump  |
| -------------------- | -----:| ---------:|
| predicted Biden                |  0 |  5  |
| predicted Trump             |   2 |     3  |

(Notice that numbers are expressed in *million of US dollars, $*)

```{r}
cost_unit = c(0,2,5,3)
```


- Naive version:
In this case we would consider a **naive** approach consisting on assigning Trump as a winner in each county (as done previously). We see the cost is 2.36 million $. This would be an initial point in order to reduce costs.  
```{r}
cost_naive = prop.table(table(ifelse(data$Biden_2020>0.5, "Biden", "Trump")))
cost = 0.16*cost_unit[1] + 0.84*cost_unit[4]
cost
```

- Selecting best threshold:
```{r}
cost.i = matrix(NA, nrow = 50, ncol = 10)
# 20 replicates for training/testing sets for each of the 10 values of threshold

j = 0
for (threshold in seq(0.46,0.55,0.01)){
  
  j = j + 1
  cat(j)
  for(i in 1:50){
    
    d = createDataPartition(class.train$Win, p = 0.8, list = FALSE)
    train = class.train[d,]
    test  = class.train[-d,]  
    
    lrFit = train(Win ~ ., data=train, method = "glmnet",
                   tuneGrid = data.frame(alpha = 0.3, lambda = 0), preProcess = c("center", "scale"),
                   trControl = trainControl(method = "none", classProbs = TRUE))
    
    lrProb = predict(lrFit, test, type="prob")
    lrPred = rep("Trump", nrow(test))
    lrPred[which(lrProb[,1] > threshold)] = "Biden"
    
    CM = confusionMatrix(factor(lrPred), test$Win)$table
    cost = sum(as.vector(CM)*cost_unit)/sum(CM)
    cost
    
    cost.i[i,j] = cost
    
  }
}

# threshold boxplot:
boxplot(cost.i, main = "Threshold selection",
        ylab = "unit cost",
        xlab = "threshold value",
        names = seq(0.46,0.55,0.01),col="#377eb8",las=2)

# median values:
apply(cost.i, 2, median)
```

We see that an optimal threshold can be 0.47 or 0.48, taking into account that we are optimistic enough to consider that Biden would have an advantage that would make him win counties given that his probabilities to win is higher not that 50% but 47 or 48%.

*(Again, notice that in this case we should apply directly Bayes´ Rule. However, for learning purposes this section was included)*    

## Risk learning:

Now, we can take a look at the counties. We see that a possible **value** could be computing roughly the votes for Biden in each county. Hence, we should target counties with highest votes.  

Votes for Biden in each county:
```{r}
int_counties = round(test$pop_2019*lrProb[,1])
ggplot() + geom_histogram(aes(x=log(int_counties)),fill="#377eb8") + labs(x = "votes for Biden", y = "number of counties", title = "Interesting counties for Biden")
```

We should focus on counties with votes to Biden greater to the mean value for votes. This would make us to center our attention to counties with more population and consequently, more potential votes.     

```{r}
sum(int_counties>mean(int_counties))/length(int_counties)
```

Let´s see this counties:
```{r}
sort.int = sort(int_counties,decreasing=T,index.return=T)

head(names_county[sort.int$ix], 10)
```

Now that we know the most important counties in term of amount of votes, we can now set an strategy for the parties to follow: "increase electoral campaign costs and offer good promises in these counties". 


## Best Models:
Once we have computed the classification models, we can choose the one that we consider fits best with our targets in the project.  


For easier understanding it is considered a table with the values of accuracy and kappa for each model.
```{r}
models = c("Gaussian Naive Bayes", "LDA", "QDA", "DT", "Random Forest", "KNN", "Logistic Regression", "Penalized Logistic Regression","Gradient Boosting")
models_acc = c(NB_acc, LDA_acc, QDA_acc, DT_acc, RF_acc, KNN_acc, logit_acc, plogit_acc, GBM_acc)
models_kappa = c(NB_kappa, LDA_kappa, QDA_kappa, DT_kappa, RF_kappa, KNN_kappa, logit_kappa, plogit_kappa, GBM_kappa)
df_class = data.frame(model=models, acc=models_acc, kappa=models_kappa)

formattable(df_class, list(
  acc = color_tile("transparent", "lightgreen"),
  kappa = color_tile("transparent", "lightgreen")))
```

> Top 3 models: NB, Logistic Regression & Penalized Logistic Regression


## Ensemble:

```{r}
getmode = function(v) {
   uniqv = unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
```


```{r}
ensemble_pred = apply(data.frame(NB_pred, logit_pred, plogit_pred), 1, getmode)
head(ensemble_pred)
```

```{r}
ensemble_cf = confusionMatrix(factor(ensemble_pred), class.test$Win)
ensemble_cf
```

- Accuracy:  
```{r}
paste("Final Accuracy:", ensemble_cf$overall[1])
```



Finally, we select the predictions of that model in order to go deeper into its interpretation.   
```{r}
# select best model:
prediction = ensemble_pred
```


### Prediction counties:
First, we are going to consider only the **predicted** counties and compare them with the **actual** value. For doing that, we are going to use the US map.  
```{r}
# pred:
df3 = data.frame(winner=prediction, fips=fips_county[-idx])
p3 = plot_usmap(data=df3, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Predicted winner")

# actual:
df4 = data.frame(winner=data.class$Win[-idx], fips=fips_county[-idx])
p4 = plot_usmap(data=df4, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Actual winner")

# plot:
ggarrange(p3,p4, ncol=2, nrow=1)

# counties that changed:
df4_flip = df4
levels(df4_flip$winner) = c("Biden", "Trump", "Flip")

df4_flip$winner[which(df3$winner!=df4$winner)] = "Flip"
plot_usmap(data=df4_flip, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c", "#b6b164"), name = "Flipped counties") + labs(title = "Flipped counties")
```

### Predictions with all counties:
In order to have a more general view, now we are going to add the remaining counties.  
```{r}
# pred:
win_pred = data.class$Win
win_pred[-idx] = prediction

df7 = data.frame(winner = win_pred, fips=fips_county)
p7 = plot_usmap(data=df7, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Predicted winner")

# actual:
df8 = data.frame(winner = data.class$Win, fips=fips_county)
p8 = plot_usmap(data=df8, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Actual winner")

# plot:
ggarrange(p7,p8, ncol=2, nrow=1)

# counties that changed:
df8_flip = df8
levels(df8_flip$winner) = c("Biden", "Trump", "Flip")

df8_flip$winner[which(df7$winner!=df8$winner)] = "Flip"
plot_usmap(data=df8_flip, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c", "#b6b164"), name = "Flipped counties") + labs(title = "Flipped counties")
```


### Winner per state:
Another interesting thing to consider is scaling to the **state** level. In this case, we will follow the *majority vote* approach by using the **mode** in order to determine the candidate that won the most number of counties in each state.  
```{r}
# actual:
df6 = data.frame(votes = votes$Biden_2020, total = votes$Total_2020, other = votes$Other_2020, state=states_county)  %>% group_by(state)  %>%
  summarise(Biden = sum(votes)/sum(total), Other = sum(other)/sum(total))

df6$Trump = 1-df6$Biden-df6$Other
df6$winner = factor(ifelse(df6$Biden < df6$Trump, "Trump","Biden"))
```

```{r}
# pred:
class_pred = data.class$Win
class_pred[-idx] = prediction

df1 = data.frame(state=states_county, winner=class_pred) %>% group_by(state) %>% summarise(winner=getmode(winner))
p1 = plot_usmap(data=df1, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Predicted winner")


# plots:
ggarrange(p7,p1,ncol=2,nrow=1)
```


### Interpretation:
We have got the predictions but now we need to give them some interpretation in order to understand the outcomes. For doing so, we would consider some key variables as profiles.  

```{r}
detach("package:MASS", unload = TRUE)
```

#### Population vs Winner:
First we are going to consider the population.  
```{r}
t1 = votes %>% select(1,4) %>% mutate(winner=df7$winner)
```

We can see that according to the Degree of Urbanization in the US (https://blogs.worldbank.org/sustainablecities/how-do-we-define-cities-towns-and-rural-areas
), we can identify three types of settlements:

- Cities, which have a population of at least 50,000 inhabitants in contiguous dense grid cells (>1,500 inhabitants per km2).

- Towns and semi-dense areas, which have a population of at least 5,000 inhabitants in contiguous grid cells with a density of at least 300 inhabitants per km2.

- Rural areas, which consist mostly of low-density grid cells.

Taking into account this we can make a scatterplot with the population and percentage of vote for Biden in each state. We see that in counties with more population Biden wins.  
```{r}
ggplot(t1, aes(x=Total_2020, y=plot_data$Biden_2020)) +
  geom_point(alpha=0.8, aes(group=state, colour=winner, fill=winner),  show.legend = FALSE) + facet_wrap(~state) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = median(plot_data$pop_2019), linetype="dashed", color="red") + ggtitle("Distribution of population and Biden % of vote per State") + labs(x="Total population", y="Biden % of vote") + scale_colour_manual(values = c("#377eb8", "#e41a1c"), name = "winner") + theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
        axis.text.x = element_text(angle = 90, hjust = 1))
```


We can check it also by using a density plot (using log of the population):
```{r}
ggplot() +
  geom_density(aes(x=log(t1$Total_2020[which(t1$winner=="Biden")])),color="#377eb8", fill="#377eb8", alpha=0.8) + geom_density(aes(x=log(t1$Total_2020[which(t1$winner=="Trump")]), y = -..density..),color="#e41a1c",  fill="#e41a1c", alpha=0.8) + ggtitle("Density of counties by population and winner") + labs(x="log(Total_2020)", y="density")
```


Another thing that we can consider is taking the most populous city in each state and see if we can derive something by looking at the predicted winner in the US map. We see that in almost every highlighted city the winner is Biden (something we have checked also before). But the interesting thing is to consider isolated blue counties, such as *Nashville*. This phenomenon is predominant in Middle Region of USA, and this is because it is mainly full of rural areas, where the tendency is to vote red. The isolated blue counties are no more than the most populous cities in each state. This also explains why in some cases there are clusters of isolated blue counties, and this is due to the fact that this counties represent suburbs or even divisions of that high population cities.  

```{r fig.height = 11, fig.width = 10}
p_cities = plot_usmap(data=df7, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Predicted winner") + ggrepel::geom_label_repel(data = cities_t,
             aes(x = lon.1, y = lat.1, label = most_populous_city),
             size = 3, alpha = 0.8,
             label.r = unit(0.5, "lines"), label.size = 0.5,
             segment.color = "red", segment.size = 1,
             seed = 1002) +
  geom_point(data = cities_t,
             aes(x = lon.1, y = lat.1, size = city_pop),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Most Populous City in Each US State and Predicted Winner in Each US County",
       size = "City Population") +
  theme(legend.position = "none")

p_cities
```

#### Ethnic vs winner:
Another interesting profile to take a look is the ethnic of the voters. As a first thought we could say that non-white people tend to vote less to the Republican candidate. Let´s see.  
```{r}
plot_usmap(data=df7, values = "winner") + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Predicted winner")
```

One explication for the victory of Biden in some counties of the South is that the distribution of black population there is high, as we can see in the plot. We can also confirm it by looking at the white population in each county. We see that it is not that important in states of the Northeast, where Biden won.  
```{r fig.width = 10}
df9 = data.class %>% mutate(fips=fips_county) %>% select(fips, black_2019)
df10 = data.class %>% mutate(fips=fips_county) %>% select(fips, white_2019)

p9 = plot_usmap(data=df9, values = "black_2019") + labs(title = "Black vote distribution per county", fill="% Black pop.")
p10 = plot_usmap(data=df10, values = "white_2019") + labs(title = "White vote distribution per county", fill="% White pop.")

ggarrange(p9, p10, ncol = 2, nrow = 1)
```

### Winner elections:
It has no sense to consider here (classification) the winner of an election.



# Regression:  

- Split train/test:  

Before starting the models we need to first prepare the data in order to divide it into *train* and *test* datasets.
```{r}
idx = createDataPartition(data$Biden_2020, p = 0.8, list = FALSE)
data.train = data[idx,]
data.test = data[-idx,]
summary(data.train)
```

- Biden Percentage Distribution in Training:  
```{r}
ggplot(data.train) + geom_density(aes(x=Biden_2020),color="#377eb8", fill="#377eb8", alpha=0.8)
```




## Benchmark model:

First, we are going to consider a **Benchmark model**. We would predict the percentage of vote for Biden in each county with the average percentage of Biden.  
```{r}
paste("Av. percentage of Biden:", mean(data.train$Biden_2020))

# This is equivalent to
benchFit = lm(Biden_2020 ~ 1, data=data.train)
summary(benchFit)
predictions = predict(benchFit, newdata=data.test)
cor(data.test$Biden_2020, predictions)^2
```

- MSE:
```{r}
bench_RMSE = sqrt(mean((predictions - data.test$Biden_2020)^2))
bench_RMSE
```



## Model Selection:

In order to preselect a model we need to take into account the correlations between the variables and the target, **Biden_2020**:  

```{r}
corr_votes = sort(cor(data)["Biden_2020",], decreasing = T)
corr=data.frame(corr_votes)
corr = corr %>% mutate(winner=factor(ifelse(corr_votes<0, "Red", ifelse(corr_votes==1, "-", "Blue"))))

ggplot(corr,aes(x = row.names(corr), y = corr_votes)) + 
  geom_bar(stat = "identity", aes(group=winner, colour=winner, fill=winner), alpha=0.8, show.legend = F) + 
  scale_x_discrete(limits= row.names(corr)) +
  labs(x = "", y = "Biden_2020", title = "Correlations with Biden perc. of vote") + 
  theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
        axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values = c("#b6b164","#377eb8","#e41a1c"), name = "winner") + scale_colour_manual(values = c("#b6b164","#377eb8","#e41a1c"), name = "winner")
```

Now, we can have an idea of the most important variables. However, we could also use the **olsrr** library in order to select the most relevant variables.  

```{r}
library(olsrr)
```

```{r}
regFit = lm(Biden_2020 ~., data.train)

ols_step_forward_aic(regFit) 
plot(ols_step_forward_aic(regFit))
```

We see that we could take 7 variables in order to fit the Regression models.  


- Matrix:  
We create a data frame to store the actual values and the predictions of the different models:  
```{r}
test_results = data.frame(Biden = data.test$Biden_2020)
```

- Control:
```{r}
ctrl = trainControl(method = "repeatedcv", 
                     number = 5, repeats = 10)
```


## Linear Regression:

- Hyper-parameters:  
```{r}
modelLookup('lm')
```

- Train:  
```{r}
mod_lm = Biden_2020 ~ white_2019 + bachelors_2019 + lat + hispanic_2019 + age_over_18_2019 + other_single_race_2019 + uninsured_2019 + lon

lm_Fit = train(mod_lm, data = data.train, 
                 method = "lm", 
                 preProc=c('scale', 'center'),
                 trControl = ctrl)
lm_Fit
```

- Predict:
```{r}
test_results$lm = predict(lm_Fit, data.test)
postResample(pred = test_results$lm,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$lm)
test_results$lm[which(test_results$lm<0)] = min(test_results$Biden)
postResample(pred = test_results$lm,  obs = test_results$Biden)
```


- R-Square:
```{r}
lm_R2 = cor(data.test$Biden_2020, test_results$lm)^2
paste("R^2 in testing set:", lm_R2)
```

- MSE:
```{r}
lm_RMSE = sqrt(mean((test_results$lm - data.test$Biden_2020)^2))
lm_RMSE
```

- Visualization:  
```{r}
qplot(x=test_results$lm, y=test_results$Biden) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```


## Overfitted Linear Regression

- Train:
```{r}
mod_olm = Biden_2020 ~ white_2019 + bachelors_2019 + hispanic_2019 + age_over_18_2019 + other_single_race_2019 + uninsured_2019*unemployment_rate_2019 + lon*lat + black_2019*native_2019

olm_Fit = train(mod_olm, data = data.train, 
                  method = "lm", 
                  preProc=c('scale', 'center'),
                  trControl = ctrl)
summary(olm_Fit)
```

- Predict
```{r}
test_results$olm = predict(olm_Fit, data.test)
postResample(pred = test_results$olm,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$olm)
test_results$olm[which(test_results$olm<0)] = min(test_results$Biden)
postResample(pred = test_results$olm,  obs = test_results$Biden)
```

- R-Square:
```{r}
olm_R2 = cor(data.test$Biden_2020, test_results$olm)^2
paste("R^2 in testing set:", olm_R2)
```

- MSE:
```{r}
olm_RMSE = sqrt(mean((test_results$olm - data.test$Biden_2020)^2))
olm_RMSE
```

- Visualization
```{r}
qplot(test_results$olm, test_results$Biden) + 
  labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```

## Forward regression

- Hyper-parameters:  
```{r}
modelLookup('leapForward')
```

- Train:  
```{r}
mod_flm = Biden_2020 ~ white_2019 + bachelors_2019 + hispanic_2019 + age_over_18_2019 + other_single_race_2019 + uninsured_2019 + lon*lat
flm_Fit = train(mod_flm, data = data.train, 
                  method = "leapForward", 
                  preProc=c('scale', 'center'),
                  tuneGrid = expand.grid(nvmax = 3:9),
                  trControl = ctrl)

flm_Fit
plot(flm_Fit)
```

- Variables:
```{r}
coef(flm_Fit$finalModel, flm_Fit$bestTune$nvmax)
```

- Predict:  
```{r}
test_results$flm = predict(flm_Fit, data.test)
postResample(pred = test_results$flm,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$flm)
test_results$flm[which(test_results$flm<0)] = min(test_results$Biden)
postResample(pred = test_results$flm,  obs = test_results$Biden)
```

- R-Square:
```{r}
flm_R2 = cor(data.test$Biden_2020, test_results$flm)^2
paste("R^2 in testing set:", flm_R2)
```

- MSE:
```{r}
flm_RMSE = sqrt(mean((test_results$flm - data.test$Biden_2020)^2))
flm_RMSE
```

- Visualization:  
```{r}
qplot(test_results$flm, test_results$Biden) + 
  labs(title="Forward Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```


## Backward Regression

- Hyper-parameters:  
```{r}
modelLookup('leapBackward')
```

- Train:  
```{r}
mod_blm = mod_flm
back_Fit = train(mod_blm, data = data.train, 
                   method = "leapBackward", 
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 3:8),
                   trControl = ctrl)
back_Fit
plot(back_Fit)
```

- Selected Variables:
```{r}
coef(back_Fit$finalModel, back_Fit$bestTune$nvmax)
```

- Predictions:  
```{r}
test_results$blm = predict(back_Fit, data.test)
postResample(pred = test_results$blm,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$blm)
test_results$blm[which(test_results$blm<0)] = min(test_results$Biden)
postResample(pred = test_results$blm,  obs = test_results$Biden)
```

- MSE:
```{r}
blm_RMSE = sqrt(mean((test_results$blm - data.test$Biden_2020)^2))
blm_RMSE
```

- R-Square:
```{r}
blm_R2 = cor(data.test$Biden_2020, test_results$blm)^2
paste("R^2 in testing set:", blm_R2)
```

- Visualization:  
```{r}
qplot(test_results$blm, test_results$Biden) + 
  labs(title="Backward Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```

## Stepwise Regression:  

- Hyper-parameters:  
```{r}
modelLookup('leapSeq')
```

- Train:  
```{r}
mod_slm = mod_flm
step_Fit = train(mod_slm, data = data.train, 
                   method = "leapSeq", 
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 3:10),
                   trControl = ctrl)
step_Fit
plot(step_Fit)
```

- Selected Variables:
```{r}
coef(step_Fit$finalModel, step_Fit$bestTune$nvmax)
```

- Predictions:  
```{r}
test_results$slm = predict(step_Fit, data.test)
postResample(pred = test_results$slm,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$slm)
test_results$slm[which(test_results$slm<0)] = min(test_results$Biden)
postResample(pred = test_results$slm,  obs = test_results$Biden)
```

- MSE:
```{r}
slm_RMSE = sqrt(mean((test_results$slm - data.test$Biden_2020)^2))
slm_RMSE
```

- R-Square:
```{r}
slm_R2 = cor(data.test$Biden_2020, test_results$slm)^2
paste("R^2 in testing set:", slm_R2)
```

- Visualization:  
```{r}
qplot(test_results$blm, test_results$Biden) + 
  labs(title="Stepwise Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```


## Ridge Regression:

```{r}
library(elasticnet)
```

- Hyper-parameters:  
```{r}
modelLookup('ridge')
```

- Train:  
```{r}
mod_ridge = mod_flm
ridge_Fit = train(mod_ridge, data = data.train,
                    method='ridge',
                    preProc=c('scale','center'),
                    tuneGrid = expand.grid(lambda = seq(0, .1, length = 100)),
                    trControl=ctrl)
ridge_Fit
plot(ridge_Fit)
```

- Predictions:  
```{r}
test_results$ridge = predict(ridge_Fit, data.test)
postResample(pred = test_results$ridge,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$ridge)
test_results$ridge[which(test_results$ridge<0)] = min(test_results$Biden)
postResample(pred = test_results$ridge,  obs = test_results$Biden)
```

- R-Square:
```{r}
ridge_R2 = cor(data.test$Biden_2020, test_results$ridge)^2
paste("R^2 in testing set:", ridge_R2)
```

- MSE:
```{r}
ridge_RMSE = sqrt(mean((test_results$ridge - data.test$Biden_2020)^2))
lm_RMSE
```

- Visualization:  
```{r}
qplot(test_results$ridge, test_results$Biden) + 
  labs(title="Ridge Regression Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```


## The Lasso:  

- Hyper-parameters:  
```{r}
modelLookup('lasso')
```

- Train:  
```{r}
mod_lasso = mod_flm
lasso_Fit = train(mod_lasso, data = data.train,
                    method='lasso',
                    preProc=c('scale','center'),
                    tuneGrid = expand.grid(fraction = seq(.01, 1, length = 100)),
                    trControl=ctrl)
lasso_Fit
plot(lasso_Fit)
```

- Predictions:  
```{r}
test_results$lasso = predict(lasso_Fit, data.test)
postResample(pred = test_results$lasso,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$lasso)
test_results$lasso[which(test_results$lasso<0)] = min(test_results$Biden)
postResample(pred = test_results$lasso,  obs = test_results$Biden)
```

- R-Square:
```{r}
lasso_R2 = cor(data.test$Biden_2020, test_results$lasso)^2
paste("R^2 in testing set:", lasso_R2)
```

- MSE:
```{r}
lasso_RMSE = sqrt(mean((test_results$lasso - data.test$Biden_2020)^2))
lasso_RMSE
```

- Visualization:  
```{r}
qplot(test_results$lasso, test_results$Biden) + 
  labs(title="Lasso Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```

## Elastic Net:  

- Hyper-parameters:  
```{r}
modelLookup('glmnet')
```

- Train:  
```{r}
mod_glmnet = mod_flm
glmnet_Fit = train(mod_glmnet, data = data.train,
                    method='glmnet',
                    preProc=c('scale','center'),
                    tuneGrid = expand.grid(alpha = seq(0, .2, 0.01), lambda = seq(0, .1, 0.01)),
                    trControl=ctrl)
glmnet_Fit
plot(glmnet_Fit)
```

- Predictions:  
```{r}
test_results$glmnet = predict(glmnet_Fit, data.test)
postResample(pred = test_results$glmnet,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$glmnet)
test_results$glmnet[which(test_results$glmnet<0)] = min(test_results$Biden)
postResample(pred = test_results$glmnet,  obs = test_results$Biden)
```

- R-Square:
```{r}
glmnet_R2 = cor(data.test$Biden_2020, test_results$glmnet)^2
paste("R^2 in testing set:", glmnet_R2)
```

- MSE:
```{r}
glmnet_RMSE = sqrt(mean((test_results$glmnet - data.test$Biden_2020)^2))
glmnet_RMSE
```

- Visualization:  
```{r}
qplot(test_results$glmnet, test_results$Biden) + 
  labs(title="glmnet Observed VS Predicted", x="Predicted", y="Observed") +
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  theme_bw()
```


## KNN:

```{r}
library(kknn)
```

- Hyper-parameters:  
```{r}
modelLookup('kknn')
```

- Train:  
```{r}
mod_knn = mod_flm
knn_Fit = train(mod_knn, 
                  data = data.train,
                  method = "kknn",   
                  preProc=c('scale','center'),
                  tuneGrid = data.frame(kmax=c(11,13,15,19,21),distance=2,kernel='optimal'),
                  trControl = ctrl)
summary(knn_Fit)
plot(knn_Fit)
```

- Predictions:  
```{r}
test_results$knn = predict(knn_Fit, data.test)
postResample(pred = test_results$knn,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$knn)
```

- R-Square:
```{r}
knn_R2 = cor(data.test$Biden_2020, test_results$knn)^2
paste("R^2 in testing set:", knn_R2)
```

- MSE:
```{r}
knn_RMSE = sqrt(mean((test_results$knn - data.test$Biden_2020)^2))
knn_RMSE
```

## Random Forest:  

- Hyper-parameters:  
```{r}
modelLookup('rf')
```

- Train:  
```{r}
mod_rf = mod_flm
rf_Fit = train(mod_rf, 
                 data = data.train,
                 method = "rf",
                 preProc=c('scale','center'),
                 trControl = ctrl,
                 ntree = 100,
                 tuneGrid = data.frame(mtry=c(1,3,5,7)),
                 importance = TRUE)
rf_Fit
plot(rf_Fit)
```

- Predictions:  
```{r}
test_results$rf = predict(rf_Fit, data.test)
postResample(pred = test_results$rf,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$rf)
```

- R-Square:
```{r}
rf_R2 = cor(data.test$Biden_2020, test_results$rf)^2
paste("R^2 in testing set:", rf_R2)
```

- MSE:
```{r}
rf_RMSE = sqrt(mean((test_results$rf - data.test$Biden_2020)^2))
rf_RMSE
```

- Variable importance:  
```{r}
plot(varImp(rf_Fit, scale = F), scales = list(y = list(cex = .95)))
```


## Gradient Boosting:  

- Hyper-parameters:  
```{r}
modelLookup('gbm')
```

- Train:  
```{r}
mod_gb = mod_flm
gb_Fit = train(mod_gb, 
                data=data.train,
                method="gbm",
                trControl=ctrl,
                tuneGrid=expand.grid(interaction.depth = seq(1,10,2), 
                                     n.trees = seq(1,10,2), 
                                     shrinkage = seq(0,1,0.5), 
                                     n.minobsinnode = 1))
gb_Fit
```

- Predictions:  
```{r}
test_results$gb = predict(gb_Fit, data.test)
postResample(pred = test_results$gb,  obs = test_results$Biden)
```

- Check values:
If the value is lower than 0, then we assign the minimum value.
If the value is greater than 1, then we assign the maximum value.

```{r}
summary(test_results$gb)
```

- R-Square:
```{r}
gb_R2 = cor(data.test$Biden_2020, test_results$gb)^2
paste("R^2 in testing set:", gb_R2)
```

- MSE:
```{r}
gb_RMSE = sqrt(mean((test_results$gb - data.test$Biden_2020)^2))
gb_RMSE
```

## Best Models:

```{r}
formattable(head(test_results))
```

Once we have computed the classification models, we can choose the one that we consider fits best with our targets in the project. For easier understanding it is considered a table with the values of accuracy and kappa for each model.  
```{r}
models = c("Benchmark", "LM", "OLM", "FLM", "BLM", "SLM", "Ridge", "Lasso", "GLMnet","KNN", "RF", "GB")
models_MSE = c(bench_RMSE, lm_RMSE, olm_RMSE, flm_RMSE, blm_RMSE, slm_RMSE, ridge_RMSE, lasso_RMSE, glmnet_RMSE, knn_RMSE, rf_RMSE, gb_RMSE)
models_R2= c(NA, lm_R2, olm_R2, flm_R2, blm_R2, slm_R2, ridge_R2, lasso_R2, glmnet_R2, knn_R2, rf_R2, gb_R2)

df_class = data.frame(model=models, R2=models_R2, RMSE=models_MSE)
formattable(df_class, list(
  R2 = color_tile("transparent", "lightgreen"),
  RMSE = color_tile("lightgreen", "#db7070")))
```

> Top 3 models: Gradient Boosting (GB), KNN & Random Forest (RF)  


## Ensemble:  

We select the best three models and combine the results by applying the mean:  
```{r}
# combination:
test_results$comb = (test_results$gb + test_results$knn + test_results$rf)/3
postResample(pred = test_results$comb,  obs = test_results$Biden)
```

### Final predictions:

```{r}
# density plot predicted values vs actual:
ggplot() + geom_density(aes(x=test_results[,ncol(test_results)], colour="pred")) + geom_density(aes(x=test_results[,1], colour="actual")) + labs(colour="Type", x="% vote") + scale_colour_manual(values=c("blue", "red"))
```


#### Prediction Intervals:
In order to complete and to set confidence in the predictions we can compute the predictions intervals as follows:  

- Error:
```{r}
error = test_results$Biden - test_results$comb
ggplot() + geom_density(aes(x=error, colour="Error")) + labs(colour="Type") + scale_colour_manual(values=c("orange"))
```

```{r}
noise = error[1:150]
```

- Bounds with 90% confidence:

```{r}
LB = test_results$comb[151:length(test_results$comb)] + quantile(noise,0.05, na.rm=T)
UB = test_results$comb[151:length(test_results$comb)] + quantile(noise,0.95, na.rm=T)
```

- Performance
```{r}
pred_int = data.frame(actual=test_results$Biden[151:length(test_results$Biden)], pred=test_results$comb[151:length(test_results$comb)], LB=LB, UB=UB)
pred_int = pred_int %>% mutate(out=factor(if_else(actual<LB | actual>UB,1,0)))
```

We see that around 13% of the observations are out of the interval, which indicates that we could be happy about the performance of the predictions.  
```{r}
mean(pred_int$out==1)
```

- Graph:
```{r}
ggplot(pred_int, aes(x=pred, y=actual))+
  geom_point(aes(color=out)) + theme(legend.position="none") +
  xlim(0, 1) + ylim(0, 1)+
  geom_ribbon(data=pred_int,aes(ymin=LB,ymax=UB),alpha=0.3) + geom_hline(yintercept = 0.50 , linetype="dashed", color="red") + geom_vline(xintercept = 0.50 , linetype="dashed", color="red") +
  labs(title = "Prediction intervals", x = "Predicted Biden %",y="Actual Biden %")

```
In the graph we can see a ribbon that represents the confidence interval. What it is important is that the counties (points) that are in the first window *(x<0.50 and y>0.50)* are predicted as a loss for Biden but actually they were the opposite. In the fourth window *(x>0.50 and y<0.50)* are the ones that were predicted as a win for Biden but actually did not win.  

### Conversion to votes:

Now, that we have the percentage of vote for Biden in each state, we need to transform it to votes for each candidate. This would allow us to later compute the winner per state and, eventually, the winner of the elections.   
```{r}
# transform predictions that are in percentage to votes:
predictions = test_results$comb * votes$Total_2020[-idx]
```

```{r}
# all votes per county but using predictions:
votes_pred = votes$Biden_2020
votes_pred[-idx] = predictions
```


### States
```{r}
# pred:
df5 = data.frame(votes = votes_pred, total = votes$Total_2020, other = votes$Other_2020, state=states_county)  %>% group_by(state)  %>%
  summarise(Biden = sum(votes)/sum(total), Other = sum(other)/sum(total))

# actual:
df6 = data.frame(votes = votes$Biden_2020, total = votes$Total_2020, other = votes$Other_2020, state=states_county)  %>% group_by(state)  %>%
  summarise(Biden = sum(votes)/sum(total), Other = sum(other)/sum(total))
```

```{r}
# pred:
df5$Trump = 1-df5$Biden-df5$Other
df5$winner = factor(ifelse(df5$Biden < df5$Trump, "Trump","Biden"))
p5 = plot_usmap(data=df5, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Predicted winner")

# actual:
df6$Trump = 1-df6$Biden-df6$Other
df6$winner = factor(ifelse(df6$Biden < df6$Trump, "Trump","Biden"))
p6 = plot_usmap(data=df6, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Actual winner")

# plot:
ggarrange(p5, p6, ncol = 2, nrow = 1)
```

### Winner elections:

```{r}
seats = read.csv("seats2020.csv")
colnames(seats) = c("state", "reps", "peoplePerElector", "Pop")
seats = seats %>% select(1,2)

# pred:
df5 = left_join(df5, seats)
df5$reps[which(df5$state=="District of Columbia")] = 3

president_pred = df5 %>% group_by(winner) %>% summarise(seats = sum(reps))
president_pred[2,2] = president_pred[2,2] + 3 # assume trump wins Alaska

# actual:
df6 = full_join(df6, seats)
df6$reps[which(df6$state=="District of Columbia")] = 3

# actual winner:
president = df6 %>% group_by(winner) %>% summarise(seats = sum(reps))
president[2,2] = president[2,2] + 3 # assume trump wins Alaska
president = na.omit(president)

# pred winner:
president_pred = cbind(president_pred, president[,2], president_pred[,2]-president[,2])
colnames(president_pred)[2] = "pred_seats"
colnames(president_pred)[3] = "actual_seats"
colnames(president_pred)[4] = "change"

# both:
formattable(president_pred, list(
  winner = formatter("span", style = ~ style(color = ifelse(pred_seats>270, "green", "red"))),
  pred_seats = formatter("span", style = ~ style(color = ifelse(pred_seats>270, "green", "red"))),
  actual_seats = formatter("span", style = ~ style(color = ifelse(actual_seats>270, "green", "red"))),
  change = formatter("span", style = x ~ style(font.weight = "bold", color = ifelse(president_pred$change > 0, "green", ifelse(president_pred$change  < 0, "red", "black"))), x ~ icontext(ifelse(president_pred$change >0, "arrow-up", "arrow-down"), president_pred$change))
))
```




# Election Simulation:
As an extra exercise and because it would enrich the project I have considered an election simulator using regression, that would estimate first the percentage of vote for Biden in each county, then using the total votes we could recover the votes for Biden in that county and compute the total votes in each state for both Trump and Biden. Finally, we would see who wins in each state and using the Electoral College distribution of seats, we would reach to the winner of the election. The parameter **n** determines the number of simulations to consider, by the default is set to 100.  

## Algorithm:

Notice that there are no data for Alaska. For this reason and taking into account previous elections, we would consider that the GOP candidate, in this case Trump, wins the 3 electoral college seats assigned).   

```{r}
ctrl_sim = trainControl(method = "none")
```

```{r}
n = 100
paste("Number of simulations:", as.character(n), "")
biden_pred = rep (0, n)
biden_actual = rep (0, n) 
trump_pred = rep (0, n) 
trump_actual = rep (0, n)
plots = list()
df_states = as.data.frame(df1$state)

for (i in 1:n){
  
  # Split into Train and Test:
  idx = createDataPartition(data$Biden_2020, p = 0.8, list = FALSE)
  data.train = data[idx,]
  data.test = data[-idx,]
  
  
  # MLM:
  gb_Fit = train(mod_gb, 
                data=data.train,
                method="gbm",
                trControl=ctrl_sim,
                tuneGrid=expand.grid(interaction.depth = 9, 
                                     n.trees = 9, 
                                     shrinkage = 0.5, 
                                     n.minobsinnode = 1))
  
  pred_gb = predict(gb_Fit, data.test)
  
  # KNN:
  knn_Fit = train(mod_knn, 
                  data = data.train,
                  method = "kknn",   
                  preProc=c('scale','center'),
                  tuneGrid = data.frame(kmax=21,distance=2,kernel='optimal'),
                  trControl = ctrl_sim)

  pred_knn = predict(knn_Fit, data.test)
  
  # RF:
  rf_Fit = train(mod_rf, 
                 data = data.train,
                 method = "rf",
                 preProc=c('scale','center'),
                 trControl = ctrl_sim,
                 ntree = 100,
                 tuneGrid = data.frame(mtry=7),
                 importance = TRUE)

  pred_rf= predict(rf_Fit, data.test)
  
  # predictions (combination):
  predictions = (pred_gb + pred_knn + pred_rf)/3
  
  # transform predictions that are in percentage to votes:
  predictions = predictions * votes$Total_2020[-idx]
  
  # all votes per county but using predictions:
  votes_pred = votes$Biden_2020
  votes_pred[-idx] = predictions
  votes_pred = as.integer(votes_pred)

  
  #states
  
  # pred:
  df5 = data.frame(votes = votes_pred, total = votes$Total_2020, other = votes$Other_2020, state=states_county)  %>% group_by(state)  %>%
    summarise(Biden = sum(votes)/sum(total), Other = sum(other)/sum(total))
  
  # actual:
  df6 = data.frame(votes = votes$Biden_2020, total = votes$Total_2020, other = votes$Other_2020, state=states_county)  %>% group_by(state)  %>%
    summarise(Biden = sum(votes)/sum(total), Other = sum(other)/sum(total))
  
  # pred:
  df5$Trump = 1-df5$Biden-df5$Other
  df5$winner = factor(ifelse(df5$Biden < df5$Trump, "Trump","Biden"))
  p5 = plot_usmap(data=df5, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = paste("Predicted winner: Sim", as.character(i), sep=" "))
  
  # actual:
  df6$Trump = 1-df6$Biden-df6$Other
  df6$winner = factor(ifelse(df6$Biden < df6$Trump, "Trump","Biden"))
  p6 = plot_usmap(data=df6, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = paste("Predicted winner: Sim", as.character(i), sep=" "))
  
  # plot:
  plot = ggarrange(p5, p6, ncol = 2, nrow = 1)
  name = paste("Sim",i,sep="_")
  tmp = list(plot)
  plots[[name]] = tmp
  
  
  ## winner elections:
  seats = read.csv("seats2020.csv")
  colnames(seats) = c("state", "reps", "peoplePerElector", "Pop")
  seats = seats %>% select(1,2)
  seats$reps[which(seats$state=="District of Columbia")] = 3
  
  # pred:
  df5 = left_join(df5, seats)
  df5$reps[which(df5$state=="District of Columbia")] = 3

  president_pred = df5 %>% group_by(winner) %>% summarise(seats = sum(reps))
  president_pred[2,2] = president_pred[2,2] + 3 # assume trump wins Alaska
  
  # actual:
  df6 = full_join(df6, seats)
  df6$reps[which(df6$state=="District of Columbia")] = 3
  
  president = df6 %>% group_by(winner) %>% summarise(seats = sum(reps))
  president[2,2] = president[2,2] + 3 # assume trump wins Alaska
  na.omit(president)
  

  

  df_states[, ncol(df_states)+1] = df5$Biden
  colnames(df_states)[ncol(df_states)] = paste0("Biden_Sim", i)
  df_states[, ncol(df_states)+1] = df5$Trump
    colnames(df_states)[ncol(df_states)] = paste0("Trump_Sim", i)
  
  biden_pred[i] = president_pred[1,2]
  trump_pred[i] = president_pred[2,2]
  
  biden_actual[i] = president[1,2]
  trump_actual[i] = president[2,2]

}
```


## Winner Elections:  
```{r}
# gathered data:
sim_df = data.frame(Biden_Pred=unlist(biden_pred), Trump_Pred=unlist(trump_pred), Biden=unlist(biden_actual), Trump=unlist(trump_actual))
sim_df$winner = factor(ifelse(sim_df$Biden_Pred < sim_df$Trump_Pred, "Trump",ifelse(sim_df$Biden_Pred > sim_df$Trump_Pred, "Biden", "Tie")))
 


# table:
formattable(sim_df, list(
  winner = color_tile("#377eb8", "#e41a1c"),
  Trump_Pred = formatter("span", style = ~ style(color = ifelse(winner=="Trump", "green", "#e41a1c"))),
  Biden_Pred = formatter("span", style = ~ style(color = ifelse(winner=="Biden", "green", "#e41a1c")))))

# biden wins:
paste("Number of times Biden wins:", as.character(sum(sim_df$winner=="Biden")), sep=" ")
paste("Percentage Biden wins:", as.character(sum(sim_df$winner=="Biden")/length(sim_df$winner)), sep=" ")


# trump wins:
paste("Number of times Trump wins:", as.character(sum(sim_df$winner=="Trump")), sep=" ")
paste("Percentage Trump wins:", as.character(sum(sim_df$winner=="Trump")/length(sim_df$winner)), sep=" ")

summary(sim_df)
```
We can see that Biden get to achieve the White house around 69% of the times.  



## Best results:
We are also going to consider, as a curiosity, the best results for each candidate and see what happens.  

### Biden:
```{r}
formattable(sim_df[which.max(sim_df$Biden_Pred),],list(
    Trump_Pred=formatter("span", style = x ~ style(color = "red")),
    Biden_Pred=formatter("span", style = x ~ style(font.weight = "bold", color = "green")),
    winner=color_bar("#377eb8")))

num_biden = paste("Sim",which.max(sim_df$Biden_Pred),sep="_")
plots[num_biden]
```


### Trump:
```{r }
formattable(sim_df[which.max(sim_df$Trump_Pred),],list(
    Biden_Pred=formatter("span", style = x ~ style(color = "red")),
    Trump_Pred=formatter("span", style = x ~ style(font.weight = "bold", color = "green")),
    winner=color_bar("#e41a1c")))

num_trump = paste("Sim",which.max(sim_df$Trump_Pred),sep="_")
print(plots[num_trump])
```

## Average results vs Actual results:
Once we have the results of the simulation, the most interesting thing is to consider the average results and compare it to the actual results.  
```{r}
# average result:
ggplot(sim_df) +
  geom_density(aes(x=Biden_Pred),color="#377eb8", fill="#377eb8", alpha=0.8) + geom_density(aes(x=Trump_Pred, y = -..density..),color="#e41a1c",  fill="#e41a1c", alpha=0.8) + geom_vline(aes(xintercept=269), color="black", linetype="dashed", size=1) + geom_vline(aes(xintercept=mean(Biden_Pred)), alpha=0.8, color="white", linetype="dashed", size=1) + geom_vline(aes(xintercept=mean(Trump_Pred)), alpha=0.8, color="white", linetype="dashed", size=1) + ggtitle("Distribution of Biden Pred. vs Trump Pred.")

paste("Av. Biden:", mean(sim_df[,1]))
paste("Av. Trump:", mean(sim_df[,2]))
paste("Av. Diff:", mean(sim_df[,1])-mean(sim_df[,2]))
```


```{r fig.height = 10, fig.width = 10}
  # states analysis:
colnames(df_states)[1] = "state"
df_states_sim = df_states %>% mutate(Biden_Sim=rowMeans(df_states[,seq(2,n,2)]), Trump_Sim=rowMeans(df_states[,seq(3, n, 2)])) %>% select(state, Biden_Sim, Trump_Sim)
df_states_sim = df_states_sim %>% mutate(winner=factor(ifelse(df_states_sim$Biden_Sim<df_states_sim$Trump_Sim, "Trump", "Biden")))

# graph:
ggplot(df_states_sim) + geom_bar(aes(x=fct_rev(state), y = Trump_Sim), color="#e41a1c",  fill="#e41a1c", stat = "identity", position = "fill") + geom_bar(aes(x=fct_rev(state), y = Biden_Sim), color="#377eb8", fill="#377eb8", stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(yintercept = 0.50, color="white", linetype="dashed") + coord_flip()
```


```{r fig.height = 10, fig.width = 10}
p_sim = plot_usmap(data=df_states_sim, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Av. Sim winner")
p_actual = plot_usmap(data=df6, values = "winner", labels = TRUE) + scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Winner") + labs(title = "Actual winner")

# plot:
ggarrange(p_sim, p_actual, ncol = 2, nrow = 1)
```

## Av. States analysis:
```{r}
# states:
states_prob = df_states_sim

diff_winner = 100*(states_prob$Biden_Sim - states_prob$Trump_Sim)
states_prob$diff = diff_winner
states_prob = states_prob %>% mutate(category=factor(ifelse(diff>=5, "Safe Biden", ifelse(diff<5 & diff>=3, "Likely Biden",ifelse(diff<3 & diff>-3, "Toss-up",ifelse(diff<=-3 & diff>-5, "Likely Trump", "Safe Trump"))))))


states_prob = left_join(states_prob,seats)
states_prob$reps[which(states_prob$state=="District of Columbia")] = 3
```

```{r}
library(formattable)
library(kableExtra)
formattable(states_prob, list(
  winner = formatter("span", style = ~ style(color = ifelse(winner=="Biden", "#377eb8", "#e41a1c"))),
  Biden_Sim = color_bar("#377eb8"),
  Trump_Sim = color_bar("#e41a1c"),
  diff = formatter("span", style = ~ style(color = ifelse(diff>0, "green", "red"))),
  state = formatter("span", style = ~ style(color = ifelse(winner=="Biden", "#377eb8", "#e41a1c")))))
```


### Biden:

- Safe Biden states:  
```{r}
states_prob$state[which(states_prob$category=="Safe Biden")]
paste("seats:", sum(states_prob$reps[which(states_prob$category=="Safe Biden")]))
```

- Likely Biden states:  
```{r}
states_prob$state[which(states_prob$category=="Likely Biden")]
paste("seats:", sum(states_prob$reps[which(states_prob$category=="Likely Biden")]))
```

- Biden seats to Win:  
```{r}
paste("Biden seats to win:", 270-sum(states_prob$reps[which(states_prob$category=="Safe Biden")])-sum(states_prob$reps[which(states_prob$category=="Likely Biden")]))
```


### Toss-up:
```{r}
states_prob$state[which(states_prob$category=="Toss-up")]
paste("seats:", sum(states_prob$reps[which(states_prob$category=="Toss-up")]))
```

### Trump:

- Safe Trump states:  
```{r}
states_prob$state[which(states_prob$category=="Safe Trump")]
paste("seats:", sum(states_prob$reps[which(states_prob$category=="Safe Trump")]))
```


- Likely Trump states:
```{r}
states_prob$state[which(states_prob$category=="Likely Trump")]
paste("seats:", sum(states_prob$reps[which(states_prob$category=="Likely Trump")]))
```

- Trump seats to Win:
```{r}
paste("Trump seats to win:", 270-sum(states_prob$reps[which(states_prob$category=="Safe Trump")])-sum(states_prob$reps[which(states_prob$category=="Likely Trump")]))
```


### States:
```{r}
plot_usmap(data=states_prob, values = "category", labels = TRUE) + scale_fill_manual(values = c("#ed5e5f", "#377eb8","#e41a1c", "#b6b164"), name = "Category") + labs(title = "Av. Sim states category")

win_biden = sum(states_prob$reps[which(states_prob$category=="Safe Biden")])+sum(states_prob$reps[which(states_prob$category=="Likely Biden")])
diff_win_biden = 270-win_biden

win_trump = sum(states_prob$reps[which(states_prob$category=="Safe Trump")])+sum(states_prob$reps[which(states_prob$category=="Likely Trump")])
diff_win_trump = 270-win_trump
```

## Combinations to win:
In this section we are going to consider the combinations of toss-up states for each candidate that would make them to win the elections.  

### Biden:
```{r}
# combinations for biden to win (more probable results are +-10 av. biden win:
av_win_biden = mean(sim_df[,1])
f.df = NULL
for (i in 2:length(states_prob$reps[which(states_prob$category=="Toss-up")])){
  f = as.data.frame(t(combn(states_prob$state[which(states_prob$category=="Toss-up")],i)))
  f1 = f %>% select(1)
  for (j in 1:i){
    name = paste("f",j,sep="_")
    name = as.data.frame(f[,j])
    colnames(name)[1] = "state"
    name = left_join(name,states_prob[,c(1,2,7)])
    f1 = cbind(f1,name)
  }
  f1[,1] = NULL
  f1$sum = rowSums(f1[,seq(3,3*i, 3)])
  f1$perc = rowMeans(f1[,seq(2,3*i, 3)])
  f1$total_sum = f1$sum+win_biden
  f.df = list(f.df, f1[which(f1$sum>=diff_win_biden), c(seq(1,3*i,3), 3*i+1, 3*i+2, 3*i+3)])

}
f.df
```

### Trump:
```{r}
av_win_trump = mean(sim_df[,2])
f2.df = NULL
for (i in 1:length(states_prob$reps[which(states_prob$category=="Toss-up")])){
  f = as.data.frame(t(combn(states_prob$state[which(states_prob$category=="Toss-up")],i)))
  f2 = f %>% select(1)
  for (j in 1:i){
    name = paste("f",j,sep="_")
    name = as.data.frame(f[,j])
    colnames(name)[1] = "state"
    name = left_join(name,states_prob[,c(1,3,7)])
    f2 = cbind(f2,name)
  }
  f2[,1] = NULL
  if (i==1){
    f2$total_sum = f2$reps+win_trump
    f2.df = list(f2.df, f2[which(f2$sum>=diff_win_trump), -3])
    
  }else{
    f2$sum = rowSums(f2[,seq(3,3*i, 3)])
    f2$perc = rowMeans(f2[,seq(2,3*i, 3)])
    f2$total_sum = f2$sum+win_trump
    f2.df = list(f2.df, f2[which(f2$sum>=diff_win_trump), c(seq(1,3*i,3), 3*i+1, 3*i+2, 3*i+3)])
  }


}
f2.df
```


