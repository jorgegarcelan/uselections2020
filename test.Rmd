---
title: "Supervised Learning project - 2020 Elections in the US"
subtitle: "Statistical Learning - UC3M"
author: "Jorge Garcelán Gómez"
date: "10/11/2021"
output: 
  github_document
editor_options:
  chunk_output_type: console
---

```{r global_options, include=T, echo = F}
knitr::opts_chunk$set(echo = T, warning=FALSE, message=FALSE)
```

```{r echo=FALSE}
setwd("C:/Users/jorge/Desktop/UNI/2-SEGUNDO/2-1-STATISTICS/HOMEWORK/2-SUPERVISED_LEARNING/Homework")
```


```{r echo=FALSE}
# clean the workspace:
rm(list=ls())
```

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
# detach("package:MASS", unload = TRUE)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
```




https://www.census.gov/quickfacts/fact/note/US/RHI825219
https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/
https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/cc-est2020-alldata6.pdf
https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/
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

Next, using the **VIM** package we can see in a graph the distribution of NAs and the missing values for each variable. We see that in 2 variables the percentage of missing data is around 50%. This value is high but te choice is to input the values using **mice**.  
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
data$poverty_2019 = mice.com$poverty_2019

# plot without NAs
na_plot = aggr(data, col=c('#69b3a2','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of NAs","Pattern"))
```

Once we have imputed the missing values we can take a look if they are reasonable. We can see that the imputed vales are similar to the actual values. Sufficient for our needs. Notice that the density for each imputed variable is showed in magenta while the density of the actual values is in blue.  
```{r}
densityplot(mice)
```


