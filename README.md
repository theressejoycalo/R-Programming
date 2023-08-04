---
title: 'Programming in R: Solutions for the Exam-part 1 (28/01/2022)'
output:
  pdf_document: default
  html_document: default
  word_document: default
subtitle: 'THERESSE JOY VILLARIEZ CALO (DL-2022)'
layout: page
---

---
output: html_document
  use_bookdown: TRUE
--- 



```{r setup, include=FALSE}
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      eval = TRUE,
                      tidy = FALSE)
library(knitr)
```


# Introduction


You can use this file to write your answers to the exam's queations. You need to submit the solution on:

  * Date: 28/01/2022.
  * Time: 17:00.
  
\newpage



# Part 1: the <tt>nassCDS</tt> data

In this part of the exam, we focus on the <tt>nassCDS</tt> data which is a US data from police-reported car crashes (1997-2002) in which there is a harmful event (people or property). Data are restricted to front-seat occupants, include only a subset of the variables recorded. More information about the dataset can be found using the following link: https://www.rdocumentation.org/packages/DAAG/versions/1.22/topics/nassCDS. The data is a part of the <tt>DAAG</tt> R package. To get an access to the data you first need to install the package.

```{r}
library("DAAG")
data(nassCDS)
names(nassCDS)
```

## Question 1

In this question we use the <tt>nassCDS</tt> dataset and focus on the accident's outcome (the variable <tt>dead</tt>) and seatbelt usage (the variable <tt>seatbelt</tt>).

 1. How many individuals used seatbelt?
 2. What is the distribution of seatbelt usage across the accident's outcome factor ? Produce a 2X2 table that       shows the number of seatbelt users (belted/none) and accident's outcome (alive/dead)?
  3. Write a function that can be used to conduct inference for proportions in two independent populations. The null hypothesis is that there is no difference between the proportions in the two populations. Test the null hypothesis against a two sided alternative. The input of the function should be the 2X2 table in the previous item (Question 1.2) and the output should be the test statistic and the p value. Apply your function to test the null hypothesis that the proportion of deaths among individuals who used seatbelt is equal to the proportion of deaths among the individuals who did not use seatbelt.
  4. Use a barplot to visualize the distribution of the seatbelt usage across the factor levels of the accident's outcome.


### Solution for question 1.1
```{r}
sum(table(nassCDS$seatbelt[nassCDS$seatbelt=='belted']))
```
### Solution for question 1.2
```{r}
table(nassCDS$seatbelt,nassCDS$dead)
```
### Solution for question 1.3
```{r}
p.test<-function(z)
  {
  none<- z[1,1]+z[1,2]
  belted<-z[2,1]+z[2,2]
  g1.p<-z[1,2]/none
  g2.p<-z[2,2]/belted
  succcess<-c((none*g1.p),(belted*g2.p))
  sum<-c(none,belted)
  result<-prop.test(succcess,sum)
  return(result$p.value)
  } 
z<-table(nassCDS$seatbelt,nassCDS$dead)
p.test(z)
```
### Solution for question 1.4
```{r}
z<-table(nassCDS$seatbelt,nassCDS$dead)
barplot(z, main="Seatbelt Usage during Accidents", xlab="Accident's Outcome", col=c("darkblue","red"), xlim=c(0,4), legend.text = rownames(z))
```


## Question 2

In this question we focus on the outcome of the accident (dead/alive, the variable <tt>dead</tt>) and the age of the occupant (the variable <tt>ageOFocc</tt>) in the <tt>nassCDS</tt> dataset.

1. What is the mean and standard deviation of the age of occupant by accident outcome?
2. Use a boxplot to visualize the distribution of the occupants' age by accident outcome and add the data points    on the boxplot.
3. Calculate a $95\%$ confidence interval for the mean difference of the age of occupant using t distribution.


### Solution for question 2.1
```{r}
desc<-function(x,y)
  {
  a<-c(mean(x), mean(y))
  b<-c((var(x))*1/2, (var(y))*1/2)
  c<-rbind(a,b)
  colnames(c)<-c("dead", "alive")
  row.names(c)<-c("mean", "sd")
  return(c)
}
desc(nassCDS$ageOFocc[nassCDS$dead == 'dead'], nassCDS$ageOFocc[nassCDS$dead == 'alive'])
```
### Solution for question 2.2
```{r}
library(ggplot2)
library(tidyverse)
nassCDS%>%
  ggplot(aes(ageOFocc,dead))+
  geom_boxplot()+
  geom_jitter(size = 0.1)

```
### Solution for question 2.3
```{r}
ttest<-t.test(nassCDS$ageOFocc)
ttest$conf.int
```

## Question 3


1. Visualize the distribution of the occupant age by sex in the <tt>nassCDS</tt> dataset.
2. How many occupants over the age of 50 years old survived the accident?
3. Add a binary variable <tt>AgeOFocc_class</tt> that takes the value of 1 when the occupant age is over 50      years and 0  for when the occupant age is 50 years or less.
4. Create a data frame, <tt>nassCDS_o50</tt>, containing occupants older than 50 years old. This data frame should contain the variables <tt>dead</tt>, <tt>airbag</tt>, <tt>weight</tt>, and <tt>injSeverity</tt>. Remove the observations with missing values.
5. What is the dimension of the new data ?
6. Among the occupants who are older than 50 years old, use a barplot to visualize the distribution of airbag across the levels of the accident outcome (dead/alive). The variable <tt>dead</tt> should be on the x-axis. 
7. Among the occupants who are older than 50 years old, visualize the distribution of airbag across the level of    injury severity (the variable <tt>injSeverity</tt>).


### Solution for question 3.1
```{r}
nassCDS%>%
  ggplot(aes(x = ageOFocc, fill = as.factor(sex)))+
  geom_histogram(bins = 30)
```
### Solution for question 3.2
```{r}
sum(table(nassCDS$ageOFocc[nassCDS$ageOFocc > 50 & nassCDS$dead == 'alive']))
```
### Solution for question 3.3
```{r}
AgeOFocc_class<-ifelse(nassCDS$ageOFocc > 50, 1, 0)
```
### Solution for question 3.4
```{r}
install.packages("tidyverse")
library(tidyverse)
nassCDS_o50 <- na.omit(nassCDS)%>%
  filter(ageOFocc > 50)%>%
  select(dead, airbag, weight, injSeverity)
```
### Solution for question 3.5
```{r}
dim(nassCDS_o50)
```
### Solution for question 3.6
```{r}
nassCDS_o50%>%
  ggplot(aes(x = dead, fill = airbag))+
  geom_bar()
```
### Solution for question 3.7
```{r}
library(ggplot2)
ggplot(nassCDS_o50, aes(x=injSeverity, fill=airbag))+ geom_bar(position="stack")
```

## Question 4

Write a R function that receives as an input the <tt>nassCDS</tt> dataset. The function should conduct the following analysis:


1. Select only the observations for which the accident outcome is "dead".
2. Calculate percentage of deaths out of the overall number of observations.
3. Calculate the percentages of females and males among the occupants who died in the accident.
4. Show the most frequent severity of their injuries. 
5. Calculate the minimum and maximum age of the occupant (the variable <tt>ageOFocc</tt>).
6. Produce a histogram with the severity of injuries on the x axis and the frequency of occupant's age on the y axis
7. This **SINGLE** Function should return **two** outputs: 
    * Numerical output: 4.2,4.3,4.4 and 4.5 as a table.
    * Graphical output: 4.6 as a plot.
  


### Solution for question 4.1
```{r}
library(dplyr)
counts_dead<-function(x)
  {
  dead_obs<-x%>%filter(x[,3]=="dead")
  return(dead_obs)
}
new_nassCDS<-counts_dead(nassCDS)
```
### Solution for question 4.2
```{r}
percent_dead<-function(x)
  {
  p_dead<-(sum(table(x[,3][x[,3]=="dead"]))/sum(table(x[,3])))
  return(p_dead*100)
}
percent_dead(nassCDS)
```
### Solution for question 4.3
```{r}
counts_gender<-function(x)
{
total <- sum(table(x[7]))
male <- sum(table(x[7][x[7]=="m" & x[3]=="dead"]))
male_p <- ((male/total)*100)
female <- sum(table(x[7][x[7]=="f" & x[3]=="dead"]))
female_p <- ((female/total)*100)
y <- c(Male.Casualty.Percent=male_p,Female.Casualty.Percent=female_p)
return(y)
}
counts_gender(nassCDS)
```

### Solution for question 4.4
```{r}
freq.inj <- function(x)
  {
  tally <- table(x[14])
  max.tally <- tally[which.max(tally)]
  return(max.tally)
}
freq.inj(nassCDS)
```

### Solution for question 4.5
```{r}
age.extent<-function(x)
  {
  age.data <- x[8]
  min.age <- min(age.data)
  max.age <- max(age.data)
  age.final <- c(MinimumAge = min.age, MaximumAge = max.age)
  return(age.final)
}
age.extent(nassCDS)
```

### Solution for question 4.6
```{r}
hist.inj<-function(x)
{
plot<-ggplot(x, aes(ageOFocc,fill = injSeverity)) + geom_histogram(bins = 30) + facet_wrap(~x$injSeverity,ncol = 2)
return(plot)
}
hist.inj(nassCDS)
```

### Solution for question 4.7
```{r}
hist.inj<-function(x)
{
plot<- na.omit(x)%>%
  ggplot(aes(ageOFocc,fill = as.factor(injSeverity)))+
  geom_histogram(bins = 30) 
print(plot)
return(table)
}
hist.inj(nassCDS)
```

## Question 5

1. Use the <tt>nassCDS</tt> dataset to create a new data frame which contains only occupants who used seatbelt.
2. How many occupants used seatbelt ?
3. Among the individuals who used seatbelt, how many died and how many survived the accident ?
4. Among the individuals who used seatbelt, how many were drivers among the individuals who died and how many       were passengers among the individuals who survived the accident (use the variable <tt>occRole</tt> to identify drivers/passengers) ?
5. Sort the data frame according to the injury's severity and the occupant age.
6. Print the 25 occupants with the highest weight.


### Solution for question 5.1
```{r}
belted <- nassCDS %>% filter(seatbelt == "belted")
```
### Solution for question 5.2
```{r}
nrow(belted)
```
### Solution for question 5.3
```{r}
table(belted$dead)
```
### Solution for question 5.4
```{r}
belted%>%
  group_by(occRole, dead)%>%
  summarize(count=table(occRole))
```
### Solution for question 5.5
```{r}
head(belted%>%arrange(injSeverity,ageOFocc), 30)
```
### Solution for question 5.6
```{r}
print(belted%>%arrange(desc(weight))%>%top_n(25,weight))
```

## Question 6

Prepare a presentation of 5-10 slides using R markdown about the connection between the usage of seatbelt, the outcome of the accident and the severity of the injury. Make sure that your presentation includes:

 * A Title slide.
 * At least one slide with text.
 * At least one slide with a figure
 * At least one slide with text and a figure.
 
Please note that you **WILL NOT** be asked to give the presentation and you **WILL NOT** be asked questions about the presentation. Your aim in this question is to demonstrate that you know how to use R markdown to make a          presentation about your analysis. More details how to make a presentation using R markdown: https://rmarkdown.rstudio.com/lesson-11.html.

## Question 7


 1. Use the <tt>nassCDS</tt> dataset to produce Figure 7.1 presented below.


### Solution for question 7.1
```{r}
nassCDS %>%
  drop_na() %>%
  ggplot(aes(x = as.factor(injSeverity), y = ageOFocc , fill = injSeverity))+
  geom_boxplot() +
  facet_grid(airbag~seatbelt) +
  xlab("injSeverity")
  
```


# Part 2: the <tt>atmos</tt> dataset

In this part, the questions are focused on the <tt>atmos</tt> dataset which is a part of the <tt>nasaweather</tt> R package. To access the data you need to install the package. More information can be found in https://github.com/hadley/nasaweather. You can use the code below to access the data.


```{r}
library(nasaweather)
data(atmos)
names(atmos)
dim(atmos)
```



## Question 8 

 1. Use the <tt>atmos</tt> dataset to calculate the mean temperature (the variable <tt>temp</tt>) by year.
 2. Create a new data frame in which only data from 1995 are included.
 3. For 1995, calculate the mean, trimmed mean ($10\%$) and median temperature by month.
 4. Produce a dotplot for the temperature by month and include in the plot the mean temperature by month.
 5. Use a <tt>for loop</tt> to calculate a $95\%$ confidence interval for the mean temperature by month.
 6. Write a function that calculate the Pearson correlation between two vectors and applied this function, within     a <tt>for loop</tt>,  for the temperature and ozone (the variable <tt>ozone</tt>) level by year.
 7. Plot the yearly correlation between temperature and ozone and add a line for the overall correlation.


### Solution for question 8.1
```{r}
atmos %>% group_by(year) %>% summarize(temp = mean(temp))
```
### Solution for question 8.2
```{r}
atmos95 <- atmos %>% filter(year == '1995')
```
### Solution for question 8.3
```{r}
atmos95 %>%
  group_by(month) %>%
  summarize(mean = mean(temp), trimmed = mean(temp, trim=0.10), median = median(temp))
```
### Solution for question 8.4
```{r}
atmos %>%
  ggplot(aes(x=month, y=temp, col=month))+
  geom_point()+
  stat_summary(geom = "point",fun = "mean", fill = "red", shape = 24)
```
### Solution for question 8.5
```{r}
for(i in 1:12)
  {
  monthly<-t.test(atmos$temp[atmos$month == i])
  conf.int<-monthly$conf.int
  print(conf.int)
  }
```
### Solution for question 8.6
```{r}
year <- c(1:6)
coef<-c(1:6)

for(i in 1:6)
{
by <-c(1994+i)
yearly<- cor(atmos$temp[atmos$year == by],atmos$ozone[atmos$year == by])
year[i] <- c(by)
coef[i]<-c(yearly)
cor.coef <-cbind(year, coef)
}

cor.coef
```
### Solution for question 8.7
```{r}
plot(cor.coef)
abline(h=cor(atmos$temp, atmos$ozone), col = "red")
```

## Question 9

In the <tt>atmos</tt> dataset, the variable <tt>cloudlow</tt> is the Mean Low Cloud Coverage, that is the monthly mean percent of the sky covered by clouds with cloud top pressure greater than 680 mb. Let q75 be the 75\% quantile of the <tt>cloudlow</tt> distribution. 

1. Define a R object which is equal to the 75% quantile.
2. Produce a histogram for <tt>cloudlow</tt> and add a vertical line (in red) which represents the 75% quantile of the distribution. 
3. Create a new data frame which includes only observations with <tt>cloudlow</tt> > q75. How many observations     were included in the new data frame?
4. Produce a scatterplot of  <tt>cloudlow</tt> (Y) versus  <tt>ozone</tt> (X). Use different colors for data        points of each month.
5. Produce a multiway histogram in which you plot the histogram for <tt>cloudlow</tt> for each year.  Produce the    histograms in a 3X2 panel (three columns with two figures in each column).


### Solution for question 9.1
```{r}
q75<-quantile(atmos$cloudlow, probs = c(0.75), na.rm = TRUE)
q75
```
### Solution for question 9.2
```{r}
hist(atmos$cloudlow)
abline(v=q75, col = "red")
```
### Solution for question 9.3
```{r}
atmos_q75<-atmos%>%filter(cloudlow > q75)
nrow(atmos_q75)
```
### Solution for question 9.4
```{r}
atmos%>%
  ggplot(aes(x = ozone, y=cloudlow)) +
  geom_point(aes(color = as.factor(month)))
```
### Solution for question 9.5
```{r}
atmos%>%
  ggplot(aes(x=cloudlow))+
  geom_histogram(bins = 30)+ 
  facet_wrap(~year,ncol = 3, nrow=2)

```

## Question 10

 1. Use the <tt>atmos</tt> dataset to produce Figure 10.1 presented below.

   * Hint: based on the data and variables presented in the figure you need to create a new data frame and            produce a figure using the new data frame. 

### Solution for question 10.1

```{r}
atmos_567<-atmos%>%filter(year <= 1997)

atmos_567%>%
  ggplot(aes(x = ozone, y=temp, color=as.factor(month))) +
  geom_point() +
  facet_wrap(~year,ncol = 1)

```

# Part 3: the <tt> Legosets</tt> data

In this part we focus on the <tt>Legosets</tt> dataset which can be downloaded from https://github.com/seankross/lego following the instructions in the github page. The <tt>Legosets</tt> dataset contains 6172 rows and 14 columns. Make sure that you install the <tt>devtools</tt> and the <tt>lego</tt> packages. Use the code below to install and access the data.


```{r}
library(devtools)
install_github("seankross/lego")
library(lego)
data("legosets")
names(legosets)
```


## Question 11

  1. Are there missing values in the <tt>Legosets</tt> dataset? If yes, how many? 
  2. Replace the missing values by the median for the numerical columns and by the most frequent value in the categorical columns. In the case of integer columns, round up the replaced values to a whole number(if they're decimal).
  3. Create a function that summarizes the dataset, this function should output a table displaying the number of  rows, number of columns, and the averages, max, and min values for the price in British Pounds(GBP) columns and the most frequent <tt>SubTheme</tt>, <tt>Packaging</tt>, and <tt>Availability</tt> for the observations with Star Wars theme.
  4. Create a new dataframe <tt> data1</tt> that contains <tt>Pieces</tt>, <tt>GBP_MSRP</tt>, and <tt>Theme</tt> sorted in decreasing order of <tt>Pieces</tt> and increasing order of <tt>GBP_MSRP</tt>.
  5. Create a new dataframe <tt> data2 </tt> that consists of the variables <tt>Pieces</tt>, <tt>GBP_MSRP</tt>, <tt>Availability</tt> for the theme "City" and merge it with <tt> data1</tt> based on the rows in <tt>data2</tt>. 
  6. Produce the merged data frame in 11.5 in a different way.
  7.  Fit a linear regression model on the filtered data frame from question 11.5 with the price (<tt>GBP_MSRP</tt>) being the response variable and the number of pieces (<tt>Pieces</tt>) as a predictor.
  8. Produce a scatterplot of price (Y) versus pieces (X) and add the regression line to the scatterplot.
  
### Solution for question 11.1
```{r}
sum(is.na(legosets))
```
### Solution for question 11.2
```{r}
most.frequent<-function(x)
  {
  y<-sort(table(x), decreasing = TRUE)[1]
  z<-unique(x[x==names(y)])
  return(z)
}

str(legosets)
legosets$Item_Number<-replace_na(legosets$Item_Number,most.frequent(legosets$Item_Number))
legosets$Name<-replace_na(legosets$Name,most.frequent(legosets$Name))
legosets$Theme<-replace_na(legosets$Theme,most.frequent(legosets$Theme))
legosets$Subtheme<-replace_na(legosets$Subtheme,most.frequent(legosets$Subtheme))
legosets$Image_URL<-replace_na(legosets$Image_URL,most.frequent(legosets$Image_URL))
legosets$Packaging<-replace_na(legosets$Packaging,most.frequent(legosets$Packaging))
legosets$Availability<-replace_na(legosets$Availability,most.frequent(legosets$Availability))

legosets$GBP_MSRP<-replace_na(legosets$GBP_MSRP, median(legosets$GBP_MSRP, na.rm = TRUE))
legosets$USD_MSRP<-replace_na(legosets$USD_MSRP, median(legosets$USD_MSRP, na.rm = TRUE))
legosets$CAD_MSRP<-replace_na(legosets$CAD_MSRP, median(legosets$CAD_MSRP, na.rm = TRUE))
legosets$EUR_MSRP<-replace_na(legosets$EUR_MSRP, median(legosets$EUR_MSRP, na.rm = TRUE))

legosets$Year<-replace_na(legosets$Year, round(median(legosets$Year, na.rm = TRUE)))
legosets$Pieces<-replace_na(legosets$Pieces, round(median(legosets$Pieces, na.rm = TRUE)))
legosets$Minifigures<-replace_na(legosets$Minifigures, round(median(legosets$Minifigures, na.rm = TRUE)))

sum(is.na(legosets))

```
### Solution for question 11.3
```{r}
star.wars<-function(x)
  {
  x%>%
    filter(Theme == "Star Wars")%>%
    summarize(rows = nrow(x), columns = ncol(x),
              mean = mean(GBP_MSRP, na.rm = TRUE), max = max(GBP_MSRP, na.rm = TRUE),
              min= min(GBP_MSRP, na.rm = TRUE), packaging =most.frequent(Packaging),
              availability = most.frequent(Availability))
  }

star.wars(legosets)

```
### Solution for question 11.4
```{r}
data1 <- legosets %>% 
  select(Pieces, GBP_MSRP, Theme)%>%
  arrange(desc(Pieces), GBP_MSRP)

head(data1)
```
### Solution for question 11.5
```{r}
data2 <- legosets %>%
  filter(Theme == "City") %>%
  select(Pieces, GBP_MSRP, Availability)

head(merge(x = data1, y = data2, by = "Pieces", all.y = TRUE))
```
### Solution for question 11.6
```{r}
library(data.table)
data1<-data.table(data1)
data2<-data.table(data2)
head(data1[data2, on = .(Pieces)])
```
### Solution for question 11.7  
```{r}
 lm <- lm(data = data2, GBP_MSRP~Pieces)
```
### Solution for question 11.8  
```{r}
ggplot(data = data2, aes(x = Pieces, y = GBP_MSRP))+
  geom_point() +
  geom_smooth(method = "lm")
```  

## Question 12

1. Use the <tt>Legosets</tt> dataset and create a data frame <tt>pieces_and_price</tt> which contains the columns <tt>Pieces</tt> and <tt>GBP_MSRP</tt>, and remove the missing values.
2. Using the median of Pieces, define a new indicator variable which takes the value of 0 if the number of pieces for the given item is less or equal to the median and 1 otherwise.
3. Calculate the correlation between <tt>Pieces</tt> and <tt>GBP_MSRP</tt> using Pearson's correlation coefficient.
4. Produce a scatterplot of the <tt>Pieces</tt> (X) vs <tt>GBP_MSRP</tt> across the levels of the factor defined in 12.2.
5. Produce a boxplot showing the distribution of <tt>GBP_MSRP</tt> across the levels of the factor defined in 12.2.
6. Are the variances of the prices of the two groups (below and above the median) homogeneous or not? Verify that using Fisher's F test (use the function <tt>var.test()</tt>).
7. Based on the results in 12.6, conduct a t-test, to test the null hypothesis that the price of items is equal for items that have less than median of pieces and those that have a higher number of pieces than the median against a two sided alternative.

### Solution for question 12.1
```{r}
pieces_and_price <- na.exclude(data.frame(legosets$Pieces, legosets$GBP_MSRP))
names(pieces_and_price) <- c("Pieces", "Price")
names(pieces_and_price)

```
### Solution for question 12.2
```{r}
pieces_and_price<-mutate(pieces_and_price, Indicator = ifelse(Pieces <= median(Pieces), 0, 1))

head(pieces_and_price)
```
### Solution for question 12.3
```{r}
Pieces <- pieces_and_price$Pieces
Price <- pieces_and_price$Price
Indicator <- pieces_and_price$Indicator  
cor(Pieces, Price)
```
### Solution for question 12.4
```{r}
ggplot(data=pieces_and_price, aes(x = Pieces, y = Price))+
  geom_point(aes(col = as.factor(Indicator)))+
  facet_wrap(~Indicator)
```
### Solution for question 12.5
```{r}
ggplot(data=pieces_and_price, aes(y = Pieces))+
  geom_boxplot()+
  facet_wrap(~Indicator)
```
### Solution for question 12.6
```{r}
var.test(Pieces, Price)
```

### Solution for question 12.7
```{r}
t.test(Price[Indicator == "0"], Price[Indicator == "1"], var.equal = FALSE)
```

## Question 13


1. For the <tt>Legosets</tt> dataset, sum the price in <tt>USD_MSRP</tt> by the variable <tt>Theme</tt>. Identify the top 5 themes in terms of the highest total price (i.e., the top 5 most expensive themes with the highest sum).

2. Using the original <tt> Legosets</tt> data and the top 5 themes identified in Question 13.1, produce a density plot for the price in <tt> USD_MSRP</tt> across the level of the  5 themes that were identified in 13.1.

### Solution for question 13.1
```{r}
legosets.Theme <- legosets%>%
  group_by(Theme)%>%
  summarize(Price.Sum.USD = sum(USD_MSRP))%>%
  arrange(desc(Price.Sum.USD))%>%
  top_n(5, Price.Sum.USD)
legosets.Theme
```
### Solution for question 13.2
```{r}
top.theme<-c("Technic", "Advanced Models", "Dacta", "Serious Play", "Harry Potter")

legosets.Density<-legosets%>%filter(Theme %in% top.theme)

legosets.Density%>%
ggplot(aes(x = USD_MSRP, col = Theme, fill = Theme))+
geom_density()+
facet_wrap(~Theme)+
theme_bw()
```
## Question 14
The output below consists of a figure and a table with summary statistics for **the variable presented in Figure 14.1**.

 1. Produce Figure 14.1 below.
 2. Produce the table below.
 
   * Hint 1: create a new data frame based on the variables presented in the figure.
   * Hint 2: check missing values.


### Solution for question 14.1
```{r}
legosets.Year <- legosets%>%
select(USD_MSRP, Year)%>%
filter(Year >= 2010)

legosets.Year%>%
ggplot(aes(x = USD_MSRP,  fill = Year))+
geom_density()+
facet_wrap(~Year, nrow = 3)
```
### Solution for question 14.2
```{r}

legosets.Year.Complete <- na.omit(legosets)%>%
select(USD_MSRP, Year)%>%
filter(Year >= 2010)

legosets.Year.Complete%>%
group_by(Year)%>%
summarize(average = mean(USD_MSRP), standard_deviation = sd(USD_MSRP))
```


# Part 4: the <tt>titanic</tt> data

In this part of the exam, we focus on <tt>titanic</tt> dataset. It contains data of survival status of passengers on the Titanic, together with their names (<tt>Name</tt>), age (<tt>Age</tt>), sex (<tt>Sex</tt>) and passenger class (<tt>PClass</tt>). This dataset is part of package <tt>lgrdata</tt> and will be available after installing the package. More information about the data can be found in https://www.rdocumentation.org/packages/titanic/versions/0.1.0. To access the data you need to install the <tt>lgrdata</tt> package.


```{r}
#install.packages("lgrdata")
library(lgrdata)
data(titanic)
dim(titanic)
names(titanic)
head(titanic)
```


## Question 15
1. For the analysis of this question, exclude all the observation with missing values in the <tt>titanic</tt> dataset. How many observations are included ? How many male and female there are among the passengers ?
2. Produce the $2 \times 2$ table below which shows the survival distribution (dead/alive) by gender.
3. Produce the data frame below. All your calculation should be done in R.
4. Produce Figure 15.1 and 15.2.



### Solution 15.1
```{r}
titanic.Complete <- na.omit(titanic) 

addmargins(table(titanic.Complete$Sex),margin=1)
```

### Solution 15.2
```{r}
table(titanic.Complete$Sex, titanic.Complete$Survived)
```
### Solution 15.3
```{r}
titanic.Death <- table(titanic.Complete$Sex,titanic.Complete$Survived)

titanic.Death <-addmargins(titanic.Death,margin=2)

colnames(titanic.Death) <- list("Died", "Survived", "%Died")
titanic.Death

```
### Solution 15.4
```{r}
titanic.Complete$Survived <- as.factor(titanic.Complete$Survived)

titanic.Complete%>%
ggplot(aes(x = Survived, y = Age, fill = Survived))+
geom_violin()+
facet_wrap(~PClass)

```

```{r}
titanic.Complete%>%
ggplot(aes(x = Survived, y = Age, fill = Survived))+
geom_violin()+
facet_wrap(~PClass+Sex)+
theme_bw()
```

## Question 16

1. For the analysis of this question, exclude all the observation with missing values in the <tt>titanic</tt> dataset. Create a new data frame which contains only female. How many observations are included ? 
2. For the new data frame, calculate the mean age by class and create the data frame below.
3. Produce Figure 16.1 (density of age by class).
4. Test the hypothesis that (for female passengers) the mean age is equal across the classes using One-Way ANOVA model. 
5. Let us define the standardized residuals as:
 $$ e_{si}=\frac{residual_{i}}{\sqrt{MSE}},$$
  What is the estimate for the MSE obtained for the model in (3). Define a new R object that equal to $e_{si}$ and produce Figure 16.2 (normal probability plot for the residuals).



### Solution 16.1
```{r}
dim(titanic.Female <- na.omit(titanic)%>%filter(Sex == 'female'))
```
### Solution 16.2
```{r}
titanic.Female%>%
  group_by(PClass)%>%
  summarize(mean1 = round(mean(Age),digits = 1), median1 = median(Age))
```
### Solution 16.3
```{r}
titanic.Female%>%
  ggplot(aes(x = Age))+
  geom_density()+
  facet_wrap(~PClass)

```
### Solution 16.4
```{r}
titanic.Female$PClass<- ordered(titanic.Female$PClass, levels = c("1st", "2nd", "3rd"))
fClass.aov <- aov(titanic.Female$Age~titanic.Female$PClass)
summary(fClass.aov)
```
### Solution 16.5
```{r}
lm.Female <- lm(Age~PClass, data = titanic.Female)
res.Female <- resid(lm.Female )
MSE.Female <- mean(lm.Female$residuals^2)
e.si <- (res.Female/ ((MSE.Female)^1/2))
qqnorm(e.si)
qqline(e.si, col = "red")
```



