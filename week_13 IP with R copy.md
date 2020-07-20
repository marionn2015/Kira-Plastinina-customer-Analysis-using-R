# This is an Unsupervised Machine Learning Project to understand the customer characteristics of Kira Plastinina.

# 1. Problem Statement

#Kira Plastinina is a Russian brand that is sold through a defunct chain of retail stores in Russia, Ukraine, Kazakhstan, Belarus, China, Philippines, and Armenia. The brand’s Sales and Marketing team would like to understand their customer’s behavior from data that they have collected over the past year. More specifically, they would like to learn the characteristics of customer group

#2.  Metrics of Success

#Problem Definition
#Data Sourcing
#Check the Data
#Perform Data Cleaning
#Perform Exploratory Data Analysis (Univariate, Bivariate & Multivariate)
#Implement the Solution
#Challenge the Solution
#Follow up Questions

#4. Data sourcing

# dataset link("http://bit.ly/EcommerceCustomersDataset")
#The dataset consists of 10 numerical and 8 categorical attributes. The 'Revenue' attribute can be used as the class label.

#"Administrative", "Administrative Duration", "Informational", "Informational Duration", "Product Related" and "Product Related Duration" represents the number of different types of pages visited by the visitor in that session and total time spent in each of these page categories. The values of these features are derived from the URL information of the pages visited by the user and updated in real-time when a user takes an action, e.g. moving from one page to another. 

#The "Bounce Rate", "Exit Rate" and "Page Value" features represent the metrics measured by "Google Analytics" for each page in the e-commerce site. 

#The value of the "Bounce Rate" feature for a web page refers to the percentage of visitors who enter the site from that page and then leave ("bounce") without triggering any other requests to the analytics server during that session. 

#The value of the "Exit Rate" feature for a specific web page is calculated as for all pageviews to the page, the percentage that was the last in the session.

#The "Page Value" feature represents the average value for a web page that a user visited before completing an e-commerce transaction. 

#The "Special Day" feature indicates the closeness of the site visiting time to a specific special day (e.g. Mother’s Day, Valentine's Day) in which the sessions are more likely to be finalized with the transaction. The value of this attribute is determined by considering the dynamics of e-commerce such as the duration between the order date and delivery date. For example, for Valentina’s day, this value takes a nonzero value between February 2 and February 12, zero before and after this date unless it is close to another special day, and its maximum value of 1 on February 8. 

#The dataset also includes the operating system, browser, region, traffic type, visitor type as returning or new visitor, a Boolean value indicating whether the date of the visit is weekend, and month of the year.





```{r}
## Importing the required packages
install.packages("tidyverse")
install.packages("iterators")  
install.packages("caret") 
install.packages('ranger')
install.packages('caTools')
install.packages("DataExplorer") 
install.packages("Hmisc")
install.packages("pastecs")
install.packages("psych")
install.packages("corrplot")
install.packages("factoextra")
install.packages("Rtsne")
```


```{r}
```


```{r}
```


```{r}
install.packages("caret")
```


```{r}
install.packages("devtools")
install.packages("contrib.url")
##Importing Libraries we need for this Project analysis.

library(tidyverse)
library(data.table)
library(lattice)
library(caret)
library(rmarkdown)
library(tinytex)
library(devtools)
library(tidyverse)
library(magrittr)
library(warn = -1)
library(RColorBrewer)
library(ggplot2)
library(lattice)
library(corrplot)
library(DataExplorer)
library(Hmisc)
library(pastecs)
library(psych)
library(factoextra)
library(Rtsne)

```


```{r}
# loading the dataset
comm <- read.csv("http://bit.ly/EcommerceCustomersDataset")

```


```{r}
# loading the first five observations
head(comm)
```
```{r}
# loading the last observations
tail(comm)
```
```{r}
# checking for the number of rows and columns
dim(comm)
#the dataset has 12330 rows and 18 columns
```
```{r}
# this is to check for attributes
sapply(comm, class)
```
```{r}
# this is to get a summary statistics of the dataset
#install.packages("iterators")
summary(comm)
```
```{r}
# Data cleaning
## check for missing values in our data
colSums(is.na(comm))

```
```{r}
# Filling the missing values using mutate function $ pipe operator
# we will fill each column with its own mean
comm<- comm %>%
  
  mutate(Administrative=replace(Administrative,is.na(Administrative),mean(Administrative,na.rm=TRUE)))%>%
  
  mutate(Administrative_Duration=replace(Administrative_Duration,is.na(Administrative_Duration),mean(Administrative_Duration,na.rm=TRUE)))%>%
  
  mutate(Informational = replace(Informational, is.na(Informational), mean(Informational, na.rm = TRUE)))%>%
  
  mutate(Informational_Duration=replace(Informational_Duration,is.na(Informational_Duration),mean(Informational_Duration,na.rm=TRUE)))%>%
  
  mutate(ProductRelated=replace(ProductRelated,is.na(ProductRelated),mean(ProductRelated,na.rm=TRUE)))%>%
  
  mutate(ProductRelated_Duration = replace(ProductRelated_Duration, is.na(ProductRelated_Duration), mean(ProductRelated_Duration, na.rm = TRUE)))%>%
  
  mutate(BounceRates =replace(BounceRates, is.na(BounceRates),mean(BounceRates,na.rm=TRUE)))%>%
  
  mutate(ExitRates = replace(ExitRates, is.na(ExitRates), mean(ExitRates, na.rm = TRUE)))
```

```{r}
sum(colSums(is.na(comm)))

```
```{r}
#checking for duplicates in the dataset
duplicated_c <- comm[duplicated(comm),]
dim(duplicated_c)
# There is 119 duplicated
```
```{r}
# dealing with duplicates

comm <- comm[!duplicated(comm),]
dim(comm)
```
```{r}
#Checking for outliers
#we will use boxplots
boxplot(comm$Administrative)
```
```{r}
boxplot(comm$Administrative_Duration)
```
```{r}
boxplot(comm$Informational)
```
```{r}
boxplot(comm$Informational_Duration)
```
```{r}
boxplot(comm$ProductRelated)
```

```{r}
boxplot(comm$ProductRelated_Duration)
```
```{r}
boxplot(comm$BounceRates)
```
```{r}
boxplot(comm$ExitRates)
```
 we will not deal with outliers as they may provide insight about the customers

UNIVARIATE ANALYSIS

univariate analysis includes the following:

Mean, Median, Mode

Measures of dispersion: Minimum, Maximum, Range, Quartiles, Standard deviation $ Variance

Kurtosis $ Skewness

Graphs:Box plots$Histograms 

```{r}
# statistical analysis for column bounce rates
summary(comm$BounceRates)

```

```{r}

# Frequency Tables

table(comm$Revenue)
```


```{r}
#Bar plots
# Bar plot of the visitor type

comm %>%
    ggplot() +
    geom_bar(aes(fct_infreq(VisitorType)), color = "green", fill = "orange" ) +
    coord_flip() +
    labs(title = "Visitor Type",
         x = "Visitor type",
         y = "Frequency")+
    theme_minimal()


```
```{r}
# Bar plot for categorical variables
# Bar plots of the categorical/factor modes variables
par(mfrow=c(3,1))
for(i in 10:12) {
	counts <- table(comm[,i])
	name <- names(comm)[i]
	barplot(counts, main=name, col = heat.colors(14))}

```

```{r}
# Plotting a histogram Exit Rates by Revenue 

comm %>%
    ggplot(aes(ExitRates)) +
    geom_histogram(color = "red",fill = "lightblue") +
    labs(title = "Distribution of Exit Rates by Revenue",
         x = "Exit Rates",
         y = "Frequency") +
    facet_grid(Revenue~.)
```
The histogram of relative and exit rates is skewed to the right

```{r}
# Plotting a Histogram of the Bounce rates by Weekend

comm %>%
    ggplot(aes(BounceRates)) +
    geom_histogram(color = "white",fill = "orange") +
    labs(title = "Distribution of Bounce Rates by Weekend",
         x = "Bounce Rates",
         y = "Frequency") +
    facet_grid(Weekend~.)
```

```{r}
# Plotting a Histogram of the Product Related to Weekend

comm %>%
    ggplot(aes(ProductRelated)) +
    geom_histogram(color = "white",fill = "orange") +
    labs(title = "Distribution of Product Related by Weekend",
         x = "Product Related",
         y = "Frequency") +
    facet_grid(Weekend~.)
```

```{r}

hist(comm$ExitRates,
     main = "Exit Rates Histogram",
     xlab = "Exit Rates",
     col = "yellow")
```

```{r}
hist(comm$Informational,
     main = "Informational Histogram",
     xlab = "Exit Rates",
     col = "lightblue")
```

BIVARIATE ANALYSIS
This will include scatter plots,stacked bars and pair plots:

Scatter Plots
```{r}
# Plotting a scatter plot

plot(ExitRates ~ BounceRates, data = comm, 
      col = "red",
      main = "Bounce vs Exit Rates Scatter Plot")


```
```{r}
# Plotting a scatter plot 

plot(ProductRelated ~ BounceRates, data = comm, 
      col = "gold",
      main = "Bounce vs Product Related Scatter Plot")
```
```{r}
# Scatter Plot using ggplots to establish the relationship between two variables and the association with a categorical variable

ggplot(comm, aes(x=BounceRates, y=ExitRates, shape= Weekend, color= Weekend, size= Weekend)) +
  geom_point()+
  labs(title = " scatter plotBounce vs Exit Rates By Weekend/Weekday ")
```

```{r}
# Scatter Plot using ggplots to find realtionship between two variables 
# and their association with a categorical variable

ggplot(comm, aes(x=ProductRelated, y=ExitRates, shape= Weekend, color= Weekend, size= Weekend)) +
  geom_point()+
  labs(title = "A scatter plot of ProductRelated vs Exit Rates By Weekend/Weekday")
```

```{r}
# Scatter Plot using ggplots to find relationship between two variables 
# and their association with a categorical variable

ggplot(comm, aes(x=BounceRates, y=ExitRates, shape= Month, color= Month, size= Month)) +
  geom_point()+
  labs(title = "A scatter plot of Bounce Rates and Exit Rates By Months")
```
# the graph only shows six months and december having the highest reates of bounce and exit.

```{r}
# STACKED BARS
# Stacked bar of Revenue vs Month
comm %>%
    ggplot(aes(Revenue)) +
    geom_bar(aes(fill = Month))+
    labs(title = "A Stacked Bar Revenue by Month")
```
# the comapny makes the most revenue in march,may and november

# Stacked bar chart: Revenue vs Weekend
```{r}
comm %>%
    ggplot(aes(Revenue)) +
    geom_bar(aes(fill = Weekend))+
    labs(title = "Stacked Chart: Revenue by Weekend")
```
```{r}
# a stack bar showing the relationship between revenue and visitors type
comm %>%
    ggplot(aes(Revenue)) +
    geom_bar(aes(fill = VisitorType))+
    labs(title = "Stacked bar of Revenue by Visitor Type")
```
# MULTIVARIATE ANALYSIS
```{r}

# pair plot FOR Continous variables

pairs(comm[,1:10])
```


```{r}
# correlation
correlation <- cor(comm[,1:10])

corrplot(correlation, methods="squares")

```
#Blue stands for positive relationship while red is for the negative correlation.the deeper the colour the more correlated a variable is to itself or others

```{r}
# K MEANS CLUSTERING
#Step 1
#A glimpse of the data
glimpse(comm)
#we will convert columns into the correct data type


```
```{r}
#Hot encoding of the factor variables.

commy = dummyVars(" ~ .", data = comm)

comm1 = data.frame(predict(commy, newdata = comm))
```

```{r}
# Checking the data types of each attribute
sapply(comm1, class)
```
```{r}
# confirming the data type has changed
glimpse(comm1)

```


```{r}
# Step 2
# We will use the Revenue as the class label,

comm1_copy <- comm1[, -c(30:31)]
comm.class<- comm[, "Revenue"]

comm1_copy_copy <- comm1[, -c(30,31)]
```

```{r}
# Previewing the class
head(comm.class)
```

```{r}
# Previewing the class with dummies
head(comm1_copy)
```

```{r}
# Step 3:  we will perform Normalizing and scaling and chose which work the best 
# This is important to ensure that no particular attribute,

comm1_scaled <- scale(comm1_copy)
```

```{r}
# After scaling the data lets see what we find in the output
summary(comm1_scaled)

```

#Scaling makes the data changes the data to have a mean 0.
#We will normalize the data and see if we get different resul

```{r}
# Normalize the copy of the original data

comm_norm <- as.data.frame(apply(comm1_copy, 2, function(x) (x - min(x))/(max(x)-min(x))))
```


```{r}
# summary of the normalized data.
summary(comm_norm)

```
# we will use the normalized data for out clustering


```{r}
# Applying the K-means clustering algorithm with no. of centroids(k)=3
# ---
# 
result<- kmeans(comm_norm,3)
```

```{r}
# Previewing the number of records in each cluster

result$size
```
```{r}
# Previewing the number of records in each cluster

result$size

```
```{r}
# Plotting product related and product related distribution variables to see how their data points 


plot(comm_norm[, 5:6], col = result$cluster)
```
```{r}

# exit rates vs bounce rates
plot(comm_norm[, 7:8], col = result$cluster)



```

# HIERACHICAL CLUSTERING

```{r}
# Before hierarchical clustering, we can compute some descriptive statistics
# ---
# 
desc_stats <- data.frame(
  Min = apply(comm1, 2, min),    # minimum
  Med = apply(comm1, 2, median), # median
  Mean = apply(comm1, 2, mean),  # mean
  SD = apply(comm1, 2, sd),      # Standard deviation
  Max = apply(comm1, 2, max)     # Maximum
)
desc_stats <- round(desc_stats, 1)
head(desc_stats)
```

```{r}
# We use R function hclust() 
#We will compute the euclidean distance btwn obs using the dist()
# 
d <- dist(comm_norm, method = "euclidean")

# We then apply hierarchical clustering using the Ward's method

res.hc <- hclust(d, method = "ward.D2")

# Lastly we plot the obtained dendrogram
#--

plot(res.hc, cex = 0.6, hang = -1)

```

# PRINCIPAL COMPONENT ANALYSIS(PCA)


```{r}
# Applying PCA
# We pass df_norm to the prcomp().
# We also set two arguments, center and scale, 
# to be TRUE then preview our object with summary
comm_norm_pca <- prcomp(comm_norm,
              center = TRUE,
              scale = FALSE) 
summary(comm_norm_pca)


```
#we obtained 29 principal components,
#PC1 explains 20.5% of the total variance, which is  low.
#PC2 accounts for 15.5% of the total variance
#We will compare this with the t-SNE results

#t-Distributed Stochastic Neighbor Embedding (t-SNE)

```{r}
# Preparation for plotting
colors <- rainbow(length(unique(comm.class)))
names(colors) = unique(comm.class)

```

```{r}
# Applying t-SNE

tsne <- Rtsne(comm_norm, dims =2, perplexity = 30, verbosity = TRUE,
      max_iter = 500)

# time it takes to execute

exeTimeTsne <- system.time(Rtsne(comm_norm, dims = 2, perplexity = 30,
verbose = TRUE, max_iter = 500))

```

```{r}
# Plotting our graph and closely examining the graph

plot(tsne$Y, t = 'n', main = "tnse")
text(tsne$Y, labels =comm$Revenue, col = "lightblue")
```




















