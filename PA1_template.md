---
title: "Reproducible Research: Peer Assessment 1"

output: 

  html_document:

    keep_md: true
---


### 1.Loading and Preprocessing the data

##### *Load the data*


```r
     unzip("repdata_data_activity.zip",exdir = "data")
```

##### *Process/Transform the data*

###### Read the data into a data frame

```r
     rawData <- read.csv("data/activity.csv", stringsAsFactors=FALSE)
     summary(rawData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```


###### Transform Data: convert date to POSIXct class using lubridate package


```r
    library(lubridate)
    rawData$date <- ymd(rawData$date)
    activity <- rawData[complete.cases(rawData),]
    str(activity)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```




### 2. What is mean total number of steps taken per day?


##### *Calculate the total number of steps taken per day*


```r
library(dplyr)

StepsPerDay<-activity%>%group_by(activity$date)%>%summarise(TotSteps=sum(steps))
```

##### *Plot for total number of steps taken per day*


```r
barplot(height = StepsPerDay$TotSteps,names.arg=StepsPerDay$date,cex.names=0.68,las=3,col="orange")
```

```
## Warning: Unknown or uninitialised column: 'date'.
```

```r
abline(h=median(StepsPerDay$TotSteps), lty=2,lwd=3, col="black")

abline(h=mean(StepsPerDay$TotSteps), lty=2,lwd=3, col="red")

text(x = 0,y=median(StepsPerDay$TotSteps),pos=3,labels = "median")

text(x = 0,y=mean(StepsPerDay$TotSteps),pos=1,labels = "mean",col="red")
```

![](PA1_template_files/figure-html/barplot-1.png)<!-- -->



##### *Calculate and report the mean and median of the total number of steps taken per day*




```r
mean_steps <- mean(StepsPerDay$TotSteps,na.rm=TRUE)

median_steps <- median(StepsPerDay$TotSteps,na.rm=TRUE)
```

Mean and median of the total number of steps taken per day are 1.076619\times 10^{4} steps and 10765 steps, respectively.



### 3.What is the average daily activity pattern?

##### *Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*




```r
library(dplyr,quietly = TRUE)

daily_activity_patterns <- activity %>% group_by(interval) %>% summarise(average=mean(steps,na.rm=TRUE))

plot(x = 1:nrow(daily_activity_patterns),y = daily_activity_patterns$average,type = "l",

     col = "red", xaxt = "n",xlab="Intervals", 

     ylab = "Average for given interval across all days")

axis(1,labels=daily_activity_patterns$interval[seq(1,288,12)],

     at = seq_along(daily_activity_patterns$interval)[seq(1,288,12)])
```

![](PA1_template_files/figure-html/daily-1.png)<!-- -->



##### *Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
max_numb_steps_interval <- filter(daily_activity_patterns,average==max(average))
```



Interval **"835"** contains on average the maximum number of steps (**206.17 steps**).



### 4.Imputing missing values



##### *Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*




```r
na_number <- sum(is.na(activity$steps))

na_number
```

```
## [1] 0
```

```r
percentage_na <- mean(is.na(activity$steps))

percentage_na
```

```
## [1] 0
```

Total number of missing values in the dataset amounts to **0 ** (what is **0** % of total observations).



##### *Devise a strategy for filling in all of the missing values in the dataset*



As the number of missing values in this dataset is fairly large, we cannot be sure if there is no bias introduced by missing values. Therefore we impute missing values based on average number of steps in particular 5-minutes interval. 



##### *Create a new dataset that is equal to the original dataset but with the missing data filled in.*


```r
non_NAs <- numeric(nrow(activity))

for (i in 1:nrow(activity))
{
      if (is.na(activity[i,"steps"])==TRUE)
          {
                  non_NAs[i]<-filter(daily_activity_patterns,interval==activity[i,"interval"]) %>% select(average)
            } 
        else
            {
                  non_NAs[i]<-activity[i,"steps"]

            }

}

activity_non_NAs<-mutate(activity,steps_no_NAs=non_NAs)
head(activity_non_NAs)
```

```
##   steps       date interval steps_no_NAs
## 1     0 2012-10-02        0            0
## 2     0 2012-10-02        5            0
## 3     0 2012-10-02       10            0
## 4     0 2012-10-02       15            0
## 5     0 2012-10-02       20            0
## 6     0 2012-10-02       25            0
```



Below code is just to verify if process of imputing missing values correctly preserved original values (lines with no NAs)   


```r
check <- filter(activity_non_NAs,!is.na(steps)) %>% mutate(ok = (steps==steps_no_NAs))

mean(check$ok)
```

```
## [1] 1
```



##### *Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day*




```r
StepsPerDay_noNAs <- activity_non_NAs %>% mutate(steps_no_NAs=as.numeric(steps_no_NAs)) %>% group_by(date) %>% summarise(TotSteps=sum(steps_no_NAs))

hist(StepsPerDay_noNAs$TotSteps,col="green",breaks=20,main="Total steps per day",xlab="Steps per day")

abline(v=median(StepsPerDay$TotSteps),lty=3, lwd=2, col="blue")

legend(legend="median","topright",lty=3,lwd=2,bty = "n")
```

![](PA1_template_files/figure-html/histogram_no_NAs-1.png)<!-- -->




```r
summary(StepsPerDay_noNAs$TotSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```



Imputing missing values, mean of the total number of steps taken per day  increased while median decreased,compared to estimates from the first part (ingoring missing values). Imputing missing data resulted in increase of total daily number of steps (instead of each NAs we have average that is always >=0)



### 5.Are there differences in activity patterns between weekdays and weekends?



##### *Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day*




```r
library(lubridate)

is_weekday <-function(date){
        if(wday(date)%in%c(1,7)) result<-"weekend"
        else
                result<-"weekday"
        result
}

activity_non_NAs <- mutate(activity_non_NAs,date=ymd(date)) %>% mutate(day=sapply(date,is_weekday))

table(activity_non_NAs$day)
```

```
## 
## weekday weekend 
##   11232    4032
```


##### *Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)*



```r
library(ggplot2)

daily_activity_patterns <- activity_non_NAs %>% mutate(day=factor(day,levels=c("weekend","weekday")),steps_no_NAs=as.numeric(steps_no_NAs)) %>% group_by(interval,day) %>% summarise(average=mean(steps_no_NAs))

qplot(interval,average,data=daily_activity_patterns,geom="line",facets=day~.)
```

![](PA1_template_files/figure-html/weekend_comparison-1.png)<!-- -->
