# Courseera-reproducible research
Sankar  
May 3, 2017  



## R Markdown
The Purpose of the document is as below

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. 

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Read the data for analysis from a CSV file which is collected from a group enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks


```r
data1=read.csv("activity.csv")
data1$date=as.Date(data1$date)
```

    
    
      

Create a histogram to show the total number of steps taken during each day  


```r
library(ggplot2)
data2=aggregate(data1$steps,by=list(data1$date),FUN="sum")
names(data2)=c("date","steps")

ggplot(data2,aes(x=date,y=steps))+geom_bar(stat = "identity")+
  ylab("Steps")+xlab("Date")+
    scale_x_date(date_labels = "%b %d",date_breaks = "1 day"  ,limits=c(min(data2$date),max(data2$date)))+
  theme_bw()+theme(axis.text.x = element_text(angle=90,hjust = 0))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
  
Do univariate analysis - calculate the mean and median of the steps taken every day


```r
mean(data2$steps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(data2$steps,na.rm=TRUE)
```

```
## [1] 10765
```
  
Time series plot on the average number of steps taken in each 5 minute interval

```r
data3=aggregate(data1$steps,by=list(data1$interval),FUN="mean",na.rm=TRUE)
names(data3)=c("interval","steps")
ggplot(data3,aes(x=interval,y=steps))+geom_line()+scale_x_continuous(breaks = seq(0000,2355,25))+theme(axis.text.x = element_text(angle=90,hjust = 0))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Identify the 5 minute interval which has the maximum number of steps

```r
data3$interval[which.max(data3$steps)]
```

```
## [1] 835
```

Missing data stategy, assign the mean of steps from the time interval to missing values based on time interval. This wouldnt skew data  

```r
data4=merge(data1,data3,by="interval")
data4$steps.x[is.na(data4$steps.x)]=data4$steps.y[is.na(data4$steps.x)]
```

The histogram of steps walked on daily basis after missing values were replaced

```r
data5=aggregate(data4$steps.x,by=list(data4$date),FUN="sum")
names(data5)=c("date","steps")

ggplot(data5,aes(x=date,y=steps))+geom_bar(stat = "identity")+
  ylab("Steps")+xlab("Date")+
    scale_x_date(date_labels = "%b %d",date_breaks = "1 day"  ,limits=c(min(data5$date),max(data5$date)))+
  theme_bw()+theme(axis.text.x = element_text(angle=90,hjust = 0))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

What is the the mean and median of the steps taken every day after the data has been fixed


```r
mean(data5$steps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(data5$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```
As we have added the mean value from each 5 minute interval, the mean is still the same, but the median is now aligned with mean.

Panel plot compare average steps taken per 5 minute interval across weekday and weekend


```r
data4$daytype=ifelse(weekdays(data4$date) %in% c("Sunday","Saturday"),"Weekend","Weekday")
data4$daytype=as.factor(data4$daytype)
data5=aggregate(data4$steps.x,by=list(data4$interval,data4$daytype),FUN="mean",na.rm=TRUE)
names(data5)=c("Interval","daytype","steps")
ggplot(data5,aes(x=Interval,y=steps,group=daytype))+geom_line()+facet_wrap(~daytype)+scale_x_continuous(breaks = seq(0000,2355,50))+theme(axis.text.x = element_text(angle=90,hjust = 0))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Looking at the data we could make this Inference

1. In the weekdays there is a high activity in the morning which could be due to the effort spent in reaching to the workplace/ morning exercise. The activity is moderate throughout the day and again picks up in the evening, when the work is finished

2. In the weekends teh effort is consistently spread across the entire day time implying uniform activity. But the activity is not at a peak level as in weekday morning, but the mean activity throughout the day is more in weekends than in weekdays
