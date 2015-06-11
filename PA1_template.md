# Reproducible Research: Peer Assessment 1
Aashish Shetty  

### Report generated on June 12, 2015




```r
#Load Libraries needed
library(data.table)
library(dplyr)
library(lattice)
```

## Loading and preprocessing the data

```r
    #Load the data (i.e. read.csv())
    #csv<-read.csv("activity.csv",colClasses=c("numeric","Date","numeric",NA) )
    csv<-read.csv("activity.csv",as.is=F )

    #Transform  data to data table to help with analysis
    data_table<-data.table(csv)
```



## What is mean total number of steps taken per day?

```r
    by_date<-group_by(data_table,date)
    steps_summary<-summarise(by_date,step_count=sum(steps,na.rm=T))
    histogram(~step_count,
        data=steps_summary,
        type="count",
        ylim=c(0,12),
        breaks=40,
        xlab="No. of Steps per day",
        main="Histogram of no. of steps per day")
```

![](PA1_template_files/figure-html/steps_summary-1.png) 

###Mean of no. of steps is 9354.2295  
###Median of no. of steps is 10395   
  
***  
  
## What is the average daily activity pattern?


```r
by_interval<-group_by(data_table,interval)
interval_summary<-summarise(by_interval,interval_avg=mean(steps,na.rm=T))

xyplot( interval_avg ~ interval,
    data = interval_summary,
    type = "l",
    xlab= "Interval", 
    ylab= "Average number of steps take in an interval across all days",
    main="Time series plot:average steps of the 5-minute interval, across all days",
    col.line = c(rep("blue",3), "red"))
```

![](PA1_template_files/figure-html/avg_daily_pattern-1.png) 


###On average across all the days in the dataset, interval 835 contains the maximum number of steps
    
***  

## Imputing missing values

```r
new_data_table<-copy(data_table)

for(i in 1:nrow(new_data_table))
    {
        if(is.na(new_data_table[i]$steps))
        {
            new_data_table[i]$steps = 
                as.integer(interval_summary[interval==new_data_table[i]$interval]$interval_avg)
        }
    }

by_date2<-group_by(new_data_table,date)
steps_summary2<-summarise(by_date2,step_count=sum(steps,na.rm=T))

histogram(~step_count,
    data=steps_summary2,
    type="count",
    ylim=c(0,12),
    breaks=40,
    xlab="No. of Steps per day",
    main="Histogram of no. of steps per day")
```

![](PA1_template_files/figure-html/imputing_missing_values-1.png) 


###Mean of no. of steps is 9354.2295  
###Median of no. of steps is 10395   
###Means of no. of steps is 10749.7705  
###Median of no. of steps is 10641   

***  

## Are there differences in activity patterns between weekdays and weekends?


```r
isWeekday<-function(x){
    if(grepl("Sunday|Saturday",x))
        return("Weekend")
    else
        return("Weekday")
}

#Add Weekday/Weekend - day of the week column as day
data_table<-data_table[,day:=sapply(weekdays(as.Date(date , format = "%Y-%m-%d")),isWeekday)]

day_interval<-group_by(data_table,day,interval)
week_summary<-summarise(day_interval,interval_avg=mean(steps,na.rm=T))

xyplot( interval_avg ~ interval| day,
    data = week_summary,
    type = "l",
    lty = c(1, 2, 2, 1),
    lwd = c(1, 1, 1, 3),
    layout=c(1,2),
    col.line = c(rep("blue",3), "red"))
```

![](PA1_template_files/figure-html/weekend_activity-1.png) 


