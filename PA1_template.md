Loading and preprocessing the data
----------------------------------

    unzip("repdata_data_activity.zip")
    raw_data <- read.csv("activity.csv")
    raw_data$date <- as.Date(raw_data$date)

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(knitr)

    ## Warning: package 'knitr' was built under R version 3.4.4

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.4.4

What is mean total number of steps taken per day?
-------------------------------------------------

    steps_per_day <- raw_data %>% 
                    select("steps","date") %>% 
                    group_by(date) %>% 
                    summarise_at("steps",sum,na.rm = T)
    ggplot(steps_per_day, aes(x = steps)) + geom_histogram(fill = "blue",binwidth = 1000) + labs(title = "Histogram of Steps per Day") 

![](PeerAssessment1_files/figure-markdown_strict/TotalNumberPerDay-1.png)

What is the average daily activity pattern?
-------------------------------------------

    Steps_per_Interval <- raw_data %>%
            group_by(interval) %>%
            summarise_at("steps", mean, na.rm = T)

    ggplot(Steps_per_Interval ,aes(x = interval, y = steps)) + geom_line(color = "green")+labs(title = "Average Daily Activity Pattern") 

![](PeerAssessment1_files/figure-markdown_strict/Interval-1.png)

    max <- round(Steps_per_Interval [which.max(Steps_per_Interval$steps),],0)

    kable(max,caption = "Maxiumum Steps and Interval",)

<table>
<caption>Maxiumum Steps and Interval</caption>
<thead>
<tr class="header">
<th align="right">interval</th>
<th align="right">steps</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">835</td>
<td align="right">206</td>
</tr>
</tbody>
</table>

Imputing missing values
-----------------------

-   Total Number of Missing Value

<!-- -->

    missing_val <- sum(is.na(raw_data$steps))
    missing_val_percent <- round(mean(is.na(raw_data$steps)) * 100,0)

The total number of missing values are 2304, 13%

-   Fill in all of the missing value in the dataset

<!-- -->

    Interval_mean <- mean(Steps_per_Interval$steps,na.rm = T)

    fill_data <- raw_data

    fill_data$steps[is.na(fill_data$steps)] <- Interval_mean

    sum(is.na(fill_data$steps))

    ## [1] 0

-   Make a histogram of total number of steps each day, report the mean
    and median.

<!-- -->

    steps_per_day2 <- fill_data %>%  
                    select("steps","date") %>% 
                    group_by(date) %>% 
                    summarise_at("steps",sum,na.rm = T)

    ggplot(steps_per_day2,aes(x = steps)) + geom_histogram(fill = "red",binwidth = 1000) + labs(title = "Histogram of Steps per Day(Imputing Missing Value)") 

![](PeerAssessment1_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    new_mean <-round(mean(steps_per_day2$steps),3)
    new_median <- round(median(steps_per_day2$steps),3)

New Mean is 1.076618910^{4}, New Median is 1.076618910^{4}

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    fill_data$weekday <- as.factor(weekdays(fill_data$date))

    weekend_data <- fill_data %>% filter(weekday == "土曜日"|weekday == "日曜日")
    weekday_data <- fill_data %>% filter(weekday != "土曜日"& weekday != "日曜日")

    weekend_data[,"weekend"] <- "weekend" 
    weekday_data[,"weekend"] <- "weekday" 

    bind_data <- rbind(weekend_data,weekday_data)
    bind_data$weekend <- as.factor(bind_data$weekend)

    week_data <- bind_data %>% group_by(weekend,interval) %>% summarise_at("steps",mean)

    ggplot(week_data,aes(x = interval, y = steps)) + geom_line(color = "orange") + facet_wrap(~ weekend, nrow = 2) + labs(x = "Interval", y = "Numbers of steps")

![](PeerAssessment1_files/figure-markdown_strict/unnamed-chunk-6-1.png)
