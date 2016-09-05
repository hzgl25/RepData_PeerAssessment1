## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository and the SHA-1 commit ID for your
repository state.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.



### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data (if necessary) into a format suitable for your analysis

install.packages("reshape2", repos='http://cran.us.r-project.org')
install.packages("ggplot2", repos='http://cran.us.r-project.org')
library("reshape2", lib.loc="C:/Users/hzgl25/Documents/R/win-library/3.3")
library("ggplot2", lib.loc="C:/Users/hzgl25/Documents/R/win-library/3.3")

activity <- read.csv("activity.csv")


### What is mean total number of steps taken per day?
#####The data from this csv will be transformed, a mean and median calculated and a histogram displayed.
meltedActivity <- melt(activity, id=c("date"), na.rm=TRUE, measure.vars="steps")
castedActivity <- dcast(meltedActivity, date ~ variable, sum)
hist(castedActivity$steps)

actMean <- format(round(mean(castedActivity$steps), 2), nsmall = 2)
actMedian <- median(castedActivity$steps)

The mean is 10766.19 and the median is 10765.

### What is the average daily activity pattern?

The data is reshaped to produce a plot based on interval time.

meltedInterval <- melt(activity, id=c("interval"), na.rm=TRUE, measure.vars="steps")
castedInterval <- dcast(meltedInterval, interval ~ variable, mean)
plot( castedInterval$interval, castedInterval$steps, type="l")

maxRow <- castedInterval[castedInterval$steps==max(castedInterval$steps),]

The maximum number of steps is 206.1698 at time interval 835.

### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

x <- activity$steps
x1 <- length(which(is.na(x)))

There are 2304 missing values.
We will now impute the missing values. The method to impute the value is by assigning the average number of steps for each interval into those intervals with NA's. With the imputed missing values we will create a histogram and calculate a mean and median as before.

activityNa <- is.na(activity$steps)
castedIntervalAdj <- cbind(castedInterval, as.integer(round(castedInterval$steps)))
nonNaActivity <- activity[!activityNa,]
NaActivity <- activity[activityNa,]
NaResolved <- merge(NaActivity, castedIntervalAdj, by.x = "interval", by.y = "interval", all=FALSE )
NaResolved$steps.x <- NULL
NaResolved$steps.y <- NULL
names(NaResolved)[3] <- paste("steps")
NaResolvedActivity <- rbind(NaResolved, nonNaActivity)
meltedActivity <- melt(NaResolvedActivity, id=c("date"), na.rm=TRUE, measure.vars="steps")
castedActivity <- dcast(meltedActivity, date ~ variable, sum)
hist(castedActivity$steps)

impMean <- format(round(mean(castedActivity$steps), 2), nsmall = 2)
impMedian <- median(castedActivity$steps)

The mean is 10765.64 and the median is 10762. In this case the mean and median both were lower which seemed counter intuitive. But when you looked at the missing values, they were missing for entire days not for selected intervals within a day. What happened is that the methodology used to calcualte the missing values was based on interval time. Due to rounding the sum for a day for these intervals were a little lower than the mean for the days. This could have gone the other way. Also if it was some intervals that were missing for days that had other values, the mean and median could have increased.


### Are there differences in activity patterns between weekdays and weekends?
This is a time series of the data that is imputed for weekends and weekdays. As you can see, the weekend distribution of steps is more spread out over the time periods than the weekday spread.

wd <- !(weekdays(as.Date(NaResolvedActivity$date)) %in% c('Saturday','Sunday'))
wdwe <- c("", "")
for (i in 1:length(wd)) {
  if (wd[i]) {wdwe[i] <- "Weekday"} else {wdwe[i] <- "Weekend"}
}
NaResolvedActivity[, "dayType"] <- factor(wdwe)
p <- ggplot(NaResolvedActivity, aes(x=interval, y=steps)) + geom_line()
melted <- melt(NaResolvedActivity, id=c("interval", "dayType"), na.rm=TRUE, measure.vars="steps")
casted <- dcast(melted, interval + dayType ~ variable, mean)
p <- ggplot(casted, aes(x=interval, y=steps)) + geom_line() + ylab("Number of Steps")
p + facet_wrap(~ dayType, ncol=1)
