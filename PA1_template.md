# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

'''{r, echo=TRUE}
setwd("C:/Users/Adarsh/Desktop/Coursera/repres")
y <- read.csv("activity.csv", header=TRUE)
y1 <- nrow(subset(y, is.na(steps)))
x <- subset(y, !(is.na(steps)))
z <- aggregate(x$steps, FUN= sum, list(x$date))
hist(z$x, xlab = "#steps", main = "Histogram of #steps")
'''


## What is mean total number of steps taken per day?
'''{r, echo = TRUE}
z1 <- aggregate(x$steps, FUN= mean, list(x$date))
'''

## What is the average daily activity pattern?
'''{r, echo = TRUE}
z3 <- aggregate(x$steps, FUN= mean, list(x$interval))
plot(z3$Group.1, z3$x,xlab= "Time interval", ylab= "Average number of steps")
'''

## Imputing missing values
'''{r, echo = TRUE}
for (i in 1:nrow(MyData))
     {if (is.na(MyData[i,1]))
        {for (j in 1: nrow(z3))
            {if (MyData[i,3] == z3[j,1])
              MyData[i,1] <- z3[j,1]
          }
        }
      }

write.csv(MyData, file = "MyData.csv")

t1 <- aggregate(MyData$steps, FUN= mean, list(MyData$date))
t2 <- aggregate(MyData$steps, FUN= median, list(MyData$date))
t3 <- aggregate(MyData$steps, FUN= mean, list(MyData$interval))
plot(t3$Group.1, t3$x,xlab= "Time interval", ylab= "Average number of steps")

'''

## Are there differences in activity patterns between weekdays and weekends?
''' {r, echo = TRUE}
t4 <- data.frame(as.POSIXlt(t1$date)$wday, t1$steps))
plot(t4)


library(knitr)
knit2html(input = "PA1_template.Rmd")
'''

