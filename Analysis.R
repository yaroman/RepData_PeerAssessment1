
##df <- read.csv("AMD/activity.csv",stringsAsFactors = FALSE)
df <- read.csv("Reproducible_Research/RepData_PeerAssessment1/activity.csv")
##df <- df[which(is.na(df$steps) != TRUE),]
df$date <- as.Date(df$date, format = "%Y-%m-%d")

#---------Summarize-------------------------------------
#library(dplyr)
#by_day <- group_by(df, date)
#df.byday <- summarize(by_day , steps = sum(steps))
#hist(df.byday$steps, xlab='Steps', main = 'Histogram of total steps')

## OR

##df.byday <- aggregate(df[, 'steps'], by=list(df$date), FUN=sum)
df.byday <- aggregate(df[, 'steps'], by=list(df$date), FUN=sum, na.rm=TRUE)
names(df.byday) <- c('date','steps')
hist(df.byday$steps, xlab='Steps', main = 'Histogram of total steps')

# OR

#df.byday <- tapply(df$steps, df$date, sum)
#hist(df.byday, xlab='Steps', main = 'Histogram of total steps')

#--------------------------------------------------------------




df.byday.mean <- toString(round(mean(df.byday$steps, na.rm=TRUE)))
df.byday.median <- toString(round(median(df.byday$steps, na.rm=TRUE)))

abline(v = df.byday.mean, col = "red", lwd = 4)
abline(v = df.byday.median, col = "blue", lwd = 4)

legend("topright", legend = c(paste("Mean is", df.byday.mean), 
                              paste("Median is ", df.byday.median)), col = c("red", "blue"), 
       lty = 1)
#===========================================================

#-------------------------Time series---------

df.byinterval <-  aggregate(df[, 'steps'], by=list(df$interval), FUN=mean, na.rm=TRUE)
names(df.byinterval) <- c('interval','steps')

with(df.byinterval, {
        plot(
                x=interval,
                y=steps,
                type="l",
                main="Time-Series of Average Steps against Interval",
                xlab="5-minute Interval",
                ylab="Average Steps, Average across all Days"
                
        )
})

df.byinterval.max.steps <- toString(df.byinterval[which(df.byinterval$steps == max(df.byinterval$steps)),]$interval)
#===========================================================

#---------------------------------------------Imputing missing values-------------------------------------
# 1.
nrow(subset(df, is.na(df$steps) == TRUE))

# 2. 3. 
# use mean function to fill out NA value
df.adj <- df
df.adj$steps[is.na(df.adj$steps)] <- ave(df.adj$steps, 
                                         df.adj$interval, 
                                         FUN=function(x) 
                                                 round(mean(x, na.rm = T)))[is.na(df.adj$steps)] 

4. 
df.adj.byday <- aggregate(df.adj[, 'steps'], by=list(df.adj$date), FUN=sum)
names(df.adj.byday) <- c('date','steps')
hist(df.adj.byday$steps, xlab='Steps', main = 'Histogram of total steps')


df.adj.byday.mean <- toString(round(mean(df.adj.byday$steps, , na.rm=TRUE)))
df.adj.byday.median <- toString(round(median(df.byday$steps, na.rm=TRUE)))

abline(v = df.adj.byday.mean, col = "red", lwd = 4)
abline(v = df.adj.byday.median, col = "blue", lwd = 4)

legend("topright", legend = c(paste("Mean is", df.adj.byday.mean), 
                              paste("Median is ", df.adj.byday.median)), col = c("red", "blue"), 
       lty = 1)
#===========================================================


#---Are there differences in activity patterns between weekdays and weekends?-------------------
1. 
my.weekend.function <- function(dte){
        if (weekdays(dte) == 'Saturday' || weekdays(dte) == 'Sunday'){
                result <- 'weekend'
        }
        else{
                result <- 'weekday'	
        }
        
        return(result)
}

df$date_type <- sapply(df[,2], my.weekend.function)
df$date_type <-as.factor(df$date_type)

df.byinterval.bydatetype <-  aggregate(df[, 'steps'], by=list(df$interval, df$date_type), FUN=mean, na.rm=TRUE)
names(df.byinterval.bydatetype) <- c('interval','date_type', 'steps')

#str(df.byinterval.bydatetype)


library("lattice")

xyplot(
        type="l",
        data=df.byinterval.bydatetype,
        steps ~ interval | date_type,
        xlab="Interval",
        ylab="Number of steps",
        layout=c(1,2)
)


#===========================================================


