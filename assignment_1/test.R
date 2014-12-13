# Loading Data
options(warn=-1)
data<- read.csv("activity.csv")
# Subsetting and finding mean steps per day
day <- levels(data$date)
steps <- vector("numeric",length(day))
    for (i in seq_along(day)){
        vect <- which(data$date == day[i])
        steps[i]<-sum(data$steps[vect], na.rm=TRUE)
    }

p1 <- hist(steps, col = 'blue', breaks = seq(from = 0, to = 25000, by = 1000), main = 'Histogram of the total number of steps taken each day', xlab = 'Steps')

   # Subsetting to cover 5 minute interval
   interval <- data$interval[1:288]
   insteps <- vector("numeric",length(interval))
   for (i in seq_along(interval)){
       vect <- which(data$interval == interval[i])
       insteps[i]<-mean(data$steps[vect], na.rm = TRUE)
   }

good <- complete.cases(data)
missing <- length(data$steps)-length(data$steps[good])
# replaced NA values over average steps/interval over all intervals
temp <- replace(data$steps, is.na(data$steps), mean(insteps))
newsteps <- vector("numeric",length(day))
for (i in seq_along(day)){
    vect <- which(data$date == day[i])
    newsteps[i]<-sum(temp[vect])
}

p2 <- hist(newsteps, col = 'red', breaks = seq(from = 0, to = 25000, by = 1000), main = 'Histogram of the total number of steps taken each day', xlab = 'Steps')

plot( p1, col=rgb(0,0,1,1/4), ylim = c(0,20) )  # first histogram
plot( p2, col=rgb(1,0,0,1/4), ylim = c(0,20), add=T)  # second

