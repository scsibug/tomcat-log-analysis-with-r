library(intervals)
library(ggplot2)
library(plyr)
library(reshape)
epoch <- '1970-01-01'
logfile <- "perf.2014-04-12.txt"
# Read the logfile, split on spaces, consider dash as N/A
logs <- read.table( file = logfile, na.strings = "-", stringsAsFactors = FALSE)
# Combine the date and timezone into one string
cat_dates <- apply(logs[,3:4], 1, function(d) paste(d, collapse = ' '))
# Remove the surrounding 
dates <- as.POSIXct(strptime(cat_dates, "[%d/%b/%Y:%T %z]"))
# No need to keep the user field
logs <- logs[,-2]
# Or the extra timezone field
logs <- logs[,-3]
# or the client IP
logs <- logs[,-1]
# And we'll store the parsed date where the string date is
logs[,1] <- dates
# Give the columns labels
colnames(logs) <- c("request_end", "elapsed", "response_bytes", "method", "uri", "status")
# Convert userids into factors
#logs$userid <- as.factor(logs$userid)

# Calculate the request start time
logs$request_begin <- logs$request_end - logs$elapsed
# Reorder columns
logs <- logs[,c(7,1,2,3,4,6,5)]


# Now, we want to compute the concurrent connection count
first_connect <- as.numeric(logs$request_begin[1])
last_connect <- as.numeric(logs$request_end[dim(logs)[1]])
i <- Intervals(cbind(logs$request_begin, logs$request_end))
# Make a dataframe combining the timestamp and connection count
conn_count_ts <- as.POSIXct(first_connect:last_connect,origin=epoch)
conn_count <- data.frame(time=as.POSIXct(first_connect:last_connect,origin=epoch),
                         connections=sapply(first_connect:last_connect, function(x) length(interval_overlap(x,i)[[1]])))

# convenient rounding function
mround <- function(x,base){ 
  base*round(x/base) 
} 

# We need to create a metric for the delay
# Create a new dataset with times in seconds (in 5 sec chunks) and delay time
logs$request_begin_10sec <- mround(as.numeric(logs$request_begin),10)
res_stats <- ddply(logs, .(request_begin_10sec),summarize,
                   mean_response = mean(elapsed),
                   max_response = max(elapsed),
                   median_response = median(elapsed),
                   sd_response = sd(elapsed),
                   rsd_response = 100*(sd(elapsed)/mean(elapsed)));
res_stats$request_begin <- as.POSIXct(res_stats$request_begin_10sec, origin='1970-01-01')
# get rid of unnecessary columns, for now
res_stats$request_begin_10sec <- NULL
res_stats$rsd_response <- NULL
res_stats$sd_response <- NULL


### Plotting ###
# Plot concurrent connections over time.
conn_count_long <- melt(conn_count, id=c("time"))
cplot <- ggplot(conn_count_long, aes(x=time, y=value, group=variable)) +
  geom_line() +
  ylab("HTTP Connections") +
  xlab("Time") +
  ggtitle("Tomcat Concurrent Connections")
cplot

# Plot delay statistics over time
res_stats_long <- melt(res_stats, id=c("request_begin"))
dplot <- ggplot(res_stats_long, aes(x=request_begin, y=value, group=variable)) +
  geom_point(aes(colour = variable)) +
  labs(x="Time", y="Response (seconds)", title="Tomcat HTTP Response Time") +
  scale_colour_hue(name="Metric",
                      breaks=c("mean_response", "max_response", "median_response"),
                      labels=c("Mean", "Maximum", "Median"))
dplot


# My graphs, combined
# change connections to 10sec rounding
conn_count$request_begin_10sec <- mround(as.numeric(conn_count$time),10)
conn_count_max <- ddply(conn_count, .(request_begin_10sec),summarize,
                   max_conns = max(connections),na.rm=F)

# conn_count will have an entry for all times, so use its X value
x <- conn_count_max$request_begin_10sec
d1 <- data.frame(x=x)
# merge d1 with res_stats, keeping all of d1, 
d1merge <- merge(x = d1, y = res_stats, by.x = "x", by.y="request_begin", all.x=TRUE)
d1 <- data.frame(x=x,y=d1merge$max_response)
d2 <- data.frame(x=x,y=conn_count_max$max_conns)


d1$panel <- "Max Response"
d2$panel <- "Connections"

d <- rbind(d1,d2)
# Use dates
d$x <- as.POSIXct(d$x, origin=epoch)

p <- ggplot(data = d, mapping = aes(x = x, y = y))
p <- p + facet_grid(panel ~ ., scale = "free")
p <- p + layer(data = d1,  geom = c( "point"), stat = "identity")
p <- p + layer(data = d2, geom = c("line"), stat="identity")
p
