library(intervals)
library(ggplot2)
library(plyr)
library(reshape)

# Tomcat log file, log format "%a %u %t %T %b %m %U %s"
log_ts <- "2014-04-12"
logfile <- paste0("perf.",log_ts,".txt")
# All of our timeseries data will be based on unix epoch
epoch <- '1970-01-01'

#### Read and preprocess data ####
# Read the logfile, split on spaces, consider dash as N/A
logs <- read.table( file = logfile, na.strings = "-", stringsAsFactors = FALSE)
colnames(logs) <- c("address", "remote_user", "request_end", "request_end_tz", "elapsed", "response_bytes", "method", "uri", "status")
# Combine the date and timezone into one string
cat_dates <- apply(logs[,c('request_end','request_end_tz')], 1, function(d) paste(d, collapse = ' '))
# Parse dates
logs$request_end <- as.POSIXct(strptime(cat_dates, "[%d/%b/%Y:%T %z]"))
# No need to keep most of these fields for this demo
logs$address <- NULL
logs$remote_user <- NULL
logs$response_bytes <- NULL
logs$method <- NULL
logs$uri <- NULL
logs$request_end_tz <- NULL

# Convert status code into factors
logs$status <- as.factor(logs$status)

# Calculate the request start time
logs$request_begin <- logs$request_end - logs$elapsed
# Reorder columns
logs <- logs[,c('request_begin','request_end','elapsed','status')]


# Now, we want to compute the concurrent connection count
# create a sequence containing every second from the first request starting, to the last request completing.
time_range <- logs$request_begin[1]:logs$request_end[dim(logs)[1]]
# create an interval structure for every request
i <- Intervals(cbind(logs$request_begin, logs$request_end))
# Make a dataframe combining the timestamp and connection count
# Connection count is determined from how many connection intervals overlap each second.
conn_count <- data.frame(time=as.POSIXct(time_range,origin=epoch),
                         connections=sapply(time_range, function(x) length(interval_overlap(x,i)[[1]])))

# round to a multiple
mround <- function(x,multiple){ 
  multiple*round(x/multiple) 
} 

# Compute various metrics for the request delay (mean, max, median)
# Create a new dataset with times in seconds (in 5 sec chunks) and delay time
logs$request_begin_10sec <- mround(as.numeric(logs$request_begin),10)
res_stats <- ddply(logs, .(request_begin_10sec),summarize,
                   mean_response = mean(elapsed),
                   median_response = median(elapsed),
                   max_response = max(elapsed),
                   min_response = min(elapsed))
res_stats$request_begin <- as.POSIXct(res_stats$request_begin_10sec, origin=epoch)
# delete unnecessary column
res_stats$request_begin_10sec <- NULL
# Add NA's to measurements where we have no data.  Prevents lines from being added where we have no data.
res_stats_complete <- data.frame(request_begin=mround(time_range,10))
res_stats_complete <- merge(x = res_stats_complete, y = res_stats, all.x=TRUE)


### Plotting ###
# Plot concurrent connections over time.
conn_count_long <- melt(conn_count, id=c("time"))
cplot <- ggplot(conn_count_long, aes(x=time, y=value, group=variable)) +
  geom_line() +
  ylab("HTTP Connections") +
  xlab("Time") +
  ggtitle(paste0("Tomcat Concurrent Connections (",log_ts,")"))
cplot
ggsave(plot=cplot, file=paste0("concurrent-conns.",log_ts,".png"))

# Plot delay statistics over time
res_stats_long <- melt(res_stats_complete, id=c("request_begin"))
dplot <- ggplot(res_stats_long, aes(x=request_begin, y=value, group=variable)) +
  geom_line(aes(colour = variable)) +
  labs(x="Time", y="Response Time (seconds)", title=paste0("Tomcat HTTP Response Time (",log_ts,")")) +
  scale_colour_hue(name="Statistic",
                      breaks=c("mean_response", "median_response", "max_response","min_response"),
                      labels=c("Mean", "Median", "Maximum","Minimum"))
dplot
ggsave(plot=dplot, file=paste0("response-time.",log_ts,".png"))

# # Graphs Combined
# # change connections to 10sec rounding
# conn_count$request_begin_10sec <- mround(as.numeric(conn_count$time),10)
# conn_count_max <- ddply(conn_count, .(request_begin_10sec),summarize,
#                    max_conns = max(connections),na.rm=F)
# 
# # conn_count will have an entry for all times, so use its X value
# x <- conn_count_max$request_begin_10sec
# d1 <- data.frame(x=x)
# # merge d1 with res_stats, keeping all of d1, 
# d1merge <- merge(x = d1, y = res_stats, by.x = "x", by.y="request_begin", all.x=TRUE)
# d1 <- data.frame(x=x,y=d1merge$max_response)
# d2 <- data.frame(x=x,y=conn_count_max$max_conns)
# 
# 
# d1$panel <- "Max Response"
# d2$panel <- "Connections"
# 
# d <- rbind(d1,d2)
# # Use dates
# d$x <- as.POSIXct(d$x, origin=epoch)
# 
# p <- ggplot(data = d, mapping = aes(x = x, y = y))
# p <- p + facet_grid(panel ~ ., scale = "free")
# p <- p + layer(data = d1,  geom = c( "point"), stat = "identity")
# p <- p + layer(data = d2, geom = c("line"), stat="identity")
# p
