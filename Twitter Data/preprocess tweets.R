require(stringr)
library(plyr)
library(ggplot2)
setwd('/Users/apple/Desktop/phd/files-for-Jiale-previously-Ben')

# things to count
wanted_regex <- c('^(?=.*\\b()\\b)(?=.*\\b(freight|not moving|
standstill|stand still|slow|distrupt|stuck|stucks|delayed|delays|
delay|standing around|hanging around|redsignal|marooned|non moving|
not moving|stoppage|lingering|holding|setback|problem|issue|snail|
wait|waits|waste|wasted|late|cancelled|crawling|poor|miss)\\b).*$')


# these can be arbitrarily complex

# use read.table to chunk file into a data.frame
N <- 100000  # number of rows / tweets to read at a time
fh <- file('cleaned_tweets2015.csv', 'r')  # file handle
while(TRUE) {
  # get chunk
  df <- read.table(
    fh,
    nrow = N,
    header = FALSE,
    col.names = c('timestamp', 'text'),
    sep = ",",
    quote = "\"",
    stringsAsFactors = FALSE,
    fill = TRUE
  )
  
  
  
  # convert time to the hour past epoch [1970-01-01 (UTC)] i.e. we will bin the results per hour
  hours <- floor( unclass( as.POSIXct(df$timestamp)) / 900 )
  
  # regex the tweets
  rv <- aggregate(df$text, by = list(hour_past_epoch = hours), FUN = function(x) {
    # apply each regex in turn
    vapply(wanted_regex, FUN = function(regex) {
      # note that grepl is vectorized so we are counting how many of the tweets for a bin / hour match the regex
      sum(grepl(regex, x, ignore.case = TRUE, perl = TRUE) )
    }, FUN.VALUE = integer(1) )
  })
  
  # this is a bit hacky
  if(exists('results')){
    results <- rbind(results, rv)
  } else {
    results <- rv
  }
  
  # reached the end of the file
  if( nrow(df) < N ){
    break
  }
}
#close( fh )


# finally aggregate the chunk results
results <- aggregate(.~hour_past_epoch , results, FUN = sum)

results$hour_past_epoch=as.POSIXct(results$hour_past_epoch*900, origin="1970-01-01") # Converting to normal time

date=substr(results$hour_past_epoch,start=0,stop=10)
time=str_sub(results$hour_past_epoch,start=-8)
results=results[,!names(results) %in% "hour_past_epoch"]
delays=dim(df)[1]
results=data.frame(date,time,results)



par(mfrow = c(1,1))
par(mar = c(4.5,4.5,0.5,0.5))
counts <- aggregate(results$results, by=list(Category=results$time), FUN=sum)
plot(seq(0.25,24, by = 0.25),counts$x, type = 'b', lty = 1, lwd = 2, col = 2,
     ylab = 'Number of Delayed Tweets', xlab = 'Hour in the Day (15 mins)', cex.axis = 2, cex.lab = 2)








timedata=data.frame(results$time,results$results)
table=ddply(timedata,"results.time",numcolwise(sum))
qplot(x=results.time,y=results.results,data=table)+geom_line(aes(group=1))

datedata=data.frame(results$date,results$results)
table=ddply(datedata,"results.date",numcolwise(sum))
qplot(x=results.date,y=results.results,data=table)+geom_line(aes(group=1))
 

saveRDS(results, 'aggregated1.Rdata')

aggregated1 <- readRDS('aggregated1.Rdata')
aggregated <- readRDS('aggregated.Rdata')