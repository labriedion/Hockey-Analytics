# Goalie time series analysis by Étienne Labrie-Dion
# Calculate the save percentage per minute in an NHL game.

require(dplyr)
require(TTR)
require(ggplot2)

SVminute <- function(pbpseason) {    
  #5v5 any score
  load(pbpseason)
  pbp %>%
    filter(Strength.State == "5v5") ->
    pbp_5
  
  #classify seconds into minute bins
  bins <- seq(0 , 3600, by=60)
  Minutes <- seq(0, 59)
  pbp_5$Seconds <- as.numeric(pbp_5$Seconds)
  pbp_5$Minutes <- cut(pbp_5$Seconds, bins, labels = Minutes)
  
  #count goals and shots and calculate total SV per minute
  goals <- aggregate(pbp_5$Event == "GOAL", by=list(pbp_5$Minutes), sum)
  shots <- aggregate(pbp_5$Event == "SHOT", by=list(pbp_5$Minutes), sum)
  sv =  ((shots[2]/(shots[2]+goals[2])))
  return(sv)
}

#Loop over the play-by-play for every available season at Corsica Hockey http://www.corsica.hockey/data/
sv20072008 <- SVminute("~/Downloads/pbp20072008.Rda")
sv20082009 <- SVminute("~/Downloads/pbp20082009.Rda")
sv20092010 <- SVminute("~/Downloads/pbp20092010.Rda")
sv20102011 <- SVminute("~/Downloads/pbp20102011.Rda")
sv20112012 <- SVminute("~/Downloads/pbp20112012.Rda")
sv20122013 <- SVminute("~/Downloads/pbp20122013.Rda")
sv20132014 <- SVminute("~/Downloads/pbp20132014.Rda")
sv20142015 <- SVminute("~/Downloads/pbp20142015.Rda")
sv20152016 <- SVminute("~/Downloads/pbp20152016.Rda")

svtotal <- cbind(sv20072008,sv20082009, sv20092010, sv20102011,sv20112012, 
                          sv20122013, sv20132014,sv20142015,sv20152016)

svtotal <- cbind(seq(0,59), svtotal)
colnames(svtotal) <- c("Minute", "2007-2008","2008-2009","2009-2010", "2010-2011", "2011-2012", 
                       "2012-2013", "2013-2014", "2014-2015", "2015-2016" )

svmean <- rowMeans(svtotal[,-1])


#We need to split the periods otherwise the rolling average will average values from one end of a period with the start of the other
#Splitting the periods separately to perform rolling average

p1 <- svmean[1:20]
p2 <- svmean[21:40]
p3 <- svmean[41:59]

#Calculating the rolling average for the three periods.
#Rolling average with TTR
p1TMA <- SMA(p1, n=5)
p2TMA <- SMA(p2, n=5)
p3TMA <- SMA(p3, n=5)
sma_average <- c(p1TMA,p2TMA,p3TMA)
plot.ts(sma_average)

#Another option is HoltWinters exponential smoothing 
p1expo <- HoltWinters(p1, gamma=FALSE)
p2expo <- HoltWinters(p2, gamma=FALSE)
p3expo <- HoltWinters(p3, gamma=FALSE)
exponential_average <- c(p1expo$fitted[,1], p2expo$fitted[,1], p3expo$fitted[,1])
plot.ts(expo)

#Plot the time series
any5v5 <- c(p1TMA,p2TMA,p3TMA)
plot.ts(any5v5)

#Graphing with ggplot2
data <- sma_average #or exponential_average
names(data) <- c("Minute", "SV")
ggplot(data, aes(x = Minute, y = SV)) +
  ggtitle("Average save percentage by minute in NHL, 2007-2016")+
  geom_line(color = 'coral1', size = 1)+
  xlab("Minute") +
  ylab("Save Percentage") +
  theme(legend.position='none')
