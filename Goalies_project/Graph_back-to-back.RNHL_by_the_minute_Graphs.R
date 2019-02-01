#NHL_by_the_minute_Timeseries_Analysis by Étienne Labrie-Dion
#Analyze and output time series graphs showing average save percentage for a given Season, Season.Type or Goalie
#Uses the NHL_by_minute or NHL_by_minute5v5 datasets

require(dplyr)
require(TTR)
require(ggplot2)

##### Options and Filtering #####

#Options
averages <- 5 #number of minutes included in the rolling average

# Filter the dataset here:
NHL_by_minute %>%
  filter(Season.Type == 'Regular' & Starter == TRUE & Finisher == TRUE) -> df

# How to filter the dataset
# Ex: (Season == '20132014' & Season.Type == 'Regular & Goalie == 'CAREY.PRICE')
# To classify starters and relievers:
#   Played the whole game: (Starter == TRUE & Finisher == TRUE)
#   Reliever:              (Starter == FALSE & Finisher == TRUE)
#   Replaced mid-game:     (Starter == TRUE & Finisher == FALSE)


##### Calculations #####

#Save percentage per minute
goals <- aggregate(df$GA, by=list(df$Minute), sum)
shots <- aggregate(df$SA, by=list(df$Minute), sum)
sv =  ((shots[2]/(shots[2]+goals[2])))

#Spliting the periods for rolling averages
p1 <- sv[1:20,]
p2 <- sv[21:40,]
p3 <- sv[41:60,]

#Rolling average with TTR
p1TMA <- SMA(p1, n=averages)
p2TMA <- SMA(p2, n=averages)
p3TMA <- SMA(p3, n=averages)
sma_average <- c(p1TMA, p2TMA, p3TMA)

#Another option is HoltWinters exponential smoothing 
p1expo <- HoltWinters(p1, gamma=FALSE)
p2expo <- HoltWinters(p2, gamma=FALSE)
p3expo <- HoltWinters(p3, gamma=FALSE)
exponential_average <- c(p1expo$fitted[,1], p2expo$fitted[,1], p3expo$fitted[,1])


##### Graphing with ggplot2 #####
data <- cbind(seq(0,59), sma_average) # sma_average or exponential_average
data <- data.frame(data)
names(data) <- c("Minute", "SV")
ggplot(data, aes(x = Minute, y = SV)) +
  ggtitle(paste0("Average save percentage for each minute of an NHL game")) +
  geom_line(color = 'coral2', size = 1) +
  xlab("Minute") +
  ylab("Save Percentage") +
  theme(legend.position='none')
