# NHL_by_the_minute_Timeseries_Analysis for Relievers by Étienne Labrie-Dion
# Playing minutes are aligned in this analysis; 0 minute is the start of the goalie's shift, not the game

# Analyze and output time series graphs showing average save percentage for a given Season, Season.Type or Goalie
# Uses the NHL_by_minute or NHL_by_minute5v5 datasets, compiled from Corsica Hockey's pbp datasets

require(dplyr)
require(TTR)
require(ggplot2)

##### Options and Filtering #####

#Options
averages <- 5 #number of minutes included in the rolling average

# Filter the dataset here:
NHL_by_minute5v5 %>%
  filter(Season.Type == 'Regular' & Starter == FALSE & Finisher == TRUE) -> df

# How to filter the dataset
# Ex: (Season == '20132014' & Season.Type == 'Regular & Goalie == 'CAREY.PRICE')
# To classify starters and relievers:
#   Played the whole game: (Starter == TRUE & Finisher == TRUE)
#   Relievers:             (Starter == FALSE & Finisher == TRUE)
#   Replaced mid-game:     (Starter == TRUE & Finisher == FALSE)


##### Calculations #####

#Save percentage per minute
goals <- aggregate(df$GA, by=list(df$MinutesPlayed), sum)
shots <- aggregate(df$SA, by=list(df$MinutesPlayed), sum)
sv =  ((shots[2]/(shots[2]+goals[2])))

#Can't split the periods easily if goalies start at random times
#Rolling average with TTR
sma_average <- SMA(sv[1:40,], n=averages)

#Another option is HoltWinters exponential smoothing 
exponential_average <- HoltWinters(sv[1:41,], beta=FALSE, gamma=FALSE)

##### Graphing with ggplot2 #####
data <- cbind(seq(1,40), sma_average) #sma_average or exponential_average$fitted[,1]
data <- data.frame(data)
names(data) <- c("Minute", "SV")
ggplot(data, aes(x = Minute, y = SV)) +
  ggtitle("Average save percentage for relievers for each minute played") +
  geom_line(color = 'coral2', size = 1) +
  xlab("Minute") +
  ylab("Save Percentage") +
  theme(legend.position='none')
