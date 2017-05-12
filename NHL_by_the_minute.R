#NHL_by_the_minute.R by Étienne Labrie-Dion

#Compile shots and goals for each minute of NHL games played from 2007 to 2017 
#for time series analysis of goalie performance
#This script works with Corsica Hockey's play-by-play stats: http://www.corsica.hockey/data/ and is based on Emmanuel Perry's script from Introduction to R (http://corsica.teachable.com/p/an-intro-to-r)

require(dplyr)

by_minute <- function(pbpseason) {   
  
  load(pbpseason)

  # Add the Minute column
  bins <- seq(0 , 10800, by=60)
  Minute <- seq(0, 179)
  pbp$Seconds <- as.numeric(pbp$Seconds)
  pbp$Minute <- cut(pbp$Seconds, bins, labels = Minute)
  
  # Remove strange strength states; you will need to export the different strength states as separate files for analysis
  pbp %>%
    filter(Strength.State %in% c("3v3", "3v4", "4v3", "3v5", "5v3", 
                                 "4v4", "4v5", "5v4", "5v5", "EvE"))->
    pbp_reg
  
  
  # Compile Stats for each Minute of an NHL game
  bind_rows(pbp_reg %>%
              group_by(Home.Goalie, Game.ID, Season, Season.Type, Date, Home.Team, Minute) %>%
              rename(Goalie = Home.Goalie, Team = Home.Team) %>%
              summarise(Venue = "Home",
                        TOI = sum(na.omit(as.numeric(Event.Length)))/60,
                        SA = sum(Event %in% c("GOAL", "SHOT") & ev.team == Away.Team),
                        GA = sum(Event == "GOAL" & ev.team == Away.Team)
              ),
            
            pbp_reg %>%
              group_by(Away.Goalie, Game.ID, Season, Season.Type, Date, Away.Team, Minute) %>%
              rename(Goalie = Away.Goalie, Team = Away.Team) %>%
              summarise(Venue = "Away",
                        TOI = sum(na.omit(as.numeric(Event.Length)))/60,
                        SA = sum(Event %in% c("GOAL", "SHOT") & ev.team == Home.Team),
                        GA = sum(Event == "GOAL" & ev.team == Home.Team)
              )
  ) %>%
    na.omit() %>%
    group_by(Team,Season, Season.Type, Date, Game.ID, Goalie, Minute) %>%
    summarise(SA = sum(SA),
              GA = sum(GA)
    ) %>%
    data.frame() -> season
 
}

s20072008 <- by_minute("~/Downloads/pbp20072008.Rda")
s20082009 <- by_minute("~/Downloads/pbp20082009.Rda")
s20092010 <- by_minute("~/Downloads/pbp20092010.Rda")
s20102011 <- by_minute("~/Downloads/pbp20102011.Rda")
s20112012 <- by_minute("~/Downloads/pbp20112012.Rda")
s20122013 <- by_minute("~/Downloads/pbp20122013.Rda")
s20132014 <- by_minute("~/Downloads/pbp20132014.Rda")
s20142015 <- by_minute("~/Downloads/pbp20142015.Rda")
s20152016 <- by_minute("~/Downloads/pbp20152016.Rda")
s20162017 <- by_minute("~/Downloads/pbp20162017.Rda")

# Compile all seasons together and save it
NHL_by_minute <- rbind(s20072008,s20082009, s20092010, s20102011, s20112012, 
                      s20122013, s20132014, s20142015, s20152016, s20162017)
save(NHL_by_minute,file="~/Documents/NHL_by_minute.Rda")

