#Back-to-back.R by Étienne Labrie-Dion

#When teams are playing a back-to-back game, does the goalie save percentage vary whether they also played the night before?
#Calculate goalie SV% during either 1) Their team is playing a B2B and they are too, 2) Their team is playing a B2B but they are rested, and 3) The rest of the season

#This script works with Corsica Hockey's play-by-play stats: http://www.corsica.hockey/data/ and is based on Emmanuel Perry's script from Introduction to R (http://corsica.teachable.com/p/an-intro-to-r)

require(dplyr)

B2B_calc <- function(pbpseason) {   
  
  load(pbpseason)

  # Remove playoff games and strange strength states
  pbp %>%
    filter(Season.Type == "Regular" & Strength.State %in% c("3v3", "3v4", "4v3", "3v5", "5v3", 
                                                            "4v4", "4v5", "5v4", "5v5", "EvE")) ->
    pbp_reg
  
  # Compile Goalie Stats
  bind_rows(pbp_reg %>%
              group_by(Home.Goalie, Game.ID, Date,Home.Team) %>%
              rename(Goalie = Home.Goalie, Team = Home.Team) %>%
              summarise(Venue = "Home",
                        TOI = sum(na.omit(as.numeric(Event.Length)))/60,
                        SA = sum(Event %in% c("GOAL", "SHOT") & ev.team == Away.Team),
                        GA = sum(Event == "GOAL" & ev.team == Away.Team),
                        Start = min(Round.Seconds),
                        End = max(Round.Seconds)
              ),
            
            pbp_reg %>%
              group_by(Away.Goalie,Game.ID, Date,Away.Team) %>%
              rename(Goalie = Away.Goalie, Team = Away.Team) %>%
              summarise(Venue = "Away",
                        TOI = sum(na.omit(as.numeric(Event.Length)))/60,
                        SA = sum(Event %in% c("GOAL", "SHOT") & ev.team == Home.Team),
                        GA = sum(Event == "GOAL" & ev.team == Home.Team),
                        Start = min(Round.Seconds),
                        End = max(Round.Seconds)
              )
  ) %>%
    data.frame() %>%
    group_by(Team,Goalie,Game.ID,Date) %>%
    summarise(TOI = sum(TOI),
              SA = sum(SA),
              GA = sum(GA),
              Start = Start,
              End = End
    ) %>%
    data.frame() %>%
    
    #Calculate the time between games played by a goalie
    group_by(Goalie) %>%
    arrange(Goalie,Date) %>%
    mutate(DaysDiffGoalies = as.numeric(as.Date(Date) - lag(as.Date(Date),default = as.Date(Date[1])))
    ) %>%
    data.frame() %>%
    
    #Same thing but at the team level
    filter(Start <= 10) %>% #remove the relievers
    group_by(Team) %>%
    arrange(Team,Date) %>%
    mutate(DaysDiffTeams = as.numeric(as.Date(Date) - lag(as.Date(Date),default = as.Date(Date[1])))
    )%>%
    data.frame() ->
    b2b_sv
  
    #Change the first game of the season for goalie and team to NA or 99 (to categorize it with 2 days+)
    b2b_sv$DaysDiffGoalies[b2b_sv$DaysDiffGoalies == 0] <-99
    b2b_sv$DaysDiffTeams[b2b_sv$DaysDiffTeams == 0] <-NA
  
    #Bin the differences and label them
    b2b_sv$B2BGoalies <- cut(b2b_sv$DaysDiffGoalies, breaks=c(1, 2, 300), 
                             labels=c("B2B", "2 days+"), right=FALSE)
    b2b_sv$B2BTeams <- cut(b2b_sv$DaysDiffTeams, breaks=c(1, 2, 300), 
                             labels=c("B2B", "2 days+"), right=FALSE)
    
    #When teams are playing a B2B: Split goalies into rested (2days+) and tired (B2B), Calc SV% and GP
    b2b_sv %>%
      na.omit() %>%  #if counting the first game of seasons as NA, need to remove it
      filter(B2BTeams == "B2B") %>%
      group_by(B2BGoalies) %>%
      summarise(SA = sum(SA),
                GA = sum(GA),
                GP = n()
      ) %>%
      mutate(SV = (SA-GA)/SA) %>%
      data.frame() -> teamB2B_goaliesv
    
    #When teams are not playing a B2B: All goalies, Calc SV% and GP
    b2b_sv %>%
      na.omit() %>%  #if counting the first game of seasons as NA, need to remove it
      filter(B2BTeams != "B2B") %>%
      group_by(B2BGoalies) %>%
      summarise(SA = sum(SA),
                GA = sum(GA),
                GP = n()
      ) %>%
      mutate(SV = (SA-GA)/SA) %>%
      data.frame() -> restOfSeason_goaliesv
    
    total = cbind(Goalie_State=c("Team B2B - Goalie Tired", "Team B2B - Goalie Rested", "Normal Game"),
                  rbind(teamB2B_goaliesv[,-1], restOfSeason_goaliesv[,-1]))
}

sv20072008 <- B2B_calc("~/Downloads/pbp20072008.Rda")
sv20082009 <- B2B_calc("~/Downloads/pbp20082009.Rda")
sv20092010 <- B2B_calc("~/Downloads/pbp20092010.Rda")
sv20102011 <- B2B_calc("~/Downloads/pbp20102011.Rda")
sv20112012 <- B2B_calc("~/Downloads/pbp20112012.Rda")
sv20122013 <- B2B_calc("~/Downloads/pbp20122013.Rda")
sv20132014 <- B2B_calc("~/Downloads/pbp20132014.Rda")
sv20142015 <- B2B_calc("~/Downloads/pbp20142015.Rda")
sv20152016 <- B2B_calc("~/Downloads/pbp20152016.Rda")
sv20162017 <- B2B_calc("~/Downloads/pbp20162017.Rda")


B2Btotalteam <- cbind(sv20072008[,5],sv20082009[,5], sv20092010[,5], sv20102011[,5],
                      sv20112012[,5], sv20122013[,5], sv20132014[,5],sv20142015[,5],
                      sv20152016[,5], sv20162017[,5])
colnames(B2Btotalteam) <- c("2007-2008","2008-2009","2009-2010", "2010-2011", "2011-2012", 
                         "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017")
rownames(B2Btotalteam) <- c("Team B2B - Goalie Tired", "Team B2B - Goalie Rested", "Normal Game")

plot(B2Btotalteam)
