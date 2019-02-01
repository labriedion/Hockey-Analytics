"""Simulate the rest of a QJMHL season with Monte Carlo simulations based on an Expected Goals model."""


import pandas as pd
import numpy as np
from math import pi
from sklearn.linear_model import LogisticRegression
import requests
from time import sleep


def get_expected_goals(previous_season_pbp, df, df_pbp):
    #first, standardise the x and y coordinates
    # (0,0) should be the center of the goal. Both zones brought into the same corner and both sides of the ice on the same side too.
    previous_season_pbp['x_location_normal'] = np.where(previous_season_pbp['x_location'] <= 300, previous_season_pbp['x_location'] - 50, 550 - previous_season_pbp['x_location'])
    previous_season_pbp['y_location_normal'] = np.abs(150 - previous_season_pbp['y_location'])
    previous_season_pbp['shot_distance'] = np.sqrt(np.power(previous_season_pbp['x_location_normal'],2) + np.power(previous_season_pbp['y_location_normal'],2))
    previous_season_pbp['shot_angle'] = np.arcsin( previous_season_pbp['y_location_normal'] / previous_season_pbp['shot_distance'] ) * 180 / pi
    
    df_pbp['x_location_normal'] = np.where(df_pbp['x_location'] <= 300, df_pbp['x_location'] - 50, 550 - df_pbp['x_location'])
    df_pbp['y_location_normal'] = np.abs(150 - df_pbp['y_location'])
    df_pbp['shot_distance'] = np.sqrt(np.power(df_pbp['x_location_normal'],2) + np.power(df_pbp['y_location_normal'],2))
    df_pbp['shot_angle'] = np.arcsin( df_pbp['y_location_normal'] / df_pbp['shot_distance'] ) * 180 / pi

    
    #this counts the actual number of goals scored (except shootout and penalty shot goals)
    previous_season_pbp['goal'] = np.where(previous_season_pbp.quality > 3, 1, 0)
    
    #prepare the dataset for the logistic regression
    X = previous_season_pbp.loc[previous_season_pbp.event == 'shot', ['shot_distance', 'shot_angle']].values
    y = previous_season_pbp.loc[previous_season_pbp.event == 'shot', 'goal'].values

    #perform the logisitic regression
    logreg = LogisticRegression()
    logreg.fit(X, y)
    
    #df = get_current_season_games(187)
    #df_pbp = get_current_season_pbp(df)
    #previous_season = pd.read_csv('pbpQMJHL_regular_2015-2016.csv')
    X = df_pbp.loc[df_pbp.event == 'shot', ['shot_distance', 'shot_angle']].values
    pred = logreg.predict_proba(X)[:,1]
    df_pbp.loc[df_pbp.event == 'shot','expected_goals'] = pred
    expected_goals = df_pbp.groupby(['game_id', 'home'])['expected_goals'].sum().unstack().reset_index()
    expected_goals.columns = ['game_id', 'visiting_expected_goals', 'home_expected_goals']
    df = df.join(expected_goals.set_index('game_id'), on = 'game_id')
    
    return df

def get_current_season_games(season_id):
    r = requests.get('http://cluster.leaguestat.com/feed/?feed=modulekit&view=schedule&key=f109cf290fcf50d4&fmt=json&client_code=lhjmq&lang=en&season_id=%d&team_id=undefined&league_code=&fmt=json' %season_id)
    df1 = pd.read_json(r.text).transpose()
    df1 = df1.Schedule.apply(pd.Series).transpose()
    df1 = df1.SiteKit.apply(pd.Series)
    df1 = df1[df1['home_team_city'] != 'Ottawa - OHL']
    
    return df1

def get_current_season_pbp(df1):
    df_pbp = pd.DataFrame()
    games = df[df.final == 1].game_id.values
    for number in games:
        r = requests.get('http://cluster.leaguestat.com/feed/index.php?feed=gc&key=f109cf290fcf50d4&client_code=lhjmq&game_id=%d&lang_code=en&fmt=json&tab=pxpverbose' %number)
        r.raise_for_status()
        text = r.text
        if text.find('No such game') >= 0:
            sleep(0.4)
            continue
        df1 = pd.read_json(r.text).transpose()
        df1 = df1.Pxpverbose.apply(pd.Series).transpose()
        df1 = df1.GC.apply(pd.Series)
        df1['game_id'] = number
        df_pbp = df_pbp.append(df1)
    
    return df_pbp

def label_b2b(df):
    df_diff = pd.DataFrame()
    df['GameDate'] = pd.to_datetime(df['date_played'])
    
    for team in df.home_team_city.unique():
        games = df.loc[(df.home_team_city == team) | (df.visiting_team_city == team)]
        games['home_b2b'] = (games.GameDate.sort_values().diff() < '2 days') & (games.home_team_city == team)
        games['home_b3b'] = (games.GameDate.sort_values().diff(2) < '4 days') & (games.home_team_city == team)
        games['away_b2b'] = (games.GameDate.sort_values().diff() < '2 days') & (games.visiting_team_city == team)
        games['away_b3b'] = (games.GameDate.sort_values().diff(2) < '4 days') & (games.visiting_team_city == team)
        df_diff = df_diff.append(games)
    
    df = df.merge(df_diff.groupby('game_id')['home_b2b'].any().reset_index(), on = 'game_id')
    df = df.merge(df_diff.groupby('game_id')['home_b3b'].any().reset_index(), on = 'game_id')
    df = df.merge(df_diff.groupby('game_id')['away_b2b'].any().reset_index(), on = 'game_id')
    df = df.merge(df_diff.groupby('game_id')['away_b3b'].any().reset_index(), on = 'game_id')
    
    return df

def simulate_rest_of_season(df, iterations):
    
    results = pd.DataFrame()
    
    
    df_simul = df.copy(deep = True)
    
    #remove unfinished games and remove shootout goals
    df_played = df[df.final == 1]
    df_played.loc[df_played.game_status == 'Final SO','home_goal_count'] = np.minimum(df_played.home_goal_count, df_played.visiting_goal_count)
    df_played.loc[df_played.game_status == 'Final SO','visiting_goal_count'] = np.minimum(df_played.home_goal_count, df_played.visiting_goal_count)
    
    team_ratings = pd.DataFrame()
    
    for team in df.home_team_city.unique():
        temp = pd.DataFrame({'Team':[team]})
        home = df_played[df_played.home_team_city == team]
        away = df_played[df_played.visiting_team_city == team]
        temp['GamesPlayed'] = home.game_id.count() + away.game_id.count()
        temp['GoalsForAvg'] = (home.home_goal_count.sum() + away.visiting_goal_count.sum()) / temp.GamesPlayed
        temp['GoalsAgainstAvg'] = (home.visiting_goal_count.sum() + away.home_goal_count.sum()) / temp.GamesPlayed
        temp['GoalsExpectedFor'] = (home.home_expected_goals.sum() + away.visiting_expected_goals.sum()) / temp.GamesPlayed
        temp['GoalsExpectedAgainst'] = (home.visiting_expected_goals.sum() + away.home_expected_goals.sum()) / temp.GamesPlayed
        team_ratings = team_ratings.append(temp)
    
    team_ratings['Offensive_Rating'] =  (team_ratings.GoalsForAvg + team_ratings.GoalsExpectedFor ) / (team_ratings.GoalsForAvg.mean() + team_ratings.GoalsExpectedFor.mean())
    team_ratings['Defensive_Rating'] =  (team_ratings.GoalsAgainstAvg + team_ratings.GoalsExpectedAgainst) / (team_ratings.GoalsAgainstAvg.mean() + team_ratings.GoalsExpectedAgainst.mean())  
    
    for i in range(iterations):
        ratings = team_ratings.copy(deep = True)
        
        for game_id in df[df.final == 0].game_id.values:
            game = df[df.game_id == game_id]
            home_team = game.home_team_city.values[0]
            away_team = game.visiting_team_city.values[0]
            home = ratings[ratings.Team == home_team]
            away = ratings[ratings.Team == away_team]
            
            #b2b adjustment
            if game.home_b2b.values[0] == False:
                if game.away_b2b.values[0] == False:
                    b2b = 0.239
                elif game.away_b2b.values[0] == True:
                    b2b = 0.356
            elif game.home_b2b.values[0] == True:
                if game.away_b2b.values[0] == False:
                    b2b = 0.018
                elif game.away_b2b.values[0] == True:
                    b2b = 0.249
            goals_for = home.Offensive_Rating * away.Defensive_Rating * ratings.GoalsForAvg.mean() + b2b
            goals_against = away.Offensive_Rating * home.Defensive_Rating * ratings.GoalsAgainstAvg.mean() - b2b
            
            goals_for = np.random.poisson(goals_for, 1)
            goals_against = np.random.poisson(goals_against, 1)
            
            if goals_for == goals_against:
                overtime = np.random.rand()
                if overtime < 0.25:
                    goals_against += 1
                    final = "Final OT"
                if overtime >= 0.25 and overtime < 0.5:
                    goals_against += 1
                    final = "Final SO"
                if overtime >= 0.5 and overtime < 0.75:
                    goals_for += 1
                    final = "Final SO"
                if overtime >= 0.75:
                    goals_for += 1
                    final = "Final OT"
            else:
                final = "Final"

            df_simul.loc[df_simul.game_id == game_id, 'final'] = 1
            df_simul.loc[df_simul.game_id == game_id,'home_goal_count'] = goals_for
            df_simul.loc[df_simul.game_id == game_id, 'visiting_goal_count'] = goals_against
            df_simul.loc[df_simul.game_id == game_id, 'game_status'] = final
        
            
            #update the ratings
            ratings.loc[ratings.Team == away_team, 'GoalsForAvg'] = (away.GoalsForAvg * away.GamesPlayed + goals_against) / (away.GamesPlayed + 1)
            ratings.loc[ratings.Team == away_team, 'GoalsAgainstAvg'] = (away.GoalsAgainstAvg * away.GamesPlayed + goals_for) / (away.GamesPlayed + 1)
            ratings.loc[ratings.Team == home_team, 'GoalsForAvg'] = (home.GoalsForAvg * home.GamesPlayed + goals_for) / (home.GamesPlayed + 1)
            ratings.loc[ratings.Team == home_team, 'GoalsAgainstAvg'] = (home.GoalsAgainstAvg * home.GamesPlayed + goals_against) / (home.GamesPlayed + 1)
            
            ratings.loc[ratings.Team == away_team, 'Offensive_Rating'] =  (away.GoalsForAvg + away.GoalsExpectedFor ) / (ratings.GoalsForAvg.mean() + ratings.GoalsExpectedFor.mean())
            ratings.loc[ratings.Team == away_team, 'Defensive_Rating'] =  (away.GoalsAgainstAvg + away.GoalsExpectedAgainst) / (ratings.GoalsAgainstAvg.mean() + ratings.GoalsExpectedAgainst.mean())
            ratings.loc[ratings.Team == home_team, 'Offensive_Rating'] =  (home.GoalsForAvg + home.GoalsExpectedFor ) / (ratings.GoalsForAvg.mean() + ratings.GoalsExpectedFor.mean())
            ratings.loc[ratings.Team == home_team, 'Defensive_Rating'] =  (home.GoalsAgainstAvg + home.GoalsExpectedAgainst) / (ratings.GoalsAgainstAvg.mean() + ratings.GoalsExpectedAgainst.mean())
            
            ratings.loc[ratings.Team == away_team, 'GamesPlayed'] += 1
            ratings.loc[ratings.Team == home_team, 'GamesPlayed'] += 1
            
        df_simul['won'] = np.where(df_simul.home_goal_count > df_simul.visiting_goal_count, df_simul.home_team_city, df_simul.visiting_team_city)
        df_simul['lost'] = np.where(df_simul.home_goal_count < df_simul.visiting_goal_count, df_simul.home_team_city, df_simul.visiting_team_city)      
        won = df_simul.groupby(['won', 'game_status']).size().unstack().reset_index().fillna(0)
        lost = df_simul.groupby(['lost', 'game_status']).size().unstack().reset_index().fillna(0)
        won['Final'] = won['Final'] + won['Final OT'] + won['Final SO']
        del won['Final OT'] 
        del won['Final SO']
        lost.columns = ['Team', 'Loss', 'OT Loss', 'SO Loss']
        won.columns = ['Team', 'Win']
        end = lost.merge(won, on = 'Team')
        end.fillna(0)
        
        GF = df_simul.groupby('home_team_city')['home_goal_count'].sum() + df_simul.groupby('visiting_team_city')['visiting_goal_count'].sum() 
        GA = df_simul.groupby('visiting_team_city')['home_goal_count'].sum() + df_simul.groupby('home_team_city')['visiting_goal_count'].sum()
        GF = GF.reset_index()
        GA = GA.reset_index()
        GF.columns = ['Team', 'GF']
        GA.columns = ['Team', 'GA']
        end = end.merge(GF, on = 'Team')
        end = end.merge(GA, on = 'Team')
        end['Points'] = end['OT Loss'] + end['SO Loss'] + (2 * end.Win)
        end['Standing'] = end['Points'].rank(ascending = False)
        
        results = results.append(end)
        
        
    return results


#2016-2017 season simulation
previous_season_pbp = pd.read_csv('pbpQMJHL_regular_2016-2017.csv')
df = get_current_season_games(187).convert_objects(convert_numeric=True)
df_pbp = get_current_season_pbp(df)

df = get_expected_goals(previous_season_pbp, df, df_pbp)

df = label_b2b(df)

simulated = simulate_rest_of_season(df, 10)
average = simulated.groupby('Team').mean().reset_index()
average.to_csv('average.csv')

