#EPL Project
#Joseph Reichert

library(tidyverse)
library(lubridate)

EPL_Standings <- function(date, season) {
  
  #Getting the data for the correct season
  l <- substr(season, 3, 7)
  m<-gsub('/','',l)
  x <- 'http://www.football-data.co.uk/mmz4281/change/E0.csv'
  y<-gsub('change', m, x)
  EPL <- read_csv(y)
  
  #Changing the Date to consistent time to make calculation easier
  EPL$Date <- dmy(EPL$Date)
  EPL$Date
  
  #Changing Date input to the same time structure
  useDate <- mdy(date)
  
  #Adding a new column for the points for the home team and the points for away team
  #Adding these new columns make it simple to get the total points for any club by adding how much they had from home games and then away games
  EPL1 <- EPL %>%
            filter(Date <= useDate) %>%
            group_by(HomeTeam) %>%
            mutate(PFHT = ifelse(FTR =='D', '1', ifelse(FTR == 'H', '3', ifelse(FTR == 'A', '0', 'no')))) %>%
            mutate(PFAT = ifelse(FTR =='D', '1', ifelse(FTR == 'A', '3', ifelse(FTR == 'H', '0', 'no')))) %>%
            select('Date', 'HomeTeam', 'AwayTeam', 'FTHG', 'FTAG', 'FTR', 'PFHT', 'PFAT') %>%
            arrange(HomeTeam)
  
  #This block of code makes a new column that shows whether a team won, lost or tied in any given match, both at home and on the road
  
  EPL1 <- EPL1 %>%
    mutate(HomeTeamW = ifelse(FTR =='H', '1','0')) %>%
    mutate(HomeTeamD = ifelse(FTR =='D', '1','0')) %>%
    mutate(HomeTeamL = ifelse(FTR =='A', '1','0')) %>%
    mutate(AwayTeamW = ifelse(FTR =='A', '1','0')) %>%
    mutate(AwayTeamD = ifelse(FTR =='D', '1','0')) %>%
    mutate(AwayTeamL = ifelse(FTR =='H', '1','0')) %>%
    select(everything())
  
  #This makes these columns integer values so that they can be summed 
  EPL1$HomeTeamW <- strtoi(EPL1$HomeTeamW)
  EPL1$HomeTeamD <- strtoi(EPL1$HomeTeamD)
  EPL1$HomeTeamL <- strtoi(EPL1$HomeTeamL)
  
  #HomeTeam Record
  EPL2 <- EPL1 %>%
    group_by(HomeTeam)%>%
    summarize(WinsAtHome = sum(HomeTeamW),
              DrawsAtHome = sum(HomeTeamD),
              LossesAtHome = sum(HomeTeamL))%>%
    select('HomeTeam', 'WinsAtHome', 'DrawsAtHome', 'LossesAtHome')
  
  EPL2 <- EPL2 %>%
    rename(TeamName = HomeTeam)
  
  #This section turns these into integer values to do the same caluclations as above, but for the away team instead
  
  EPL1$AwayTeamW <- strtoi(EPL1$AwayTeamW)
  EPL1$AwayTeamD <- strtoi(EPL1$AwayTeamD)
  EPL1$AwayTeamL <- strtoi(EPL1$AwayTeamL)
  
  #AwayTeam Record
  
  EPL3 <- EPL1 %>%
    group_by(AwayTeam)%>%
    summarize(WinsOnRoad = sum(AwayTeamW),
              DrawsOnRoad = sum(AwayTeamD),
              LossesOnRoad = sum(AwayTeamL))%>%
    select('AwayTeam', 'WinsOnRoad', 'LossesOnRoad', 'DrawsOnRoad')
  
  EPL3 <- EPL3 %>%
    rename(TeamName = AwayTeam)
  
  #Join the two structures to calculate total Record, by adding the Home Record and the Away Record
  EPL4 <- EPL2 %>% inner_join(EPL3)
  EPL4 <- EPL4 %>%
    mutate(TotalWins = WinsAtHome + WinsOnRoad,
           TotalLosses = LossesAtHome + LossesOnRoad,
           TotalDraws = DrawsAtHome + DrawsOnRoad)
  
  #Currently the record is stored in 3 seperate columns, one for total wins, total losses, and total draws
  #We want the W-L-D format so they need to be in the same column, and this block of code combines them into a single column 
 
  
  EPL5<- EPL4 %>%
    unite('Record', TotalWins, TotalLosses, TotalDraws)%>%
    unite('HomeRec', WinsAtHome, LossesAtHome, DrawsAtHome)%>%
    unite('AwayRec', WinsOnRoad:DrawsOnRoad)%>%
    select('TeamName', 'Record', 'HomeRec', 'AwayRec')
  
  #The unite function combines with _ rather than - so here i sub in -'s to get the correct format
  
  EPL5$Record <- gsub('_','-', EPL5$Record)    
  EPL5$HomeRec <- gsub('_','-', EPL5$HomeRec)
  EPL5$AwayRec <- gsub('_','-', EPL5$AwayRec)
  
  #This join gets all the information from EPL4 back into 5, and while this step could be avoided it helps keep everything together
  
  EPL5 <- EPL5 %>% inner_join(EPL4)
    
  
  #Converting these values to integer values to ensure calculations are valid 
  
  EPL1$PFHT <- strtoi(EPL1$PFHT)
  EPL1$PFAT <- strtoi(EPL1$PFAT) 
  
  #Now that we calculated record, we will move on to the number of points, to do this we will calculate points at home, then points on the road
  #Calculate the number of points at Home
  
  pointsAtHome <- EPL1 %>%
    group_by(HomeTeam) %>%
    summarize(PointsAtHome = sum(PFHT)) %>%
    select('HomeTeam', 'PointsAtHome') %>%
    arrange(HomeTeam)
  
  #Remane HomeTeam to TeamName so that it can be joined with the away points
  pointsAtHome <- pointsAtHome %>%
    rename(TeamName = HomeTeam)
  
  #Calculate the away points
  pointsAway <- EPL1 %>%
    group_by(AwayTeam) %>%
    summarize(PointsOnRoad = sum(PFAT)) %>%
    select('AwayTeam', 'PointsOnRoad') %>%
    arrange(AwayTeam)
  
  #Renaming away team to TeamName so that it can be joined with the pointsathome
  pointsAway <- pointsAway %>%
    rename(TeamName = AwayTeam)
  
  #Joining the two and then calculating the total points
 total <- pointsAtHome %>% inner_join(pointsAway)
 total <- total %>%
   group_by(TeamName, PointsAtHome, PointsOnRoad)%>%
   summarize(Points = PointsAtHome + PointsOnRoad) %>%
   select('TeamName', 'PointsAtHome','PointsOnRoad', 'Points')%>%
   arrange(desc(Points))
  
 #Calculate Goals Scored
 #This calculation will be done by Home Team goals and then Away Team goals and joining the results, then taking the sum
 z <- EPL1 %>%
   group_by(HomeTeam) %>%
   summarize(GoalAtHome = sum(FTHG), GoalsAllowedHome = sum(FTAG))%>%
   select('HomeTeam','GoalAtHome','GoalsAllowedHome')%>%
   arrange(HomeTeam)
 
 #Changing the HomeTeam name to TeamName so it can be joined as above.
 z <- z %>%
   rename(TeamName = HomeTeam)
 
 y <- EPL1 %>%
   group_by(AwayTeam) %>%
   summarize(GoalsAway = sum(FTAG), GoalsAllowedOnRoad = sum(FTHG))%>%
   select('AwayTeam','GoalsAway','GoalsAllowedOnRoad')%>%
   arrange(AwayTeam)
 
 #Changing the AwayTeam name to TeamName so it can be joined as above.
 
 y <- y %>%
   rename(TeamName = AwayTeam)
 
 #Joining the goals at Home, and the goals Away to calculate total goals
 total1 <- y %>% inner_join(z)
 total1 <- total1 %>%
   group_by(TeamName)%>%
   summarize(GS = GoalAtHome +GoalsAway, GA = GoalsAllowedOnRoad + GoalsAllowedHome) %>%
   select('TeamName', 'GS', 'GA')%>%
   arrange(desc(GS)) 
 
 #Combines the totalgoals table calculated with the total points table so they are all in the same table
 xy <- total1 %>% inner_join(total)
 xy <- xy%>%
   arrange(desc(Points))
  
 
 #Number of Games Played at home
 GamesAtHome <- EPL1 %>%
   count(HomeTeam) 

 #Renaming the columns to join with the Awaygames 
 GamesAtHome <- GamesAtHome%>%
   rename(TeamName = HomeTeam)%>%
   rename(HomeGames = n)
 
 #Number of Games Played on Road
 GamesOnRoad <- EPL1 %>%
   ungroup()%>%
   count(AwayTeam)
 
 #Renaming for the join with the HomeGames
 GamesOnRoad <- GamesOnRoad %>%
   rename(TeamName = AwayTeam) %>%
   rename(AwayGames = n)
 
 #Joining together and calculating the total number of games
 
 GamesTotal <- GamesAtHome %>% inner_join(GamesOnRoad)
 GamesTotal <- GamesTotal %>%
   group_by(TeamName, HomeGames, AwayGames) %>%
   summarize(MatchesPlayed = HomeGames + AwayGames) %>%
   select('TeamName', 'MatchesPlayed', 'HomeGames', 'AwayGames')

 #Calculate the Streak and then the Last 10 Record

 #This calculation was the toughest to get to work
 #The route i decided to take was to make a table with columns for Wins at home, Losses at home, and draws at home. 
 #Then another table with the same columns but for away teams, then perform a full join on them to get in a single column then order by date
 
 #Table with column for wins at home, losses at home, and draws at home
 EPL20 <- EPL1 %>%
   group_by(HomeTeam, Date)%>%
   summarize(WinsAtHome = sum(HomeTeamW),
             DrawsAtHome = sum(HomeTeamD),
             LossesAtHome = sum(HomeTeamL))%>%
   select('HomeTeam', 'WinsAtHome', 'DrawsAtHome', 'LossesAtHome', 'Date')
 
 
 #Renaming the columns to allow for an easier join
 EPL20 <- EPL20 %>%
   rename(TeamName = HomeTeam)
 
 #Table with column for wins on road, losses on road, and draws on road
 
 EPL30 <- EPL1 %>%
   group_by(AwayTeam, Date)%>%
   summarize(WinsOnRoad = sum(AwayTeamW),
             DrawsOnRoad = sum(AwayTeamD),
             LossesOnRoad = sum(AwayTeamL))%>%
   select('AwayTeam', 'WinsOnRoad', 'LossesOnRoad', 'DrawsOnRoad', 'Date')
 
 
 #Renaming the columns to allow for an easier join
 
 EPL30 <- EPL30 %>%
   rename(TeamName = AwayTeam)
 
 
 #Doing a full join and then creating new columns so that every game with be in columns ordered by newest date
 EPL9 <- EPL20 %>% full_join(EPL30)
 EPL9 <- EPL9 %>%
   group_by(TeamName, Date)%>%
   mutate(WinOnRoad = ifelse(WinsOnRoad == 1, 1, ifelse(DrawsAtHome == '1',0, ifelse(LossesAtHome == 1, 0, ifelse(LossesOnRoad == 1, 0, ifelse(DrawsOnRoad == 1, 0, 'no')))))) %>%
   mutate(WinAtHome = ifelse(WinsAtHome==1, 1, 0)) %>%
   mutate(DrawsHome = ifelse(DrawsAtHome == 1, 1, ifelse(WinsOnRoad == 1, 0,  ifelse(LossesAtHome == 1, 0, ifelse(LossesOnRoad == 1, 0, ifelse(WinsAtHome == 1, 0, 'no')))))) %>%
   mutate(DrawsRoad = ifelse(DrawsOnRoad==1, 1, 0)) %>%
   mutate(LossRoad = ifelse(LossesOnRoad == 1, 1, ifelse(DrawsAtHome == '1',0, ifelse(WinsAtHome == 1, 0, ifelse(WinsOnRoad == 1, 0, ifelse(DrawsOnRoad == 1, 0, 'no')))))) %>%
   mutate(LossHome = ifelse(LossesAtHome==1, 1, 0)) %>%
   arrange(desc(Date))
 
 
#Fill in any values that were left as NA after the mutate, so that they can be added together 
 EPL9$WinOnRoad[is.na(EPL9$WinOnRoad)] = 0
 EPL9$WinAtHome[is.na(EPL9$WinAtHome)] = 0
 EPL9$DrawsHome[is.na(EPL9$DrawsHome)] = 0
 EPL9$DrawsRoad[is.na(EPL9$DrawsRoad)] = 0
 EPL9$LossRoad[is.na(EPL9$LossRoad)] = 0
 EPL9$LossHome[is.na(EPL9$LossHome)] = 0


 #Adding the respective road and home columns together to get the Totalwins, totallosses, and totaldraws for the season in order from newest to oldest
 #Coded as either ones or zeros so they can be summed to get the streak and the record
 EPL9 <- EPL9 %>%
   mutate(TOTAlWin = WinOnRoad + WinAtHome) %>%
   mutate(TotalLOSS = LossHome + LossRoad) %>%
   mutate(TotalDRAW = DrawsHome + DrawsRoad) %>%
   select(everything())

 #Here we will calculate these sums over the 10 most recent games, to whatever date is input in the function
 EPL15<- EPL9 %>%
   summarize(x = sum(TOTAlWin),
             TOTALLOSS = sum(TotalLOSS),
             TOTALDRAW = sum(TotalDRAW))%>%
   select(everything()) %>%
   arrange(TeamName, desc(Date))%>%
   slice(1:10)
 
 #Calculates the record over the last 10 games
 
 EPL15 <- EPL15 %>%
   summarize(Last10Wins = sum(x),
             Last10Losses = sum(TOTALLOSS),
             Last10Draws = sum(TOTALDRAW))
 
 #Now we can unite them to get the record in the W_L_D form
 
 EPL10 <- EPL15 %>%
   unite('Last10', Last10Wins, Last10Losses, Last10Draws)
 
 #Replace the _ with -'s to get the desired format
 
 EPL10$Last10 = gsub('_','-', EPL10$Last10)
 
 #Calculating the streak
 #We cannot just use the same data from last 10 games since Man City finished the 2018/19 season with a 14 game win streak. 
 #According to google this broke the record, so we will slice with 15 to ensure we get the max Wstreak, instead of 10 as we did above.
 #And the longest consecutive losing streak is 15, so 15 seems like a safe bet here.
 
 EPL9 <- EPL9 %>%
   summarize(x = sum(TOTAlWin),
             TOTALLOSS = sum(TotalLOSS),
             TOTALDRAW = sum(TotalDRAW))%>%
   select(everything()) %>%
   arrange(TeamName, desc(Date))%>%
   slice(1:15)
 

 #Select the columns that are the most useful
EPL8 <- EPL9 %>%
  select('TeamName','Date', 'x', 'TOTALLOSS', 'TOTALDRAW')

#Creates new columns that will add until they get to a 0, with the way i have coded the 0's and 1's this is precisely the streak

EPL8 <- EPL8 %>%
  summarize(WStreak = sum(x[cumsum(x == 0)==0]),
            LStreak = sum(TOTALLOSS[cumsum(TOTALLOSS == 0)==0]),
            DStreak = sum(TOTALDRAW[cumsum(TOTALDRAW == 0)==0]))

#Currently there are three columns 1 of which has a value, 2 of them have 0 values,
#create a new column that will give the letter according to if it is a win streak, losing streak, or a tie streak
#Also create a column that gives the length of the strength

EPL8 <- EPL8 %>%
  mutate(Streak1 = ifelse(WStreak > 0, 'W', ifelse(LStreak>0, 'L', ifelse(DStreak > 0, 'T', 'No Streak'))),
         number = WStreak + LStreak + DStreak) 

#Now that we have a column with the appropriate letter, as well as the streak length we can unite them 

EPL8 <- EPL8 %>%
  unite('Streak', Streak1, number)

#This replaces the _ with no space.

EPL8$Streak <- gsub('_','', EPL8$Streak)

#Combine the data frames together and create the calculated columns 'PPM', 'PtPct', 'GSM', 'GAM'

Use <- xy %>% inner_join(GamesTotal)
Use <- Use %>%
  mutate(PPM = Points/MatchesPlayed) %>%
  mutate(PtPct = (Points/3) * MatchesPlayed) %>%
  mutate(GSM = GS/MatchesPlayed)%>%
  mutate(GAM = GA/MatchesPlayed)

#Bring everything into a single dataframe EPLFinal
EPLFinal <- Use %>% inner_join(EPL5)

EPLFinal <- EPLFinal %>%
  select('TeamName', 'Record', 'HomeRec', 'AwayRec', 'MatchesPlayed', 'Points', 'PPM', 'PtPct', 'GS', 'GSM', 'GA', 'GAM')%>%
  arrange(desc(PPM), desc(Points), desc(GSM), GAM)


#We now have all columns in the final data frame except for 'Last10' and 'Streak'
#So we will now add 'Last10'

EPLFinal <- EPLFinal %>% inner_join(EPL10)

#Next join the Streak column

EPLFinal <- EPLFinal %>% inner_join(EPL8)

#This join has added the three columns used to create streak, so we will remove them now, since they are not requested in the final data frame

EPLFinal <- EPLFinal %>% 
  select(-'WStreak', -'LStreak', -'DStreak')

#Now all desired fields are in the data frame 
#Round all calculated fields to three decimal places to make the table a little cleaner

EPLFinal$PPM = round(EPLFinal$PPM, 3)
EPLFinal$PtPct = round(EPLFinal$PtPct, 3)
EPLFinal$GSM = round(EPLFinal$GSM, 3)
EPLFinal$GAM = round(EPLFinal$GAM, 3)

 #Print Directly to the console the standings, and display the entire table
 
 message('Standings')
 print(EPLFinal['TeamName'])
 
 view(EPLFinal)
}


A <- EPL_Standings("06/25/2021", "2018/19")





