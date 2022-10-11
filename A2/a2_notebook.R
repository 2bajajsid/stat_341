setwd("C:/Users/2baja/OneDrive/Desktop/STAT 341/A2")
full_nba_player_boxscore <- read.csv("NBA_Player_Boxscore_2021-22.csv")

#1 a)

# hoopR - Access Men's Basketball Play by Play Data
# wehoop - Access Women's Basketball Play by Play Data

#1 b) 

#https://hoopr.sportsdataverse.org/reference/load_nba_player_box.html

#1 c) 

team_frequency_table <- table(full_nba_player_boxscore$team_abbreviation)

par(mar=c(5.1,4.1,4.1,2.1))
hist(team_frequency_table)

# most teams are greater than 800 
# mean(team_frequency_table) = 868.8125
# median(team_frequency_table) = 895.5
# outlier - Team Durant and Team LeBron which are 12 and 11 respectively

#1 d)

nba_player_boxscore <- subset(full_nba_player_boxscore, team_abbreviation != "LEB" & team_abbreviation != "DUR")

#2 a)
par(mfrow=c(1,3))

boxplot(nba_player_boxscore$reb)
hist(nba_player_boxscore$reb)

# have to manually build a quantile plot

y <- sort(nba_player_boxscore$reb)
x <- (1:length(y)) / length(y)

plot(x, y)

#2 b) Yes. The box plot shows the right skew, the location of the median, and the outliers / spread in the population

#2 c) mean(nba_player_boxscore[which(nba_player_boxscore$athlete_position_name == "Small Forward"), "reb"])
by(nba_player_boxscore$reb, nba_player_boxscore$athlete_position_name, mean)

#2 d) less skewness compared to the individual box plots
par(mfrow=c(1,7))
by(nba_player_boxscore$reb, nba_player_boxscore$athlete_position_name, boxplot)

#2 e) Yes. Because the represent the outliers, skewness, and central location

#2 f) 
par(mfrow=c(1,7))
by(jitter(nba_player_boxscore$reb), nba_player_boxscore$athlete_position_name, boxplot)

#2 g)
powerfun <- function(y, alpha) {
  if(sum(y <= 0) > 0) stop("y must be positive")
  if (alpha == 0)
    log(y)
  else if (alpha > 0) {
    y^alpha
  } else -(y^alpha)
}

to_be_optimized <- function(alpha) {
  return(abs(skewness(powerfun(nba_player_boxscore$reb + 1, alpha))))
}

nlminb(start=1, objective=to_be_optimized)
hist(powerfun(nba_player_boxscore$reb + 1, 0.24446))
hist(powerfun(nba_player_boxscore$reb + 1, 0.2))
hist(powerfun(nba_player_boxscore$reb + 1, 0))
