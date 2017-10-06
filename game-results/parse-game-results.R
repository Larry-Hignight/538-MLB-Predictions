library(stringr)
library(lubridate)

setwd('/home/larry/Github-Public/538-MLB-Predictions/game-results/')
original <- read.csv('SBR-original-odds-and-results-2017.csv', header = TRUE)

Date <- as.character(original$Date[seq(1, nrow(original), 2)])
Date <- ymd(str_c("2017-", str_sub(Date, end = -3), "-", str_sub(Date, start = str_length(Date) - 1)))
Visitor <- as.character(original$Team[seq(1, nrow(original), 2)])
Home <- as.character(original$Team[seq(2, nrow(original), 2)])
Pitcher.V <- as.character(original$Pitcher[seq(1, nrow(original), 2)])
Pitcher.H <- as.character(original$Pitcher[seq(2, nrow(original), 2)])
Score.V <- as.character(original$Final[seq(1, nrow(original), 2)])
Score.H <- as.character(original$Final[seq(2, nrow(original), 2)])

# Replace these team name abbvs with those used by 538 and Oddsshark
replace_teams <- function(x) {
  x[x == 'CUB'] <- 'CHC'
  x[x == 'CUB'] <- 'CHC'
  x[x == 'CWS'] <- 'CHW'
  x[x == 'KAN'] <- 'KC'
  x[x == 'LOS'] <- 'LAD'
  x[x == 'SDG'] <- 'SD'
  x[x == 'SFO'] <- 'SF'
  x[x == 'TAM'] <- 'TB'
  x[x == 'WAS'] <- 'WSH'
  x
}

Visitor <- replace_teams(Visitor)
Home <- replace_teams(Home)

x <- data.frame(Date, Visitor, Home, Pitcher.V, Pitcher.H, Score.V, Score.H)
x <- x[x$Date > ymd("2017-07-31"), ]
write.csv(x, file = '../parsed-data/game-results-2017.csv', row.names = F)
