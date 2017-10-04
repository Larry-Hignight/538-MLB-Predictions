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

x <- data.frame(Date, Visitor, Home, Pitcher.V, Pitcher.H, Score.V, Score.H)
x <- x[x$Date > ymd("2017-07-31"), ]
write.csv(x, file = 'game-results-2017.csv', row.names = F)
