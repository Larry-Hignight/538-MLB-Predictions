library(stringr)
library(lubridate)

calc_payout <- function(ml, bet = 100) {
  payout_neg_ml <- function(ml, bet) (bet / ml) * (ml + 100)
  payout_pos_ml <- function(ml, bet) (bet / 100) * (ml + 100)
  if (ml < 0) payout_neg_ml(abs(ml), bet)
  else payout_pos_ml(ml, bet)
}

calc_ev <- function(p, ml, bet = 100) (p * (calc_payout(ml, bet) - bet)) + ((1 - p) * -bet)

## --- Analysis --- ##

setwd('/home/larry/Github-Public/538-MLB-Predictions/parsed-data')
preds <- read.csv('538-predictions.csv', header = TRUE)
lines <- read.csv('oddsshark-money-lines.csv', header = TRUE)

# Fix the dates
preds$Date.prediction <- ymd(preds$Date.prediction)
preds$Date.game <- ymd(preds$Date.game)
lines$Date.line <- ymd(str_sub(as.character(lines$Date.line), end = -10))
lines$Date.game <- ymd(lines$Date.game)

# This initial analysis will only consider games and lines on a given day
preds <- preds[preds$Date.prediction == preds$Date.game, ]
lines <- lines[lines$Date.line == lines$Date.game, ]
dates <- intersect(unique(preds$Date.game), unique(lines$Date.game))
preds <- preds[preds$Date.game %in% dates, ]
lines <- lines[lines$Date.game %in% dates, ]
lines <- lines[!is.na(lines$Best), ]  # Remove any lines missing all line data

# Merge the prediction and line info into a single data frame
sim <- data.frame()
for (i in 1:nrow(preds)) {
  print(sprintf("Merging prediction #%d", i))
  x <- preds[i, ]
  line1 <- lines[lines$Date.game == x$Date.game & as.character(lines$Team) == as.character(x$Visiting), ]
  line2 <- lines[lines$Date.game == x$Date.game & as.character(lines$Team) == as.character(x$Home), ]
  x$EV.v <- ifelse(nrow(line1) == 1, calc_ev(p = x$Chance.visiting, ml = line1$Best, bet = 100), NA)   # Visiting team
  x$EV.h <- ifelse(nrow(line2) == 1, calc_ev(p = x$Chance.home, ml = line2$Best, bet = 100), NA)       # Home team
  x$ML.v <- ifelse(nrow(line1) == 1, line1$Best, NA)
  x$ML.h <- ifelse(nrow(line2) == 1, line2$Best, NA)
  sim <- rbind(sim, x)
}

# Additional formatting...
sim <- sim[ , c(-1, -8)]
colnames(sim)[5:6] <- c('P.v', 'P.h')
head(sim, 12)

# Run a Monte Carlo sim
for (i in 1:20) {
  sim2 <- data.frame(Rand = runif(nrow(sim)), 
                     Win.v = sapply(sim$ML.v, function(ml) ifelse(is.na(ml), NA, calc_payout(ml))),
                     Win.h = sapply(sim$ML.h, function(ml) ifelse(is.na(ml), NA, calc_payout(ml))),
                     Total = 0)
  
  # Determine which bets would have been placed... 
  ev.cutoff <- 2
  v.bets <- which(sim$EV.v >= ev.cutoff)
  h.bets <- which(sim$EV.h >= ev.cutoff)
  sim2$Total[v.bets] <- ifelse(sim2$Rand[v.bets] <= sim$P.v[v.bets], sim2$Win.v[v.bets], -100)
  sim2$Total[h.bets] <- ifelse(sim2$Rand[h.bets] > sim$P.v[h.bets], sim2$Win.h[h.bets], -100)
  head(sim2, 12)
  
  plot(cumsum(sim2$Total), type = 'b', pch = 19, col = 'blue', main = sprintf("Sim #%d", i),
       xlab = 'Bet #', ylab = 'Amount Win/Lose ($)')
  abline(h = 0)
}
