library(stringr)
library(lubridate)

calc_payout <- function(ml, bet = 100) {
  payout_neg_ml <- function(ml, bet) (bet / ml) * (ml + 100)
  payout_pos_ml <- function(ml, bet) (bet / 100) * (ml + 100)
  if (ml < 0) payout_neg_ml(abs(ml), bet)
  else payout_pos_ml(ml, bet)
}

calc_ev <- function(p, ml, bet = 100) (p * (calc_payout(ml, bet) - bet)) + ((1 - p) * -bet)

# Import the parsed data
setwd('/home/larry/Github-Public/538-MLB-Predictions/parsed-data')
preds <- read.csv('538-predictions.csv', header = TRUE, stringsAsFactors = FALSE)
lines <- read.csv('oddsshark-money-lines.csv', header = TRUE, stringsAsFactors = FALSE)
results <- read.csv('game-results-2017.csv', header = TRUE, stringsAsFactors = FALSE)

# Data cleanup
preds <- preds[ , -ncol(preds)]
lines$Date.line <- ymd(str_sub(as.character(lines$Date.line), end = -10))

# This initial analysis will only consider games and lines on a given day
preds <- preds[preds$Date.prediction == preds$Date.game, -1]                 # Remove the Date.prediction col
lines <- lines[lines$Date.line == lines$Date.game, -1]                       # Remove the Date.line col
dates <- intersect(unique(preds$Date.game), unique(lines$Date.game))
preds <- preds[preds$Date.game %in% dates, ]
lines <- lines[lines$Date.game %in% dates, ]
results <- results[results$Date %in% dates, ]
lines <- lines[!is.na(lines$Best), ]                                         # Remove any lines missing all line data
results <- results[!is.na(results$Score.V), ]                                # The NA scores that I checked were due to weather

# Add the game winner to the results data frame
visitor.wins <- results$Score.V > results$Score.H
results$Winner[visitor.wins] <- results$Visitor[visitor.wins]
results$Winner[!visitor.wins] <- results$Home[!visitor.wins]
head(results)
if (any(is.na(results$Winner))) stop(warning("One or more tie games are present in the data"))
tail(results, 6)


# Add the game results to preds
preds$Winner <- NA
preds$Test.V <- NA
preds$Test.H <- NA
for (i in 1:nrow(preds)) {
  tmp <- results[results$Date == preds$Date.game[i] & results$Visitor == preds$Visiting[i] & results$Home == preds$Home[i], ]
  if (nrow(tmp) == 1) {
    preds$Winner[i] <- tmp$Winner
    preds$Test.V[i] <- tmp$Visitor
    preds$Test.H[i] <- tmp$Home
  }
}

preds <- preds[!is.na(preds$Winner), ]

## Sanity Check
if (any(preds$Visiting != preds$Test.V)) stop(warning("Team mismatch #1"))
if (any(preds$Home != preds$Test.H)) stop(warning("Team mismatch #2"))
preds <- preds[ , 1:(ncol(preds) - 2)]
head(preds)


# IMPORTANT - Because a single row in preds can match multiple lines, you must iterate from preds to lines
lines <- lines[ , c('Date.game', 'Team', 'Bovada.lv', 'Caesars', 'Westgate', 'Station', 'Best')]
lines$P <- NA
lines$Winner <- NA
lines$Test <- NA
for (i in 1:nrow(preds)) {
  x <- preds[i, ]
  line1 <- which(lines$Date.game == x$Date.game & lines$Team == x$Visiting)
  line2 <- which(lines$Date.game == x$Date.game & lines$Team == x$Home)
  if (length(line1) == 1) {
    lines$P[line1] <- x$Chance.visiting
    lines$Winner[line1] <- x$Winner
    lines$Test[line1] <- x$Visiting
  }
  if (length(line2) == 1) {
    lines$P[line2] <- x$Chance.home
    lines$Winner[line2] <- x$Winner
    lines$Test[line2] <- x$Home
  }
}

lines <- lines[!is.na(lines$P), ]                         # Remove any lines missing a 538 prediction
lines$Winner <- lines$Team == lines$Winner                # Change Winner to T/F
lines$EV <- mapply(calc_ev, lines$P, lines$Best)          # Add the EV

## Sanity Check
if(any(lines$Team != lines$Test)) stop(warning("Team mismatch #3"))
lines <- lines[ , colnames(lines) != 'Test']
head(lines)



## ---- Simulation ---- ##

# Bet Selection Functions
select_by_ev_cutoff <- function(ev.cutoff, start = 0, randomized = FALSE) {
  bets <- cbind(lines[lines$EV >= ev.cutoff, ], data.frame(start, bet = NA, won = NA, end = NA))
  if (randomized) bets[sample(nrow(bets)), ] else bets
}

# Betting Functions
constant_bet <- function(amt) function(bets) amt

percentage_bet <- function(perc, max.bet = Inf) 
  function(bets) min(bets$start[nrow(bets)] * perc / 100, max.bet)

kelly_bet <- function(max.bet = Inf, kelly.coef = 1) {
  calc_b = function(ml) calc_payout(ml, 1) - 1
  kelly_fraction = function(b, p) (p * (b + 1) - 1) / b
  function(bets) {
    x <- bets[nrow(bets), ]
    b <- calc_b(x$Best)
    bet.amt <- x$start * kelly_fraction(b, x$P) * kelly.coef
    if (bet.amt < 0) bet.amt <- 0
    min(bet.amt, max.bet)
  }
}

# The betting_amount function is allowed to access bets from 1:i
sim <- function(bets, betting_amount) {
  for (i in 1:nrow(bets)) {
    if (i > 1) bets$start[i] <- bets$end[i - 1]
    bet <- betting_amount(bets[1:i, ])
    bets$bet[i] <- bet
    bets$won[i] <- ifelse(bets$Winner[i], calc_payout(ml = bets$Best[i], bet), -bet)
    bets$end[i] <- bets$start[i] + bets$won[i]
  }
  bets
}


f50 <- sim(bets, constant_bet(50))
f100 <- sim(bets, constant_bet(100))
f200 <- sim(bets, constant_bet(200))
p5 <- sim(bets, percentage_bet(5))
p10 <- sim(bets, percentage_bet(10))
p20 <- sim(bets, percentage_bet(20))
p30 <- sim(bets, percentage_bet(30))


for (i in 1:20) {
  start_amount <- 200
  bets <- select_by_ev_cutoff(ev.cutoff = 1, start = start_amount, randomized = TRUE)
  constant_bets <- lapply(c(10, 50), function(amt) sim(bets, constant_bet(amt)))
  percent_bets <- lapply(c(5, 20), function(percent) sim(bets, percentage_bet(percent, max.bet = 20000)))
  kelly_bets <- lapply(c(1, 1.5), function(coef) sim(bets, kelly_bet(kelly.coef = coef, max.bet = 20000)))

  results <- c(constant_bets, percent_bets, kelly_bets)
  max.y <- max(sapply(results, function(x) max(x$end)))
  min.y <- min(0, sapply(results, function(x) min(x$end)))

  plot(rep(start_amount, nrow(bets)), type = 'l', col = 'black', main = sprintf("Sim #%d", i),
       xlab = 'Bet #', ylab = '$', xlim = c(1, nrow(bets)), ylim = c(min.y, max.y))
  lines(constant_bets[[1]]$end, type = 'l', col = 'blue', lty = 1)
  lines(constant_bets[[2]]$end, type = 'l', col = 'blue', lty = 2)
  lines(percent_bets[[1]]$end, type = 'l', col = 'green', lty = 1)
  lines(percent_bets[[2]]$end, type = 'l', col = 'green', lty = 2)
  lines(kelly_bets[[1]]$end, type = 'l', col = 'black', lwd = 1.5, lty = 1)
  lines(kelly_bets[[2]]$end, type = 'l', col = 'black', lwd = 1.5, lty = 2)
}

