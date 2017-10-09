library(stringr)
library(lubridate)

## Betting Related Functions ----------------------------------------------------------------------

calc_payout <- function(ml, bet = 100) {
  payout_neg_ml <- function(ml, bet) (bet / ml) * (ml + 100)
  payout_pos_ml <- function(ml, bet) (bet / 100) * (ml + 100)
  if (ml < 0) payout_neg_ml(abs(ml), bet)
  else payout_pos_ml(ml, bet)
}

calc_ev <- function(p, ml, bet = 100) (p * (calc_payout(ml, bet) - bet)) + ((1 - p) * -bet)

# Bet Selection Functions
select_by_ev_cutoff <- function(ev.cutoff, start = 0, randomized = FALSE) {
  bets <- cbind(lines[lines$EV >= ev.cutoff, ], data.frame(start, bet = NA, won = NA, end = NA))
  if (randomized) bets[sample(nrow(bets)), ] else bets
}

# Betting Strategy Functions
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


# Used to calculate the results for the 'bets' data frame
# Requires the following columns:  Winner, Best, start, bet, won, end
# The betting_amount function is one of the betting strategy functions above (eg kelly_betting)
# The betting_amount function is allowed to access bets from 1:i
calc_bets <- function(bets, betting_amount) {
  for (i in 1:nrow(bets)) {
    if (i > 1) bets$start[i] <- bets$end[i - 1]
    bet <- betting_amount(bets[1:i, ])
    bets$bet[i] <- bet
    bets$won[i] <- ifelse(bets$Winner[i], calc_payout(ml = bets$Best[i], bet), -bet)
    bets$end[i] <- bets$start[i] + bets$won[i]
  }
  bets
}


## Import the Parsed Data -------------------------------------------------------------------------

import_data <- function(data_dir = '/home/larry/Github-Public/538-MLB-Predictions/parsed-data') {
  setwd(data_dir)
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
  
  # Sanity Check
  if (any(preds$Visiting != preds$Test.V)) stop(warning("Team mismatch #1"))
  if (any(preds$Home != preds$Test.H)) stop(warning("Team mismatch #2"))
  preds <- preds[ , 1:(ncol(preds) - 2)]
  #head(preds)
  
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
  
  # Sanity Check
  if(any(lines$Team != lines$Test)) stop(warning("Team mismatch #3"))
  lines <- lines[ , colnames(lines) != 'Test']
  # head(lines)
  
  # Return a list that contains all of the data
  list("Predictions" = preds, "Lines" = lines, "Results" = results)
}  

