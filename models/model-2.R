setwd('/home/larry/Github-Public/538-MLB-Predictions//models/')
source('models-common.R')

# Import the parsed data
data <- import_data()
head(data$Predictions)
head(data$Lines)
head(data$Results)

preds <- data$Predictions
lines <- data$Lines
results <- data$Results


## Simulation -------------------------------------------------------------------------------------

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

# Simulation using the game results
# The betting_amount function is allowed to access bets from 1:i
game_result_sim <- function(bets, betting_amount) {
  for (i in 1:nrow(bets)) {
    if (i > 1) bets$start[i] <- bets$end[i - 1]
    bet <- betting_amount(bets[1:i, ])
    bets$bet[i] <- bet
    bets$won[i] <- ifelse(bets$Winner[i], calc_payout(ml = bets$Best[i], bet), -bet)
    bets$end[i] <- bets$start[i] + bets$won[i]
  }
  bets
}


# Run the simulation i times and plot the results
for (i in 1:20) {
  start_amount <- 200
  bets <- select_by_ev_cutoff(ev.cutoff = 1, start = start_amount, randomized = TRUE)
  constant_bets <- lapply(c(10, 50), function(amt) game_result_sim(bets, constant_bet(amt)))
  percent_bets <- lapply(c(5, 20), function(percent) game_result_sim(bets, percentage_bet(percent, max.bet = 20000)))
  kelly_bets <- lapply(c(1, 1.5), function(coef) game_result_sim(bets, kelly_bet(kelly.coef = coef, max.bet = 20000)))

  results <- c(constant_bets, percent_bets, kelly_bets)
  max.y <- max(sapply(results, function(x) max(x$end)))
  min.y <- min(0, sapply(results, function(x) min(x$end)))
  
  div <- ifelse(max.y < 100000, 1, 1000)
  plot(rep(start_amount, nrow(bets)), type = 'l', col = 'black', main = sprintf("Sim #%d", i),
       xlab = 'Bet #', ylab = ifelse(div == 1000, '$ (K)', '$'), 
       xlim = c(1, nrow(bets)), ylim = c(min.y %/% div, max.y %/% div))

  lines(constant_bets[[1]]$end %/% div, type = 'l', col = 'blue', lty = 1)
  lines(constant_bets[[2]]$end %/% div, type = 'l', col = 'blue', lty = 2)
  lines(percent_bets[[1]]$end %/% div, type = 'l', col = 'green', lty = 1)
  lines(percent_bets[[2]]$end %/% div, type = 'l', col = 'green', lty = 2)
  lines(kelly_bets[[1]]$end %/% div, type = 'l', col = 'black', lwd = 1.5, lty = 1)
  lines(kelly_bets[[2]]$end %/% div, type = 'l', col = 'black', lwd = 1.5, lty = 2)
}

