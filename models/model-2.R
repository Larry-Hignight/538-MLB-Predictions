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

# Run the simulation i times and plot the results
for (i in 1:20) {
  start_amount <- 200
  bets <- select_by_ev_cutoff(ev.cutoff = 1, start = start_amount, randomized = TRUE)[1:50, ]
  constant_bets <- lapply(c(10, 50), function(amt) calc_bets(bets, constant_bet(amt)))
  percent_bets <- lapply(c(5, 20), function(percent) calc_bets(bets, percentage_bet(percent, max.bet = 20000)))
  kelly_bets <- lapply(c(1, 1.5), function(coef) calc_bets(bets, kelly_bet(kelly.coef = coef, max.bet = 20000)))

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

