setwd('/home/larry/Github-Public/538-MLB-Predictions//models/')
source('models-common.R')

# Import the parsed data
data <- import_data()
head(data$Predictions)
head(data$Lines)
head(data$Results)

preds <- data$Predictions
lines <- data$Lines


## Simulation -------------------------------------------------------------------------------------

start_amt <- 0
bet_amt <- 100
sim <- lapply(1:20, function(n) {
  bets <- select_by_ev_cutoff(2, start = start_amt, randomized = TRUE)[1:30, ]
  bets$Rand <- runif(n = nrow(bets))
  bets$Winner <- bets$Rand <= bets$P
  bets <- calc_bets(bets, constant_bet(bet_amt))
})

# Visualize the simulation results
for (i in 1:length(sim)) {
  plot(sim[[i]]$end, type = 'b', pch = 19, cex = .5, col = 'blue', 
       main = sprintf("Sim #%d", i), xlab = 'Bet #', ylab = 'Amount Win/Lose ($)')
  abline(h = 0)
}

