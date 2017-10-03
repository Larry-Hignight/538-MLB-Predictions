library(stringr)
library(lubridate)

extract_dates <- function(x) {
  dates <- str_extract_all(x, '"short_date":"[SMTWF][a-z]{2} [A-Z][a-z]{2} [0-9]+"')[[1]]
  dates <- str_sub(dates, start = 19, end = -2)
  mdy(str_c(dates, " ", year(now())))
}

extract_teams <- function(x) str_extract_all(str_extract_all(x, '"short_name":"[A-Z]+"'), '[A-Z]+')[[1]]

extract_operators <- function(x) {
  operators <- unique(str_extract_all(x, 'class="op-item op-spread op-.*?"')[[1]])
  str_sub(operators, start = 26, end = -2)
}               

extract_lines <- function(x, operator) {
  lines <- str_extract_all(x, str_c(operator, '.*?</div>'))[[1]]
  lines <- str_extract(lines, 'data-op-moneyline.*?,')
  as.numeric(str_extract(lines, '-?[0-9]{3}'))
}


## -- Parse the Oddsshark files -- ##

setwd('/home/larry/Github-Public/538-MLB-Predictions/odds2/')
acc <- data.frame()
for (f in list.files()) {
  print(sprintf("Processing file: %s", f))
  x <- readLines(f)
  x <- x[which(grepl(pattern = 'short_date', x))]  # This value changed from 24 to 14 on 9/26/2017

  # Count the number of games played each day
  days <- str_split(x, 'short_date', simplify = T)[1, ]
  days <- days[-1]
  games <- sapply(days, function(day) length(extract_teams(day)) / 2)
  names(games) <- 1:length(games)

  dates <- extract_dates(x)
  teams <- extract_teams(x)
  operators <- extract_operators(x)
  lines <- sapply(operators, function(op) extract_lines(x, op))
  colnames(lines) <- str_sub(colnames(lines), start = 4)
  df <- data.frame(date.line = dates[1], date.game = dates[1], team = teams)
  df <- cbind(df, lines)

  df$date.game <- dates[unlist(mapply(rep, 1:length(games), games, SIMPLIFY = FALSE))]
  df$best <- apply(df[ , 5:ncol(df)], 1, function(x) {
    if (all(is.na(x))) NA
    else max(x, na.rm = TRUE)
  })

  colnames(df) <- str_to_title(colnames(df))
  acc <- rbind(acc, df)
}

write.csv(acc, file = "../oddsshark-money-lines.csv", row.names = F)
