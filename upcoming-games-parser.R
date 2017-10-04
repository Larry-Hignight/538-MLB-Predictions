library(stringr)
library(lubridate)

## Extract the date for each day and use it as a name for day
## Note - vector / list names have to be a character
extract_date <- function(x) {
  date <- str_extract(x, str_c(", .*</time>"))
  if (length(date) != 1) stop(warning("date should be of length 1"))
  date <- str_sub(date, 3, -8)
  mdy(paste(date, year(now()), collapse = " "))
}

extract_data_index2 <- function(x = upcoming) as.numeric(str_extract(extract_data_index(x), "[0-9]"))

# 'data-index' that can be used to split the games
# alt="team" is the only place that an alt tag is used
# the pitching is often incomplete several days out
extract_data_index <- function(x = upcoming) str_extract_all(str_extract_all(x, 'data-index="[0-9]+-[0-9]+"'), "[0-9]+-[0-9]+")[[1]]
extract_teams <- function(x = upcoming) str_extract_all(str_extract_all(x, 'alt="[A-Z]+"'), "[A-Z]+")[[1]]
extract_chance <- function(x = upcoming) as.numeric(str_extract_all(str_extract_all(x, '"chance">[0-9]+%'), '[0-9]+')[[1]]) / 100
extract_pitcher <- function(x = upcoming) str_extract_all(x, '[A-Za-z]+ vs. [A-Za-z]+')[[1]]

acc <- data.frame()
setwd('/home/larry/Github-Public/538-MLB-Predictions/upcoming-games/')
for (f in list.files()) {
  print(sprintf("Processing file: %s", f))

  ## This first section reads the input file and splits the upcoming game predictions by day
  x <- readLines(f)
  upcoming <- x[15]                                                           # Line 15 contains the upcoming game predictions
  start <- str_locate(upcoming, '<div class="games index-upcoming">')[1]      # Start of the upcoming game predictions
  end <- str_locate(upcoming, '<div class="notes">')[1]                       # End of the upcoming game predictions
  upcoming <- str_sub(upcoming, start, end)                                   # Remove everything proceeding the game predictions

  split.days <- '<time class="subhead subhead-index-upcoming show-desktop"'   # Used to split each days predictions
  idx <- str_locate_all(upcoming, split.days)[[1]][ , 1]                      # Start of each days upcoming game predictions
  day <- str_sub(upcoming, start = idx,                                       # Upcoming game predictions split by day
                 end = c(idx[-1] - str_length(split.days), str_length(upcoming)))
  length(day)

  names(day) <- sapply(day, function(x) as.character(extract_date(x)))
  names(day)

  teams <- extract_teams()
  chance <- extract_chance()
  pitchers <- extract_pitcher()

  # Extracts the first number from the data-index pattern
  ymd(names(day)[1]) + extract_data_index2()


  df <- data.frame(date.prediction = ymd(names(day)[1]),
                   date.game = ymd(names(day)[1]) + extract_data_index2(),
                   index = extract_data_index(),
                   visiting = teams[seq(1, length(teams), by = 2)],
                   home = teams[seq(2, length(teams), by = 2)],
                   chance.visiting = chance[seq(1, length(chance), by = 2)],
                   chance.home = chance[seq(2, length(chance), by = 2)])
  df$pitchers <- c(pitchers, rep('unknown', nrow(df) - length(pitchers)))

  ## Perform a quick sanity check
  if(any(df$chance.visiting + df$chance.home != 1.0)) stop(warning("There appears to be an issue with the probabilities"))

  ## Merge with the accumulator
  acc <- rbind(acc, df)
}

colnames(acc) <- str_to_title(colnames(acc))
write.csv(acc, file = "../parsed-data/538-predictions.csv", row.names = F)
