# 538-MLB-Predictions

This repository contains 538's MLB upcoming games predictions.



| Models    | Description                                                                                             |
| ----------| ------------------------------------------------------------------------------------------------------- |
| model-1.R | Assumes the 538 predictions are true;  Runs a Monte Carlo simulation to determine the betting outcomes  |
| model-2.R | A Monte Carlo simulation using game result data and various betting strategies (eg. Kelly Criterion)    |



| Download / Parser Files       | Description                                  |
| ----------------------------- | -------------------------------------------- |
| download-all.sh               | Simply invokes the other 3 download scripts  |
| download-bovada.sh            | Downloads the lines from Bovada              |
| download-oddsshark.sh         | Downloads the lines from Oddsshark           |
| download-upcoming-games.sh    | Downloads the 538 MLB game predictions       |
| oddsshark-parser.R            | Parses the Oddshark lines                    |
| upcoming-games-parser.R       | Parses the 538 game predictions              |



| Data Archive Directories      | Description                                                                            |
| ----------------------------- | -------------------------------------------------------------------------------------  |
| upcoming-games                | Contains the gzipped upcoming game predictions from the 538 website                    |
| bovada                        | Contains the lines from the Bovada website;  NOT BEING USED (Reference Only)           |
| game-results                  | Contains game results from the Sportsbookreview website                                |
| mma                           | Contains some MMA lines that needs to be moved to a separate repository                |
| oddsshark                     | Contains money lines from the Oddshark website                                         |
| parsed-data                   | Contains the parsed upcoming-games, oddshark and game-results data used in modeling    |
