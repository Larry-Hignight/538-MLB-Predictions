# 538-MLB-Predictions

This repository contains 538's MLB upcoming games predictions.



| Models / Files   | Description                                                                                                             |
| -----------------| ----------------------------------------------------------------------------------------------------------------------- |
| model-1.R        | This model assumes the 538 predictions are true;  Runs a Monte Carlo simulation to determine the outcomes of bettering  |
| model-2.R        | This model uses the game result data to simulate results for various betting strategies                                 |



| Download / Parser Files       | Description                                                                            |
| ----------------------------- | -------------------------------------------------------------------------------------- |
| README.md                     | This readme file                                                                       |
| download-all.sh               | A script that simply invokes the other 3 download scripts                              |
| download-bovada.sh            | A script to down the lines from Bovada                                                 |
| download-oddsshark.sh         | A script to download the lines from Oddsshark                                          |
| download-upcoming-games.sh    | A script to download the MLB game predictions published on 538's website in gz format  |
| oddsshark-parser.R            | Used to parse the Oddshark lines                                                       |
| upcoming-games-parser.R       | Used to parsed the 538 game predictions                                                |



| Data Archive Directories      | Description                                                                            |
| ----------------------------- | -------------------------------------------------------------------------------------  |
| upcoming-games                | Contains the gzipped upcoming game predictions from the 538 website                    |
| bovada                        | Contains the lines from the Bovada website;  NOT BEING USED (Reference Only)           |
| game-results                  | Contains game results from the Sportsbookreview website                                |
| mma                           | Contains some MMA lines that needs to be moved to a separate repository                |
| oddsshark                     | Contains money lines from the Oddshark website                                         |
| parsed-data                   | Contains the parsed upcoming-games, oddshark and game-results data used in modeling    |
