library(dplyr)
library(tidyr)
library(plotly)

# Source files
source("clean.R")
source("batsmanFunction.R")

BatsmenRelPerf <- function(batsmen,func,matchType) {
    # Map batsmen names
    file <- NULL
    if (matchType == "ODI"){
        for(i in 1:length(batsmen)){
            batsman <- maptoBatsman(batsmen[i])
            file[i] <- paste("./odi/batsman/",batsman,".csv",sep="")
        }
    } else {
        for(i in 1:length(batsmen)){
            batsman <- maptoBatsman(batsmen[i])
            file[i] <- paste("./tt/batsman/",batsman,".csv",sep="")
        }
    }

    # Check if file exists in the directory. This check is necessary when moving between matchType
    if(!file.exists(file[1]))
        return()

    # Call function
    if(func =="Mean Strike Rate : Relative"){

      relative_mean_strike_rate(file,batsmen)

    } else if (func == "Runs Freq Performance : Relative"){
      relative_freq_run_rate(file,batsmen)
    }
}
