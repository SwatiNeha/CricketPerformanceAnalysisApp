library(dplyr)
library(tidyr)
library(plotly)

# Source files
source("cleanbowl.R")
source("bowlerFunction.R")

BowlersRelPerf <- function(bowlers,func,matchType) {
    # Map names to files
    file <- NULL
   if (matchType == "ODI"){
        for(i in 1:length(bowlers)){
            bowler <- maptoBowler(bowlers[i])
            file[i] <- paste("./odi/bowler/",bowler,".csv",sep="")

        }
    } else if (matchType == "TT"){
        for(i in 1:length(bowlers)){
            bowler <- maptoBowler(bowlers[i])
            file[i] <- paste("./tt/bowler/",bowler,".csv",sep="")

        }
    }
    # Check if file exists in the directory. This check is necessary when moving between matchType
    if(!file.exists(file[1]))
        return()

    if(matchType == "ODI") {
        if (func =="Economy Rate : Relative"){
          
          relative_economy_rate(file,bowlers)
        }
        else if (func == "Wicket Percentage : Relative"){
          
          relative_wkts_percentage(file,bowlers)
        }
    }

    else if(matchType == "TT") {
        if (func =="Economy Rate : Relative"){
          
          relative_economy_rate(file,bowlers)
        }
        else if (func == "Wicket Percentage : Relative"){
          
          relative_wkts_percentage(file,bowlers)
        }
        else if (func == "Wicket Rate : Relative"){

          relative_wicket_rate_vs_deliveries(file,bowlers)
        }
    }
}
