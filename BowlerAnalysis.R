library(dplyr)
library(tidyr)
library(plotly)

# Source files
source("cleanbowl.R")
source("bowlerFunction.R")

image_bowlerPrint <- function(player) {
  bowler <- maptoBowler(player)
  image_path <- paste0("www_bowler/", bowler, ".jpg")
  if (file.exists(image_path)) {
    renderImage({
      list(
        src = image_path,
        contentType = "image/jpeg",
        width = "200px",
        height = "200px"
      )
    }, deleteFile = FALSE)
  }
}

BowlerAnalysis <- function(name,func,matchType) {

    # Return when name is NULL at start
    if(is.null(name))
        return()

    # Map bowler name
    bowler <- maptoBowler(name)
    if(matchType == "ODI"){
        file <- paste("./odi/bowler/",bowler,".csv",sep="")
        filesp <- paste("./odi/bowler/",bowler,"sp.csv",sep="")
    } else {
        file <- paste("./tt/bowler/",bowler,".csv",sep="")
        filesp <- paste("./tt/bowler/",bowler,"sp.csv",sep="")
    }

    # Check if file exists in the directory. This check is necessary when moving between matchType
    if(!file.exists(file))
        return()

    if(func =="Avg Wickets at Ground"){
        AvgWktsGround(file,name)
    } else if (func == "Avg Wicket against opposition"){
        AvgWktsOpposition(file,name)
    } else if (func == "Economy Rate"){
        EconRate(file,name)
    } else if (func == "Moving Average"){
        MovingAverage(file,name)
    } else if (func == "Wickets Freq Percent"){
        WktsFreqPercent(file,name)
    } else if (func == "Wickets-Runs plot"){
        WktsRunsPlot(file,name)
    } else if (func == "Wicket Rate"){
        WktRateTT(file,name)
    }

}

# Map bowler names to file
maptoBowler <- function(name){
    bowler <- NULL
    if(name == "Jasprit Bumrah"){
        bowler <- "bumrah"
    } else if (name == "Pat Cummins"){
        bowler = "cummins"
    } else if (name == "Josh Hazlewood"){
        bowler = "hazlewood"
    } else if (name == "Ravindra Jadeja"){
        bowler = "jadeja"
    } else if (name == "Keshav Maharaj"){
        bowler = "keshav"
    } else if (name == "Nathan Lyon"){
        bowler = "lyon"
    } else if (name == "Kagiso Rabada"){
        bowler = "rabada"
    } else if (name == "Rashid Khan"){
        bowler = "rashid"
    } else if (name == "Kemar Roach"){
        bowler = "roach"
    } else if (name == "Mohammed Siraj"){
        bowler = "siraj"
    } else if (name == "Kuldeep Yadav"){
        bowler = "yadav"
    } else if (name == "Adam Zampa"){
        bowler = "zampa"
    } else if (name == "Mitchell Starc"){
        bowler = "starc"
    } else if (name == "T A Boult"){
        bowler = "boult"
    } else if (name == "Shakib Al Hasan"){
        bowler = "shakib"
    } else if (name == "Sunil Narine"){
        bowler = "narine"
    } else if (name == "Shaheen Afridi"){
        bowler = "afridi"
    } else if (name == "Ravichandran Ashwin"){
        bowler = "ashwin"
    } else if (name == "Axar Patel"){
      bowler = "axar"
    } else if (name == "Yuzvendra Chahal"){
      bowler = "chahal"
    } else if (name == "Jason Holder"){
      bowler = "holder"
    } else if (name == "Lungi Ngidi"){
      bowler = "ngidi"
    } else if (name == "Adil Rashid"){
      bowler = "adil"
    } else if (name == "Mitchell Santner"){
      bowler = "santner"
    } else if (name == "Tim Southee"){
      bowler = "southee"
    }
    bowler
}
