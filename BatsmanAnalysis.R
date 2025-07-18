library(dplyr)
library(tidyr)
library(plotly)

# Source files
source("clean.R")
source("batsmanFunction.R")

image_batsmanPrint <- function(player) {
  batsman <- maptoBatsman(player)
  image_path <- paste0("www_batsman/", batsman, ".jpg")
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

BatsmanAnalysis <- function(name,func,matchType) {
    # Return when name is NULL at start
    if(is.null(name))
        return()

    # Read file from appropriate directory
    batsman <- maptoBatsman(name)
    if(matchType == "ODI"){
        file <- paste("./odi/batsman/",batsman,".csv",sep="")
        filesp <- paste("./odi/batsman/",batsman,"sp.csv",sep="")
    } else {
        file <- paste("./tt/batsman/",batsman,".csv",sep="")
        filesp <- paste("./tt/batsman/",batsman,"sp.csv",sep="")
    }
    # Check if file exists in the directory. This check is necessary when moving between matchType
    if(!file.exists(file))
        return()

    if(func =="Percentage of Runs"){
        runpercentage(file,name)
    } else if (func == "No. of Fours"){
        scored4s(file,name)
    } else if (func == "No. of Sixes"){
        scored6s(file,name)
    } else if (func == "Ground Avg"){
        groundavg(file,name)
    } else if (func == "Avg Runs scored vs opposition"){
        avgrunsoppo(file,name)
    } else if (func == "Dismissal of Batsman"){
        dismissBatsman(file,name)
    } else if (func == "Mean Strike Rate"){
        meanstrike(file,name)
    } else if (func == "Moving Average"){
        movingavg(file,name)
    } else if (func == "Runs-Freq. Chart"){
        runfrequency(file,name)
    } else if (func == "Run Ranges"){
        runranges(file,name)
    }
}



# Function to map name to file name
maptoBatsman <- function(name){
    batsman <- NULL
    if(name == "Babar Azam"){
        batsman <- "azam"
    } else if (name == "KL Rahul"){
        batsman = "rahul"
    } else if (name == "Jos Buttler"){
        batsman = "buttler"
    } else if (name == "Chris Gayle"){
        batsman = "gayle"
    } else if (name == "Shubman Gill"){
        batsman = "gill"
    } else if (name == "Kane Williamson"){
        batsman = "kane"
    } else if (name == "Dimuth Karunaratne"){
        batsman = "karunaratne"
    } else if (name == "Glenn Maxwell"){
        batsman = "maxwell"
    }else if (name == "Marnus Labuschagne"){
        batsman = "marnus"
    } else if (name == "Daryl Mitchell"){
        batsman = "mitchell"
    } else if (name == "Rishabh Pant"){
        batsman = "pant"
    } else if (name == "Joe Root"){
        batsman = "root"
    } else if (name == "Steve Smith"){
        batsman = "smith"
    }  else if (name == "Rohit Sharma"){
        batsman = "rohit"
    } else if (name == "Virat Kohli"){
        batsman = "kohli"
    } else if (name == "M S Dhoni"){
        batsman = "dhoni"
    } else if (name == "Ben Stokes"){
      batsman = "stokes"
    } else if (name == "David Warner"){
      batsman = "warner"
    } else if (name == "Aiden Markram"){
      batsman = "aiden"
    } else if (name == "Quinton de Kock"){
      batsman = "kock"
    } else if (name == "David Miller"){
      batsman = "miller"
    } else if (name == "Brendon McCullum"){
      batsman = "mccullum"
    } else if (name == "Kyle Mayers"){
      batsman = "mayers"
    } else if (name == "Surya Kumar Yadav"){
      batsman = "surya"
    } else if (name == "Marcus Stoinis"){
      batsman = "stoinis"
    } else if (name == "Rilee Rossouw"){
      batsman = "rilee"
    }
    batsman
}
