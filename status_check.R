status_check <- function(matchType,playerType,player){
  if(is.null(player))
    return()
  form <- NULL
  if(playerType == "Batsman"){
    batsman <- maptoBatsman(player)
    if(matchType == "ODI"){
      file <- paste("./odi/batsman/",batsman,".csv",sep="")
      
    }
    else {
      file <- paste("./tt/batsman/",batsman,".csv",sep="")
      
    }
    # Check if file exists in the directory. This check is necessary when moving between matchType
    if(!file.exists(file)){
      return()
    }
    form <- checkBatsmanStatus(file,player)
  } else if (playerType == "Bowler"){
    bowler <- maptoBowler(player)
    if(matchType== "ODI"){
      file <- paste("./odi/bowler/",bowler,".csv",sep="")
      
    }
    else {
      file <- paste("./tt/bowler/",bowler,".csv",sep="")
      
    }
    # Check if file exists in the directory. This check is necessary when moving between matchType
    if(!file.exists(file)){
      return()
    }
    form <-checkBowlerStatus(file,player)
  }
  form
}