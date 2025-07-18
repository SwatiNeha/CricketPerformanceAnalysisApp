cleanBowl <-function (file)
{
  BPO <- Overs <- NULL
  df <- read.csv(file, stringsAsFactor = FALSE, na.strings = c(NA,"-"))
  a <- df$Overs != "DNB"
  bowler <- df[a, ]
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c, ]
  c <- complete.cases(bowler)
  bowlerComplete <- bowler[c, ]
  bowlerComplete$Opposition = gsub("^v ", "", bowlerComplete$Opposition)
  if (names(bowlerComplete)[3] == "BPO") {
    bowlerComplete <- mutate(bowlerComplete, Overs = ifelse(BPO == 8, as.numeric(Overs) * 8/6, Overs))
  }
  bowlerComplete
}
