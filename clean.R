clean <-function (file)
{
  df <- read.csv(file, stringsAsFactor = FALSE, na.strings = c(NA,"-"))
  a <- df$Runs != "DNB"
  batsman <- df[a, ]
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c, ]
  d <- batsman$Runs != "absent"
  batsman <- batsman[d, ]
  batsman$Runs <- as.numeric(gsub("\\*", "", batsman$Runs))
  c <- complete.cases(batsman)
  batsmanComplete <- batsman[c, ]
  batsmanComplete$Opposition = gsub("^v ", "", batsmanComplete$Opposition)
  list(val = dim(batsmanComplete), names = names(batsmanComplete),
       h = head(batsmanComplete))
  batsmanComplete
}
