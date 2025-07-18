library(lubridate)
library(forecast)

percentWkts<-function (file)
{
  bowler <- cleanBowl(file)
  wktsDF <- NULL
  wktsTable <- table(bowler$Wkts)
  wktsDF <- as.data.frame(wktsTable)
  wktsDF <- wktsDF[2:nrow(wktsDF), ]
  colnames(wktsDF) <- c("Wickets", "Freq")
  wktsDF$freqPercent <- (wktsDF$Freq/sum(wktsDF$Freq)) * 100
  wktsDF$Wickets <- as.numeric(as.character(wktsDF$Wickets))
  wktsDF <- wktsDF[order(as.numeric(as.character(wktsDF$Wickets))),
  ]
  return(wktsDF)
}

WR <- function (file)
{
  bowler <- cleanBowl(file)
  wktRate <- NULL
  w <- NULL
  for (i in 0:max(as.numeric(as.character(bowler$Wkts)))) {
    balls <- as.numeric(bowler[bowler$Wkts == i, ]$Overs) *
      6
    if (length(balls != 0)) {
      wktRate[i] <- lapply(list(balls), mean)
      w[i] <- i
    }
  }
  a <- sapply(wktRate, is.null)
  wktRate[a] <- NaN
  return(wktRate)
}

ER <- function (file)
{
  bowler <- cleanBowl(file)
  econRate <- NULL
  for (i in 0:max(as.numeric(as.character(bowler$Wkts)))) {
    a <- bowler[bowler$Wkts == i, ]$Econ
    b <- as.numeric(as.character(a))
    econRate[i + 1] <- lapply(list(b), mean)
  }
  return(econRate)
}


WktsRunsPlot <- function(file, name = "A Googly") {
  # Clean data
  b <- cleanBowl(file)

  # Create palette
  p1 <- colorRampPalette(c("red", "blue"))
  palette <- p1(length(unique(b$Wkts)))

  # Create Plotly box plot
  p <- plot_ly(b, x = ~factor(Wkts), y = ~Runs, type = "box", color = ~factor(Wkts), colors = palette) %>%
    layout(title = paste(name, "- Wkts vs Runs given"),
           xaxis = list(title = "Wickets"),
           yaxis = list(title = "Runs conceded"),
           legend = list(title = list(text = "Wickets")),
           showlegend = TRUE)

  # Display the plot
  return(p)
}

WktsFreqPercent <- function(file, name = "A Bowler") {
  bowler <- cleanBowl(file)
  wktsTable <- table(bowler$Wkts)
  wktsDF <- as.data.frame(wktsTable)
  wktsDF <- wktsDF[2:nrow(wktsDF), ]
  colnames(wktsDF) <- c("Wickets", "Freq")
  wktsDF$freqPercent <- (wktsDF$Freq/sum(wktsDF$Freq)) * 100
  wktsDF <- wktsDF[order(as.numeric(as.character(wktsDF$Wickets))), ]
  atitle <- paste(name, "'s", "Wkts freq (%) vs Wkts")

  p <- plot_ly(
    wktsDF,
    x = ~as.numeric(as.character(Wickets)),
    y = ~freqPercent,
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(size = 10, color = 'blue'),
    line = list(width = 3, color = 'blue')
  ) %>%
    layout(
      title = atitle,
      xaxis = list(title = "Wickets"),
      yaxis = list(title = "Wicket Freq Percentages (%)", range = c(0, 50)),
      margin = list(t = 50, b = 50)
    )

  return(p)
}

AvgWktsGround <- function (file, name = "A Chinaman")
{
  Ground <- Wkts <- NULL
  bowler <- cleanBowl(file)
  meanWkts <- bowler %>% group_by(Ground) %>% summarise(Average_Wickets = mean(Wkts))
  countInnings <- bowler %>% group_by(Ground) %>% summarise(Innings = length(Wkts))
  meanWkts <- meanWkts %>% arrange(Ground)
  countInnings <- countInnings %>% arrange(Ground)

  atitle <- paste(name, "'s Average Wickets at Ground")

  p <- plot_ly(
    meanWkts,
    x = ~Ground,
    y = ~Average_Wickets,
    type = 'bar',
    text = ~paste("Ground: ", Ground, "<br>",
                  "Average Wickets: ", round(Average_Wickets, 2), "<br>",
                  "No of Innings: ", countInnings$Innings),
    hoverinfo = "text",
    marker = list(color = rainbow(length(meanWkts$Ground)))
  ) %>%
    layout(
      title = atitle,
      xaxis = list(title = "Ground", tickangle = -45),
      yaxis = list(title = "Average Wickets"),
      shapes = list(
        list(
          type = "line",
          x0 = -0.5,
          x1 = length(meanWkts$Ground) - 0.5,
          y0 = 4,
          y1 = 4,
          line = list(dash = "dot", width = 2, color = "black")
        )
      ),
      margin = list(b = 150),
      annotations = list(
        x = length(meanWkts$Ground) - 1,
        y = 4,
        text = "4 wickets threshold",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "bottom"
      )
    )

  return(p)
}

AvgWktsOpposition <- function(file, name = "A Chinaman") {
  # Clean the data
  bowler <- cleanBowl(file)

  # Calculate mean wickets by opposition
  meanWkts <- bowler %>%
    group_by(Opposition) %>%
    summarise(Average_Wickets = mean(Wkts),
              No_of_Innings = n())

  # Create Plotly bar plot
  p <- plot_ly(meanWkts, x = ~Opposition, y = ~Average_Wickets,
               text = ~paste("Opposition: ", Opposition, "<br>",
                             "Average Wickets: ", round(Average_Wickets, 2), "<br>",
                             "No of Innings: ", No_of_Innings),
               hoverinfo = "text", type = "bar", marker = list(color = rainbow(length(meanWkts$Opposition)))) %>%
    layout(title = paste(name, "'s Average Wickets versus Opposition"),
           xaxis = list(title = "Opposition", tickangle = -45),
           yaxis = list(title = "Average Wickets"),
           showlegend = FALSE)

  return(p)
}

EconRate <- function(file, name = "A Bowler") {
# Clean the data
bowler <- cleanBowl(file)

# Calculate mean economy rate for each wicket count
econRate <- sapply(0:max(as.numeric(as.character(bowler$Wkts))), function(i) {
  a <- bowler[bowler$Wkts == i, ]$Econ
  b <- as.numeric(as.character(a))
  mean(b)
})

# Create Plotly line plot
p <- plot_ly(x = 0:max(as.numeric(as.character(bowler$Wkts))),
             y = econRate,
             type = "scatter",
             mode = "lines+markers",
             marker = list(color = "red", size = 8),
             line = list(color = "red", width = 3),
             text = paste("Wickets: ", 0:max(as.numeric(as.character(bowler$Wkts))), "<br>",
                          "Mean Economy Rate: ", round(econRate, 2)),
             hoverinfo = "text") %>%
  layout(title = paste(name, "'s - Mean Economy rate (%) vs Wickets"),
         xaxis = list(title = "Wickets"),
         yaxis = list(title = "Mean Economy Rate"),
         showlegend = FALSE)

return(p)
}
MovingAverage <- function(file, name = "A Doosra") {
  # Clean the data
  df <- cleanBowl(file)

  # Extracting data
  wickets <- df$Wkts
  date <- dmy(df$Start.Date)

  # Create dataframe
  timeframe <- data.frame(wickets, date)

  # Calculate moving average using loess regression
  moving_avg <- predict(loess(wickets ~ as.numeric(date), timeframe))

  # Create Plotly line plot
  p <- plot_ly(x = timeframe$date,
               y = timeframe$wickets,
               type = "scatter",
               mode = "lines",
               line = list(color = "grey"),
               name = "Wickets taken",
               text = paste("Date: ", timeframe$date, "<br>",
                            "Wickets: ", timeframe$wickets),
               hoverinfo = "text") %>%
    add_trace(x = timeframe$date,
              y = moving_avg,
              type = "scatter",
              mode = "lines",
              line = list(color = "blue"),
              name = "Moving Average",
              text = paste("Date: ", timeframe$date, "<br>",
                           "Moving Average: ", round(moving_avg, 2)),
              hoverinfo = "text") %>%
    layout(title = paste(name, "'s Moving Average (wickets)"),
           xaxis = list(title = "Year"),
           yaxis = list(title = "Wickets"),
           showlegend = TRUE,
           legend = list(x = 0.1, y = 0.9)
    )

  return(p)
}

WktRateTT <- function(file, name = "A Bowler") {
  bowler <- cleanBowl(file)
  wktRate <- NULL
  w <- NULL

  for (i in 0:max(as.numeric(as.character(bowler$Wkts)))) {
    balls <- as.numeric(bowler[bowler$Wkts == i, ]$Overs) * 6
    if (length(balls) != 0) {
      wktRate[i] <- mean(balls)
      w[i] <- i
    }
  }

  a <- sapply(wktRate, is.null)
  wktRate[a] <- NaN

  # Convert to data frame for Plotly
  plot_data <- data.frame(
    Wickets = w,
    Mean_Deliveries = wktRate
  )

  # Plot title
  atitle <- paste(name, "- No. balls vs wkts")

  # Create a Plotly line plot
  p <- plot_ly(plot_data, x = ~Wickets, y = ~Mean_Deliveries, type = 'scatter', mode = 'lines+markers',
               line = list(color = 'red', width = 3), marker = list(symbol = 'circle', size = 8)) %>%
    layout(
      title = atitle,
      xaxis = list(title = "Wickets"),
      yaxis = list(title = "Mean number of deliveries")
    )
  return(p)
}

relative_wicket_rate_vs_deliveries <- function(frames, names) {
  col1 <- rainbow(length(frames))
  p <- plot_ly() # Initialize an empty plotly object

  for (i in 1:length(frames)) {
    bowler <- cleanBowl(frames[[i]])
    wkts <- c(0:max(bowler$Wkts))
    wkts <- wkts[wkts != 0]
    wktRate <- WR(frames[[i]])
    a <- sapply(wktRate, is.null)
    wktRate <- wktRate[!a]
    wkts <- wkts[!a]

    # Add a trace for each frame
    p <- p %>% add_lines(
      x = wkts,
      y = wktRate,
      name = names[i],
      line = list(color = col1[i], width = 3),
      hoverinfo = 'x+y',
      showlegend = TRUE # Show legend only for the first trace
    ) %>%
      add_markers(
        x = wkts,
        y = wktRate,
        name = names[i],
        marker = list(color = col1[i], size = 8, symbol = "circle"),
        hoverinfo = 'x+y',
        showlegend = FALSE
      )
  }

  p <- p %>% layout(
    title = "Relative Wicket Rate vs Deliveries",
    xaxis = list(title = "Wickets", range = c(1, 7)),
    yaxis = list(title = "Mean number of deliveries", range = c(18, 25)),
    legend = list(x = 1, y = 0.5, traceorder = "normal"))

  return(p)
}


relative_economy_rate <- function(frames, names) {
  col1 <- rainbow(length(frames))
  p <- plot_ly() # Initialize an empty plotly object
  
  for (i in 1:length(frames)) {
    bowler <- cleanBowl(frames[[i]])
    wkts <- c(0:max(bowler$Wkts))
    eRate <- ER(frames[[i]])
    
    # Add a trace for each frame
    p <- p %>% add_lines(
      x = wkts, 
      y = eRate, 
      name = names[i],
      line = list(color = col1[i], width = 3),
      hoverinfo = 'x+y'
    )
  }
  
  p <- p %>% layout(
    title = "Relative Economy Rate",
    xaxis = list(title = "Wickets"),
    yaxis = list(title = "Economy Rate"),
    legend = list(x = 1, y = 0.5, traceorder = "normal")
  )
  
  return(p)
}

relative_wkts_percentage <- function(frames, names) {
  col1 <- rainbow(length(frames))
  p <- plot_ly() # Initialize an empty plotly object
  
  for (i in 1:length(frames)) {
    pWkts <- percentWkts(frames[[i]])
    
    # Add a trace for each frame
    p <- p %>% add_lines(
      x = pWkts$Wickets, 
      y = pWkts$freqPercent, 
      name = names[i],
      line = list(color = col1[i], width = 2),
      hoverinfo = 'x+y'
    )
  }
  
  p <- p %>% layout(
    title = "Relative Wickets Percentage",
    xaxis = list(title = "Wickets", range = c(1, 10)),
    yaxis = list(title = "Wicket percentages (%)", range = c(0, 50)),
    legend = list(x = 1, y = 0.5, traceorder = "normal")
  )
  
  return(p)
}

checkBowlerStatus <- function (file, name = "A N Inswinger", alpha = 0.05) 
{
  bowler <- cleanBowl(file)
  wkts <- bowler$Wkts
  len <- length(wkts)
  poplen <- floor(0.9 * len)
  popwkts <- wkts[1:poplen]
  mu <- round(mean(popwkts), 2)
  m <- poplen + 1
  sample <- wkts[m:len]
  xbar <- round(mean(sample), 2)
  s <- round(sd(sample), 2)
  n <- len - poplen
  t <- (xbar - mu)/(s/sqrt(n))
  pValue <- round(pt(t, n, lower.tail = TRUE), 6)
  if (pValue > alpha) {
    str4 <- paste("<b>", name, "</b>'s Current Form Status: This player is <span style='color: darkblue;'><b>In-Form</b></span> because the p value: <b>", pValue, "</b> is greater than alpha= ", alpha)
  } else {
    str4 <- paste("<b>", name, "</b>'s Current Form Status: This player is <span style='color: red;'><b>Out-of-Form</b></span> because the p value: <b>", pValue, "</b> is less than alpha= ", alpha)
  }
  m1 <- paste("<div style='background-color: lightyellow; padding: 10px; border-radius: 5px;'>")
  m2 <- paste("<b>---------------------------------- Current Form status of", name, "--------------------------------</b><br><br>")
  m3 <- paste("> Population size:", poplen, "<br>")
  m4 <- paste("> Mean of population:", mu, "<br>")
  m5 <- paste("> Sample size:", n, "<br>") 
  m6 <- paste("> Mean of sample:", xbar, "<br>")
  m7 <- paste("> SD of sample:", s, "<br><br>")
  m8 <- paste("Null hypothesis H0 :", name, "'s sample average is within 95% confidence interval of population average<br>")
  m9 <- paste("Alternative hypothesis Ha :", name, "'s sample average is below the 95% confidence interval of population average<br><br>")
  m10 <- paste(str4, "<br>")
  m11 <- paste("<b>---------------------------------------------------------------------------------------------------------------------------------------------------</b><br><br>")
  m12 <- paste("</div>")
  m13 <- paste(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
  m13
}
