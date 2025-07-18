library(lubridate)
library(forecast)

scored4s <- function(file, name = "A Hookshot") {
  df <- clean(file)
  x4s <- as.numeric(as.vector(df$X4s))
  runs <- as.numeric(df$Runs)
  atitle = paste(name, "-", "Runs scored vs No of 4s")

  # Create a scatter plot
  p <- plot_ly(
    x = runs,
    y = x4s,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'rgba(255, 0, 0, 0.5)'),
    name = 'Data points'
  ) %>% layout(
    title = atitle,
    xaxis = list(title = "Runs"),
    yaxis = list(title = "Number of 4's"),
    annotations = list(
      x = 50, y = predict(lm(x4s ~ poly(runs, 2, raw = TRUE)), data.frame(runs = 50)),
      text = "50 Runs",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 2
    )
  )

  # Fit a polynomial regression model
  fit2 <- lm(x4s ~ poly(runs, 2, raw = TRUE))
  xx <- seq(from = 0, to = max(runs), by = 20)
  yy <- predict(fit2, data.frame(runs = xx))

  # Add the regression line to the plot
  p <- p %>% add_lines(x = xx, y = yy, line = list(color = 'blue'), name = 'Fit')

  # Add vertical and horizontal lines for 50 and 100 runs
  p <- p %>%
    add_lines(x = c(50, 50), y = c(0, max(x4s)), line = list(dash = 'dot'), inherit = FALSE, showlegend = FALSE) %>%
    add_lines(x = c(100, 100), y = c(0, max(x4s)), line = list(dash = 'dot'), inherit = FALSE, showlegend = FALSE) %>%
    add_lines(x = c(0, max(runs)), y = c(predict(fit2, data.frame(runs = 50)), predict(fit2, data.frame(runs = 50))), line = list(dash = 'dot'), inherit = FALSE, showlegend = FALSE) %>%
    add_lines(x = c(0, max(runs)), y = c(predict(fit2, data.frame(runs = 100)), predict(fit2, data.frame(runs = 100))), line = list(dash = 'dot'), inherit = FALSE, showlegend = FALSE)


  # Return the plot
  return(p)
}

scored6s <- function(file, name = "A Hookshot") {
  df <- clean(file)
  b <- filter(df, X6s != 0)
  x6s <- as.numeric((b$X6s))
  runs <- as.numeric(b$Runs)

  atitle <- paste(name, "-", "Runs scored vs No of 6s")

  # Define the color for the boxplot
  boxplot_color <- "darkblue"

  plot_ly(
    data = b,
    x = ~factor(x6s),
    y = ~runs,
    type = 'box',
    boxpoints = 'all',
    jitter = 0.3,
    pointpos = -1.8,
    marker = list(color = boxplot_color), # Change the boxplot color here
    text = ~paste("Number of 6s:", x6s, "<br>Runs scored:", runs),
    hoverinfo = "text"
  ) %>% layout(
    title = atitle,
    xaxis = list(title = "Number of 6s"),
    yaxis = list(title = "Runs scored")
  )
}

runpercentage <- function(frames, names) {
  batsman4s6s <- NULL

  # Iterate over each frame
  for (i in 1:length(frames)) {
    batsman <- clean(frames[[i]])
    Runs <- sum(batsman$Runs)
    batsman4s <- sum(batsman$X4s * 4)
    batsman6s <- sum(batsman$X6s * 6)
    a <- c(Runs, batsman4s, batsman6s)
    batsman4s6s <- cbind(batsman4s6s, a)
  }

  # Set row and column names
  rownames(batsman4s6s) <- c("Runs(1s,2s,3s)", "4s", "6s")
  colnames(batsman4s6s) <- names

  # Calculate proportions
  prop <- prop.table(batsman4s6s, margin = 2) * 100

  # Create plotly bar plot
  plot_ly(
    x = colnames(prop),
    y = prop[1, ],
    type = 'bar',
    name = rownames(prop)[1],
    marker = list(color = 'rgba(255, 153, 51, 0.7)')
  ) %>%
    add_trace(
      y = prop[2, ],
      name = rownames(prop)[2],
      marker = list(color = 'rgba(51, 153, 255, 0.7)')
    ) %>%
    add_trace(
      y = prop[3, ],
      name = rownames(prop)[3],
      marker = list(color = 'rgba(153, 51, 255, 0.7)')
    ) %>%
    layout(
      barmode = 'stack',
      title = "Runs(1s,2s,3s), 4s, 6s as % of Total Runs",
      yaxis = list(title = "% of total runs")
    )
}

dismissBatsman <- function(file, name = "A squarecut") {
  batsman <- clean(file)
  d <- batsman$Dismissal
  dismissal <- data.frame(table(d))

  lbls <- dismissal$d
  slices <- dismissal$Freq
  pct <- round(slices / sum(slices) * 100)

  atitle <- paste(name, "- Pie chart of dismissals")

  # Create an interactive pie chart with plotly
  p <- plot_ly(
    labels = lbls,
    values = slices,
    type = 'pie',
    textinfo = 'percent',
    insidetextorientation = 'radial',
    hoverinfo = 'label+percent'
  ) %>% layout(
    title = atitle,
    showlegend = TRUE,
    legend = list(title = list(text = '<b>Dismissal Types</b>'))
  )

  # Return the plot
  return(p)
}

meanstrike <- function(file, name = "A Hookshot") {
  batsman <- clean(file)
  atitle <- paste(name, "- Runs vs Balls Faced")

  # Create scatter plot with green data points
  p <- plot_ly(data = batsman, x = ~BF, y = ~Runs, type = 'scatter', mode = 'markers',
               marker = list(color = 'rgba(0, 125, 0, 0.5)', size = 10),showlegend=FALSE) %>%
    layout(title = atitle,
           xaxis = list(title = "Balls Faced"),
           yaxis = list(title = "Runs"),
           hovermode = "closest",showlegend=TRUE)

  # Fit polynomial regression
  fit2 <- lm(Runs ~ poly(BF, 2, raw = TRUE), data = batsman)
  xx <- seq(from = 0, to = max(batsman$BF), by = 5)
  yy <- predict(fit2, data.frame(BF = xx))

  # Add regression line
  p <- add_trace(p, x = xx, y = yy, type = 'scatter', mode = 'lines',
                 line = list(color = 'blue', width = 2), name = 'Regression Line',showlegend=TRUE)

  # Add vertical lines for specific points
  bf_points <- c(50, 100)
  runs <- predict(fit2, data.frame(BF = bf_points))
  p <- add_trace(p, x = bf_points, y = runs, type = 'scatter', mode = 'markers',
                 marker = list(color = c("blue", "red"), size = 10),
                 name = c("BF = 50", "BF = 100"),showlegend=TRUE)

  return(p)
}

runfrequency <- function(file, name = "A Hookshot") {
  df <- clean(file)
  maxi <- (max(df$Runs/10) + 1) * 10
  v <- seq(0, maxi, by = 10)
  a <- hist(df$Runs, breaks = v, plot = FALSE)
  Runs <- a$mids
  RunFrequency <- a$counts
  df1 <- data.frame(Runs, RunFrequency)
  atitle <- paste(name, "'s", " Runs frequency vs Runs")

  # Create plotly histogram
  p <- plot_ly(df1, x = ~Runs, y = ~RunFrequency, type = "scatter", mode = "markers", showlegend=FALSE) %>%
    layout(title = atitle,
           xaxis = list(title = "Runs"),
           yaxis = list(title = "Runs Frequency"),
           hovermode = "closest") %>%
    add_trace(marker = list(color = "red", size = 8),
              x = ~Runs, y = ~RunFrequency, name = "Data Points", showlegend=TRUE)

  # Add smoothed curve
  p <- add_trace(p, type = "scatter", mode = "lines",
                 x = ~Runs, y = ~predict(loess(RunFrequency ~ Runs)),
                 line = list(color = "blue", width = 3), name = "Smoothed Curve",showlegend=TRUE)

  return(p)
}

runranges <-function(file, name = "A Hookshot") {
  df <- clean(file)
  f <- cut(df$Runs, breaks = seq(from = 0, to = 400, by = 20))

  g <- table(f)
  percentRuns <- (g / sum(g)) * 100

  plot_data <- data.frame(
    Run_Range = factor(names(g), levels = names(g)),  # Ensure correct order
    Percent_Runs = as.vector(percentRuns)
  )

  atitle <- paste(name, "Runs % vs Run ranges")

  p <- plot_ly(plot_data, x = ~Run_Range, y = ~Percent_Runs, type = 'bar', name = name,
               marker = list(color = 'blue'))%>%
    layout(
      title = atitle,
      xaxis = list(title = "Runs scored"),
      yaxis = list(title = "% times runs scored in range (%)", range = c(0, 100))
    )

  return(p)
}

groundavg <- function(file, name = "A Latecut") {
  batsman <- clean(file)
  meanRuns <- batsman %>% group_by(Ground) %>% summarise(Average_Runs = mean(Runs), No_of_Innings = n())

  # Create Plotly bar plot
  p <- plot_ly(meanRuns, x = ~Ground, y = ~Average_Runs, text = ~paste("Ground: ", Ground, "<br>",
                                                                       "Average Runs: ", round(Average_Runs, 2), "<br>",
                                                                       "No of Innings: ", No_of_Innings),
               hoverinfo = "text", type = "bar", marker = list(color = rainbow(length(meanRuns$Ground)))) %>%
    layout(title = paste(name, "'s Average Runs at Ground"),
           xaxis = list(title = "Ground", tickangle = -45),
           yaxis = list(title = "Average Runs"),
           showlegend = FALSE,
           shapes = list(
             list(
               type = "line",
               x0 = -0.5,
               x1 = length(meanRuns$Ground) - 0.5,
               y0 = 50,
               y1 = 50,
               line = list(dash = "dot", width = 2, color = "red")
             ),
             list(
               type = "line",
               x0 = -0.5,
               x1 = length(meanRuns$Ground) - 0.5,
               y0 = 100,
               y1 = 100,
               line = list(dash = "dot", width = 2, color = "blue")
             )
           ),
           annotations = list(
             list(
               x = length(meanRuns$Ground) - 1,
               y = 50,
               text = "50 runs average threshold",
               showarrow = TRUE,
               arrowhead = 2,
               ax = 20,
               ay = -40
             ),
             list(
               x = length(meanRuns$Ground) - 1,
               y = 100,
               text = "100 runs average threshold",
               showarrow = TRUE,
               arrowhead = 2,
               ax = 20,
               ay = -40
             )
           ),
           margin = list(b = 150)
    )

  return(p)
}

avgrunsoppo <- function(file, name = "A Latecut") {
  batsman <- clean(file)
  meanRuns <- batsman %>% group_by(Opposition) %>% summarise(Average_Runs = mean(Runs), No_of_Innings = n())

  # Create Plotly bar plot
  p <- plot_ly(meanRuns, x = ~Opposition, y = ~Average_Runs, text = ~paste("Opposition: ", Opposition, "<br>",
                                                                           "Average Runs: ", round(Average_Runs, 2), "<br>",
                                                                           "No of Innings: ", No_of_Innings),
               hoverinfo = "text", type = "bar", marker = list(color = rainbow(length(meanRuns$Opposition)))) %>%
    layout(title = paste(name, "'s Average Runs versus Opposition"),
           xaxis = list(title = "Opposition"),
           yaxis = list(title = "Average Runs"),
           showlegend = FALSE)

  return(p)
}

movingavg <- function(file, name = "A Squarecut") {
  df <- clean(file)
  runs <- df$Runs
  date <- dmy(df$Start.Date)  # Assuming 'Start.Date' column contains date information
  timeframe <- data.frame(runs, date)
  atitle <- paste(name, "'s Moving average (Runs)")

  # Fit a loess model to calculate the moving average
  loess_fit <- loess(runs ~ as.numeric(date), timeframe)

  # Predict the moving average values
  ma <- predict(loess_fit)

  # Create Plotly line plot
  p <- plot_ly() %>%
    add_trace(x = ~date, y = ~runs, type = "scatter", mode = "lines", line = list(color = "grey"), name = "Runs scored") %>%
    add_trace(x = ~date, y = ~ma, type = "scatter", mode = "lines", line = list(color = "blue"), name = "Moving Average") %>%
    layout(title = atitle,
           xaxis = list(title = "Year"),
           yaxis = list(title = "Runs"),
           showlegend = TRUE,
           legend = list(x = 0.02, y = 0.98))

  return(p)
}

relative_mean_strike_rate <- function(frames, names) {
  col1 <- rainbow(length(frames))
  p <- plot_ly() # Initialize an empty plotly object
  
  for (i in 1:length(frames)) {
    batsman <- clean(frames[[i]])
    maxi <- (max(batsman$Runs / 15) + 1) * 15
    v <- seq(0, maxi, by = 15)
    a <- hist(batsman$Runs, breaks = v, plot = FALSE)
    SR <- NULL
    for (j in 2:length(a$breaks)) {
      b <- batsman$Runs > a$breaks[j - 1] & batsman$Runs <= a$breaks[j]
      c <- batsman[b, ]
      SR[j - 1] <- mean(as.numeric(as.character(c$SR)))
    }
    b <- !is.na(SR)
    c <- a$mid[b]
    SR <- SR[b]
    
    # Add lines and markers to the plot
    p <- p %>%
      add_lines(x = c, y = predict(loess(SR ~ c)), name = names[i],
                line = list(color = col1[i], width = 3),
                hoverinfo = 'x+y') %>%
      add_markers(x = c, y = predict(loess(SR ~ c)), name = paste(names[i], "Data Points"),
                  marker = list(symbol = 'circle', color = col1[i], size = 6),
                  hoverinfo = 'text',
                  text = paste('Runs:', c, '<br>Mean SR:', round(SR, 2)), 
                  showlegend=FALSE)
    
  }
  
  p <- p %>%
    layout(title = "Relative Mean Strike Rate",
           xaxis = list(title = "Runs", range = c(0, 250)),
           yaxis = list(title = "Mean Strike Rate", range = c(40, 250)),
           legend = list(x = 1, y = 0.5, traceorder = "normal"))
  
  return(p)
}

relative_freq_run_rate <- function(frames, names) {
  col1 <- rainbow(length(frames))
  p <- plot_ly() # Initialize an empty plotly object

  for (i in 1:length(frames)) {
    g <- seq(from = 5, to = 395, by = 10)
    batsman <- clean(frames[[i]])
    f <- cut(batsman$Runs, breaks = seq(from = 0, to = 400, by = 10))
    m <- table(f)
    percentRuns <- (m / sum(m)) * 100
    pR <- as.data.frame(percentRuns)

    # Add lines and markers to the plot
    p <- p %>%
      add_lines(x = g, y = pR$Freq, name = names[i],
                line = list(color = col1[i], width = 2.5),
                hoverinfo = 'x+y') %>%
      add_markers(x = g, y = pR$Freq, name = paste(names[i], "Data Points"),
                  marker = list(symbol = 'circle', color = col1[i], size = 6),
                  hoverinfo = 'text',
                  text = paste('Runs:', g, '<br>Run Frequency:', round(pR$Freq, 2))
                  , showlegend = FALSE)
  }

  p <- p %>% layout(title = "Relative runs freq (%) vs Runs",
                    xaxis = list(title = "Runs"),
                    yaxis = list(title = "Run frequency Percentages (%)", range = c(0, 50)),
                    legend = list(x = 1, y = 0.5, traceorder = "normal")

  )

  return(p)
}

checkBatsmanStatus<-function (file, name = "A Hitter", alpha = 0.05) 
{
  batsman <- clean(file)
  runs <- batsman$Runs
  len <- length(runs)
  poplen <- floor(0.9 * len)
  popruns <- runs[1:poplen]
  mu <- round(mean(popruns), 2)
  m <- poplen + 1
  sample <- runs[m:len]
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