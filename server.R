library(shiny)
library(plotly)
library(shinyjs)

# Source files
source("definitions.R")
source("BatsmanAnalysis.R")
source("BowlerAnalysis.R")
source("BatsmanRelPerf.R")
source("BowlerRelPerf.R")
source("status_check.R")


shinyServer(function(input, output,session) {

  # Setup dynamic UI for Batsman Analysis
  output$batsmanList <- renderUI({
    if (input$matchType == "ODI") {
      selectInput('batsman', 'Choose Batsman', choices = odiBatsman, selected = NULL)
    } else {
      selectInput('batsman', 'Choose Batsman', choices = ttBatsman, selected = NULL)
    }
  })
  
  output$batsmanFunctionList <- renderUI({
    selectInput('batsmanFunc', 'Choose Statistics', choices = funcsODITT, selected = NULL)
  })
  
  output$batsmanPlot <- renderPlotly({
    req(input$batsman, input$batsmanFunc)
    BatsmanAnalysis(input$batsman, input$batsmanFunc, input$matchType)
  })
  
  output$batsmanImage <- renderUI({
    req(input$batsman,input$batsmanFunc)
      image_batsmanPrint(input$batsman)
  })
  
  # Setup dynamic UI for Bowler Analysis
  output$bowlerList <- renderUI({
    if (input$matchType1 == "ODI") {
      selectInput('bowler', 'Choose Bowler', choices = odiBowler, selected = NULL)
    } else {
      selectInput('bowler', 'Choose Bowler', choices = ttBowler, selected = NULL)
    }
  })
  
  output$bowlerFunctionList <- renderUI({
    if (input$matchType1 == "ODI") {
    choice1 <- funcs1ODI
  } else {
    choice1 <- funcs1TT
  } 
    selectInput('bowlerFunc', 'Choose Statistics', choices = choice1, selected = NULL)
  })
  
  output$bowlerPlot <- renderPlotly({
    req(input$bowler, input$bowlerFunc)
    BowlerAnalysis(input$bowler, input$bowlerFunc, input$matchType1)
  })
  
  output$bowlerImage <- renderUI({
    req(input$bowler, input$bowlerFunc)
    image_bowlerPrint(input$bowler)
  })
  
  # Setup dynamic UI for Relative Batsman Performance
  output$relBatsmen <- renderUI({
    if (input$matchType3 == "ODI") {
      selectInput('batsmen', 'Choose Batsmen', choices = odiBatsman, selected = NULL, multiple = TRUE)
    } else {
      selectInput('batsmen', 'Choose Batsmen', choices = ttBatsman, selected = NULL, multiple = TRUE)
    }
  })
  
  output$relBatsmenFunction <- renderUI({
    selectInput('batsmenFunc', 'Choose Statistics', choices = funcsRelBatsman, selected = NULL)
  })
  
  output$relBatsmenPlot <- renderPlotly({
    req(input$batsmen, input$batsmenFunc)
    BatsmenRelPerf(input$batsmen, input$batsmenFunc, input$matchType3)
  })
  
  # Setup dynamic UI for Relative Bowler Performance
  output$relBowlers <- renderUI({
    if (input$matchType4 == "ODI") {
      selectInput('bowlers', 'Choose Bowlers', choices = odiBowler, selected = NULL, multiple = TRUE)
    } else {
      selectInput('bowlers', 'Choose Bowlers', choices = ttBowler, selected = NULL, multiple = TRUE)
    }
  })
  
  output$relBowlersFunction <- renderUI({
    if (input$matchType4 == "ODI") {
    choice1 <- funcsRelBowlerODI
  } else {
    choice1 <- funcsRelBowlerTT
  } 
    selectInput('bowlersFunc', 'Choose Statistics', choices = choice1, selected = NULL)
  })
  
  output$relBowlersPlot <- renderPlotly({
    req(input$bowlers, input$bowlersFunc)
    BowlersRelPerf(input$bowlers, input$bowlersFunc, input$matchType4)
  })
  
  # Check the form of the player and display text
  output$playerList <- renderUI({
    if (input$playerType == "Batsman") {
      if (input$matchType5 == "ODI") {
        selectInput('player', 'Choose Player', choices = odiBatsman, selected = NULL)
      } else {
        selectInput('player', 'Choose Player', choices = ttBatsman, selected = NULL)
      }
    } else {
      if (input$matchType5 == "ODI") {
        selectInput('player', 'Choose Player', choices = odiBowler, selected = NULL)
      } else {
        selectInput('player', 'Choose Player', choices = ttBowler, selected = NULL)
      }
    }
  })
  
  # Check the form of the player and display text
  output$status <- renderUI({
    req(input$matchType5, input$playerType, input$player)
    HTML(status_check(input$matchType5, input$playerType, input$player))
  })
  
  # Reset function for each tab
  observeEvent(input$resetBatsman, {
    reset("matchType")
    reset("batsman")
    reset("batsmanFunctionList")
  })
  
  observeEvent(input$resetBowler, {
    reset("matchType1")
    reset('bowler')
    reset('bowlerFunc')
  })
  
  observeEvent(input$resetRelBatsmen, {
    reset("matchType3")
    reset('batsmen')
    reset('batsmenFunc')
  })
  
  observeEvent(input$resetRelBowlers, {
    reset("matchType4")
    reset('bowlers')
    reset('bowlersFunc')
  })
  
  observeEvent(input$resetStatus, {
    reset("matchType5")
    reset('playerType')
    reset('player')
  })
  
})
