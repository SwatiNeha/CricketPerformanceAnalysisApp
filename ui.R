library(plotly)
library(shinyjs)

ui <- fluidPage(useShinyjs(),  # Initialize shinyjs
                navbarPage(
                  title = span("Cricket Performance Analysis Application (CPAA)", style = "background-color: lightblue; color:black; font-family: 'Times New Roman';"),
                  
                  # About app tab
                  tabPanel("About App",
                           h2("CPAA - Shiny Analysis of Cricket", style = "color: darkblue; font-weight: bold; font-family: 'Times New Roman';"),
                           div(
                             p("The primary objective of this Shiny app is to analyze player performance in T20 and ODI cricket matches comprehensively. The application aims to provide actionable insights into player strengths, weaknesses, and trends.",
                               style = "font-size: 18px; color: black; font-family: 'Times New Roman';"),
                             p(HTML("The data for this App is taken from ESPN Cricinfo - <a href='https://www.espncricinfo.com/' target=>https://www.espncricinfo.com</a>"), style = "font-size: 18px; color: black; font-family: 'Times New Roman';"),
                             p("Features of the CPAA include:", style = "font-size: 18px; color: black; font-family: 'Times New Roman';"),
                             tags$ul(
                               tags$li("Detailed player performance metrics including batting averages, strike rates, and bowling economies.", style = "font-size: 16px; color: darkblue; font-weight: bold; font-family: 'Times New Roman';"),
                               tags$li("Comparison of player statistics across different matches and series.", style = "font-size: 16px; color: darkblue; font-weight: bold; font-family: 'Times New Roman';"),
                               tags$li("Visualization tools to identify performance trends over time.", style = "font-size: 16px; color: darkblue; font-weight: bold; font-family: 'Times New Roman';"),
                               tags$li("Interactive charts and tables for an enhanced user experience.", style = "font-size: 16px; color: darkblue; font-weight: bold; font-family: 'Times New Roman';")
                             ),
                             p("I hope you find CPAA useful and insightful. For any feedback or suggestions, please feel free to reach out to me.", style = "font-size: 18px; color: black; font-weight: bold; font-family: 'Times New Roman';")
                           )
                  ),
                  
                  # Batsman tab
                  tabPanel("Batsman Analysis",
                           div(titlePanel(div("Batsman Analysis", style = "color: #e91e63; font-weight: bold;"))),
                           fluidRow(
                             column(3,
                                    radioButtons("matchType", label = h4("Match Format", style = "font-weight: bold;"),
                                                 choices = list("ODI" = "ODI",
                                                                "20-20" = "TT"),
                                                 inline = TRUE,
                                                 selected = "ODI"),
                                    uiOutput("batsmanList"),
                                    uiOutput("batsmanFunctionList"),
                                    actionButton("resetBatsman", "Reset", class = "btn-primary")
                             ),
                             # Show a plot of the generated distribution
                             column(6,
                                    plotlyOutput("batsmanPlot")
                             ),
                             column(3,
                                    uiOutput("batsmanImage")
                             )
                           )
                  ),
                  
                  # Bowlers tab
                  tabPanel("Bowlers Analysis",
                           div(titlePanel(div("Bowlers Analysis", style = "color: #3f51b5; font-weight: bold;"))),
                           fluidRow(
                             column(3,
                                    radioButtons("matchType1", label = h4("Match Format", style = "font-weight: bold;"),
                                                 choices = list("ODI" = "ODI",
                                                                "20-20" = "TT"),
                                                 inline = TRUE,
                                                 selected = "ODI"),
                                    uiOutput("bowlerList"),
                                    uiOutput("bowlerFunctionList"),
                                    actionButton("resetBowler", "Reset", class = "btn-primary")
                             ),
                             # Show a plot of the generated distribution
                             column(6,
                                    plotlyOutput("bowlerPlot")
                             ),
                             column(3,
                                    uiOutput("bowlerImage")
                             )
                           )
                  ),
                  
                  # Relative batsmen plot
                  tabPanel("Batsman Relative Performance",
                           div(titlePanel(div("Batsman Relative Performance", style = "color: #ff9800; font-weight: bold;"))),
                           fluidRow(
                             column(3,
                                    radioButtons("matchType3", label = h4("Match Format", style = "font-weight: bold;"),
                                                 choices = list("ODI" = "ODI",
                                                                "20-20" = "TT"),
                                                 inline = TRUE,
                                                 selected = "ODI"),
                                    uiOutput("relBatsmen"),
                                    uiOutput("relBatsmenFunction"),
                                    actionButton("resetRelBatsmen", "Reset", class = "btn-primary")
                             ),
                             # Show a plot of the generated distribution
                             column(6,
                                    plotlyOutput("relBatsmenPlot")
                             )
                           )
                  ),
                  
                  # Bowlers Relative Performance
                  tabPanel("Bowlers Relative Performance",
                           div(titlePanel(div("Bowlers Relative Performance", style = "color: #4caf50; font-weight: bold;"))),
                           fluidRow(
                             column(3,
                                    radioButtons("matchType4", label = h4("Match Format", style = "font-weight: bold;"),
                                                 choices = list("ODI" = "ODI",
                                                                "20-20" = "TT"),
                                                 inline = TRUE,
                                                 selected = "ODI"),
                                    uiOutput("relBowlers"),
                                    uiOutput("relBowlersFunction"),
                                    actionButton("resetRelBowlers", "Reset", class = "btn-primary")
                             ),
                             # Show a plot of the generated distribution
                             column(6,
                                    plotlyOutput("relBowlersPlot")
                             )
                           )
                  ),
                  
                  # Tab for In-Form status for both batsman & bowler
                  tabPanel("Current status",
                           div(titlePanel(div("Current status", style = "color: olive; font-weight: bold;"))),
                           fluidRow(
                             column(3,
                                    radioButtons("matchType5", label = h4("Match Format", style = "font-weight: bold;"),
                                                 choices = list("ODI" = "ODI",
                                                                "20-20" = "TT"),
                                                 inline = TRUE,
                                                 selected = "ODI"),
                                    radioButtons("playerType", label = h4("Player Type", style = "font-weight: bold;"),
                                                 choices = list("Batsman" = "Batsman",
                                                                "Bowler" = "Bowler"),
                                                 inline = TRUE,
                                                 selected = "Batsman"),
                                    uiOutput("playerList"),
                                    actionButton("resetStatus", "Reset", class = "btn-primary")
                             ),
                             column(6,
                                    uiOutput("status")
                             )
                           )
                  )
                ))
