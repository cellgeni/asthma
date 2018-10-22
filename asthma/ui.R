library(shiny)

ui <- fluidPage(
  sidebarPanel(uiOutput("dataset"),
               uiOutput("gene"),
               actionButton("do", "Plot / Update", icon=icon("refresh"))),
  mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                 plotOutput("plotgraph1"), plotOutput("plotgraph2"))),
            fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                 plotOutput("plotgraph3"), plotOutput("plotgraph4")))))
