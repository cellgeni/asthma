library(shiny)

filename = Sys.getenv('DATASET')
if (filename != ""){
  dataset = readRDS(filename)
}else{
  dataset = readRDS('dataset.RDS')
}

ui <- fluidPage(
  sidebarPanel(selectizeInput("gene", label = "Gene", 
                              choices=sort(rownames(megasrat[[1]][['data']])), 
                              selected=sort(rownames(megasrat[[1]][['data']]))[1]),
               selectInput("dataset", "Dataset", choices = names(megasrat)),
               actionButton("do", "Update View", icon=icon("refresh"))),
  mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                 plotOutput("plotgraph1"), plotOutput("plotgraph2"))),
            fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                 plotOutput("plotgraph3"), plotOutput("plotgraph4")))))
