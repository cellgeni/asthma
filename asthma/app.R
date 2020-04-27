library(shiny)
library(Matrix)
library(ggplot2)
library(reshape2)
library(gridExtra)

filename = Sys.getenv('DATASET')
if (filename != ""){
  megasrat <- readRDS(filename)
}else{
  megasrat <- readRDS('/data/dataset.RDS')
}

ui <- fluidPage(
  titlePanel("Lung Cell Atlas"),
    sidebarPanel(
      selectizeInput(
        "gene", 
        label = "Select or type a Gene name:", 
        choices=sort(rownames(megasrat[[1]][['data']])), 
        selected=sort(rownames(megasrat[[1]][['data']]))[1]
      ),
      selectInput("dataset", "Select a Dataset", choices = names(megasrat)),
      actionButton("do", "Update View", icon=icon("refresh")),
      tags$div(class= "alert alert-info", style = "margin-top: 1rem;", checked = NA,
        tags$p("Please note that the cell type labelled as neutrophils is more likely to be composed of monocytes.")
      )
  ),
    mainPanel(
      conditionalPanel(
        condition="$('html').hasClass('shiny-busy')",
        fluidRow(
          column(
            width = 8,
            tags$div(
              HTML(
                "<br><br><br><br>
                <div class='progress progress-striped active'>
                <div class='progress-bar' style='width: 100%'>
                Loading...</div>
                </div>"
              )
            ), 
            offset = 2
          )
        )
      ),
      conditionalPanel(
        condition="!$('html').hasClass('shiny-busy')",
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), 
                                   plotOutput("plotgraph1"), plotOutput("plotgraph2"))),
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), 
                                   plotOutput("plotgraph3"), plotOutput("plotgraph4"))))
      )
    )

server <- function(input,output,session)
{
  observe({updateSelectizeInput(session, "gene", 
                                choices=sort(rownames(megasrat[[input$dataset]][['data']])), 
                                selected=sort(rownames(megasrat[[input$dataset]][['data']]))[1], server=TRUE)})
  get_tsne <- eventReactive(input$do,{
    tsne = megasrat[[input$dataset]][['tsne']]
    tsne
  })
  get_tsne_gene <- eventReactive(input$do,{
    tsne = megasrat[[input$dataset]][['tsne']]
    tsne$gene = megasrat[[input$dataset]][['data']][input$gene,]
    tsne
  })
  get_cluster_labels <- eventReactive(input$do,{
    tsne = megasrat[[input$dataset]][['tsne']]
    cluslab = matrix(0,length(unique(megasrat[[input$dataset]][['tsne']]$clus)),2)
    rownames(cluslab) = unique(megasrat[[input$dataset]][['tsne']]$clus)
    for (clus in unique(megasrat[[input$dataset]][['tsne']]$clus))
    {
      cluslab[clus,1] = median(tsne[tsne$clus==clus,1])
      cluslab[clus,2] = median(tsne[tsne$clus==clus,2])
    }
    cluslab = data.frame(cluslab)
    cluslab$clus = rownames(cluslab)
    cluslab
  })
  get_heatmap <- eventReactive(input$do,{
    tsne = megasrat[[input$dataset]][['tsne']]
    tsne$gene = megasrat[[input$dataset]][['data']][input$gene,]
    heatmat = matrix(0,length(unique(tsne$clus)),
                     length(unique(tsne$loc)))
    rownames(heatmat) = unique(tsne$clus)
    colnames(heatmat) = unique(tsne$loc)
    for (clus in unique(tsne$clus))
    {
      for (loc in unique(tsne$loc))
      {
        mask = (tsne$clus == clus) & (tsne$loc == loc)
        heatmat[clus,loc] = mean(tsne$gene[mask])
      }
    }
    #nans appear here for scenarios with no cells in a given clus/loc
    heatmat[is.nan(heatmat)] = 0
    heatmat = melt(heatmat)
    names(heatmat) = c('Clusters','Location','value')
    heatmat
  })
  output$plotgraph1<-renderPlot({
    ggplot(get_tsne(),aes(x=tSNE_1,y=tSNE_2,color=clus)) + 
      geom_point(size=1) + theme(legend.position = "none") + 
      geom_text(data = get_cluster_labels(), mapping = aes(x = X1, y = X2, label = clus), 
                size = 4, colour='black') + ggtitle(isolate(input$dataset)) + scale_colour_hue() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  output$plotgraph2<-renderPlot({
    ggplot(get_tsne_gene(),aes(x=tSNE_1,y=tSNE_2)) + geom_point(aes(colour=gene),size=1) +
      scale_color_gradient(low = 'lightgray', high = 'red') + ggtitle(isolate(input$gene)) + 
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "black"), legend.title=element_blank())
  })
  output$plotgraph3<-renderPlot({
    ggplot(get_tsne(),aes(x=tSNE_1,y=tSNE_2,color=loc)) + geom_point(size=1) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.title=element_blank()) + guides(colour = guide_legend(override.aes = list(size=2)))
  })
  output$plotgraph4<-renderPlot({
    ggplot(get_heatmap(), aes(x=Location, y=Clusters)) + 
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = 'white', high = 'red') + 
      theme(axis.title.x=element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "black"), legend.title=element_blank()) +
      theme(axis.text.x = element_text(angle=90, hjust=1), axis.title.y=element_blank())
  })
}

shinyApp(ui, server)
