library(shiny)
library(Matrix)
library(ggplot2)
library(reshape2)
library(gridExtra)

filename = Sys.getenv('DATASET')
if (filename != ""){
  dataset <- readRDS(filename)
}else{
  dataset <- readRDS('dataset.RDS')
}

server <- function(input,output,session)
{
  output$dataset <-  renderUI({
    selectInput("data", "Dataset", choices = names(dataset))
  })
  
  data <- reactive({
    dataset[[input$data]]
  })
  
  genes <- reactive({
    sort(rownames(data()[['data']]))
  })
  
  output$gene <-  renderUI({
    if(!is.null(input$data)) {
      selectizeInput("gene", label = "Gene", 
                   choices=genes(), options = list(maxOptions = 34000))
    }
  })

  get_tsne <- eventReactive(input$do,{
    tsne = data()[['tsne']]
    tsne
  })
  get_tsne_gene <- eventReactive(input$do,{
    tsne = data()[['tsne']]
    tsne$gene = data()[['data']][input$gene,]
    tsne
  })
  get_cluster_labels <- eventReactive(input$do,{
    tsne = data()[['tsne']]
    cluslab = matrix(0,length(unique(data()[['tsne']]$clus)),2)
    rownames(cluslab) = unique(data()[['tsne']]$clus)
    for (clus in unique(data()[['tsne']]$clus))
    {
      cluslab[clus,1] = median(tsne[tsne$clus==clus,1])
      cluslab[clus,2] = median(tsne[tsne$clus==clus,2])
    }
    cluslab = data.frame(cluslab)
    cluslab$clus = rownames(cluslab)
    cluslab
  })
  get_heatmap <- eventReactive(input$do,{
    tsne = data()[['tsne']]
    tsne$gene = data()[['data']][input$gene,]
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
      geom_point(size=0.0001) + theme(legend.position = "none") +
      geom_text(data = get_cluster_labels(), mapping = aes(x = X1, y = X2, label = clus), 
                size = 4, colour='black') + ggtitle(isolate(input$dataset)) + 
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  output$plotgraph2<-renderPlot({
    ggplot(get_tsne_gene(),aes(x=tSNE_1,y=tSNE_2)) + geom_point(aes(colour=gene),size=0.0001) +
      scale_color_gradient(low = 'lightgray', high = 'red') + ggtitle(isolate(input$gene)) + 
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "black"), legend.title=element_blank())
  })
  output$plotgraph3<-renderPlot({
    ggplot(get_tsne(),aes(x=tSNE_1,y=tSNE_2,color=loc)) + geom_point(size=0.0001) +
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
