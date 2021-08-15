library(shiny)
packages = c('raster','sf','tmap','clock','tidyverse','lubridate','ggiraph',
             'ggthemes','viridis','plotly','treemapify','igraph','ggpubr',
             'readr','mapview')
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

duplicates <- data.frame(last4ccnum = c("1286","6691","6899","9241","1286","1286"),
                         loyaltynum = c("L3288","L6267","L6267","L3288","L3288","L3572"),
                         n = c(15,16,23,13,15,10))
 

ui <- fluidPage(

    # Application title
    titlePanel("Cross used cards"),
        mainPanel(
           plotOutput("bigraph")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bigraph <- renderPlot({
        g <- graph.data.frame(duplicates,directed = TRUE)
        V(g)$type <- bipartite_mapping(g)$type
        col <- c("sky blue", "orange")
        shape <- c("circle", "square")
        E(g)$color <- 'steelblue'
        plot(g, layout = layout.bipartite,
             vertex.color = col[as.numeric(V(g)$type)+1], 
             vertex.size = 15, vertex.label.cex = 0.8,
             vertex.shape = shape[as.numeric(V(g)$type)+1],
             edge.label = E(g)$n, 
             edge.label.cex = 0.8, 
             edge.label.color = "black", 
             legend = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
