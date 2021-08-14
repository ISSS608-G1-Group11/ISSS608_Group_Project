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
cd <- read_csv("data/cc_data.csv")
loyalty <- read_csv("data/loyalty_data.csv")
cd$timestamp <- date_time_parse(cd$timestamp,
                                zone = "",
                                format = "%m/%d/%Y %H:%M")
loyalty$timestamp <- date_time_parse(loyalty$timestamp,
                                     zone = "",
                                     format = "%m/%d/%Y")
cd$date <- as.Date(cd$timestamp)
cd$last4ccnum <- as_factor(cd$last4ccnum)
cd_loyalty <- cd %>% 
    inner_join(loyalty, by = c("date" = "timestamp",
                               "location" = "location",
                               "price" = "price"),
               method = "osa",
               max_dist = 1,
               distance_col = "distance")
cd_loyalty1 <- cd_loyalty %>% 
    group_by(last4ccnum,loyaltynum) %>%
    count() %>%
    ungroup()

loyalty_cd_duplicates <- subset(cd_loyalty1,loyaltynum == "L6267" | loyaltynum == "L3288") 

cd_loyalty_duplicates <- subset(cd_loyalty1,last4ccnum == "1286")
duplicates <- rbind(loyalty_cd_duplicates,cd_loyalty_duplicates)
g <- graph.data.frame(duplicates,directed = TRUE)

V(g)$type <- bipartite_mapping(g)$type
col <- c("sky blue", "orange")
shape <- c("circle", "square")
E(g)$color <- 'steelblue'


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
