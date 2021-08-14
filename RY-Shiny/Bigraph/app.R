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

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Anomalies Transaction"),

    # Sidebar with a slider input for number of bins 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("boxplot")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$boxplot <- renderPlot({
        ggplot(cd, 
               aes(x = price, 
                   y = reorder(location,price)))+
            geom_boxplot(outlier.colour="tan1") +
            xlab("Price") + ylab("Location") +
            ggtitle("Transactions of each place") +
            theme(axis.text.x = element_text(face="bold", color="#000092",
                                             size=8, angle=0),
                  axis.text.y = element_text(face="bold", color="#000092",
                                             size=8, angle=0))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
