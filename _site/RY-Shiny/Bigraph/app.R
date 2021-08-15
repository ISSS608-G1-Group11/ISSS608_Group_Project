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
cd$timestamp <- date_time_parse(cd$timestamp,
                                zone = "",
                                format = "%m/%d/%Y %H:%M")
cd$last4ccnum <- as_factor(cd$last4ccnum)
cd$date <- as.Date(cd$timestamp)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Anomalies Transaction"),

    # Sidebar with a slider input for number of bins 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("boxplot"),
           DT::dataTableOutput(outputId = "boxtable")
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
    
    output$boxtable <- DT::renderDataTable({
        cd1<- cd 
        cd1$time<-format(as.POSIXct(cd1$timestamp), format = "%H:%M:%S")
        cd1<-cd1%>%
            relocate(date,time,price,location,last4ccnum)
        cd1 <- cd1[order(cd1$time,cd1$date), ]
        cd1<- cd1%>%
             filter(time>="01:00:00"&time<="05:00:00")
        DT::datatable(cd1 %>% select (1:5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
