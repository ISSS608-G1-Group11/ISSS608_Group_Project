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
cd_locations <- unique(cd$location)
cdcount_location <- cd %>% group_by(location) %>% 
    summarize(count = n())
oldvalues <- c("Abila Airport","Abila Scrapyard","Abila Zacharo",
               "Ahaggo Museum","Albert's Fine Clothing",
               "Bean There Done That","Brew've Been Served",
               "Brewed Awakenings","Carlyle Chemical Inc.",
               "Chostus Hotel","Coffee Cameleon","Coffee Shack",
               "Desafio Golf Course","Frank's Fuel",
               "Frydos Autosupply n' More","Gelatogalore",
               "General Grocer","Guy's Gyros","Hallowed Grounds",
               "Hippokampos","Jack's Magical Beans","Kalami Kafenion",
               "Katerina's Cafe","Kronos Mart","Kronos Pipe and Irrigation",
               "Maximum Iron and Steel","Nationwide Refinery",
               "Octavio's Office Supplies","Ouzeri Elian",
               "Roberts and Sons","Shoppers' Delight",
               "Stewart and Sons Fabrication","U-Pump")
newvalues <- factor(c("Business","Business","Unknown",
                      "Living","Living","Unknown","Dinning",
                      "Unknown","Business","Living","Dinning",
                      "Dinning","Living","Unknown","Unknown",
                      "Dinning","Living","Dinning","Dinning",
                      "Living","Living","Unknown","Dinning",
                      "Living","Business","Business","Business",
                      "Business","Unknown","Business","Living",
                      "Business","Unknown"
)) 
cdcount_location$type <- newvalues[ match(cdcount_location$location, oldvalues) ]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Popular locations"),
    sidebarLayout(
        sidebarPanel(
            checkboxInput(inputId = "showdata",
                          label = "Show data table",
                          value = TRUE)
        ),
        mainPanel(plotOutput("barchart"),
                  DT::dataTableOutput(outputId = "bartable"))
    )
)
       

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barchart <- renderPlot({
        ggplot(cdcount_location,
               aes(x = count,
                   y = reorder(location,count),
                   fill = type,
                   stringr::str_wrap(cdcount_location$location,15)))+
           geom_col(color = "grey") +
           xlab("Frequency") + ylab("Location") +
           ggtitle("Popularity of each place (Credit)") +
           theme(axis.text.x = element_text(face="bold", color="#000092",
                                            size=8, angle=0),
                 axis.text.y = element_text(face="bold", color="#000092",
                                            size=8, angle=0),
                 panel.background = element_blank(),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank())
    })
    
    output$bartable <- DT::renderDataTable({
        if(input$showdata){
            DT::datatable(data = cdcount_location %>% select (1:3),
                          options = list(pageLength = 10),
                          rownames = FALSE)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
