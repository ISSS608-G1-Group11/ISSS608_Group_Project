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
cd <- read.csv("data/cc_data.csv")
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
               "Katerina’s Café","Kronos Mart","Kronos Pipe and Irrigation",
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



ui <- navbarPage(
    "Mini Challenge 2"
    ,
    tabPanel("Home"
             ,
             fluidPage(
                 titlePanel("Abstract"),
                 mainPanel("Here is for Abstract"
                 )
                 
             )),
    
    navbarMenu("Chart"
               ,
               tabPanel("Map"
                        ,
                        fluidPage(
                            titlePanel("Map"),
                            sidebarLayout(
                                sidebarPanel(
                                    dateRangeInput(inputId = 'Date',
                                                   label = 'Date range of map',
                                                   start = '2014-01-06',
                                                   end = '2014-01-19',
                                                   min = '2014-01-06',
                                                   max = '2014-01-19'
                                    ),
                                    
                                    sliderInput(inputId = 'Timerange',
                                                label = 'choose the time range of car path',
                                                min = 0,
                                                max = 24,
                                                value = c(0,24)),
                                    
                                    checkboxGroupInput(inputId = 'Name',
                                                       label = 'Employee names',
                                                       choices = list("Nils Calixto" = 1,
                                                                      "Lars Azada" = 2,
                                                                      "Felix Balas" = 3,
                                                                      "Ingrid Barranco" = 4,
                                                                      "Isak Baza" = 5,
                                                                      "Linnea Bergen" = 6,
                                                                      "Elsa Orilla" = 7,
                                                                      "Lucas Alcazar" = 8,
                                                                      "Gustav Cazar" = 9,
                                                                      "Ada Campo-Corrente" = 10,
                                                                      "Axel Calzas" = 11,
                                                                      "Hideki Cocinaro" = 12,
                                                                      "Inga Ferro" = 13,
                                                                      "Lidelse Dedos" = 14,
                                                                      "Loreto Bodrogi" = 15,
                                                                      "Isia Vann" = 16,
                                                                      "Sven Flecha" = 17,
                                                                      "Birgitta Frente" = 18,
                                                                      "Vira Frente" = 19,
                                                                      "Stenig Fusil" = 20,
                                                                      "Hennie Osvaldo" = 21,
                                                                      "Adra Nubarron" = 22,
                                                                      "Varja Lagos" = 23,
                                                                      "Minke Mies" = 24,
                                                                      "Kanon Herrero" = 25,
                                                                      "Marin Onda" = 26,
                                                                      "Kare Orilla" = 27,
                                                                      "Isande Borrasca" = 28,
                                                                      "Bertrand Ovan" = 29,
                                                                      "Felix Resumir"=30,
                                                                      "Sten Sanjorge Jr." = 31,
                                                                      "Orhan Strum" = 32,
                                                                      "Brand Tempestad" = 33,
                                                                      "Edvard Vann" = 34,
                                                                      "Willem Vasco-Pais" = 35,
                                                                      "Truck 101" = 101,
                                                                      "Truck 104" = 104,
                                                                      "Truck 105" = 105,
                                                                      "Truck 106" = 106,
                                                                      "Truck 107" = 107),
                                                       selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,
                                                                    15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                                                                    30,31,32,33,34,35,101,104,105,106,107))
                                ),
                                mainPanel("MAP")
                            )
                        )),
               
               tabPanel("Bar chart"
                        ,
                        fluidPage(
                            # Application title
                            titlePanel("Popular locations"),
                            sidebarLayout(
                                sidebarPanel(
                                    checkboxInput(inputId = "showdata",
                                                  label = "Show data table",
                                                  value = TRUE),
                                    width = 2
                                ),
                                mainPanel(plotOutput("barchart"),
                                          DT::dataTableOutput(outputId = "bartable"))
                                        )
                                )
                        ),
               tabPanel("Heatmap"
                        ,
                        fluidPage(
                            #put heatmaps here
                            #add a checkbox to choose different heatmap
                            )),          
               tabPanel("Parallel Chart"
                        ,
                        "here to show parallel chart")),

    navbarMenu("Answer"
               ,
               tabPanel("Q1"
                        ,
                        fluidPage(
                            titlePanel("R Shiny App"),
                            sidebarLayout(
                                sidebarPanel("INput"),
                                mainPanel("OUtput")
                             )
                            )
                        ),
               tabPanel("Q2"
                        ,
                        "Q2"),
               tabPanel("Q3"
                        ,
                        "Q3"),
               tabPanel("Q4&5",
                        "Q4&5")
    ),
    tabPanel("Reference"
             ,
             "Reference"),
    tabPanel("Webpage"
             ,
             "webpage")
    
)

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
shinyApp(ui, server)