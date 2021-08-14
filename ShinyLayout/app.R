#Install Packages
packages = c('shiny','raster','sf','tmap','clock','tidyverse','lubridate','ggiraph',
             'ggthemes','viridis','plotly','treemapify','igraph','ggpubr',
             'readr','mapview',"shinythemes")
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

# Read Data
cd <- read.csv("data/cc_data.csv")
gps <- read_csv("data/gps.csv")
## modify data
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



cd$timestamp <- date_time_parse(cd$timestamp,
                                zone = "",
                                format = "%m/%d/%Y %H:%M")
gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone = "",
                                 format = "%m/%d/%Y %H:%M")
gps$id <- as_factor(gps$id)
gps$hour <- hour(gps$Timestamp) 
cd$day <- day(cd$timestamp) %>% as_factor
cd$hour <- as.numeric(format(cd$timestamp,"%H"))
cd_calendar <- cd %>% count(day, location) %>% as_factor()
cd_calendar2 <- cd %>% count(hour, location) %>% as_factor()

car_data <- gps %>% 
  group_by(id,hour) %>% 
  summarise(n = n()) %>% 
  ungroup()



ui <- navbarPage(
    "Mini Challenge 2"
    ,
    theme = shinytheme("cosmo"),
    tabPanel("Home"
             ,
             fluidPage(
                 titlePanel("Abstract"),
                 mainPanel("Here is for Abstract"
                 )
                 
             )),
    
    navbarMenu("EDA"
               ,
                 tabPanel("Bar chart"
                        ,
                        fluidPage(
                            titlePanel("Popular Locations"),
                            sidebarLayout(
                                sidebarPanel(
                                    width = 2,
                                    checkboxInput(inputId = "showdata",
                                                  label = "Show data table",
                                                  value = TRUE),
                                            ),
                                mainPanel(plotOutput("barchart"),
                                          DT::dataTableOutput(
                                            outputId = "bartable"
                                            )
                                          )
                                        )
                                )
                        ),
               tabPanel("Heatmap"
                        ,
                        fluidPage(
                            titlePanel("Heatmap of "),
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons(inputId = "heatmapID",
                                                 "Heatmap Type:",
                                                 c("By Date" = "heatmap1",
                                                   "By Hour" = "heatmap2",
                                                   "By people" = "heatmap3")),
                                    width = 3),
                                mainPanel(plotOutput("heatmap"))
                            )
                        )
                        ),          
               tabPanel("Boxplot"
                        ,
                        fluidPage(
                          #put Boxplot here
                          )
                        ),
              tabPanel("Bipartite graph",
                       fluidPage( #add Bipartite graph here
                         
                                )
                      )
              ),
    tabPanel("Geospatial Analysis",
             fluidPage("MAp"
               
             )),
    tabPanel("Dataframe"
             ,
             "Dataframe"),
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
    
    output$heatmap <- renderPlot({
        if(input$heatmapID == "heatmap1"){
            ggplot(complete(cd_calendar, day, location), 
                   aes(x = day, y = location)) +
                geom_tile(aes(fill = n), color = "black", size = 0.1) +
                scale_fill_gradient(low = "light blue", 
                                    high = "blue", 
                                    na.value = "light grey") +
                scale_y_discrete(expand = expansion(add = 1),
                                 limits=rev) +
                labs(title = "Heatmap of Visit Frequency",
                     subtitle = "(Credit card data)",
                     x = "Day of Month",
                     fill = "Frequency") +
                theme_bw() +
                theme(axis.ticks = element_blank(),
                      panel.border = element_blank(),
                      panel.spacing = unit(0.5, "cm"),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      text = element_text(size=7),
                      axis.title.x = element_text(vjust=-5),
                      axis.title.y = element_blank(),
                      legend.position = "top")
        }
        else if(input$heatmapID == "heatmap2"){
            ggplot(complete(cd_calendar2, hour, location), aes(x = hour, y = location)) + 
                scale_x_continuous(breaks = 0:24)+
                geom_tile(aes(fill = n), color = "black", size = 0.1) +
                scale_fill_gradient(low = "light grey", high = "black", na.value = "white") +
                scale_y_discrete(expand = expansion(add = 1),
                                 limits=rev) +
                labs(title = "Heatmap of Visit Frequency",
                     subtitle = "(Credit card data)",
                     x = "Hour",
                     fill = "Frequency") +
                theme_bw() +
                theme(axis.ticks = element_blank(),
                      panel.spacing = unit(0.5, "cm"),
                      panel.grid.major = element_blank(), 
                      text = element_text(size=7),
                      axis.title.x = element_text(vjust=-5),
                      legend.position = "top")
        }
        else if(input$heatmapID == "heatmap3"){
            ggplot(car_data,aes(x = hour,y = id,fill = n)) + geom_tile()+
                scale_fill_gradient(low = "light grey", high = "black")
        }
        
    })
}
shinyApp(ui, server)