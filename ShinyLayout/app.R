#Install Packages

#packages = c('shiny','sf','tmap','clock','tidyverse','lubridate','ggiraph',
#             'ggthemes','viridis','plotly','treemapify','igraph','ggpubr',
#             'readr','mapview',"shinythemes","rgdal","shinyTime")
#for (p in packages){
#    if(!require(p, character.only = T)){
#        install.packages(p)
#    }
#    library(p,character.only = T)
#}
library(raster)
library(sf)
library(tmap)
library(clock)
library(tidyverse)
library(lubridate)
library(ggiraph)
library(ggthemes)
library(viridis)
library(plotly)
library(treemapify)
library(igraph)
library(ggpubr)
library(readr)
library(mapview)
library(shinythemes)
library(rgdal)
library(shinyTime)
library(shiny)




# Read Data
cd <- read.csv("data/cc_data.csv")
gps <- read_csv("data/gps.csv")
loyalty <- read.csv("data/loyalty_data.csv")
## modify data
cd_locations <- unique(cd$location)
cdcount_location <- cd %>% group_by(location) %>% 
    summarize(count = n())
loy_locations <- unique(loyalty$location)
loycount_location <- loyalty %>% group_by(location) %>% 
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
names(cdcount_location) <- c("Location","Number_of_Times_People_Visit","Type")
loycount_location$type <- newvalues[ match(loycount_location$location, oldvalues) ]

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
cd$date <- as.Date(cd$timestamp)
cd_calendar <- cd %>% count(day, location) %>% as_factor()
cd_calendar2 <- cd %>% count(hour, location) %>% as_factor()

car_data <- gps %>% 
  group_by(id,hour) %>% 
  summarise(n = n()) %>% 
  ungroup()

ownership <- read.csv("data/cardowners.csv")
ownership1 <- read.csv("data/cardowners1.csv")
ownership2 <- read.csv("data/cardowners2.csv")
duplicates <- data.frame(last4ccnum = c("1286","6691","6899","9241","1286","1286"),
                         loyaltynum = c("L3288","L6267","L6267","L3288","L3288","L3572"),
                         n = c(15,16,23,13,15,10))



#import files
gps_path <- readRDS("data/gps_path.rds")
bgmap <- raster('data/MC2-tourist.tif')

#plot initial map
tmap_mode("plot")
tm_shape(bgmap) + tm_raster(bgmap,legend.show = FALSE)


# Driver Name to add to gps df later?
car <- read_csv('data/carwithfullname.csv')


##########################################################################

ui <- navbarPage(
    "A Shiny Application for the Analysis of Spatial and Transaction Data"
    ,
    theme = shinytheme("cosmo"),
    tabPanel("Home"
             ,
             fluidPage(
                 titlePanel("Abstract"),
                 mainPanel("The research uses transaction and movement data of employees within a 2-week timespan between January 5th to January 19th, 2014.  In general, people would only use tables, maps or GIS to deal with the geospatial and transaction data, some professionals may employ coding language, which do not have interactive functions and hard to learn in a short time. Hence, a Shiny application is provided for those people who are interested in geospatial data analysis but have less knowledge in programming. The research will use a case from VAST challenge 2021, we aim to help identify suspicious movements and behaviour in the data as well as demonstrate that this application is easy to learn and can be used in similar cases. "
                 )
                 
             )),
    
    navbarMenu("Exploratory Data Analysis"
               ,
                 tabPanel("Bar chart"
                        ,
                        fluidPage(
                            titlePanel("Popular Locations"),
                            sidebarLayout(
                                sidebarPanel(
                                    checkboxInput(inputId = "showdata",
                                                  label = "Show data table",
                                                  value = TRUE),
                                    radioButtons(inputId = "barchartID",
                                                 "Card Type:",
                                                 c("Credit card" = "barchart1",
                                                   "Loyalty card" = "barchart2")),
                                    width = 2),
                                mainPanel(plotOutput("barchart"),
                                          DT::dataTableOutput(outputId = "bartable"))
                                        )
                                )
                        ),
               tabPanel("Heatmap"
                        ,
                        fluidPage(
                            titlePanel("Visit Frequency"),
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons(inputId = "heatmapID",
                                                 "Heatmap Type:",
                                                 c("By Date" = "heatmap1",
                                                   "By Hour" = "heatmap2",
                                                   "By people" = "heatmap3")),
                                    width = 3),
                                mainPanel(plotlyOutput("heatmap"))
                            )
                        )
                        ),          
               tabPanel("Boxplot"
                        ,
                        fluidPage(
                          titlePanel("Anomalies Transaction"),
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              checkboxInput(inputId = "showdata1",
                                            label = "Show data table",
                                            value = TRUE)
                            ),
                            mainPanel(plotOutput("boxplot"),
                                      DT::dataTableOutput(
                                        outputId = "boxtable"
                                      )
                            )
                          )
                        )
                        ),
              tabPanel("Bipartite graph",
                       fluidPage(

                         titlePanel("Cross used cards"),
                         mainPanel(
                           plotOutput("bigraph")
                         )
                         
                       )
                       
                      )
              ),
    tabPanel("Geospatial Analysis",
             ui <- fluidPage(
               
               # Application title
               titlePanel("Employee Movement Tracker"),
               fluidRow(
                 mainPanel(tmapOutput("map")),
                 column(4,actionButton("do","Plot Path"),
                        dateRangeInput(
                          "daterange", h5("Select Date Range:"),
                          start = min(gps_path$day),
                          end = max(gps_path$day),
                          min = min(gps_path$day),
                          max = max(gps_path$day)),
                        fluidRow(column(7,timeInput("timestart","Starting Date Time:",minute.steps = 5)),
                                 column(7,timeInput("timeend","End Date Time:",minute.steps = 5)))
                 )),
               fluidRow(
                 column(12,
                        checkboxGroupInput("empID",
                                           h3("Select Employees"),
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
                                           selected = c(1,2,3,4,5),
                                           inline = TRUE))
                 
               ))),
    tabPanel("Card Ownership"
             ,
             fluidPage(
               titlePanel("Ownership of the cards"),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons(inputId = "ownerID",
                                "Likelihood:",
                                c("Fully Match" = "owner2",
                                  "Approximatly Match" = "owner1",
                                  "Can not Match" = "owner")),
                   width = 3),
                 mainPanel(DT::dataTableOutput("cardowners"))
               )
             )
             ),
    tabPanel("Reference"
             ,
             fluidRow(
               column("[1] Griffin, K. 2014. UCD-Griffin-MC2. VAST Challenge 2014.",width = 12),
               column("[2] Kim, H. 2014. GT-Stasko-MC2. VAST Challenge 2014.", width = 12),
               column("[3] Wood, J. 2014. Centre-Wood-MC2. VAST Challenge 2014.", width = 12),
               column("[4] Zhao, Y. 2014. CSU-Zhao-MC2", width = 12)
             ))
    
    )

server <- function(input, output) {
    
    output$barchart <- renderPlot({
      if(input$barchartID == "barchart1"){
        ggplot(cdcount_location,
               aes(x = Number_of_Times_People_Visit,
                   y = reorder(Location,Number_of_Times_People_Visit),
                   fill = Type,
                   stringr::str_wrap(cdcount_location$Location,15)))+
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
      }
      else if(input$barchartID == "barchart2"){
        ggplot(loycount_location, 
               aes(x = count, 
                   y = reorder(location,count), 
                   fill = type,
                   stringr::str_wrap(loycount_location$location, 15))) +
          geom_col(color = "grey")+
          xlab("Frequency") + ylab("Location") +
          ggtitle("Popularity of each place (Loyalty)") +
          theme(axis.text.x = element_text(face="bold", color="#000092",
                                           size=8, angle=0),
                axis.text.y = element_text(face="bold", color="#000092",
                                           size=8, angle=0),
                panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank())
      }
        
    })
    
    output$bartable <- DT::renderDataTable({
        if(input$showdata){
            DT::datatable(data = cdcount_location %>% select (1:3),
                          options = list(pageLength = 10),
                          rownames = FALSE)
        }
    })
    
    output$heatmap <- renderPlotly({
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
                      #panel.border = element_blank(),
                      #panel.spacing = unit(0.5, "cm"),
                      #panel.grid.major = element_blank(), 
                      #panel.grid.minor = element_blank(),
                      text = element_text(size=7),
                      axis.title.x = element_text(vjust=-5),
                      axis.title.y = element_blank(),
                      legend.position = "top")
        }
        else if(input$heatmapID == "heatmap2"){
            ggplot(complete(cd_calendar2, hour, location), aes(x = hour, y = location)) + 
                scale_x_continuous(breaks = 0:24)+
                geom_tile(aes(fill = n), color = "black", size = 0.1) +
                scale_fill_gradient(low = "light blue", high = "blue", na.value = "white") +
                scale_y_discrete(expand = expansion(add = 1),
                                 limits=rev) +
                labs(title = "Heatmap of Visit Frequency",
                     subtitle = "(Credit card data)",
                     x = "Hour",
                     fill = "Frequency") +
                theme_bw() +
                theme(axis.ticks = element_blank(),
                      panel.spacing = unit(0.5, "cm"),
                      #panel.grid.major = element_blank(), 
                      text = element_text(size=7),
                      axis.title.x = element_text(vjust=-5),
                      legend.position = "top")
        }
        else if(input$heatmapID == "heatmap3"){
            ggplot(car_data,aes(x = hour,y = id,fill = n)) + geom_tile()+
                scale_fill_gradient(low = "light blue", high = "blue")
        }
        
    })
    
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
      if(input$showdata1){
        cd1<- cd 
        cd1$time<-format(as.POSIXct(cd1$timestamp), format = "%H:%M:%S")
        cd1<-cd1%>%
          relocate(date,time,price,location,last4ccnum)
        cd1 <- cd1[order(cd1$time,cd1$date), ]
        cd1<- cd1%>%
          filter(time>="01:00:00"&time<="05:00:00")
        DT::datatable(cd1 %>% select(1:5),
                      options = list(pageLength = 10),
                      rownames = FALSE)
      }
    })
    
    data_filtered <- eventReactive(input$do,{
      idfilt <- gps_path %>% filter(id %in% input$empID)
      timefilt <- idfilt[idfilt$m >=
                           paste(format(input$daterange[1]),
                                 strftime(input$timestart,"%R")) &
                           idfilt$m <=
                           paste(format(input$daterange[2]),
                                 strftime(input$timeend,"%R")),]
      timefilt
    })
    output$map <- renderTmap({
      m <- tm_shape(bgmap)+
        tm_rgb(bgmap, r =1, g = 2, b = 3,
               alpha=NA, saturation = 1, interpolate = TRUE,
               max.value = 255) + 
        tm_shape(data_filtered()) + 
        tm_lines(col="name")
    })
    
    output$cardowners <- DT::renderDataTable({
      if(input$ownerID == "owner"){
        ownership
      }
      else if(input$ownerID == "owner1"){
        ownership1
      }
      else if(input$ownerID == "owner2"){
        ownership2
      }
      
    })
    
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
shinyApp(ui, server)