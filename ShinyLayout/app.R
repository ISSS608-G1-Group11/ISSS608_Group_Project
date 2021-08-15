#Install Packages
packages = c('shiny','raster','sf','tmap','clock','tidyverse','lubridate','ggiraph',
             'ggthemes','viridis','plotly','treemapify','igraph','ggpubr',
             'readr','mapview',"shinythemes","rgdal","shinyTime")
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

# Read Data
cd <- read.csv("data/cc_data.csv")
gps <- read_csv("data/gps.csv")
loyalty <- read_csv("data/loyalty_data.csv")
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
cd$date <- as.Date(cd$timestamp)
cd_calendar <- cd %>% count(day, location) %>% as_factor()
cd_calendar2 <- cd %>% count(hour, location) %>% as_factor()

car_data <- gps %>% 
  group_by(id,hour) %>% 
  summarise(n = n()) %>% 
  ungroup()

ownership <- read.csv("data/cardowners.csv")
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
                          titlePanel("Anomalies Transaction"),
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              checkboxInput(inputId = "showdata1",
                                            label = "Show data table",
                                            value = TRUE),
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
             fluidPage(
               
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
    tabPanel("Dataframe"
             ,
             DT::dataTableOutput("cardowners")),
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
                           paste(input$daterange[1],
                                 input$timestart) &
                           idfilt$m <=
                           paste(input$daterange[2],
                                 input$timeend),]
      timefilt
    })
    output$map <- renderTmap({
      m <- tm_shape(bgmap)+
        tm_rgb(bgmap, r =1, g = 2, b = 3,
               alpha=NA, saturation = 1, interpolate = TRUE,
               max.value = 255)
      m <- m + tm_shape(data_filtered()) + tm_lines(col="name")
      m
    })
    output$cardowners <- DT::renderDataTable({
      ownership
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