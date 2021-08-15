# Import libraries
library(shiny)
library(raster)
library(sf)
library(tmap)
library(clock)
library(tidyverse)
library(rgdal)
library(mapview)
library(shinyTime)

#import files
gps_path <- readRDS("gps_path.rds")
bgmap <- raster('MC2-tourist.tif')

#plot initial map
tmap_mode("plot")
tm_shape(bgmap) + tm_raster(bgmap,legend.show = FALSE)


# Driver Name to add to gps df later?
car <- read_csv('carwithfullname.csv')


# Define UI for application that draws a histogram
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
             
        ))

        # Show a plot of the generated distribution
        
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
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
  
  
  
  
  #Map Plotting
  output$map <- renderTmap({
    m <- tm_shape(bgmap)+
      tm_rgb(bgmap, r =1, g = 2, b = 3,
             alpha=NA, saturation = 1, interpolate = TRUE,
             max.value = 255)
    m <- m + tm_shape(data_filtered()) + tm_lines(col="name")
    m
  })
  }
  
  

# Run the application 
shinyApp(ui = ui, server = server)
