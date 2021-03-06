library(shiny)

cd <- read_csv("data/cc_data.csv")
gps <- read_csv("data/gps.csv")
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

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Heatmap of Popularity"),
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



server <- function(input, output) {
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

# Run the application 
shinyApp(ui = ui, server = server)
