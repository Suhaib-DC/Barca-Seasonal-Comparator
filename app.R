library(shiny)
library(ggplot2)
library(ggsoccer)
library(gridExtra)
library(purrr)
library(scatterpie)
library(plyr);library(dplyr)

# main.R is where all the functions are defined
source("main.R")

# to use with removing players options 
'%notin%' <- Negate('%in%')

# to choose between plots
plot.type.names <- c("Passes to penalty",
                     "Pressure",
                     "Passing Network",
                     "Retain Possission")

# i have no idea what this code for, but I'm afraid to delete it
players1 <- "Select season"
players2 <- "Select season"


ui <- fluidPage(
    
    title = "Barca Seasonal Performance Meter",
    fluidRow(
        
        # for choosing the type of the plot
        column(3,
               h4("The Barca-meter"),
               selectInput('plot.type', 'Plot Type', plot.type.names, "Passes to penalty"),
               br()
        ),
        
        # for choosing two seasons for comparison
        column(4, offset = 1,
               selectInput('season1', 'The First Season', unique(matches$season.season_name), "2010/2011"),
               selectInput('season2', 'The Second Season', unique(matches$season.season_name), "2013/2014")
        ),
        
        
        # select players to see how the performance would be without them
        column(4,
               selectInput('removePlayers1', 'Removing Players', players1 ,multiple = T),
               selectInput('removePlayers2', 'Removing Players', players2 ,multiple = T)
        )
    ),
    
    # The plots
    imageOutput('image1', height = "100%", width = "100%"),
    imageOutput('image2', height = "100%", width = "100%"),
    hr(),
)
# Define server logic required to draw the selected plot
server <- function(input, output, session) {
    
    # drawing the first plot as png
    output$image1 <- renderImage({
        
        # reading events for the selected season 
        events <- read.csv(paste("data/events_", gsub("/", "", input$season1), ".csv", sep = "")) 
        
        if (input$plot.type != "Passing Network") {
            events <- events %>% filter(player.name %notin% input$removePlayers1)
        }
        
        # draw the selected plot
        if (input$plot.type == "Passes to penalty") {
            plot1 <- passes2penalty(events, season = input$season1)
            
        } else if (input$plot.type == "Pressure") {
            plot1 <- pressure.plot(events, season = input$season1)
            
        } else if (input$plot.type == "Passing Network") {
            plot1 <- passing.network.plot(events, lineups.df, season = input$season1)
        }else {
            plot1 <- ballRetain(events)
            
        }
        # A temp file to save the output.
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile, 
            width = 1100*8, 
            height = 770*8,
            res = 72*8)
        print(plot1)
        dev.off()
        
        # Return a list containing the file name
        list(src = outfile,
             contentType = 'image/png',
             width = 1100,
             height = 770,
             alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    # drawing the first plot as png
    output$image2 <- renderImage({
       
        
        # reading events for the selected season 
        events <- read.csv(paste("data/events_", gsub("/", "", input$season2), ".csv", sep = "")) 
        
        if (input$plot.type != "Passing Network") {
            events <- events %>% filter(player.name %notin% input$removePlayers1)
        }
        # draw the selected plot
        if (input$plot.type == "Passes to penalty") {
            plot2 <- passes2penalty(events, season = input$season2)
            
        } else if (input$plot.type == "Pressure") {
            plot2 <- pressure.plot(events, season =  input$season2)
            
        } else if (input$plot.type == "Passing Network") {
            plot2 <- passing.network.plot(events, lineups.df, season = input$season2)
        }else {
            plot2 <- ballRetain(events)
            
        }
        # A temp file to save the output.
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile, 
            width = 1100*8, 
            height = 770*8,
            res = 72*8)
        print(plot2)
        dev.off()
        
        # Return a list containing the file name
        list(src = outfile,
             contentType = 'image/png',
             width = 1100,
             height = 770,
             alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    observe({
        
        # reading events for 2 seasons
        events1 <- read.csv(paste("data/events_", gsub("/", "", input$season1), ".csv", sep = ""))
        events2 <- read.csv(paste("data/events_", gsub("/", "", input$season2), ".csv", sep = ""))
        
        # extract the players names for each season
        players1 <- events1 %>% filter(team.name == "Barcelona") %>% select(player.name) %>% unique() %>% na.omit()
        players2 <- events2 %>% filter(team.name == "Barcelona") %>% select(player.name) %>% unique() %>% na.omit()
        
        # update the select input for each season
        updateSelectInput(session, "removePlayers1",
                          choices = players1)
        
        updateSelectInput(session, "removePlayers2",
                          choices = players2)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
