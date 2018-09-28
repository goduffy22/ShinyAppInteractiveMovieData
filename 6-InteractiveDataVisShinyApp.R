#Load shiny
#install.packages("shiny")
library("shiny")

#Create SIMPLEST ShinyApp (more advance one after)
ui <- fluidPage("Hello World!")

server <- function(input, output) {}

shinyApp(
  ui = ui,
  server = server
)

#UI with I/O controls
ui <- fluidPage(
  titlePanel("Input and Output"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "number", #Inputs and outputs must
        #have ids so you can distinguish between each input
        #and output in the server function
        label = "Choose a number",
        min = 0,
        max = 100,
        value = 25)),
    mainPanel(
      textOutput(
        outputId = "text"))))

#Create a server that maps input to output
server <- function(input, output) {
  output$text <- renderText({
    paste(
      "You selected ",
      input$number)})
}

#Create a shiny app, wire up UI to server
shinyApp(
  ui = ui,
  server = server
)


#Finally, an interactive data visualisation in Shiny

#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("dplyr")
library(dplyr)
movies <- read.csv("Movies.csv")
colors <- brewer.pal(4, "Set3")

#UI Code
ui <- fluidPage(
  titlePanel("Interactive Movie Data"),
  sidebarLayout(
    mainPanel(
      plotOutput(
        outputId = "plot")),
    sidebarPanel(
      sliderInput(
        inputId = "year",
        label = "Year",
        min = 2000,
        max = 2015,
        value = c(2000, 2016), #This makes it with two sliders
        sep = ""),
      checkboxGroupInput(
        inputId = "rating",
        label = "Rating",
        choices = c("G", "PG", "PG-13", "R"),
        selected = c("G", "PG", "PG-13", "R")
      ),
    textInput(
        inputId = "title",
        label = "Title"
      )
    )))

#Create a server
server <- function(input, output) {
  output$plot <- renderPlot({
    subset <- movies %>%
      filter(Year >= input$year[1] ) %>%
      filter(Year <= input$year[2]) %>%
      filter(Rating %in% input$rating) %>%
      filter(grepl(input$title, Title)) %>%
      as.data.frame()
    plot(
      x = subset$Critic.Score,
      y = subset$Box.Office,
      col = colors[as.integer(subset$Rating)],
      pch = 19,
      cex = 1.5,
      xlim = c(0, 100),
      ylim = c(0, 800),
      xlab = "Critic Score",
      ylab = "Box Office Revenue"
    )
    legend(
      x = "topleft",
      as.character(levels(movies$Rating)),
      col = colors[1:4],
      pch = 19,
      cex = 1.5
    )
  })
}

#Shiny app
shinyApp(
  ui = ui,
  server = server)

#We can now publish it to our own 'on premise webserver'
#using the free open source version of ShinyServer Or
#the paid version of ShinyServerPro OR can deploy shinyapp to the
#cloud using a hosted shinyserver at shinyapps.io, see shiny.rstudio.com
#for more info/options. 
