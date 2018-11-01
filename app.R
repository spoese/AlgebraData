library(shiny)
library(tidyverse)

ui <- fluidPage(
        
        headerPanel("Displaying IKC vs. Midterm Grades"),
        sidebarPanel(
                radioButtons("type", "Type of class:",
                             choices = c("45","50","All"),
                             selected = "All",
                             inline = TRUE),
                checkboxGroupInput("data","Which variables would you like to look at?",
                                   choices = c("Test Scores","IKC Scores","Time Spent", "Topics/Hour"),
                                   selected = c("Test Scores","IKC Scores"),
                                   inline = TRUE)
        ),
        mainPanel(
                plotOutput("firstPlot")
        )
)

dat <- as.data.frame(read_csv("FormattedData.csv"))

server <- function(input, output) {
        myDat <- reactive({
                if (input$type == "All"){
                        dat
                } else {
                        filter(dat,Type == input$type)
                }
        })
        output$firstPlot <- renderPlot({
                g <- ggplot(myDat(),aes(x=IKC.Percent,y=Midterm.Percent)) +
                        geom_point() +
                        geom_smooth(method="lm")
                g
        })
}

shinyApp(ui = ui, server = server)

