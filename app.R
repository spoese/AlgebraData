library(shiny)
library(tidyverse)

ui <- fluidPage(
        
        headerPanel("Displaying IKC vs. Midterm Grades"),
        sidebarPanel(
                radioButtons("type", "Type of class:",
                             choices = c("45","50","All"),
                             selected = "All",
                             inline = TRUE),
                radioButtons("where", "Where does the class meet?",
                             choices = c("DL","On-Campus","Anywhere"),
                             selected = "Anywhere",
                             inline = TRUE),
                radioButtons("when", "When did the class start?",
                             choices = c("On-time","Late","Any"),
                             selected = "Any",
                             inline = TRUE),
                checkboxGroupInput("days","How many days/week does the class meet?",
                                   choices = c("0","1","2","3","4"),
                                   selected = c("0","1","2","3","4"),
                                   inline = TRUE)
        ),
        mainPanel(
                h2(textOutput("error")),
                tags$head(tags$style("#error{color: red")),
                plotOutput("firstPlot")
        )
)

dat <- as.data.frame(read_csv("FormattedData.csv"))

server <- function(input, output) {
        xlims <- reactive({
                range(dat$IKC.Percent,na.rm = TRUE)
        })
        ylims <- reactive({
                range(dat$Midterm.Percent, na.rm = TRUE)
        })
        myDat <- reactive({
                if (input$type == "All"){
                        temp <- dat
                } else {
                        temp <- filter(dat,Type == input$type)
                }
                if (input$where == "Anywhere"){
                        temp <- temp
                } else {
                        temp <- filter(temp,Where == input$where)
                }
                if (input$when == "Any"){
                        temp <- temp
                } else {
                        temp <- filter(temp,When == input$when)
                }
                temp <- filter(temp,Days %in% input$days)
                temp
        })
        output$error <- renderText({
                if (dim(myDat())[1] == 0){
                        error <- "Warning: No students fit the filters you chose."
                } else {
                        error <- ""
                }
                error
        })
        output$firstPlot <- renderPlot({
                g <- ggplot(myDat(),aes(x=IKC.Percent,y=Midterm.Percent)) +
                        geom_point() +
                        geom_smooth(method = "lm") +
                        xlim(xlims()) +
                        ylim(ylims())
                g
        })
}

shinyApp(ui = ui, server = server)

