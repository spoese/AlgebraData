library(shiny)
library(tidyverse)
library(ggthemes)

ui <- fluidPage(
        
        headerPanel("Data for Algebra (through Oct. 28)"),
        sidebarPanel(
                h4(strong("Filters:")),
                textInput("CRN","CRNs: (separate with commas, leave blank for any)"),
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
                                   inline = TRUE),
                h4(strong("Data:")),
                checkboxInput("line","Include regression line?",value=TRUE),
                fluidRow(
                        column(6,selectInput("x","Variable 1:",
                                     choices = c("Initial Knowledge Check",
                                                 "Topic Goals",
                                                 "Assignments",
                                                 "Tests",
                                                 "Overall Grade",
                                                 "Topics/Hour",
                                                 "Time Spent"),
                                     selected = "Initial Knowledge Check"),
                               conditionalPanel(
                                       condition = "input.x == 'Topic Goals'",
                                       selectInput("topic.x","Number",
                                                   choices = c("All",1:8))
                               ),
                               conditionalPanel(
                                       condition = "input.x == 'Assignments'",
                                       selectInput("assignment.x","Number",
                                                   choices = c("All","1A","1B",
                                                               "2A","2B","2C",
                                                               "3A","3B","3C",
                                                               "4A","4B",
                                                               "5A","5B",
                                                               "6A","6B",
                                                               "7A","7B",
                                                               "8A","8B",
                                                               "9A","9B"))
                               ),
                               conditionalPanel(
                                       condition = "input.x == 'Tests'",
                                       selectInput("test.x","Number",
                                                   choices = c("All",1:3,"Midterm"))
                               ),
                               conditionalPanel(
                                       condition = "input.x == 'Time Spent'",
                                       selectInput("time.x","Where?",
                                                   choices = c("Overall","In-class"))
                               ),
                               conditionalPanel(
                                       condition = "input.x == 'Topics/Hour'",
                                       selectInput("rate.x","Week",
                                                   choices = c("Aug26","Sep02",
                                                               "Sep09","Sep16",
                                                               "Sep23","Sep30",
                                                               "Oct7","Oct14",
                                                               "Oct21"))
                               )
                               ),
                        column(6,radioButtons("y","Variable 2:",
                                     choices = c("Initial Knowledge Check",
                                                 "Test 1",
                                                 "Test 2",
                                                 "Test 3",
                                                 "Midterm",
                                                 "Topic Goals",
                                                 "Assignments",
                                                 "All Tests",
                                                 "Overall Grade",
                                                 "Total Time"),
                                     selected = "Midterm")))
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Plot",
                                 h2(textOutput("error")),
                                 tags$head(tags$style("#error{color: red")),
                                 plotOutput("firstPlot")),
                        tabPanel("Table",
                                 tableOutput("table"))
                )
        )
)

dat <- as.data.frame(read_csv("FormattedData.csv"))
correspond <- data.frame(Choice = c("Initial Knowledge Check",
                                    "Test 1",
                                    "Test 2",
                                    "Test 3",
                                    "Midterm",
                                    "Topic Goals",
                                    "Assignments",
                                    "All Tests",
                                    "Overall Grade",
                                    "Total Time"),
                         Name = c("IKC.Percent",
                                  "Test1.Percent",
                                  "Test2.Percent",
                                  "Test3.Percent",
                                  "Midterm.Percent",
                                  "Topics.Grade",
                                  "Assignment.Grade",
                                  "Test.Grade",
                                  "Total.Grade",
                                  "Total.Time"))

server <- function(input, output) {
        xvar <- reactive({
                if (input$x == "Initial Knowledge Check") {
                        "IKC.Percent"
                } else if (input$x == "Topic Goals") {
                        if (input$topic.x == "All") {
                                "Topics.Grade"
                        } else {
                                paste("Topics",input$topic.x,sep="")
                        }
                } else if (input$x == "Assignments") {
                        if (input$assignment.x == "All") {
                                "Assignment.Grade"
                        } else {
                                paste("Assignment",input$assignment.x,sep="")
                        }
                } else if (input$x == "Tests") {
                        if (input$test.x == "All") {
                                "Test.Grade"
                        } else {
                                paste("Test",input$test.x,".Percent",sep="")
                        }
                } else if (input$x == "Overall Grade") {
                        "Total.Grade"
                } else if (input$x == "Topics/Hour") {
                        paste("Rate",input$rate.x,sep=".")
                } else if (input$x == "Time Spent") {
                        if (input$time.x == "Overall") {
                                "Total.Time"
                        } else {
                                "Time.In.Class"
                        }
                }
        })
        yvar <- reactive({
                as.character(correspond$Name[grep(input$y,correspond$Choice)])
        })
        xlims <- reactive({
                range(select(dat,!!xvar()),na.rm = TRUE)
        })
        ylims <- reactive({
                range(select(dat,!!yvar()), na.rm = TRUE)
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
                if (input$CRN == ""){
                        temp <- temp
                } else {
                        CRNs <- str_split(input$CRN,",",simplify=TRUE)
                        temp <- filter(temp,Class.Name %in% CRNs)
                }
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
                g <- ggplot(myDat(),aes_string(xvar(),yvar())) +
                        geom_point() +
                        xlim(xlims()) +
                        ylim(ylims()) +
                        theme_fivethirtyeight()
                if (input$line == TRUE) {
                        g <- g + geom_smooth(method = "lm")
                }
                g
        })
        output$table <- renderTable({
                myDat() %>%
                        select(Student.Name,Class.Name,!!xvar(),!!yvar())
        })
}

shinyApp(ui = ui, server = server)

