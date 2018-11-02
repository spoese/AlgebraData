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
                selectInput("plotType","Which type of plot would you like to see?",
                            choices = c("Dotplot","Histogram","Boxplot"),
                            selected = "Dotplot"),
                radioButtons("plotFacet","Split by:",
                             choiceNames = list("045/050",
                                                "None",
                                                "DL/In-class",
                                                "On-time/Late start",
                                                "Days per Week"),
                             choiceValues = list("Type",
                                                 NA,
                                                 "Where",
                                                 "When",
                                                 "Days"),
                             inline = TRUE),
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
                        conditionalPanel(condition = "input.plotType == 'Dotplot'",
                                         column(6,selectInput("y","Variable 2:",
                                                              choices = c("Initial Knowledge Check",
                                                                          "Topic Goals",
                                                                          "Assignments",
                                                                          "Tests",
                                                                          "Overall Grade",
                                                                          "Topics/Hour",
                                                                          "Time Spent"),
                                                              selected = "Tests"),
                                                conditionalPanel(
                                                        condition = "input.y == 'Topic Goals'",
                                                        selectInput("topic.y","Number",
                                                                    choices = c("All",1:8))
                                                ),
                                                conditionalPanel(
                                                        condition = "input.y == 'Assignments'",
                                                        selectInput("assignment.y","Number",
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
                                                        condition = "input.y == 'Tests'",
                                                        selectInput("test.y","Number",
                                                                    choices = c("All",1:3,"Midterm"),
                                                                    selected = "Midterm")
                                                ),
                                                conditionalPanel(
                                                        condition = "input.y == 'Time Spent'",
                                                        selectInput("time.y","Where?",
                                                                    choices = c("Overall","In-class"))
                                                ),
                                                conditionalPanel(
                                                        condition = "input.y == 'Topics/Hour'",
                                                        selectInput("rate.y","Week",
                                                                    choices = c("Aug26","Sep02",
                                                                                "Sep09","Sep16",
                                                                                "Sep23","Sep30",
                                                                                "Oct7","Oct14",
                                                                                "Oct21"))
                                                )
                                         ),
                                         checkboxInput("line","Include regression line?",value=TRUE)
                        )
                )
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Plot",
                                 h2(textOutput("error")),
                                 tags$head(tags$style("#error{color: red")),
                                 plotOutput("firstPlot"),
                                 conditionalPanel(
                                         condition = "input.plotType == 'Histogram'",
                                         h3("Table of Bins"),
                                         tableOutput("histTable")
                                 )),
                        tabPanel("Table",
                                 tableOutput("table"))
                )
        )
)

###############################################################################
dat <- as.data.frame(read_csv("FormattedData.csv"))
dat$Assignment3C <- as.numeric(dat$Assignment3C)
dat$Type <- as.factor(dat$Type)
dat$Where <- as.factor(dat$Where)
dat$When <- as.factor(dat$When)
dat$Days <- as.factor(dat$Days)
get_hist <- function(p) {
        d <- ggplot_build(p)$data[[1]]
        data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}
###############################################################################

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
                        } else if (input$test.x == "Midterm") {
                                "Midterm.Percent"
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
                if (input$y == "Initial Knowledge Check") {
                        "IKC.Percent"
                } else if (input$y == "Topic Goals") {
                        if (input$topic.y == "All") {
                                "Topics.Grade"
                        } else {
                                paste("Topics",input$topic.y,sep="")
                        }
                } else if (input$y == "Assignments") {
                        if (input$assignment.y == "All") {
                                "Assignment.Grade"
                        } else {
                                paste("Assignment",input$assignment.y,sep="")
                        }
                } else if (input$y == "Tests") {
                        if (input$test.y == "All") {
                                "Test.Grade"
                        } else if (input$test.y == "Midterm") {
                                "Midterm.Percent"
                        } else {
                                paste("Test",input$test.y,".Percent",sep="")
                        }
                } else if (input$y == "Overall Grade") {
                        "Total.Grade"
                } else if (input$y == "Topics/Hour") {
                        paste("Rate",input$rate.y,sep=".")
                } else if (input$y == "Time Spent") {
                        if (input$time.y == "Overall") {
                                "Total.Time"
                        } else {
                                "Time.In.Class"
                        }
                }
        })
        xlims <- reactive({
                if (input$x == "Topics/Hour") {
                        range(dat[,55:63],na.rm=TRUE)
                } else if (input$x == "Time Spent") {
                        range(dat[,64:65],na.rm=TRUE)
                } else {                
                        range(select(dat,!!xvar()),na.rm = TRUE)
                }
        })
        ylims <- reactive({
                if (input$y == "Topics/Hour") {
                        temp <- range(dat[,55:63],na.rm=TRUE)
                } else if (input$y == "Time Spent") {
                        temp <- range(dat[,64:65],na.rm=TRUE)
                } else {                
                        temp <- range(select(dat,!!yvar()),na.rm = TRUE)
                }
                temp
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
        makeHist <- reactive({
                max <- max(select(myDat(),!!xvar()))
                min <- min(select(myDat(),!!yvar()))
                bw <- max-min/10
                xlimit <- c(min-bw,max+bw)
                g <- ggplot(myDat(),aes_string(x=xvar())) +
                        geom_histogram(aes(y=..count../sum(..count..)),bins=10,
                                       na.rm = TRUE) +
                        xlim(xlims()+c(-bw,bw)) +
                        ylim(c(0,1)) +
                        theme_fivethirtyeight()
                if (!is.na(input$plotFacet)){
                        g <- g + facet_grid(as.formula(paste(input$plotFacet,"~.")))
                }
                g
        })
        output$firstPlot <- renderPlot({
                if (input$plotType == "Dotplot"){
                        if (is.na(input$plotFacet)) {
                                g <- ggplot(myDat(),aes_string(xvar(),yvar()))
                        } else {
                                g <- ggplot(myDat(),aes_string(xvar(),yvar(),color=input$plotFacet))
                        }
                        g <- g + geom_point(na.rm = TRUE) +
                                xlim(xlims()) +
                                ylim(ylims())
                        if (input$line == TRUE) {
                                g <- g + geom_smooth(method = "lm",na.rm = TRUE)
                        }
                        g <- g + theme_fivethirtyeight()
                } else if (input$plotType == "Histogram") {
                        g <- makeHist()
                } else if (input$plotType == "Boxplot") {
                        g <- ggplot(myDat(),aes_string("factor(0)",xvar())) +
                                geom_boxplot(na.rm = TRUE) +
                                theme_fivethirtyeight()
                        if (!is.na(input$plotFacet)) {
                                g <- g + geom_boxplot(aes_string(input$plotFacet,xvar()))
                        }
                }
                g
        })
        output$table <- renderTable({
                temp <- myDat() %>%
                        select(Student.Name,Class.Name,!!xvar())
                if(input$plotType == "Dotplot"){temp <- cbind(temp,select(myDat(),!!yvar()))}
                temp
        })
        output$histTable <- renderTable({
                p <- get_hist(makeHist())
                ranges <- apply(p,1,function(x) paste("[",max(0,round(x[2],2)),",",min(1,round(x[3],2)),")",sep=""))
                q <- data.frame(Range = ranges, Percent = p[,4]*100)
                q
        })
}

shinyApp(ui = ui, server = server)

