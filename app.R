library(shiny)

ui <- fluidPage(
   
        headerPanel("Displaying IKC vs. Midterm Grades"),
        sidebarPanel(
                radioButtons("type", "Type of class:",
                            choices = c("045","050","All"),
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

dat <- read_csv("FormattedData.csv")

server <- function(input, output) {
        output$firstPlot <- renderPlot({
                g <- ggplot(dat,aes(x=IKC.Percent,y=Midterm.Percent)) +
                        geom_point()
                g
        })
}

shinyApp(ui = ui, server = server)

