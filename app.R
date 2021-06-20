library(shiny)
library(tidyverse)
library(fuzzyjoin)


ui <- fluidPage(
    
    # Application title
    titlePanel("Join messy data with user approval"),
    
    # Sidebar with file inputs
    sidebarLayout(
        sidebarPanel(
            fileInput("user_info",
                      "your file for base-users goes here", 
                      # accept = ".csv"
            ),
            fileInput("join_info",
                      "the data you want to connect to your base-users goes here",
                      # accept = ".csv"
            ),
            checkboxGroupInput("try",
                               "joins to try",
                               choices = c("exact",
                                           "id_exact&name_by_1",
                                           "id_by_1&name_exact")
            )
        ),
        
        # matched and not matched data
        mainPanel(
            h5("Perfectly matched data"),
            tableOutput("good"),
            h5("Data that has not been matched yet"),
            tableOutput("not_yet")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    user_info <- reactive(read_csv(input$user_info$datapath))
    join_info <- reactive(read_csv(input$join_info$datapath))
    
    
    
    output$good <- renderTable({
        req(input$user_info)
        req(input$join_info)
        inner_join(user_info(), join_info())
    })
    
    output$not_yet <- renderTable({
        req(input$user_info)
        req(input$join_info)
        anti_join(join_info(), user_info())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# a <- read_csv(file.choose())
# b <- read_csv(file.choose())
# 
# left_join(a,b)
# inner_join(a,b)
# 
# anti_join(b, a)
